# LLVM Design: Optimizing Unaligned HWASan Memory Access Checks (AArch64)

## 1. Problem Statement & Motivation

In Hardware-assisted AddressSanitizer (HWASan) on AArch64, memory tags are assigned to **16-byte granules** in shadow memory.

### Current Behavior
- **Aligned Accesses (`align >= size`):** The access is guaranteed to reside within a single 16-byte granule. LLVM emits a 1-instruction call to a specialized register-specific stub:
  ```arm64
  bl __hwasan_check_x19_3_short_v2
  ```
  In LLVM TableGen (`AArch64InstrInfo.td`), this stub is defined with `Defs = [ X16, X17, LR, NZCV ]`. Because it only clobbers scratch registers `x16`, `x17`, `lr`, and flags, **`x0`–`x15` and `x19`–`x28` are preserved, requiring zero register spills**.

- **Unaligned Accesses (`align < size`):** When accessing memory through byte-aligned pointers (such as `const unsigned char *in` in crypto routines), the access could potentially cross a 16-byte granule boundary. Because the standard stubs only check a single tag byte, LLVM's `HWAddressSanitizer.cpp` falls back to emitting generic C runtime calls:
  ```arm64
  mov x0, x22             // Set pointer argument
  mov w1, #0x8            // Set size argument
  bl  __hwasan_loadN@plt  // Standard C runtime call
  ```

### The Problem
Because `__hwasan_loadN` and `__hwasan_storeN` are standard C functions following the AAPCS64 ABI:
1. **Instruction Expansion at Callsite:** Expands from 1 instruction to 3–4 instructions (`mov x0`, `mov w1`, `bl`).
2. **PLT Overhead:** In shared libraries, calls go through the PLT trampoline (`@plt`).
3. **Severe Register Spilling:** LLVM treats all caller-saved registers (`x0`–`x17`) and NEON registers as destroyed across the call. This forces the register allocator to spill local variables to stack slots and save significantly more callee-saved registers (`x19`–`x28`) in function prologues.

In `libcrypto.so`, register spills and reloads caused by these calls account for **+56,526 extra instructions (28.6% of total `.text` bloat)**.

---

## 2. Option A: Register-Preserving Unaligned Stubs (Recommended)

### Overview
Extend LLVM's outlined check mechanism to support unaligned accesses. Instead of falling back to the standard C function `__hwasan_loadN(x0, size)`, emit an outlined call to an unaligned register-specific stub:
```arm64
bl __hwasan_check_unaligned_x22_3_short_v2
```

Like the aligned stubs, this pseudo-instruction specifies `Defs = [ X16, X17, LR, NZCV ]`, guaranteeing that `x0`–`x15` are preserved.

```text
Caller Function (.text)
+-----------------------------------------------------------------------------+
| bl __hwasan_check_unaligned_x22_3_short_v2 (1 instruction, no spills)       |
+-----------------------------------------------------------------------------+
                                       |
                                       v
Shared Unaligned Stub (.text.hot)
+-----------------------------------------------------------------------------+
| 1. Compute start shadow address: sbfx x16, x22, #4, #52                     |
| 2. Check boundary crossing:      (x22 & 15) + Size > 16                     |
|                                                                             |
| [No Boundary Cross]                                [Crosses 16B Boundary]   |
|   ldrb w16 + cmp tag                                 ldrb tag[0] && tag[1]  |
|          |                                                    |             |
|          +-------------------------+--------------------------+             |
|                                    |                                        |
|             [Tag Match]            |            [Tag Mismatch]              |
|                  v                 |                   v                    |
|          ret (x0-x15 untouched)    |        b __hwasan_tag_mismatch_v2      |
+-----------------------------------------------------------------------------+
```

---

### Implementation Details in LLVM

#### 1. `llvm/include/llvm/IR/IntrinsicsAArch64.td`
Define new intrinsics for unaligned memaccess checks:
```tablegen
def int_aarch64_hwasan_check_memaccess_unaligned_shortgranules
    : Intrinsic<[], [llvm_i64_ty, llvm_ptr_ty, llvm_i32_ty], [IntrInaccessibleMemOnly]>;
```

#### 2. `llvm/lib/Target/AArch64/AArch64InstrInfo.td`
Define the machine pseudo-instruction with restricted register clobbers:
```tablegen
let Uses = [ X20 ], Defs = [ X16, X17, LR, NZCV ] in {
def HWASAN_CHECK_MEMACCESS_UNALIGNED_SHORTGRANULES : Pseudo<
  (outs), (ins GPR64noip:$ptr, i32imm:$accessinfo),
  [(int_aarch64_hwasan_check_memaccess_unaligned_shortgranules X20, GPR64noip:$ptr, (i32 timm:$accessinfo))]>,
  Sched<[]>;
}
```

#### 3. `llvm/lib/Transforms/Instrumentation/HWAddressSanitizer.cpp`
In `instrumentMemAccess()`, when `Alignment < AccessSize / 8`:
```cpp
// Replace the fallback IRB.CreateCall(HwasanMemoryAccessCallbackSized, ...) with:
if (OutlinedChecks && TargetTriple.isAArch64()) {
  IRB.CreateIntrinsic(
      Intrinsic::aarch64_hwasan_check_memaccess_unaligned_shortgranules,
      {ShadowBase, Ptr, ConstantInt::get(Int32Ty, AccessInfo)});
}
```

#### 4. `llvm/lib/Target/AArch64/AArch64AsmPrinter.cpp`
In `emitHwasanMemaccessSymbols()`, generate the boundary-aware check stub in `.text.hot` using only `x16` and `x17`:
```arm64
__hwasan_check_unaligned_x22_3_short_v2:
    // 1. Calculate shadow address for start of access
    sbfx    x16, x22, #4, #52
    ldrbb   w16, [x20, x16]            // Load tag for granule #1
    subs    xzr, x16, x22, lsr #56     // Compare with pointer tag
    b.ne    .Lhandle_mismatch_or_partial

    // 2. Check if (ptr & 15) + size > 16 (spans into next granule)
    and     x17, x22, #15
    add     x17, x17, #8               // size = 8
    cmp     x17, #16
    b.ls    .Lmatch                    // If within single granule, done!

    // 3. Spans boundary: check granule #2
    add     x16, x22, #7               // End byte address
    sbfx    x16, x16, #4, #52
    ldrbb   w16, [x20, x16]            // Load tag for granule #2
    subs    xzr, x16, x22, lsr #56
    b.ne    .Lhandle_mismatch_or_partial

.Lmatch:
    ret                                // Return to caller, x0-x15 untouched!

.Lhandle_mismatch_or_partial:
    b       __hwasan_tag_mismatch_v2
```

---

## 3. Option B: Selective Inlining of Unaligned Checks

### Overview
Instead of inlining *all* checks (which bloats code) or calling `__hwasan_loadN` (which spills registers), selectively inline the fast-path boundary check **only for unaligned memory accesses**.

```text
Callsite (.text)
+-----------------------------------------------------------------------------+
| 1. ldrb w16, [x20, xAddr, lsr #4]   // Load shadow tag                     |
| 2. cmp  w16, xAddr, lsr #56         // Compare top byte                    |
| 3. b.ne .Lslow_path_mismatch        // Branch on tag mismatch/boundary     |
| 4. ldr  d0, [xAddr]                 // Actual memory access (inline)       |
+-----------------------------------------------------------------------------+
               | (on tag mismatch / crossing boundary)
               v
Function Out-of-Line Block (.text)
+-----------------------------------------------------------------------------+
| .Lslow_path_mismatch:                                                       |
|   Call __hwasan_loadN (or trigger fatal crash trap)                         |
+-----------------------------------------------------------------------------+
```

---

### Implementation Details in LLVM

#### 1. `llvm/lib/Transforms/Instrumentation/HWAddressSanitizer.cpp`
Introduce a targeted flag/policy for unaligned inlining:
```cpp
static cl::opt<bool> ClInlineUnalignedChecks(
    "hwasan-inline-unaligned-checks",
    cl::desc("Inline fast-path checks for unaligned memory accesses"),
    cl::Hidden, cl::init(true));
```

In `instrumentMemAccess()`:
```cpp
bool IsUnaligned = (O.Alignment && *O.Alignment < O.TypeStoreSize / 8);

if (IsUnaligned && ClInlineUnalignedChecks) {
  // Inline the single-granule tag compare at the callsite.
  // Move the slow multi-granule / error check to a cold basic block.
  instrumentUnalignedMemAccessInline(Addr, O.IsWrite, AccessSizeIndex, O.getInsn(), DTU, LI);
}
```

#### 2. Generated Assembly at Callsite
```arm64
    // 3 inline instructions at callsite:
    ldrb    w16, [x20, x22, lsr #4]    // Load shadow tag
    cmp     w16, x22, lsr #56          // Compare top byte
    b.ne    .Lslow_unaligned_check     // Jump to cold block on mismatch/boundary
    ldr     d0, [x22]                  // Actual load
```
Cold slow-path block emitted at end of function:
```arm64
.Lslow_unaligned_check:
    mov     x0, x22
    mov     w1, #8
    bl      __hwasan_loadN
    b       .Lresume_after_check
```

---

## 4. Comparison of Options

| Criterion | Option A: Register-Preserving Stubs | Option B: Selective Inlining |
| :--- | :--- | :--- |
| **Callsite Instruction Count** | **1 instruction** (`bl __hwasan_check_unaligned_xN`) | **3–4 instructions** (`ldrb`, `cmp`, `b.ne`) |
| **Register Spill Overhead** | **Zero** (`x0`–`x15` preserved by stub) | **Zero on fast path** (register allocator sees inline ldrb/cmp) |
| **Binary Text Size Impact** | **Highest reduction** (reclaims callsites + spills) | **Moderate reduction** (reclaims spills, but adds inline insns) |
| **Runtime Performance** | Minor branch overhead (`bl` + `ret`) | Direct inline execution on fast path |
| **LLVM Changes Required** | Intrinsic + AArch64 TD + AsmPrinter stub | `HWAddressSanitizer.cpp` IR builder update |

---

## 5. Recommended Path

**Option A (Register-Preserving Unaligned Stubs)** is strongly recommended for Fuchsia:
1. It matches the existing architectural philosophy of AArch64 HWASan outlined checks (`HWASAN_CHECK_MEMACCESS_SHORTGRANULES`).
2. It completely eliminates register spills without expanding the callsite beyond a single `bl` instruction.
3. In `libcrypto.so`, this is projected to eliminate **over 40,000 instructions (~160 KB)** of spill/reload/argument code.
