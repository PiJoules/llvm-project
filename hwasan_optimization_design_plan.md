# HWASan Optimization Design Plan: Check Coalescing, Loop Hoisting, and ASan Parity

## Executive Summary

Hardware-assisted AddressSanitizer (HWASan) expands binary `.text` size significantly (e.g. **+58%** in `libcrypto.so`). While AddressSanitizer (ASan) incorporates several redundancy elimination optimizations, **HWASan currently lacks basic check deduplication, check coalescing, and loop hoisting**.

This document outlines a design plan to implement these optimizations in LLVM, closing the optimization gap with ASan and reclaiming code size in Fuchsia's HWASan builds.

---

## 1. Analysis of Current HWASan Flags

### A. `-hwasan-use-stack-safety`
- **What it does:** Integrates LLVM's `StackSafetyGlobalAnalysis` into HWASan. It statically analyzes all stack allocas in a function. If an alloca's address never escapes and all loads/stores to it have provably in-bounds constant offsets, HWASan **skips tagging the stack slot** and **skips checks on those accesses**.
- **Default Status:** Enabled by default (`cl::init(true)`).
- **Clarification:** It should **never be set to false**, as disabling it would re-introduce instrumentation for hundreds of provably safe stack variables, further bloating code size.

### B. `-hwasan-inline-fast-path-checks`
- **What it does:** Controls whether the tag-matching fast path is inlined at the callsite:
  - **When `false` (Fuchsia/Android default):** Emits an outlined call `bl __hwasan_check_xN` or `bl __hwasan_loadN` for every memory access.
  - **When `true`:** Inlines the fast-path tag check directly at the callsite:
    ```arm64
    ldrb    w16, [xShadowBase, xAddr, lsr #4]
    cmp     w16, xAddr, lsr #56
    b.ne    .Lslow_path
    ```
- **Trade-off:** Inlining saves register spills (since LLVM's register allocator sees exact register usage rather than treating `x0`–`x17` as clobbered by `bl`), but directly adds 3–4 inline instructions at every single load and store. Fuchsia disables it by default to favor outlined code density.

---

## 2. ASan vs. HWASan Optimization Gap Analysis

Inspecting LLVM's `AddressSanitizer.cpp` vs. `HWAddressSanitizer.cpp` reveals that ASan contains multiple optimizations that were never ported to HWASan:

```text
ASan Optimizations (AddressSanitizer.cpp)          HWASan Current State (HWAddressSanitizer.cpp)
+----------------------------------------+        +---------------------------------------------+
| TempsToInstrument (Intra-BB Dedup)     | -----> | Missing (Checks every load/store)           |
| Inbound Global Opt (ObjectSizeOffset)  | -----> | Unimplemented (Line 882 TODO comment)       |
| Scalar Stack Filtering (ClOptStack)    | -----> | StackSafetyGlobalAnalysis (Allocas only)    |
| Dynamic Callback Threshold             | -----> | Static Outlined / Inlined Modes             |
+----------------------------------------+        +---------------------------------------------+
```

| Optimization | ASan Implementation | HWASan Status | Impact if Added to HWASan |
| :--- | :--- | :--- | :--- |
| **Intra-BB Same-Pointer Deduplication** (`ClOptSameTemp`) | Caches instrumented base pointers in `TempsToInstrument` set per basic block; skips duplicate checks if no intervening call occurs. | **Missing** (every load/store checked independently). | Eliminates duplicate check sequences for repeated reads/writes to the same pointer within basic blocks. |
| **Inbound Global Variable Optimization** (`ClOptGlobals`) | Uses `ObjectSizeOffsetVisitor` to prove accesses are within known global boundaries; elides checks. | **Unimplemented** (explicit `// TODO` at line 882). | Eliminates all check calls for compile-time constant global array/table accesses. |
| **Adjacent Check Coalescing** | Partially handled via merged vector operations. | **Missing** | Merges multiple contiguous struct member writes into a single range check. |
| **Loop-Invariant Check Hoisting** | Missing (handled by external LICM passes for some intrinsics). | **Missing** | Hoists tag validation outside loop preheaders. |

---

## 3. Proposed Optimization Pipeline & Architecture

We propose introducing a dedicated **`HWASanOpt` pass** (or extending `HWAddressSanitizer.cpp`) before code generation:

```text
       +--------------------------------------------------------+
## 3. Integrated Architecture Inside `HWAddressSanitizer.cpp`

Matching ASan's internal architecture in `AddressSanitizer.cpp`, **all optimizations are performed directly inside the existing `HWAddressSanitizer` pass** during operand collection and instrumentation, rather than adding separate pipeline stages or passes:

```text
       +-----------------------------------------------------------------+
       |                          LLVM IR Input                          |
       +-----------------------------------------------------------------+
                                        |
                                        v
       +-----------------------------------------------------------------+
       |                    StackSafetyGlobalAnalysis                    |
       +-----------------------------------------------------------------+
                                        |
                                        v
+-------------------------------------------------------------------------------+
|         HWAddressSanitizer::sanitizeFunction() (Single Monolithic Pass)        |
|                                                                               |
|  [Step 1: Operand Collection & Inbound Global Filtering]                      |
|    - ObjectSizeOffsetVisitor elides provably safe global/table accesses       |
|                                       |                                       |
|                                       v                                       |
|  [Step 2: Intra-BB Deduplication & Coalescing (TempsToInstrument)]            |
|    - DenseSet<Value*> filters repeated loads/stores to same base pointer      |
|    - Coalesce adjacent struct field writes into single range checks           |
|                                       |                                       |
|                                       v                                       |
|  [Step 3: Loop-Invariant Check Hoisting (LICM)]                               |
|    - Hoist range validation to loop preheader using SCEV                      |
|    - Strip instrumentation on loop body operands                              |
|                                       |                                       |
|                                       v                                       |
|  [Step 4: Instrumentation & Outlined Stub Lowering]                           |
|    - Emit __hwasan_check_xN / unaligned stubs for remaining operands          |
+-------------------------------------------------------------------------------+
                                        |
                                        v
       +-----------------------------------------------------------------+
       |                  Optimized & Instrumented IR                    |
       +-----------------------------------------------------------------+
```

---

## 4. Detailed Optimization Implementations Inside `HWAddressSanitizer.cpp`

### Phase 1: Intra-Basic-Block Redundant Check Elimination (`TempsToInstrument`)
- **Location:** Inside `HWAddressSanitizer::sanitizeFunction()` during basic block iteration (identical to `AddressSanitizer.cpp:3158`).
- **Mechanism:** Maintain a `DenseSet<Value*>` of instrumented pointers within each Basic Block.
- **Algorithm:**
  1. For each `LoadInst` / `StoreInst`, resolve the base pointer `Ptr`.
  2. If `Ptr` is already in the set and no memory-invalidating instruction (e.g. `free`, `realloc`, or an opaque function call) has occurred in the block, **skip adding it to `OperandsToInstrument`**.
  3. Clear the set upon encountering any `CallBase` that might deallocate or retag memory.
- **Expected Savings:** ~10%–15% reduction in check calls.

---

### Phase 2: Inbound Global Access Elision (`ObjectSizeOffsetVisitor`)
- **Mechanism:** Implement the pending `// TODO` at line 882 of `HWAddressSanitizer.cpp`.
- **Algorithm:**
  ```cpp
  if (GlobalVariable *GV = dyn_cast<GlobalVariable>(getUnderlyingObject(Ptr))) {
    if (GV->hasInitializer() && !GV->isThreadLocal()) {
      uint64_t ObjectSize;
      if (ObjSizeVis.computeObjectSize(GV, ObjectSize)) {
        if (isStaticallyInBounds(Ptr, AccessSize, ObjectSize)) {
          return true; // Skip check
        }
      }
    }
  }
  ```
- **Expected Savings:** Eliminates all HWASan checks on fixed lookup tables, static S-boxes, and constant string/data arrays in `libcrypto.so`.

---

### Phase 3: Contiguous Struct/Array Access Coalescing
- **Mechanism:** When multiple stores or loads access consecutive fields of a struct or fixed array offsets:
  ```c
  // Source:
  p->a = x; // offset 0, size 8
  p->b = y; // offset 8, size 8
  ```
- **Current HWASan Codegen:** Emits two 4-instruction sequences (`__hwasan_store8` / `__hwasan_storeN`).
- **Coalesced Codegen:**
  1. Group contiguous accesses in the same Basic Block to base pointer `BasePtr` spanning total range $[Offset_{min}, Offset_{max} + Size]$.
  2. If total span $\le 16$ bytes (1 shadow granule) or alignment guarantees a single span:
     - Emit **one** check: `__hwasan_check_memaccess(BasePtr + Offset_min, SpanSize)`.
     - Emit the individual stores without check instrumentation.
- **Expected Savings:** 40%–60% reduction in check calls for struct-heavy initialization routines (like `RC4_set_key`, `DES_set_key`).

---

### Phase 4: Loop-Invariant Tag Check Hoisting (LICM for HWASan)
- **Mechanism:** For loops iterating over arrays or buffers (`for (int i = 0; i < N; i++) buf[i] = ...`):
  1. Identify loop-invariant base pointer `BufBase` with SCEV-analyzable bounds `[0, N * ElemSize)`.
  2. Prove that memory inside the loop is not deallocated/retagged.
  3. **In the Loop Preheader:** Insert a single range validation check:
     `__hwasan_check_range(BufBase, N * ElemSize)`.
  4. **Inside the Loop Body:** Mark all loads/stores to `BufBase[i]` with `!nosanitize` metadata.
- **Expected Savings:** Eliminates thousands of loop checks in cryptographic algorithms (ML-DSA, AES, Polyval).

---

### Phase 5: Coarse-Grained Single Tag per Stack Frame
- **Mechanism:** For functions requiring stack tagging:
  - Instead of assigning individual tags per alloca ($Tag_1, Tag_2, Tag_3$), assign **one common tag $Tag_{frame}$ to the entire stack frame**.
  - All allocas share $Tag_{frame}$ with redzones between them.
- **Prologue Impact:**
  - Emits **one** tag generation instruction from `TPIDR_EL0`.
  - Emits **one** `dup v0.16b` instruction.
  - Emits **one** contiguous shadow store covering the stack frame.
- **Expected Savings:** Eliminates ~7,500 instructions (~30 KB) in `libcrypto.so` alone.

---

## 5. Verification & Correctness Plan

1. **LLVM Unit & Regression Tests:**
   - Add targeted IR tests under `llvm/test/Instrumentation/HWAddressSanitizer/` verifying that:
     - Duplicate checks in basic blocks are pruned.
     - Inbound global lookups emit no checks.
     - Loops with scalar evolution bounds hoist checks to preheaders.
2. **Fuchsia Integration & HWASan Test Suite:**
   - Run existing Zircon core tests and userland test suites under HWASan (`fx test`).
   - Validate that intentionally injected Out-Of-Bounds (OOB) and Use-After-Free (UAF) tests in `zircon/system/ulib/c/test/sanitizer/` still trap reliably.
3. **Binary Size Measurement:**
   - Compare `BOOTFS` and product `fuchsia.zbi` sizes before and after each optimization phase.
