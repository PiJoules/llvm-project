# Design Document: HWASan Check Coalescing and Clustering in LLVM

## 1. Executive Summary

Hardware-assisted AddressSanitizer (HWASan) instruments memory loads and stores instruction-by-instruction in the LLVM mid-end (`HWAddressSanitizerPass`). When frontend templates or scalar operations emit sequences of small consecutive memory operations (e.g. `std::array`, small structs, bit-packed fields, FIDL message copies), HWASan inserts separate check calls interleaved between every single load and store.

This causes catastrophic code bloat in small structs:
1. **Blocks Backend Load Combining**: Interleaved check calls (`bl __hwasan_check_...`) act as opaque control/dataflow barriers, preventing SelectionDAG's `DAGCombiner::MatchLoadCombine` and `AArch64LoadStoreOpt` from combining sequential byte loads into 64-bit or 128-bit pair instructions (`ldp`/`stp`).
2. **Exponential Size Expansion**: Trivial 12-to-20 byte copy constructors explode by **20x to 46x** (e.g., `fuchsia_net::Ipv6Address` balloons from 12 B $\rightarrow$ 332 B; `fuchsia_wlan_ieee80211::CSsid` balloons from 20 B $\rightarrow$ 936 B).
3. **Register Pressure & Shift Trees**: Because each byte is loaded individually to satisfy individual checks, the backend is forced to emit dozens of shift-or (`orr Rd, Rn, Rm, lsl #N`) instructions to reconstruct words manually.

This document proposes **HWASan Check Coalescing and Clustering** inside `HWAddressSanitizer.cpp`:
- **Check Coalescing**: Merge $N$ contiguous small checks on the same base pointer into a single aggregate check (e.g. $16 \times 1\text{B} \rightarrow 1 \times 16\text{B}$ check).
- **Check Clustering & Hoisting**: Group all check calls before the sequence of loads/stores so that the memory instructions remain contiguous and can be merged into `ldp`/`stp` by the backend.

---

## 2. Problem Analysis & Case Study

### Case Study: `fuchsia_net::Ipv6Address::Ipv6Address(const Ipv6Address&)`

`Ipv6Address` wraps `std::array<uint8_t, 16>`. In the FIDL C++ runtime (`sdk/lib/fidl/cpp/include/lib/fidl/cpp/internal/natural_types.h`), `ArrayCloneHelper` unpacks the 16 elements via `std::make_index_sequence<16>`, emitting 16 scalar `load i8` instructions in LLVM IR:

```llvm
; LLVM IR before HWASan:
%v0 = load i8, ptr %1, align 1
%p1 = getelementptr inbounds nuw i8, ptr %1, i64 1
%v1 = load i8, ptr %p1, align 1
...
%p15 = getelementptr inbounds nuw i8, ptr %1, i64 15
%v15 = load i8, ptr %p15, align 1
; Followed by 14 OR/SHL instructions packing bytes into two i64 words
```

#### Comparison of Assembly Lowering

```text
+-----------------------------------------------------------------------------------+
| Non-HWASan Release (12 Bytes / 3 Insns)                                           |
|   ldp   x8, x9, [x1]        ; Combined by DAGCombiner + AArch64LoadStoreOpt       |
|   stp   x8, x9, [x0]        ; Combined into pair store                            |
|   ret                                                                             |
+-----------------------------------------------------------------------------------+
| Current HWASan (332 Bytes / 83 Insns) — 27.7x Bloat                               |
|   - 18 separate __hwasan_check / storeN function calls                            |
|   - Promoted leaf function to 48-byte stack frame + spills x19, x20, x21, x29, x30|
|   - 16 individual ldrb instructions + 14 manual orr ..., lsl #N shift instructions |
+-----------------------------------------------------------------------------------+
| With Check Coalescing (68 Bytes / 17 Insns) — 5x Size Reduction                   |
|   - 1 single 16-byte source check (__hwasan_check_x1_4_short_v2)                  |
|   - 1 single 16-byte ldp x20, x21, [x1]                                           |
|   - 1 single 16-byte destination check + 1 stp x20, x21, [x19]                    |
+-----------------------------------------------------------------------------------+
```

---

## 3. Architecture & Algorithm Design

The optimization will be integrated directly into `llvm/lib/Transforms/Instrumentation/HWAddressSanitizer.cpp`.

```text
 BasicBlock Loads/Stores
          │
          ▼
 ┌────────────────────────────────────────────────────────┐
 │ 1. Access Clustering & Analysis                        │
 │    - Group accesses by (BasePointer, IsWrite)          │
 │    - Sort by constant byte offset                      │
 └────────────────────────────────────────────────────────┘
          │
          ▼
 ┌────────────────────────────────────────────────────────┐
 │ 2. Range Coalescing (Contiguous Strides)               │
 │    - Identify contiguous ranges: [Offset, Offset+Size) │
 │    - Merge N small checks into single aggregate check  │
 └────────────────────────────────────────────────────────┘
          │
          ▼
 ┌────────────────────────────────────────────────────────┐
 │ 3. Check Hoisting / Placement                          │
 │    - Emit coalesced check(s) before the memory sequence│
 │    - Keep loads contiguous for DAGCombiner             │
 └────────────────────────────────────────────────────────┘
```

### 3.1. Access Clustering

For each `BasicBlock`, collect all candidate `LoadInst` and `StoreInst` instructions. For each instruction $I$:
1. Determine the underlying pointer `Base` and constant `Offset` using `GetPointerBaseWithConstantOffset(Ptr, DL)`.
2. Determine `Size` from the loaded/stored type (`DL.getTypeStoreSize(Ty)`).
3. Check for intervening aliasing stores or opaque function calls using `AAResults` (or basic block local safety checks).

```cpp
struct MemAccessInfo {
  Instruction *Inst;
  Value *Ptr;
  Value *Base;
  int64_t Offset;
  uint64_t Size;
  bool IsWrite;
};

struct AccessCluster {
  Value *Base;
  bool IsWrite;
  int64_t StartOffset;
  uint64_t TotalSize;
  SmallVector<MemAccessInfo, 8> Accesses;
};
```

### 3.2. Check Coalescing Logic

When a cluster has contiguous offsets ($\text{Offset}_{i+1} = \text{Offset}_i + \text{Size}_i$):

1. **Power-of-Two Aggregate Sizes ($S \in \{1, 2, 4, 8, 16\}$)**:
   - Compute $\text{AccessInfo} = \log_2(S)$.
   - Compute check address: `CheckPtr = (StartOffset == 0) ? Base : GEP(Base, StartOffset)`.
   - Emit a single `@llvm.hwasan.check.memaccess.shortgranules(Shadow, CheckPtr, AccessInfo)`.
   - Elide all $N$ individual checks for the cluster members.

2. **Non-Power-of-Two Aggregate Sizes (e.g., 12 bytes, 24 bytes, 32 bytes)**:
   - If unaligned check stubs are enabled, emit `__hwasan_check_unaligned_x<Reg>_<AccessInfo>` for the total range.
   - Alternatively, emit two power-of-two checks (e.g. 12 bytes $\rightarrow$ $1 \times 8\text{B} + 1 \times 4\text{B}$ check) or fallback to clustering.

### 3.3. Check Hoisting & Placement

Even when accesses cannot be combined into a single power-of-two check size:
- **Rule**: Emit all check intrinsics **before** the first memory access in the contiguous cluster.
- **Benefit**: Leaving the `load` instructions contiguous and adjacent in the basic block allows `SelectionDAG::DAGCombiner::MatchLoadCombine` to recognize the pattern:
  $$\text{OR}(\text{SHL}(\text{load } p+1, 8), \text{load } p+0) \implies \text{load } i16$$
  and fold all scalar loads into `ldr`/`ldp`.

---

## 4. Safety & Correctness Invariants

1. **Tag Granule Bounds & Alignment**:
   - HWASan memory tags are tracked per 16-byte granule.
   - If a coalesced range $[P, P+S)$ crosses a 16-byte granule boundary, the check stub must verify both granules.
   - For aligned struct copies starting at offset 0 with $S \le 16$, the access is guaranteed to reside within at most two granules (or one granule if 16-byte aligned). The existing short-granule check stub (`short_v2`) handles bounds within granules up to 16 bytes.
2. **Exception & Abort Semantics**:
   - HWASan checks unconditionally trap/abort upon tag mismatch.
   - Hoisting a check earlier in the same basic block preserves precise failure semantics: an invalid memory access will still trap before any memory modification occurs, with no observable side effects.
3. **No Intervening Aliasing Stores**:
   - If an intervening instruction could mutate the source buffer or shadow memory, the cluster is split at that boundary.

---

## 5. Implementation Plan

### File Changes

1. **`llvm/lib/Transforms/Instrumentation/HWAddressSanitizer.cpp`**:
   - Add helper function `clusterAndCoalesceAccesses(BasicBlock &BB, ...)`.
   - Replace the per-instruction loop in `HWAddressSanitizer::sanitizeFunction` with cluster-based instrumentation.
   - Add command-line flags:
     - `-hwasan-coalesce-checks`: Enable contiguous access check coalescing (default: `true`).
     - `-hwasan-cluster-checks`: Enable check hoisting/clustering before memory blocks (default: `true`).

2. **Tests**:
   - `llvm/test/Instrumentation/HWAddressSanitizer/check-coalescing.ll`: Verify $16 \times 1\text{B} \rightarrow 1 \times 16\text{B}$ check conversion.
   - `llvm/test/Instrumentation/HWAddressSanitizer/check-hoisting-ldp.ll`: Verify end-to-end emission of `ldp`/`stp` on AArch64.

---

## 6. Target Impact & Expected Savings

1. **Binary Size Reduction**:
   - Small struct and array copy constructors expand by $1.5\text{x}$–$2\text{x}$ instead of $20\text{x}$–$46\text{x}$.
   - Eliminates thousands of shift-or chains across FIDL-heavy Fuchsia drivers (`wlansoftmac.so`, `brcmfmac.so`, `libadaptation.shared.so`, `sherlock.so`).
2. **Register Allocation & Spills**:
   - Eliminating 10–20 function calls per trivial function prevents leaf functions from being promoted to frame-allocating non-leaf functions, saving ~40–60 bytes of prologue/epilogue spills per function.
3. **Runtime Performance**:
   - Replaces 16 sequential stub calls with a single tag check, eliminating branch and call overhead on struct copies.
