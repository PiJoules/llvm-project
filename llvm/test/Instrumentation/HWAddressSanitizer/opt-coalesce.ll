; Test that HWAddressSanitizer coalesces contiguous memory access checks and subsumes interval subsets.
;
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=1 -hwasan-opt-coalesce=1 | FileCheck %s --check-prefixes=CHECK,COALESCE
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=1 -hwasan-opt-coalesce=0 | FileCheck %s --check-prefixes=CHECK,NOCOALESCE
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=0 | FileCheck %s --check-prefixes=CHECK,NOOPT

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--linux-android10000"

; Two adjacent 4-byte stores to an 8-byte aligned struct:
; COALESCE should emit 1 check (for 8-byte store, access info 19) and skip the second.
; NOCOALESCE should emit 2 checks (4-byte stores, access info 18 each).
define void @test_two_adjacent_stores(ptr %p, i32 %a, i32 %b) sanitize_hwaddress {
entry:
  store i32 %a, ptr %p, align 8
  %p4 = getelementptr inbounds i8, ptr %p, i64 4
  store i32 %b, ptr %p4, align 4
  ret void
}

; COALESCE-LABEL: define void @test_two_adjacent_stores(
; COALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 19)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret void

; NOCOALESCE-LABEL: define void @test_two_adjacent_stores(
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 18)
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p4, i32 18)
; NOCOALESCE: ret void

; Four adjacent 4-byte stores to a 16-byte aligned struct:
; COALESCE should emit 1 check (for 16-byte store, access info 20) and skip the other 3.
define void @test_four_adjacent_stores(ptr %p, i32 %a, i32 %b, i32 %c, i32 %d) sanitize_hwaddress {
entry:
  store i32 %a, ptr %p, align 16
  %p4 = getelementptr inbounds i8, ptr %p, i64 4
  store i32 %b, ptr %p4, align 4
  %p8 = getelementptr inbounds i8, ptr %p, i64 8
  store i32 %c, ptr %p8, align 8
  %p12 = getelementptr inbounds i8, ptr %p, i64 12
  store i32 %d, ptr %p12, align 4
  ret void
}

; COALESCE-LABEL: define void @test_four_adjacent_stores(
; COALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 20)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret void

; NOCOALESCE-LABEL: define void @test_four_adjacent_stores(
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 18)
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p4, i32 18)
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p8, i32 18)
; NOCOALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p12, i32 18)
; NOCOALESCE: ret void

; Two adjacent 8-byte stores to a 16-byte aligned buffer:
; COALESCE should emit 1 check (for 16-byte store, access info 20) and skip the second.
define void @test_two_8byte_stores(ptr %p, i64 %a, i64 %b) sanitize_hwaddress {
entry:
  store i64 %a, ptr %p, align 16
  %p8 = getelementptr inbounds i8, ptr %p, i64 8
  store i64 %b, ptr %p8, align 8
  ret void
}

; COALESCE-LABEL: define void @test_two_8byte_stores(
; COALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 20)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret void

; Subsumption: an 8-byte load covers subsequent 4-byte and 2-byte loads within [0, 8)
define i32 @test_subset_interval_loads(ptr %p) sanitize_hwaddress {
entry:
  %v64 = load i64, ptr %p, align 8
  %p4 = getelementptr inbounds i8, ptr %p, i64 4
  %v32 = load i32, ptr %p4, align 4
  %p2 = getelementptr inbounds i8, ptr %p, i64 2
  %v16 = load i16, ptr %p2, align 2
  %z16 = zext i16 %v16 to i32
  %res = add i32 %v32, %z16
  ret i32 %res
}

; COALESCE-LABEL: define i32 @test_subset_interval_loads(
; COALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 3)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret i32

; NOOPT-LABEL: define i32 @test_subset_interval_loads(
; NOOPT: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 3)
; NOOPT: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p4, i32 2)
; NOOPT: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p2, i32 1)
; NOOPT: ret i32

; Non-contiguous stores with a gap must NOT be coalesced
define void @test_gap_stores(ptr %p, i32 %a, i32 %b) sanitize_hwaddress {
entry:
  store i32 %a, ptr %p, align 8
  %p8 = getelementptr inbounds i8, ptr %p, i64 8
  store i32 %b, ptr %p8, align 8
  ret void
}

; CHECK-LABEL: define void @test_gap_stores(
; CHECK: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 18)
; CHECK: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p8, i32 18)
; CHECK: ret void

; A function call between stores resets tracking and prevents coalescing
declare void @opaque_call()

define void @test_call_clobber(ptr %p, i32 %a, i32 %b) sanitize_hwaddress {
entry:
  store i32 %a, ptr %p, align 8
  call void @opaque_call()
  %p4 = getelementptr inbounds i8, ptr %p, i64 4
  store i32 %b, ptr %p4, align 4
  ret void
}

; CHECK-LABEL: define void @test_call_clobber(
; CHECK: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p, i32 18)
; CHECK: call void @opaque_call()
; CHECK: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %p4, i32 18)
; CHECK: ret void

; FIDL / std::array<uint8_t, 16> pattern: 16 contiguous 1-byte loads with GEPs.
; COALESCE should combine all 16 byte checks into a single 16-byte check on %src.
define i32 @test_array16_fidl_loads(ptr %src) sanitize_hwaddress {
entry:
  %v0 = load i8, ptr %src, align 1
  %p1 = getelementptr inbounds i8, ptr %src, i64 1
  %v1 = load i8, ptr %p1, align 1
  %p2 = getelementptr inbounds i8, ptr %src, i64 2
  %v2 = load i8, ptr %p2, align 1
  %p3 = getelementptr inbounds i8, ptr %src, i64 3
  %v3 = load i8, ptr %p3, align 1
  %p4 = getelementptr inbounds i8, ptr %src, i64 4
  %v4 = load i8, ptr %p4, align 1
  %p5 = getelementptr inbounds i8, ptr %src, i64 5
  %v5 = load i8, ptr %p5, align 1
  %p6 = getelementptr inbounds i8, ptr %src, i64 6
  %v6 = load i8, ptr %p6, align 1
  %p7 = getelementptr inbounds i8, ptr %src, i64 7
  %v7 = load i8, ptr %p7, align 1
  %p8 = getelementptr inbounds i8, ptr %src, i64 8
  %v8 = load i8, ptr %p8, align 1
  %p9 = getelementptr inbounds i8, ptr %src, i64 9
  %v9 = load i8, ptr %p9, align 1
  %p10 = getelementptr inbounds i8, ptr %src, i64 10
  %v10 = load i8, ptr %p10, align 1
  %p11 = getelementptr inbounds i8, ptr %src, i64 11
  %v11 = load i8, ptr %p11, align 1
  %p12 = getelementptr inbounds i8, ptr %src, i64 12
  %v12 = load i8, ptr %p12, align 1
  %p13 = getelementptr inbounds i8, ptr %src, i64 13
  %v13 = load i8, ptr %p13, align 1
  %p14 = getelementptr inbounds i8, ptr %src, i64 14
  %v14 = load i8, ptr %p14, align 1
  %p15 = getelementptr inbounds i8, ptr %src, i64 15
  %v15 = load i8, ptr %p15, align 1
  %z0 = zext i8 %v0 to i32
  %z1 = zext i8 %v1 to i32
  %sum = add i32 %z0, %z1
  ret i32 %sum
}

; COALESCE-LABEL: define i32 @test_array16_fidl_loads(
; COALESCE: call void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr {{%.*}}, ptr %src, i32 4)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret i32

; Aligned std::array<uint8_t, 16>: emits aligned shortgranules check (AccessInfo 4)
define i32 @test_array16_aligned_loads(ptr align 16 %src) sanitize_hwaddress {
entry:
  %v0 = load i8, ptr %src, align 16
  %p1 = getelementptr inbounds i8, ptr %src, i64 1
  %v1 = load i8, ptr %p1, align 1
  %p2 = getelementptr inbounds i8, ptr %src, i64 2
  %v2 = load i8, ptr %p2, align 1
  %p3 = getelementptr inbounds i8, ptr %src, i64 3
  %v3 = load i8, ptr %p3, align 1
  %p4 = getelementptr inbounds i8, ptr %src, i64 4
  %v4 = load i8, ptr %p4, align 1
  %p5 = getelementptr inbounds i8, ptr %src, i64 5
  %v5 = load i8, ptr %p5, align 1
  %p6 = getelementptr inbounds i8, ptr %src, i64 6
  %v6 = load i8, ptr %p6, align 1
  %p7 = getelementptr inbounds i8, ptr %src, i64 7
  %v7 = load i8, ptr %p7, align 1
  %p8 = getelementptr inbounds i8, ptr %src, i64 8
  %v8 = load i8, ptr %p8, align 1
  %p9 = getelementptr inbounds i8, ptr %src, i64 9
  %v9 = load i8, ptr %p9, align 1
  %p10 = getelementptr inbounds i8, ptr %src, i64 10
  %v10 = load i8, ptr %p10, align 1
  %p11 = getelementptr inbounds i8, ptr %src, i64 11
  %v11 = load i8, ptr %p11, align 1
  %p12 = getelementptr inbounds i8, ptr %src, i64 12
  %v12 = load i8, ptr %p12, align 1
  %p13 = getelementptr inbounds i8, ptr %src, i64 13
  %v13 = load i8, ptr %p13, align 1
  %p14 = getelementptr inbounds i8, ptr %src, i64 14
  %v14 = load i8, ptr %p14, align 1
  %p15 = getelementptr inbounds i8, ptr %src, i64 15
  %v15 = load i8, ptr %p15, align 1
  %z0 = zext i8 %v0 to i32
  %z1 = zext i8 %v1 to i32
  %sum = add i32 %z0, %z1
  ret i32 %sum
}

; COALESCE-LABEL: define i32 @test_array16_aligned_loads(
; COALESCE: call void @llvm.hwasan.check.memaccess.shortgranules(ptr {{%.*}}, ptr %src, i32 4)
; COALESCE-NOT: call void @llvm.hwasan.check.memaccess
; COALESCE: ret i32
