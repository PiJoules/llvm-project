; Test that HWAddressSanitizer hoists loop-invariant memory checks to loop preheaders.
;
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=1 -hwasan-opt-loops=1 | FileCheck %s --check-prefixes=CHECK,HOIST
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=1 -hwasan-opt-loops=0 | FileCheck %s --check-prefixes=CHECK,NOHOIST
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=0 | FileCheck %s --check-prefixes=CHECK,NOHOIST

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--linux-android10000"

; Loop with 100 iterations of 4-byte stores (400 bytes total span).
; HOIST emits 1 range check (__hwasan_storeN) in the preheader and 0 checks in the loop body.
define void @test_loop_store_hoisting(ptr %buf) sanitize_hwaddress {
entry:
  br label %for.body

for.body:
  %i = phi i64 [ 0, %entry ], [ %i.next, %for.body ]
  %gep = getelementptr inbounds i32, ptr %buf, i64 %i
  store i32 42, ptr %gep, align 4
  %i.next = add nuw nsw i64 %i, 1
  %cond = icmp eq i64 %i.next, 100
  br i1 %cond, label %for.end, label %for.body

for.end:
  ret void
}

; HOIST-LABEL: define void @test_loop_store_hoisting(
; HOIST: entry:
; HOIST: call void @__hwasan_storeN(i64 {{%.*}}, i64 400)
; HOIST: for.body:
; HOIST-NOT: llvm.hwasan.check.memaccess
; HOIST-NOT: __hwasan_store
; HOIST: store i32 42
; HOIST: ret void

; NOHOIST-LABEL: define void @test_loop_store_hoisting(
; NOHOIST: entry:
; NOHOIST-NOT: __hwasan_storeN
; NOHOIST: for.body:
; NOHOIST: call void @llvm.hwasan.check.memaccess.shortgranules
; NOHOIST: store i32 42
; NOHOIST: ret void

; Small loop with 4 iterations of 4-byte stores (16 bytes total span).
; Range check is hoisted to preheader (__hwasan_storeN) and 0 checks in loop body.
define void @test_small_loop_hoisting(ptr %buf) sanitize_hwaddress {
entry:
  br label %for.body

for.body:
  %i = phi i64 [ 0, %entry ], [ %i.next, %for.body ]
  %gep = getelementptr inbounds i32, ptr %buf, i64 %i
  store i32 10, ptr %gep, align 4
  %i.next = add nuw nsw i64 %i, 1
  %cond = icmp eq i64 %i.next, 4
  br i1 %cond, label %for.end, label %for.body

for.end:
  ret void
}

; HOIST-LABEL: define void @test_small_loop_hoisting(
; HOIST: entry:
; HOIST: call void @__hwasan_storeN(i64 {{%.*}}, i64 16)
; HOIST: for.body:
; HOIST-NOT: llvm.hwasan.check.memaccess
; HOIST-NOT: __hwasan_store
; HOIST: store i32 10
; HOIST: ret void

; Loop with unknown trip count: must NOT hoist to preheader.
define void @test_unknown_trip_count(ptr %buf, i64 %n) sanitize_hwaddress {
entry:
  %cmp0 = icmp sgt i64 %n, 0
  br i1 %cmp0, label %for.body, label %for.end

for.body:
  %i = phi i64 [ 0, %entry ], [ %i.next, %for.body ]
  %gep = getelementptr inbounds i32, ptr %buf, i64 %i
  store i32 7, ptr %gep, align 4
  %i.next = add nuw nsw i64 %i, 1
  %cond = icmp eq i64 %i.next, %n
  br i1 %cond, label %for.end, label %for.body

for.end:
  ret void
}

; CHECK-LABEL: define void @test_unknown_trip_count(
; CHECK: for.body:
; CHECK: call void @llvm.hwasan.check.memaccess.shortgranules
; CHECK: store i32 7
; CHECK: ret void
