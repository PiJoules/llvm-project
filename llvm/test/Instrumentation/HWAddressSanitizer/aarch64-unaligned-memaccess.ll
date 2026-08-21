; RUN: opt < %s -passes=hwasan -hwasan-opt-unaligned=1 -S | FileCheck %s --check-prefixes=CHECK,OPT-UNALIGNED
; RUN: opt < %s -passes=hwasan -hwasan-opt-unaligned=0 -S | FileCheck %s --check-prefixes=CHECK,FALLBACK-RUNTIME

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--linux-android10000"

define i64 @test_load_aligned(ptr %p) sanitize_hwaddress {
; CHECK-LABEL: @test_load_aligned(
; CHECK:       call void @llvm.hwasan.check.memaccess.shortgranules(ptr %{{.*}}, ptr %p, i32 3)
; CHECK-NEXT:  %val = load i64, ptr %p, align 8
; CHECK-NEXT:  ret i64 %val
entry:
  %val = load i64, ptr %p, align 8
  ret i64 %val
}

define i64 @test_load_unaligned_8b(ptr %p) sanitize_hwaddress {
; OPT-UNALIGNED-LABEL: @test_load_unaligned_8b(
; OPT-UNALIGNED:       call void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr %{{.*}}, ptr %p, i32 3)
; OPT-UNALIGNED-NEXT:  %val = load i64, ptr %p, align 1
; OPT-UNALIGNED-NEXT:  ret i64 %val
;
; FALLBACK-RUNTIME-LABEL: @test_load_unaligned_8b(
; FALLBACK-RUNTIME:       [[PTR:%.*]] = ptrtoint ptr %p to i64
; FALLBACK-RUNTIME-NEXT:  call void @__hwasan_loadN(i64 [[PTR]], i64 8)
; FALLBACK-RUNTIME-NEXT:  %val = load i64, ptr %p, align 1
; FALLBACK-RUNTIME-NEXT:  ret i64 %val
entry:
  %val = load i64, ptr %p, align 1
  ret i64 %val
}

define void @test_store_unaligned_4b(ptr %p, i32 %val) sanitize_hwaddress {
; OPT-UNALIGNED-LABEL: @test_store_unaligned_4b(
; OPT-UNALIGNED:       call void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr %{{.*}}, ptr %p, i32 18)
; OPT-UNALIGNED-NEXT:  store i32 %val, ptr %p, align 2
; OPT-UNALIGNED-NEXT:  ret void
;
; FALLBACK-RUNTIME-LABEL: @test_store_unaligned_4b(
; FALLBACK-RUNTIME:       [[PTR:%.*]] = ptrtoint ptr %p to i64
; FALLBACK-RUNTIME-NEXT:  call void @__hwasan_storeN(i64 [[PTR]], i64 4)
; FALLBACK-RUNTIME-NEXT:  store i32 %val, ptr %p, align 2
; FALLBACK-RUNTIME-NEXT:  ret void
entry:
  store i32 %val, ptr %p, align 2
  ret void
}

define <2 x i64> @test_load_unaligned_16b(ptr %p) sanitize_hwaddress {
; OPT-UNALIGNED-LABEL: @test_load_unaligned_16b(
; OPT-UNALIGNED:       call void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr %{{.*}}, ptr %p, i32 4)
; OPT-UNALIGNED-NEXT:  %val = load <2 x i64>, ptr %p, align 8
; OPT-UNALIGNED-NEXT:  ret <2 x i64> %val
;
; FALLBACK-RUNTIME-LABEL: @test_load_unaligned_16b(
; FALLBACK-RUNTIME:       [[PTR:%.*]] = ptrtoint ptr %p to i64
; FALLBACK-RUNTIME-NEXT:  call void @__hwasan_loadN(i64 [[PTR]], i64 16)
; FALLBACK-RUNTIME-NEXT:  %val = load <2 x i64>, ptr %p, align 8
; FALLBACK-RUNTIME-NEXT:  ret <2 x i64> %val
entry:
  %val = load <2 x i64>, ptr %p, align 8
  ret <2 x i64> %val
}
