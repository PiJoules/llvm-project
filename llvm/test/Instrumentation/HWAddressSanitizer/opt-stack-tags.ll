; Test that HWAddressSanitizer supports single shared tag per stack frame.
;
; RUN: opt < %s -passes=hwasan -S -hwasan-use-stack-safety=0 -hwasan-single-tag-per-frame=1 | FileCheck %s --check-prefix=SINGLETAG
; RUN: opt < %s -passes=hwasan -S -hwasan-use-stack-safety=0 -hwasan-single-tag-per-frame=0 | FileCheck %s --check-prefix=MULTITAG

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--linux-android10000"

declare void @use(ptr)

define void @test_multiple_allocas() sanitize_hwaddress {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %z = alloca i32, align 4
  call void @use(ptr %x)
  call void @use(ptr %y)
  call void @use(ptr %z)
  ret void
}

; SINGLETAG-LABEL: define void @test_multiple_allocas(
; SINGLETAG-NOT: xor i64 {{.*}}, 128
; SINGLETAG-NOT: xor i64 {{.*}}, 64
; SINGLETAG: call void @use(ptr %x.hwasan)
; SINGLETAG: call void @use(ptr %y.hwasan)
; SINGLETAG: call void @use(ptr %z.hwasan)
; SINGLETAG: ret void

; MULTITAG-LABEL: define void @test_multiple_allocas(
; MULTITAG: xor i64 {{.*}}, 128
; MULTITAG: xor i64 {{.*}}, 64
; MULTITAG: call void @use(ptr %x.hwasan)
; MULTITAG: call void @use(ptr %y.hwasan)
; MULTITAG: call void @use(ptr %z.hwasan)
; MULTITAG: ret void
