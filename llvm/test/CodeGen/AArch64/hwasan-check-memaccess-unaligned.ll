; RUN: llc < %s | FileCheck %s

target triple = "aarch64--linux-android"

define ptr @test_unaligned_short(ptr %x0, ptr %x1) {
  ; CHECK-LABEL: test_unaligned_short:
  ; CHECK: stp x30, x20, [sp, #-16]!
  ; CHECK-NEXT: .cfi_def_cfa_offset 16
  ; CHECK-NEXT: .cfi_offset w20, -8
  ; CHECK-NEXT: .cfi_offset w30, -16
  ; CHECK-NEXT: mov x20, x1
  ; CHECK-NEXT: bl __hwasan_check_unaligned_x0_2_short_v2
  ; CHECK-NEXT: ldp x30, x20, [sp], #16
  ; CHECK-NEXT: ret
  call void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr %x1, ptr %x0, i32 2)
  ret ptr %x0
}

define ptr @test_unaligned_noshort(ptr %x0, ptr %x1) {
  ; CHECK-LABEL: test_unaligned_noshort:
  ; CHECK: str x30, [sp, #-16]!
  ; CHECK-NEXT: .cfi_def_cfa_offset 16
  ; CHECK-NEXT: .cfi_offset w30, -16
  ; CHECK-NEXT: mov x9, x0
  ; CHECK-NEXT: mov x0, x1
  ; CHECK-NEXT: bl __hwasan_check_unaligned_x1_1
  ; CHECK-NEXT: ldr x30, [sp], #16
  ; CHECK-NEXT: ret
  call void @llvm.hwasan.check.memaccess.unaligned(ptr %x0, ptr %x1, i32 1)
  ret ptr %x1
}

define ptr @test_unaligned_fixedshadow(ptr %x0) {
  ; CHECK-LABEL: test_unaligned_fixedshadow:
  ; CHECK: str x30, [sp, #-16]!
  ; CHECK-NEXT: .cfi_def_cfa_offset 16
  ; CHECK-NEXT: .cfi_offset w30, -16
  ; CHECK-NEXT: bl __hwasan_check_unaligned_x0_3_fixed_4398046511104_short_v2
  ; CHECK-NEXT: ldr x30, [sp], #16
  ; CHECK-NEXT: ret
  call void @llvm.hwasan.check.memaccess.unaligned.shortgranules.fixedshadow(ptr %x0, i32 3, i64 4398046511104)
  ret ptr %x0
}

declare void @llvm.hwasan.check.memaccess.unaligned(ptr, ptr, i32)
declare void @llvm.hwasan.check.memaccess.unaligned.shortgranules(ptr, ptr, i32)
declare void @llvm.hwasan.check.memaccess.unaligned.shortgranules.fixedshadow(ptr, i32, i64)

; CHECK:      .section .text.hot,"axG",@progbits,__hwasan_check_unaligned_x0_2_short_v2,comdat
; CHECK-NEXT: .type __hwasan_check_unaligned_x0_2_short_v2,@function
; CHECK-NEXT: .weak __hwasan_check_unaligned_x0_2_short_v2
; CHECK-NEXT: .hidden __hwasan_check_unaligned_x0_2_short_v2
; CHECK-NEXT: __hwasan_check_unaligned_x0_2_short_v2:
; CHECK-NEXT: sbfx x16, x0, #4, #52
; CHECK-NEXT: ldrb w16, [x20, x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.ne .Ltmp2
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #4
; CHECK-NEXT: cmp w17, #16
; CHECK-NEXT: b.ls .Ltmp0
; CHECK-NEXT: add x17, x0, #3
; CHECK-NEXT: sbfx x16, x17, #4, #52
; CHECK-NEXT: ldrb w16, [x20, x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp0
; CHECK-NEXT: cmp w16, #15
; CHECK-NEXT: b.hi .Ltmp1
; CHECK-NEXT: add x17, x0, #3
; CHECK-NEXT: and x17, x17, #0xf
; CHECK-NEXT: cmp w16, w17
; CHECK-NEXT: b.ls .Ltmp1
; CHECK-NEXT: add x16, x0, #3
; CHECK-NEXT: orr x16, x16, #0xf
; CHECK-NEXT: ldrb w16, [x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp0
; CHECK-NEXT: b .Ltmp1
; CHECK-NEXT: .Ltmp0:
; CHECK-NEXT: ret
; CHECK-NEXT: .Ltmp2:
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #4
; CHECK-NEXT: cmp w17, #16
; CHECK-NEXT: b.hi .Ltmp1
; CHECK-NEXT: cmp w16, #15
; CHECK-NEXT: b.hi .Ltmp1
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #3
; CHECK-NEXT: cmp w16, w17
; CHECK-NEXT: b.ls .Ltmp1
; CHECK-NEXT: orr x16, x0, #0xf
; CHECK-NEXT: ldrb w16, [x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp0
; CHECK-NEXT: .Ltmp1:
; CHECK-NEXT: stp x0, x1, [sp, #-256]!
; CHECK-NEXT: stp x29, x30, [sp, #232]
; CHECK-NEXT: mov x1, #2
; CHECK-NEXT: adrp x16, :got:__hwasan_tag_mismatch_v2
; CHECK-NEXT: ldr x16, [x16, :got_lo12:__hwasan_tag_mismatch_v2]
; CHECK-NEXT: br x16

; CHECK:      __hwasan_check_unaligned_x0_3_fixed_4398046511104_short_v2:
; CHECK-NEXT: sbfx x16, x0, #4, #52
; CHECK-NEXT: mov x17, #4398046511104
; CHECK-NEXT: ldrb w16, [x17, x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.ne .Ltmp5
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #8
; CHECK-NEXT: cmp w17, #16
; CHECK-NEXT: b.ls .Ltmp3
; CHECK-NEXT: add x17, x0, #7
; CHECK-NEXT: sbfx x16, x17, #4, #52
; CHECK-NEXT: mov x17, #4398046511104
; CHECK-NEXT: ldrb w16, [x17, x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp3
; CHECK-NEXT: cmp w16, #15
; CHECK-NEXT: b.hi .Ltmp4
; CHECK-NEXT: add x17, x0, #7
; CHECK-NEXT: and x17, x17, #0xf
; CHECK-NEXT: cmp w16, w17
; CHECK-NEXT: b.ls .Ltmp4
; CHECK-NEXT: add x16, x0, #7
; CHECK-NEXT: orr x16, x16, #0xf
; CHECK-NEXT: ldrb w16, [x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp3
; CHECK-NEXT: b .Ltmp4
; CHECK-NEXT: .Ltmp3:
; CHECK-NEXT: ret
; CHECK-NEXT: .Ltmp5:
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #8
; CHECK-NEXT: cmp w17, #16
; CHECK-NEXT: b.hi .Ltmp4
; CHECK-NEXT: cmp w16, #15
; CHECK-NEXT: b.hi .Ltmp4
; CHECK-NEXT: and x17, x0, #0xf
; CHECK-NEXT: add x17, x17, #7
; CHECK-NEXT: cmp w16, w17
; CHECK-NEXT: b.ls .Ltmp4
; CHECK-NEXT: orr x16, x0, #0xf
; CHECK-NEXT: ldrb w16, [x16]
; CHECK-NEXT: cmp x16, x0, lsr #56
; CHECK-NEXT: b.eq .Ltmp3
; CHECK-NEXT: .Ltmp4:
; CHECK-NEXT: stp x0, x1, [sp, #-256]!
; CHECK-NEXT: stp x29, x30, [sp, #232]
; CHECK-NEXT: mov x1, #3
; CHECK-NEXT: adrp x16, :got:__hwasan_tag_mismatch_v2
; CHECK-NEXT: ldr x16, [x16, :got_lo12:__hwasan_tag_mismatch_v2]
; CHECK-NEXT: br x16

; CHECK:      __hwasan_check_unaligned_x1_1:
; CHECK-NEXT: sbfx x16, x1, #4, #52
; CHECK-NEXT: ldrb w16, [x9, x16]
; CHECK-NEXT: cmp x16, x1, lsr #56
; CHECK-NEXT: b.ne .Ltmp8
; CHECK-NEXT: and x17, x1, #0xf
; CHECK-NEXT: add x17, x17, #2
; CHECK-NEXT: cmp w17, #16
; CHECK-NEXT: b.ls .Ltmp6
; CHECK-NEXT: add x17, x1, #1
; CHECK-NEXT: sbfx x16, x17, #4, #52
; CHECK-NEXT: ldrb w16, [x9, x16]
; CHECK-NEXT: cmp x16, x1, lsr #56
; CHECK-NEXT: b.eq .Ltmp6
; CHECK-NEXT: b .Ltmp7
; CHECK-NEXT: .Ltmp6:
; CHECK-NEXT: ret
; CHECK-NEXT: .Ltmp8:
; CHECK-NEXT: .Ltmp7:
; CHECK-NEXT: stp x0, x1, [sp, #-256]!
; CHECK-NEXT: stp x29, x30, [sp, #232]
; CHECK-NEXT: mov x0, x1
; CHECK-NEXT: mov x1, #1
; CHECK-NEXT: adrp x16, :got:__hwasan_tag_mismatch
; CHECK-NEXT: ldr x16, [x16, :got_lo12:__hwasan_tag_mismatch]
; CHECK-NEXT: br x16
