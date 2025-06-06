; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc --mtriple=loongarch32 -mattr=+d < %s | FileCheck %s --check-prefix=LA32
; RUN: llc --mtriple=loongarch64 -mattr=+d < %s | FileCheck %s --check-prefix=LA64

;; Compare if positive and select variable or zero.
define i8 @pos_sel_variable_and_zero_i8(i8 signext %a, i8 signext %b) {
; LA32-LABEL: pos_sel_variable_and_zero_i8:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a0, $a0, 7
; LA32-NEXT:    andn $a0, $a1, $a0
; LA32-NEXT:    ret
;
; LA64-LABEL: pos_sel_variable_and_zero_i8:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a0, $a0, 7
; LA64-NEXT:    andn $a0, $a1, $a0
; LA64-NEXT:    ret
  %cmp = icmp sgt i8 %a, -1
  %sel = select i1 %cmp, i8 %b, i8 0
  ret i8 %sel
}

define i16 @pos_sel_variable_and_zero_i16(i16 signext %a, i16 signext %b) {
; LA32-LABEL: pos_sel_variable_and_zero_i16:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a0, $a0, 15
; LA32-NEXT:    andn $a0, $a1, $a0
; LA32-NEXT:    ret
;
; LA64-LABEL: pos_sel_variable_and_zero_i16:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a0, $a0, 15
; LA64-NEXT:    andn $a0, $a1, $a0
; LA64-NEXT:    ret
  %cmp = icmp sgt i16 %a, -1
  %sel = select i1 %cmp, i16 %b, i16 0
  ret i16 %sel
}

define i32 @pos_sel_variable_and_zero_i32(i32 signext %a, i32 signext %b) {
; LA32-LABEL: pos_sel_variable_and_zero_i32:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a0, $a0, 31
; LA32-NEXT:    andn $a0, $a1, $a0
; LA32-NEXT:    ret
;
; LA64-LABEL: pos_sel_variable_and_zero_i32:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a0, $a0, 31
; LA64-NEXT:    andn $a0, $a1, $a0
; LA64-NEXT:    ret
  %cmp = icmp sgt i32 %a, -1
  %sel = select i1 %cmp, i32 %b, i32 0
  ret i32 %sel
}

define i64 @pos_sel_variable_and_zero_i64(i64 signext %a, i64 signext %b) {
; LA32-LABEL: pos_sel_variable_and_zero_i64:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a1, $a1, 31
; LA32-NEXT:    andn $a0, $a2, $a1
; LA32-NEXT:    andn $a1, $a3, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: pos_sel_variable_and_zero_i64:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a0, $a0, 63
; LA64-NEXT:    andn $a0, $a1, $a0
; LA64-NEXT:    ret
  %cmp = icmp sgt i64 %a, -1
  %sel = select i1 %cmp, i64 %b, i64 0
  ret i64 %sel
}

;; Compare if not negative or zero and select the same variable as being
;; compared: smax(a, 0).
define i8 @not_neg_not_zero_sel_same_variable_i8(i8 signext %a) {
; LA32-LABEL: not_neg_not_zero_sel_same_variable_i8:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a1, $a0, 7
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: not_neg_not_zero_sel_same_variable_i8:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a1, $a0, 7
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %cmp = icmp sgt i8 %a, 0
  %sel = select i1 %cmp, i8 %a, i8 0
  ret i8 %sel
}

define i16 @not_neg_not_zero_sel_same_variable_i16(i16 signext %a) {
; LA32-LABEL: not_neg_not_zero_sel_same_variable_i16:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a1, $a0, 15
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: not_neg_not_zero_sel_same_variable_i16:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a1, $a0, 15
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %cmp = icmp sgt i16 %a, 0
  %sel = select i1 %cmp, i16 %a, i16 0
  ret i16 %sel
}

define i32 @not_neg_not_zero_sel_same_variable_i32(i32 signext %a) {
; LA32-LABEL: not_neg_not_zero_sel_same_variable_i32:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a1, $a0, 31
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: not_neg_not_zero_sel_same_variable_i32:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a1, $a0, 31
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %cmp = icmp sgt i32 %a, 0
  %sel = select i1 %cmp, i32 %a, i32 0
  ret i32 %sel
}

define i64 @not_neg_not_zero_sel_same_variable_i64(i64 signext %a) {
; LA32-LABEL: not_neg_not_zero_sel_same_variable_i64:
; LA32:       # %bb.0:
; LA32-NEXT:    srai.w $a2, $a1, 31
; LA32-NEXT:    andn $a0, $a0, $a2
; LA32-NEXT:    andn $a1, $a1, $a2
; LA32-NEXT:    ret
;
; LA64-LABEL: not_neg_not_zero_sel_same_variable_i64:
; LA64:       # %bb.0:
; LA64-NEXT:    srai.d $a1, $a0, 63
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %cmp = icmp sgt i64 %a, 0
  %sel = select i1 %cmp, i64 %a, i64 0
  ret i64 %sel
}

;; ret = (x-y) > 0 ? x-y : 0
define i8 @sub_clamp_zero_i8(i8 signext %x, i8 signext %y) {
; LA32-LABEL: sub_clamp_zero_i8:
; LA32:       # %bb.0:
; LA32-NEXT:    sub.w $a0, $a0, $a1
; LA32-NEXT:    slli.w $a1, $a0, 24
; LA32-NEXT:    srai.w $a1, $a1, 31
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: sub_clamp_zero_i8:
; LA64:       # %bb.0:
; LA64-NEXT:    sub.d $a0, $a0, $a1
; LA64-NEXT:    ext.w.b $a1, $a0
; LA64-NEXT:    srai.d $a1, $a1, 7
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %sub = sub nsw i8 %x, %y
  %cmp = icmp sgt i8 %sub, 0
  %sel = select i1 %cmp, i8 %sub, i8 0
  ret i8 %sel
}

define i16 @sub_clamp_zero_i16(i16 signext %x, i16 signext %y) {
; LA32-LABEL: sub_clamp_zero_i16:
; LA32:       # %bb.0:
; LA32-NEXT:    sub.w $a0, $a0, $a1
; LA32-NEXT:    slli.w $a1, $a0, 16
; LA32-NEXT:    srai.w $a1, $a1, 31
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: sub_clamp_zero_i16:
; LA64:       # %bb.0:
; LA64-NEXT:    sub.d $a0, $a0, $a1
; LA64-NEXT:    ext.w.h $a1, $a0
; LA64-NEXT:    srai.d $a1, $a1, 15
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %sub = sub nsw i16 %x, %y
  %cmp = icmp sgt i16 %sub, 0
  %sel = select i1 %cmp, i16 %sub, i16 0
  ret i16 %sel
}

define i32 @sub_clamp_zero_i32(i32 signext %x, i32 signext %y) {
; LA32-LABEL: sub_clamp_zero_i32:
; LA32:       # %bb.0:
; LA32-NEXT:    sub.w $a0, $a0, $a1
; LA32-NEXT:    srai.w $a1, $a0, 31
; LA32-NEXT:    andn $a0, $a0, $a1
; LA32-NEXT:    ret
;
; LA64-LABEL: sub_clamp_zero_i32:
; LA64:       # %bb.0:
; LA64-NEXT:    sub.w $a0, $a0, $a1
; LA64-NEXT:    srai.d $a1, $a0, 31
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %sub = sub nsw i32 %x, %y
  %cmp = icmp sgt i32 %sub, 0
  %sel = select i1 %cmp, i32 %sub, i32 0
  ret i32 %sel
}

define i64 @sub_clamp_zero_i64(i64 signext %x, i64 signext %y) {
; LA32-LABEL: sub_clamp_zero_i64:
; LA32:       # %bb.0:
; LA32-NEXT:    sltu $a4, $a0, $a2
; LA32-NEXT:    sub.w $a1, $a1, $a3
; LA32-NEXT:    sub.w $a1, $a1, $a4
; LA32-NEXT:    sub.w $a0, $a0, $a2
; LA32-NEXT:    srai.w $a2, $a1, 31
; LA32-NEXT:    andn $a1, $a1, $a2
; LA32-NEXT:    andn $a0, $a0, $a2
; LA32-NEXT:    ret
;
; LA64-LABEL: sub_clamp_zero_i64:
; LA64:       # %bb.0:
; LA64-NEXT:    sub.d $a0, $a0, $a1
; LA64-NEXT:    srai.d $a1, $a0, 63
; LA64-NEXT:    andn $a0, $a0, $a1
; LA64-NEXT:    ret
  %sub = sub nsw i64 %x, %y
  %cmp = icmp sgt i64 %sub, 0
  %sel = select i1 %cmp, i64 %sub, i64 0
  ret i64 %sel
}
