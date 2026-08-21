; RUN: opt -passes=hwasan -S < %s | llc -O2 -mtriple=aarch64--linux-android10000 | FileCheck %s

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64--linux-android10000"

; FIDL / std::array<uint8_t, 16> pattern: 16 contiguous 1-byte loads.
; The 16 individual byte checks should be coalesced into a single 16-byte
; unaligned shortgranules check (AccessInfo 4 = 16 bytes), avoiding 15 redundant checks.
define i32 @read_array16_unaligned(ptr %src) sanitize_hwaddress {
; CHECK-LABEL: read_array16_unaligned:
; CHECK:       bl __hwasan_check_unaligned_x0_4_short_v2
; CHECK-NOT:   bl __hwasan_check
; CHECK:       ret
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
  %sum0 = add i32 %z0, %z1
  %z2 = zext i8 %v2 to i32
  %sum1 = add i32 %sum0, %z2
  %z3 = zext i8 %v3 to i32
  %sum2 = add i32 %sum1, %z3
  %z4 = zext i8 %v4 to i32
  %sum3 = add i32 %sum2, %z4
  %z5 = zext i8 %v5 to i32
  %sum4 = add i32 %sum3, %z5
  %z6 = zext i8 %v6 to i32
  %sum5 = add i32 %sum4, %z6
  %z7 = zext i8 %v7 to i32
  %sum6 = add i32 %sum5, %z7
  %z8 = zext i8 %v8 to i32
  %sum7 = add i32 %sum6, %z8
  %z9 = zext i8 %v9 to i32
  %sum8 = add i32 %sum7, %z9
  %z10 = zext i8 %v10 to i32
  %sum9 = add i32 %sum8, %z10
  %z11 = zext i8 %v11 to i32
  %sum10 = add i32 %sum9, %z11
  %z12 = zext i8 %v12 to i32
  %sum11 = add i32 %sum10, %z12
  %z13 = zext i8 %v13 to i32
  %sum12 = add i32 %sum11, %z13
  %z14 = zext i8 %v14 to i32
  %sum13 = add i32 %sum12, %z14
  %z15 = zext i8 %v15 to i32
  %sum14 = add i32 %sum13, %z15
  ret i32 %sum14
}

; Aligned 16-byte access: coalesces into a single 16-byte aligned shortgranules check.
define i32 @read_array16_aligned(ptr align 16 %src) sanitize_hwaddress {
; CHECK-LABEL: read_array16_aligned:
; CHECK:       bl __hwasan_check_x0_4_short_v2
; CHECK-NOT:   bl __hwasan_check
; CHECK:       ret
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
  %sum0 = add i32 %z0, %z1
  %z2 = zext i8 %v2 to i32
  %sum1 = add i32 %sum0, %z2
  %z3 = zext i8 %v3 to i32
  %sum2 = add i32 %sum1, %z3
  %z4 = zext i8 %v4 to i32
  %sum3 = add i32 %sum2, %z4
  %z5 = zext i8 %v5 to i32
  %sum4 = add i32 %sum3, %z5
  %z6 = zext i8 %v6 to i32
  %sum5 = add i32 %sum4, %z6
  %z7 = zext i8 %v7 to i32
  %sum6 = add i32 %sum5, %z7
  %z8 = zext i8 %v8 to i32
  %sum7 = add i32 %sum6, %z8
  %z9 = zext i8 %v9 to i32
  %sum8 = add i32 %sum7, %z9
  %z10 = zext i8 %v10 to i32
  %sum9 = add i32 %sum8, %z10
  %z11 = zext i8 %v11 to i32
  %sum10 = add i32 %sum9, %z11
  %z12 = zext i8 %v12 to i32
  %sum11 = add i32 %sum10, %z12
  %z13 = zext i8 %v13 to i32
  %sum12 = add i32 %sum11, %z13
  %z14 = zext i8 %v14 to i32
  %sum13 = add i32 %sum12, %z14
  %z15 = zext i8 %v15 to i32
  %sum14 = add i32 %sum13, %z15
  ret i32 %sum14
}
