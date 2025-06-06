//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains assembly-optimized implementations of Scalable Matrix
/// Extension (SME) compatible memcpy and memmove functions.
///
/// These implementations depend on unaligned access support.
///
/// Routines taken from libc/AOR_v20.02/string/aarch64.
///
//===----------------------------------------------------------------------===//

#include "../assembly.h"

//
//  __arm_sc_memcpy / __arm_sc_memmove
//

#define dstin    x0
#define src      x1
#define count    x2
#define dst      x3
#define srcend1  x4
#define dstend1  x5
#define A_l      x6
#define A_lw     w6
#define A_h      x7
#define B_l      x8
#define B_lw     w8
#define B_h      x9
#define C_l      x10
#define C_lw     w10
#define C_h      x11
#define D_l      x12
#define D_h      x13
#define E_l      x14
#define E_h      x15
#define F_l      x16
#define F_h      x17
#define G_l      count
#define G_h      dst
#define H_l      src
#define H_h      srcend1
#define tmp1     x14

/* This implementation handles overlaps and supports both memcpy and memmove
   from a single entry point.  It uses unaligned accesses and branchless
   sequences to keep the code small, simple and improve performance.

   Copies are split into 3 main cases: small copies of up to 32 bytes, medium
   copies of up to 128 bytes, and large copies.  The overhead of the overlap
   check is negligible since it is only required for large copies.

   Large copies use a software pipelined loop processing 64 bytes per iteration.
   The destination pointer is 16-byte aligned to minimize unaligned accesses.
   The loop tail is handled by always copying 64 bytes from the end.
*/

DEFINE_COMPILERRT_FUNCTION(__arm_sc_memcpy)
        add     srcend1, src, count
        add     dstend1, dstin, count
        cmp     count, 128
        b.hi    7f  // copy_long
        cmp     count, 32
        b.hi    4f  // copy32_128

        /* Small copies: 0..32 bytes.  */
        cmp     count, 16
        b.lo    0f  // copy16
        ldp     A_l, A_h, [src]
        ldp     D_l, D_h, [srcend1, -16]
        stp     A_l, A_h, [dstin]
        stp     D_l, D_h, [dstend1, -16]
        ret

        /* Copy 8-15 bytes.  */
0:  // copy16
        tbz     count, 3, 1f  // copy8
        ldr     A_l, [src]
        ldr     A_h, [srcend1, -8]
        str     A_l, [dstin]
        str     A_h, [dstend1, -8]
        ret

        .p2align 3
        /* Copy 4-7 bytes.  */
1:  // copy8
        tbz     count, 2, 2f  // copy4
        ldr     A_lw, [src]
        ldr     B_lw, [srcend1, -4]
        str     A_lw, [dstin]
        str     B_lw, [dstend1, -4]
        ret

        /* Copy 0..3 bytes using a branchless sequence.  */
2:  // copy4
        cbz     count, 3f // copy0
        lsr     tmp1, count, 1
        ldrb    A_lw, [src]
        ldrb    C_lw, [srcend1, -1]
        ldrb    B_lw, [src, tmp1]
        strb    A_lw, [dstin]
        strb    B_lw, [dstin, tmp1]
        strb    C_lw, [dstend1, -1]
3:  // copy0
        ret

        .p2align 4
        /* Medium copies: 33..128 bytes.  */
4:  // copy32_128
        ldp     A_l, A_h, [src]
        ldp     B_l, B_h, [src, 16]
        ldp     C_l, C_h, [srcend1, -32]
        ldp     D_l, D_h, [srcend1, -16]
        cmp     count, 64
        b.hi    5f  // copy128
        stp     A_l, A_h, [dstin]
        stp     B_l, B_h, [dstin, 16]
        stp     C_l, C_h, [dstend1, -32]
        stp     D_l, D_h, [dstend1, -16]
        ret

        .p2align 4
        /* Copy 65..128 bytes.  */
5:  // copy128
        ldp     E_l, E_h, [src, 32]
        ldp     F_l, F_h, [src, 48]
        cmp     count, 96
        b.ls    6f  // copy96
        ldp     G_l, G_h, [srcend1, -64]
        ldp     H_l, H_h, [srcend1, -48]
        stp     G_l, G_h, [dstend1, -64]
        stp     H_l, H_h, [dstend1, -48]
6:  // copy96
        stp     A_l, A_h, [dstin]
        stp     B_l, B_h, [dstin, 16]
        stp     E_l, E_h, [dstin, 32]
        stp     F_l, F_h, [dstin, 48]
        stp     C_l, C_h, [dstend1, -32]
        stp     D_l, D_h, [dstend1, -16]
        ret

        .p2align 4
        /* Copy more than 128 bytes.  */
7:  // copy_long
        /* Use backwards copy if there is an overlap.  */
        sub     tmp1, dstin, src
        cbz     tmp1, 3b  // copy0
        cmp     tmp1, count
        b.lo    10f //copy_long_backwards

        /* Copy 16 bytes and then align dst to 16-byte alignment.  */

        ldp     D_l, D_h, [src]
        and     tmp1, dstin, 15
        bic     dst, dstin, 15
        sub     src, src, tmp1
        add     count, count, tmp1      /* Count is now 16 too large.  */
        ldp     A_l, A_h, [src, 16]
        stp     D_l, D_h, [dstin]
        ldp     B_l, B_h, [src, 32]
        ldp     C_l, C_h, [src, 48]
        ldp     D_l, D_h, [src, 64]!
        subs    count, count, 128 + 16  /* Test and readjust count.  */
        b.ls    9f  // copy64_from_end
8:  // loop64
        stp     A_l, A_h, [dst, 16]
        ldp     A_l, A_h, [src, 16]
        stp     B_l, B_h, [dst, 32]
        ldp     B_l, B_h, [src, 32]
        stp     C_l, C_h, [dst, 48]
        ldp     C_l, C_h, [src, 48]
        stp     D_l, D_h, [dst, 64]!
        ldp     D_l, D_h, [src, 64]!
        subs    count, count, 64
        b.hi    8b  // loop64

        /* Write the last iteration and copy 64 bytes from the end.  */
9:  // copy64_from_end
        ldp     E_l, E_h, [srcend1, -64]
        stp     A_l, A_h, [dst, 16]
        ldp     A_l, A_h, [srcend1, -48]
        stp     B_l, B_h, [dst, 32]
        ldp     B_l, B_h, [srcend1, -32]
        stp     C_l, C_h, [dst, 48]
        ldp     C_l, C_h, [srcend1, -16]
        stp     D_l, D_h, [dst, 64]
        stp     E_l, E_h, [dstend1, -64]
        stp     A_l, A_h, [dstend1, -48]
        stp     B_l, B_h, [dstend1, -32]
        stp     C_l, C_h, [dstend1, -16]
        ret

        .p2align 4

        /* Large backwards copy for overlapping copies.
           Copy 16 bytes and then align dst to 16-byte alignment.  */
10: // copy_long_backwards
        ldp     D_l, D_h, [srcend1, -16]
        and     tmp1, dstend1, 15
        sub     srcend1, srcend1, tmp1
        sub     count, count, tmp1
        ldp     A_l, A_h, [srcend1, -16]
        stp     D_l, D_h, [dstend1, -16]
        ldp     B_l, B_h, [srcend1, -32]
        ldp     C_l, C_h, [srcend1, -48]
        ldp     D_l, D_h, [srcend1, -64]!
        sub     dstend1, dstend1, tmp1
        subs    count, count, 128
        b.ls    12f // copy64_from_start

11: // loop64_backwards
        stp     A_l, A_h, [dstend1, -16]
        ldp     A_l, A_h, [srcend1, -16]
        stp     B_l, B_h, [dstend1, -32]
        ldp     B_l, B_h, [srcend1, -32]
        stp     C_l, C_h, [dstend1, -48]
        ldp     C_l, C_h, [srcend1, -48]
        stp     D_l, D_h, [dstend1, -64]!
        ldp     D_l, D_h, [srcend1, -64]!
        subs    count, count, 64
        b.hi    11b // loop64_backwards

        /* Write the last iteration and copy 64 bytes from the start.  */
12: // copy64_from_start
        ldp     G_l, G_h, [src, 48]
        stp     A_l, A_h, [dstend1, -16]
        ldp     A_l, A_h, [src, 32]
        stp     B_l, B_h, [dstend1, -32]
        ldp     B_l, B_h, [src, 16]
        stp     C_l, C_h, [dstend1, -48]
        ldp     C_l, C_h, [src]
        stp     D_l, D_h, [dstend1, -64]
        stp     G_l, G_h, [dstin, 48]
        stp     A_l, A_h, [dstin, 32]
        stp     B_l, B_h, [dstin, 16]
        stp     C_l, C_h, [dstin]
        ret
END_COMPILERRT_FUNCTION(__arm_sc_memcpy)

DEFINE_COMPILERRT_FUNCTION_ALIAS(__arm_sc_memmove, __arm_sc_memcpy)

