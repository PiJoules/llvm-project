; Test that HWAddressSanitizer instruments multiple accesses to the same pointer only once per basic block.
;
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=1 | FileCheck %s --check-prefix=OPT
; RUN: opt < %s -passes=hwasan -S -hwasan-opt=0 | FileCheck %s --check-prefix=NOOPT
; RUN: opt < %s -passes=hwasan -S -hwasan-opt-same-temp=0 | FileCheck %s --check-prefix=NOOPT

target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64--linux-android10000"

declare void @opaque_call()

; Load followed by store to the same pointer in the same BB.
define void @test_load_then_store(ptr %a) sanitize_hwaddress {
entry:
  %v = load i32, ptr %a, align 4
  %v2 = add i32 %v, 1
  store i32 %v2, ptr %a, align 4
  ret void
}

; OPT-LABEL: define void @test_load_then_store(
; OPT: llvm.hwasan.check.memaccess
; OPT-NOT: llvm.hwasan.check.memaccess
; OPT: ret void

; NOOPT-LABEL: define void @test_load_then_store(
; NOOPT: llvm.hwasan.check.memaccess
; NOOPT: llvm.hwasan.check.memaccess
; NOOPT: ret void

; Multiple loads to the same pointer in the same BB.
define i32 @test_multiple_loads(ptr %a) sanitize_hwaddress {
entry:
  %v1 = load i32, ptr %a, align 4
  %v2 = load i32, ptr %a, align 4
  %v3 = load i32, ptr %a, align 4
  %sum1 = add i32 %v1, %v2
  %sum2 = add i32 %sum1, %v3
  ret i32 %sum2
}

; OPT-LABEL: define i32 @test_multiple_loads(
; OPT: llvm.hwasan.check.memaccess
; OPT-NOT: llvm.hwasan.check.memaccess
; OPT: ret i32

; NOOPT-LABEL: define i32 @test_multiple_loads(
; NOOPT: llvm.hwasan.check.memaccess
; NOOPT: llvm.hwasan.check.memaccess
; NOOPT: llvm.hwasan.check.memaccess
; NOOPT: ret i32

; Load, call opaque function (which can deallocate/retag), then store to same pointer.
define void @test_call_clears_same_temp(ptr %a) sanitize_hwaddress {
entry:
  %v = load i32, ptr %a, align 4
  call void @opaque_call()
  store i32 %v, ptr %a, align 4
  ret void
}

; OPT-LABEL: define void @test_call_clears_same_temp(
; OPT: llvm.hwasan.check.memaccess
; OPT: call void @opaque_call()
; OPT: llvm.hwasan.check.memaccess
; OPT: ret void

; Across basic block boundary.
define void @test_across_bb(ptr %a, i1 %c) sanitize_hwaddress {
entry:
  %v = load i32, ptr %a, align 4
  br i1 %c, label %then, label %else

then:
  store i32 %v, ptr %a, align 4
  br label %exit

else:
  br label %exit

exit:
  ret void
}

; OPT-LABEL: define void @test_across_bb(
; OPT-LABEL: entry:
; OPT: llvm.hwasan.check.memaccess
; OPT-LABEL: then:
; OPT: llvm.hwasan.check.memaccess
; OPT-LABEL: exit:
; OPT: ret void
