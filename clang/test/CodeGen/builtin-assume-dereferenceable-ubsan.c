// RUN: %clang_cc1 -triple x86_64-unknown-unknown -emit-llvm -fsanitize=builtin -o - %s | FileCheck %s

int test1(int *a) {
  // CHECK-LABEL: @test1
  // CHECK: call void @llvm.assume
  // CHECK: call void @__ubsan_handle_nonnull_assumption
  __builtin_assume_dereferenceable(a, 10);
  return a[0];
}

int test2(int *a, int n) {
  // CHECK-LABEL: @test2
  // CHECK: call void @llvm.assume
  // CHECK: call void @__ubsan_handle_nonnull_assumption
  __builtin_assume_dereferenceable(a, n);
  return a[0];
}
