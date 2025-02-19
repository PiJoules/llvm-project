// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s

#include <stdio.h>

#if !defined(_MSC_VER) || defined(__clang__)
// Required for ld64 macOS 12.0+
__attribute__((weak)) extern "C" void foo() {}
#endif

extern "C" void __sanitizer_report_error_summary(const char *summary) {
  fprintf(stderr, "test_report_error_summary\n");
  // CHECK: test_report_error_summary
  fflush(stderr);
}

char *x;

int main() {
  x = new char[20];
  delete[] x;
  return x[0];
}
