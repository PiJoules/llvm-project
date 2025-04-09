// RUN: %clang_cc1 -Wall -Wno-unused -Wno-uninitialized -verify %s

#define CFI_UNCHECKED_CALLEE __attribute__((cfi_unchecked_callee))

void unchecked() CFI_UNCHECKED_CALLEE {}
void checked() {}

void (*checked_ptr)() = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
void (CFI_UNCHECKED_CALLEE *unchecked_ptr)() = unchecked;
void (CFI_UNCHECKED_CALLEE *from_normal)() = checked;
void (CFI_UNCHECKED_CALLEE *c_no_function_decay)() = &unchecked;

typedef void (CFI_UNCHECKED_CALLEE unchecked_func_t)();
typedef void (checked_func_t)();
typedef void (CFI_UNCHECKED_CALLEE *cfi_unchecked_func_ptr_t)();
typedef void (*checked_func_ptr_t)();
checked_func_t *cfi_func = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
unchecked_func_t *unchecked_func = unchecked;

void UsageOnImproperTypes() {
  int CFI_UNCHECKED_CALLEE i;  // expected-warning{{use of `cfi_unchecked_callee` on 'int'; can only be used on function types}}

  /// Here `cfi_unchecked_callee` is applied to the pointer here rather than the actual function.
  void (* CFI_UNCHECKED_CALLEE func)();  // expected-warning{{use of `cfi_unchecked_callee` on 'void (*)()'; can only be used on function types}}

  /// Here `cfi_unchecked_callee` is applied to the returned `void` rather than the function.
  void CFI_UNCHECKED_CALLEE returns_cfi_unchecked_void();  // expected-warning{{use of `cfi_unchecked_callee` on 'void'; can only be used on function types}}
  CFI_UNCHECKED_CALLEE void returns_cfi_unchecked_void2();  // expected-warning{{use of `cfi_unchecked_callee` on 'void'; can only be used on function types}}
}

/// Explicit casts suppress the warning.
void CheckCasts() {
  void (*should_warn)() = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}

  void (*no_warn_c_style_cast)() = (void (*)())unchecked;

  struct B {} CFI_UNCHECKED_CALLEE b;  // expected-warning{{use of `cfi_unchecked_callee` on 'struct B'; can only be used on function types}}
  struct CFI_UNCHECKED_CALLEE C {} c;  // expected-warning{{use of `cfi_unchecked_callee` on 'struct C'; can only be used on function types}}
  CFI_UNCHECKED_CALLEE struct D {} d;  // expected-warning{{use of `cfi_unchecked_callee` on 'struct D'; can only be used on function types}}

  void *ptr2 = (void *)unchecked;
}

int checked_arg_func(checked_func_t *checked_func);

void CheckDifferentConstructions() {
  void (CFI_UNCHECKED_CALLEE *arr[10])();
  void (*cfi_elem)() = arr[1];  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
  void (CFI_UNCHECKED_CALLEE *cfi_unchecked_elem)() = arr[1];

  int invoke = checked_arg_func(unchecked);  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
}

checked_func_t *returning_checked_func() {
  return unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
}

void no_args() __attribute__((cfi_unchecked_callee(10)));  // expected-error{{'cfi_unchecked_callee' attribute takes no arguments}}

void Comparisons() {
  /// Let's be able to compare checked and unchecked pointers without warnings.
  unchecked == checked_ptr;
  checked_ptr == unchecked;
  unchecked == unchecked_ptr;
  unchecked != checked_ptr;
  checked_ptr != unchecked;
  unchecked != unchecked_ptr;

  (void (*)())unchecked == checked_ptr;
  checked_ptr == (void (*)())unchecked;
}
