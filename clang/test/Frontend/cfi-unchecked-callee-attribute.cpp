// RUN: %clang_cc1 -Wall -Wno-unused -Wno-uninitialized -verify %s

#define CFI_UNCHECKED_CALLEE __attribute__((cfi_unchecked_callee))

void unchecked(void) CFI_UNCHECKED_CALLEE {}
void checked(void) {}

void (*checked_ptr)(void) = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
void (CFI_UNCHECKED_CALLEE *unchecked_ptr)(void) = unchecked;
void (CFI_UNCHECKED_CALLEE *from_normal)(void) = checked;
void (CFI_UNCHECKED_CALLEE *c_no_function_decay)(void) = &unchecked;
void (CFI_UNCHECKED_CALLEE *arr[10])(void);
void (*cfi_elem)(void) = arr[1];  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
void (CFI_UNCHECKED_CALLEE *cfi_unchecked_elem)(void) = arr[1];
void (CFI_UNCHECKED_CALLEE &ref)(void) = unchecked;
void (CFI_UNCHECKED_CALLEE &ref2)(void) = *unchecked;
void (&ref_cfi_unchecked)(void) = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void ()' to 'void ()' discards `cfi_unchecked_callee` attribute}}
void (&ref_cfi_unchecked2)(void) = *unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void ()' to 'void ()' discards `cfi_unchecked_callee` attribute}}

void (CFI_UNCHECKED_CALLEE *unchecked_from_deref)(void) = &*unchecked;
void (*checked_from_deref)(void) = &*unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}

typedef void (CFI_UNCHECKED_CALLEE unchecked_func_t)(void);
typedef void (checked_func_t)(void);
typedef void (CFI_UNCHECKED_CALLEE *unchecked_func_ptr_t)(void);
typedef void (*checked_func_ptr_t)(void);
checked_func_t *checked_func = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
unchecked_func_t *unchecked_func = unchecked;

void UsageOnImproperTypes() {
  int CFI_UNCHECKED_CALLEE i;  // expected-warning{{use of `cfi_unchecked_callee` on 'int'; can only be used on function types}}

  /// Here `cfi_unchecked_callee` is applied to the pointer here rather than the actual function.
  void (* CFI_UNCHECKED_CALLEE func)(void);  // expected-warning{{use of `cfi_unchecked_callee` on 'void (*)()'; can only be used on function types}}

  /// Here `cfi_unchecked_callee` is applied to the returned `void` rather than the function.
  void CFI_UNCHECKED_CALLEE returns_cfi_unchecked_void(void);  // expected-warning{{use of `cfi_unchecked_callee` on 'void'; can only be used on function types}}
  CFI_UNCHECKED_CALLEE void returns_cfi_unchecked_void2(void);  // expected-warning{{use of `cfi_unchecked_callee` on 'void'; can only be used on function types}}
}

/// Explicit casts suppress the warning.
void CheckCasts() {
  void (*should_warn)(void) = unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}

  void (*no_warn_c_style_cast)(void) = (void (*)(void))unchecked;
  void (*no_warn_static_cast)(void) = static_cast<void (*)(void)>(unchecked);
  void (*no_warn_reinterpret_cast)(void) = reinterpret_cast<void (*)(void)>(unchecked);

  struct A {};
  void (CFI_UNCHECKED_CALLEE A::*cfi_unchecked_member_ptr)(void);
  // expected-warning@+1{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (A::*)()' to 'void (A::*)()' discards `cfi_unchecked_callee` attribute}}
  void (A::*member_ptr)(void) = cfi_unchecked_member_ptr; 

  struct B {} CFI_UNCHECKED_CALLEE b;  // expected-warning{{use of `cfi_unchecked_callee` on 'B'; can only be used on function types}}
  struct CFI_UNCHECKED_CALLEE C {} c;  // expected-warning{{use of `cfi_unchecked_callee` on 'C'; can only be used on function types}}
  CFI_UNCHECKED_CALLEE struct D {} d;  // expected-warning{{use of `cfi_unchecked_callee` on 'struct D'; can only be used on function types}}

  void *ptr2 = (void *)unchecked;
}

void CheckDifferentConstructions() {
  checked_func_t *checked_func(unchecked_func);  // expected-warning{{implicit conversion from 'unchecked_func_t *' (aka 'void (*)()') to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
  new (checked_func_t *)(unchecked_func);  // expected-warning{{implicit conversion from 'unchecked_func_t *' (aka 'void (*)()') to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
  struct S {
    checked_func_t *checked_func;

    // expected-warning@+1{{implicit conversion from 'unchecked_func_t *' (aka 'void (*)()') to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
    S(unchecked_func_t *unchecked_func) : checked_func(unchecked_func) {}
  };

  checked_func_t *checked_func2{unchecked_func};  // expected-warning{{implicit conversion from 'unchecked_func_t *' (aka 'void (*)()') to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
  checked_ptr = checked_func_ptr_t(unchecked);
}

checked_func_t *returning_checked_func() {
  return unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}
}

int checked_arg_func(checked_func_t *checked_func);
int invoke = checked_arg_func(unchecked);  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'checked_func_t *' (aka 'void (*)()') discards `cfi_unchecked_callee` attribute}}

/// Note that function attributes aren't propagated through templates, so T
/// will always be the canonical (non-attributed) type.
template <typename T>
struct CFIParam {
  CFIParam(T *ptr) {}
};
template <typename T>
struct NoCFIParam {
  NoCFIParam(T CFI_UNCHECKED_CALLEE *ptr) {}
};
CFIParam<unchecked_func_t> s(checked);
CFIParam<unchecked_func_t> s2(unchecked);  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
CFIParam<checked_func_t> s3(checked);
CFIParam<checked_func_t> s4(unchecked);  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
NoCFIParam<unchecked_func_t> s5(checked);
NoCFIParam<unchecked_func_t> s6(unchecked);
NoCFIParam<checked_func_t> s7(checked);
NoCFIParam<checked_func_t> s8(unchecked);

void no_args() __attribute__((cfi_unchecked_callee(10)));  // expected-error{{'cfi_unchecked_callee' attribute takes no arguments}}

void bracket_cfi_unchecked(void) [[clang::cfi_unchecked_callee]] {}

void BracketNotation() {
  checked_ptr = bracket_cfi_unchecked;  // expected-warning{{implicit conversion from 'void (*)() __attribute__((cfi_unchecked_callee))' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}
}

void Comparisons() {
  /// Let's be able to compare checked and unchecked pointers without warnings.
  unchecked == checked_ptr;
  checked_ptr == unchecked;
  unchecked == unchecked_ptr;
  unchecked != checked_ptr;
  checked_ptr != unchecked;
  unchecked != unchecked_ptr;

  (void (*)(void))unchecked == checked_ptr;
  checked_ptr == (void (*)(void))unchecked;

  struct S {
    typedef void CB() CFI_UNCHECKED_CALLEE;
    constexpr bool operator==(const S &other) const {
      return cb == other.cb;
    }
    CB *cb;
  };
}

void MemberFunctionPointer() {
  struct A {
    void unchecked() CFI_UNCHECKED_CALLEE {}
    virtual void unchecked_virtual() CFI_UNCHECKED_CALLEE {}
    static void unchecked_static() CFI_UNCHECKED_CALLEE {}

    void checked() {}
    virtual void checked_virtual() {}
    static void checked_static() {}
  };

  void (CFI_UNCHECKED_CALLEE A::*unchecked_func)() = &A::unchecked;
  unchecked_func = &A::unchecked_virtual;
  unchecked_ptr = &A::unchecked_static;

  void (A::*checked_func)() = &A::unchecked;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (A::*)()' to 'void (A::*)()' discards `cfi_unchecked_callee` attribute}}
  checked_func = &A::unchecked_virtual;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (A::*)()' to 'void (A::*)()' discards `cfi_unchecked_callee` attribute}}
  checked_ptr = &A::unchecked_static;  // expected-warning{{implicit conversion from 'CFI_UNCHECKED_CALLEE void (*)()' to 'void (*)()' discards `cfi_unchecked_callee` attribute}}

  unchecked_func = &A::checked;
  unchecked_func = &A::checked_virtual;
  unchecked_ptr = &A::checked_static;

  checked_func = &A::checked;
  checked_func = &A::checked_virtual;
  checked_ptr = &A::checked_static;
}
