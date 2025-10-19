#ifndef CSAN_H
#define CSAN_H

#include "sanitizer_common/sanitizer_internal_defs.h"

using __sanitizer::u32;
using __sanitizer::u64;
using __sanitizer::uptr;

extern "C" {

// Should be called at the very beginning of the process before any instrumented
// code executes.
SANITIZER_INTERFACE_ATTRIBUTE void __csan_init();

SANITIZER_INTERFACE_ATTRIBUTE void
__csan_ctor_enter_callback(const void *this_ptr, uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_ctor_exit_callback(const void *this_ptr, uptr obj_size);
// Called from copy c'tor or copy assignment operator to mark an object instance
// as being a copy. If the instance is never modified again before its d'tor
// runs, the copy is classified as "unnecessary".
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_copy_ctor_enter_callback(const void *this_ptr, const void *other_ptr,
                                uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_copy_ctor_exit_callback(const void *this_ptr, const void *other_ptr,
                               uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_copy_assign_op_enter_callback(const void *this_ptr,
                                     const void *other_ptr, uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_copy_assign_op_exit_callback(const void *this_ptr, const void *other_ptr,
                                    uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_dtor_enter_callback(const void *this_ptr, uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void
__csan_dtor_exit_callback(const void *this_ptr, uptr obj_size);
SANITIZER_INTERFACE_ATTRIBUTE void __csan_store_callback(const void *addr,
                                                         uptr size);

SANITIZER_INTERFACE_ATTRIBUTE void *__csan_alloc(uptr size, uptr align);
SANITIZER_INTERFACE_ATTRIBUTE void __csan_mark_written(void *ptr, uptr size);
SANITIZER_INTERFACE_ATTRIBUTE void __csan_mark_read(void *ptr, uptr size);
SANITIZER_INTERFACE_ATTRIBUTE void __csan_free(void *ptr);

} // extern "C"

#endif // CSAN_H
