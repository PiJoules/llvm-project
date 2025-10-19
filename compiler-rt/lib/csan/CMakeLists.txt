include_directories(..)

# Runtime library sources and build flags.
set(CSAN_RTL_SOURCES
  csan.cpp)
  #csan_allocator.cpp
  #csan_flags.cpp
  #csan_interceptors.cpp
  #csan_new_delete.cpp
  #csan_shadow.cpp
  #csan_stack.cpp
  #csan_state.cpp)

set(CSAN_COMMON_CFLAGS ${SANITIZER_COMMON_CFLAGS})
append_rtti_flag(OFF CSAN_COMMON_CFLAGS)
# Prevent clang from generating libc calls.
append_list_if(COMPILER_RT_HAS_FFREESTANDING_FLAG -ffreestanding CSAN_COMMON_CFLAGS)

# Static runtime library.
add_compiler_rt_component(csan)

foreach(arch ${CSAN_SUPPORTED_ARCH})
  set(CSAN_CFLAGS ${CSAN_COMMON_CFLAGS})
  append_list_if(COMPILER_RT_HAS_FPIE_FLAG -fPIE CSAN_CFLAGS)
  add_compiler_rt_runtime(clang_rt.csan
    STATIC
    ARCHS ${arch}
    SOURCES ${CSAN_RTL_SOURCES}
            $<TARGET_OBJECTS:RTInterception.${arch}>
            $<TARGET_OBJECTS:RTSanitizerCommon.${arch}>
            $<TARGET_OBJECTS:RTSanitizerCommonLibc.${arch}>
            $<TARGET_OBJECTS:RTSanitizerCommonSymbolizer.${arch}>
    CFLAGS ${CSAN_CFLAGS}
    PARENT_TARGET csan)
  add_sanitizer_rt_symbols(clang_rt.csan
    ARCHS ${arch}
    EXTRA csan.syms.extra)
  add_dependencies(csan
    clang_rt.csan-${arch}-symbols)
endforeach()
