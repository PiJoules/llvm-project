#include <assert.h>
#include <stdlib.h>

#include "csan_interface_internal.h"
#include "interception/interception.h"
#include "sanitizer_common/sanitizer_allocator_dlsym.h"
#include "sanitizer_common/sanitizer_common.h"
#include "sanitizer_common/sanitizer_placement_new.h"

#include "sanitizer_common/sanitizer_common.h"
#include "sanitizer_common/sanitizer_internal_defs.h"
#include "sanitizer_common/sanitizer_stacktrace.h"

using namespace __sanitizer;

DECLARE_REAL_AND_INTERCEPTOR(void *, malloc, uptr)
DECLARE_REAL_AND_INTERCEPTOR(void *, calloc, uptr, uptr)
DECLARE_REAL_AND_INTERCEPTOR(void *, realloc, void *, uptr)
DECLARE_REAL_AND_INTERCEPTOR(void *, aligned_alloc, uptr, uptr)
DECLARE_REAL_AND_INTERCEPTOR(void, free, void *)

namespace __csan {

namespace {

uptr __csan_shadow_mem_start, __csan_shadow_mem_end;
bool csan_inited = false;

constexpr u64 kShadowScale = 3;

inline uptr MemToShadowSize(uptr size) {
  // This always ensures MemToShadowSize returns at least 1 byte.
  return RoundUpTo(size, 1 << kShadowScale) >> kShadowScale;
}

inline uptr MemToShadow(uptr p) {
  return (p >> kShadowScale) + __csan_shadow_mem_start;
}

inline void *MemToShadow(void const volatile *p) {
  return (void *)MemToShadow((uptr)p);
}

struct CsanMetadata {
  size_t requested_size;
};

// allocator info.
typedef CompactSizeClassMap InternalSizeClassMap;
struct AP32 {
  static const uptr kSpaceBeg = SANITIZER_MMAP_BEGIN;
  static const u64 kSpaceSize = SANITIZER_MMAP_RANGE_SIZE;
  static const uptr kMetadataSize = sizeof(CsanMetadata);
  typedef InternalSizeClassMap SizeClassMap;
  static const uptr kRegionSizeLog = 20;
  using AddressSpaceView = LocalAddressSpaceView;
  typedef NoOpMapUnmapCallback MapUnmapCallback;
  static const uptr kFlags = 0;
};
typedef SizeClassAllocator32<AP32> PrimaryCsanAllocator;

typedef CombinedAllocator<PrimaryCsanAllocator,
                          LargeMmapAllocatorPtrArrayStatic>
    CsanAllocator;
typedef CsanAllocator::AllocatorCache CsanAllocatorCache;

static CsanAllocator allocator;
static CsanAllocatorCache allocator_cache;

void Initialize() {
  if (csan_inited)
    return;

  const uptr max_user_va = GetMaxUserVirtualAddress();
  uptr shadow_size_bytes = max_user_va >> kShadowScale;
  uptr high_mem_end =
      max_user_va; // | ((GetMmapGranularity() << kShadowScale) - 1);
  __csan_shadow_mem_start = MapDynamicShadow(
      shadow_size_bytes, kShadowScale,
      /*min_shadow_base_alignment=*/0, high_mem_end, GetMmapGranularity());
  __csan_shadow_mem_end = __csan_shadow_mem_start + shadow_size_bytes;
  ReserveShadowMemoryRange(__csan_shadow_mem_start, __csan_shadow_mem_end,
                           "csan");
  // Printf("[csan] shadow start: 0x%lx, high mem end: 0x%lx\n",
  //        __csan_shadow_mem_start, high_mem_end);

  INTERCEPT_FUNCTION(malloc);
  INTERCEPT_FUNCTION(realloc);
  INTERCEPT_FUNCTION(calloc);
  INTERCEPT_FUNCTION(free);

  allocator.Init(kReleaseToOSIntervalNever);

  csan_inited = true;
}

void ResetApplicationMemory(const void *app_addr, uptr app_size) {
  unsigned char *shadow_addr =
      reinterpret_cast<unsigned char *>(MemToShadow(app_addr));
  uptr shadow_size = MemToShadowSize(app_size);
  internal_memset(shadow_addr, 0, shadow_size);
}

void *Allocate(uptr requested_size, uptr requested_align) {
  void *alloc =
      allocator.Allocate(&allocator_cache, requested_size, requested_align);

  // Sanity check.
  CHECK(allocator.PointerIsMine(alloc));
  CHECK_EQ(allocator.GetBlockBegin(alloc), alloc);

  CsanMetadata *MD =
      reinterpret_cast<CsanMetadata *>(allocator.GetMetaData(alloc));
  CHECK_EQ(reinterpret_cast<uptr>(MD) % alignof(CsanMetadata), 0);
  new (MD) CsanMetadata{requested_size};

  // Mark allocation memory as initialized (all zeros).
  ResetApplicationMemory(alloc, requested_size);

  // Printf("[csan] allocate %zu bytes, ptr %p\n", requested_size, alloc);
  return alloc;
}

// constexpr unsigned char kWrittenToBits = 0x0F;
// constexpr unsigned char kReadFromBits = 0xF0;
constexpr unsigned char kWrittenToBits = 0xFF;

void DumpShadowMemory(const void *app_addr, uptr app_size) {
  const unsigned char *shadow_ptr =
      (const unsigned char *)MemToShadow(app_addr);
  Printf("Shadow memory for %p, size %zu: ", app_addr, app_size);
  for (uptr i = 0; i < MemToShadowSize(app_size); ++i) {
    Printf("0x%02x ", shadow_ptr[i]);
  }
  Printf("\n");
}

void Free(void *ptr) {
  if (!allocator.PointerIsMine(ptr))
    return;

  CHECK_EQ(allocator.GetBlockBegin(ptr), ptr);

  // Check shadow mem.
  bool obj_written_to = false;
  const unsigned char *shadow_ptr = (const unsigned char *)MemToShadow(ptr);
  CsanMetadata *MD =
      reinterpret_cast<CsanMetadata *>(allocator.GetMetaData(ptr));
  for (uptr i = 0; i < MemToShadowSize(MD->requested_size); ++i) {
    unsigned char shadow_byte = shadow_ptr[i];
    if (shadow_byte == kWrittenToBits) {
      obj_written_to = true;
      break;
    }
  }

  if (obj_written_to) {
    Printf("[csan] ERROR: Object at %p was written to but never read\n", ptr);
    DumpShadowMemory(ptr, MD->requested_size);
  }

  // Printf("[csan] free ptr %p\n", ptr);

  MD->~CsanMetadata();
  allocator.Deallocate(&allocator_cache, ptr);
}

void MarkWritten(void *ptr, uptr size) {
  if (!allocator.PointerIsMine(ptr))
    return;

  void *start = allocator.GetBlockBegin(ptr);
  CsanMetadata *MD =
      reinterpret_cast<CsanMetadata *>(allocator.GetMetaData(ptr));
  // Printf("[csan] WRITE to %p of size %zu; Mark alloc at %p, size %zu as
  // written\n", ptr, size, start, MD->requested_size);

  // Poison the whole object.
  size_t shadow_size = MemToShadowSize(MD->requested_size);
  unsigned char *shadow_ptr = (unsigned char *)MemToShadow(start);
  internal_memset(shadow_ptr, kWrittenToBits, shadow_size);
}

void MarkRead(void *ptr, uptr size) {
  if (!allocator.PointerIsMine(ptr))
    return;

  void *start = allocator.GetBlockBegin(ptr);
  CsanMetadata *MD =
      reinterpret_cast<CsanMetadata *>(allocator.GetMetaData(ptr));
  // Printf("[csan] READ to %p of size %zu; Mark alloc at %p, size %zu as
  // written\n", ptr, size, start, MD->requested_size);

  // Reset the whole object back to the read state.
  size_t shadow_size = MemToShadowSize(MD->requested_size);
  unsigned char *shadow_ptr = (unsigned char *)MemToShadow(start);
  internal_memset(shadow_ptr, 0, shadow_size);
}

} // namespace

} // namespace __csan

INTERCEPTOR(void *, malloc, uptr size) {
  // return REAL(malloc)(size);
  return __csan::Allocate(size, /*align=*/8);
}

INTERCEPTOR(void *, calloc, uptr nmemb, uptr size) {
  void *res = REAL(calloc)(nmemb, size);
  return res;
}

INTERCEPTOR(void, free, void *ptr) {
  __csan::Free(ptr);
  // REAL(free)(ptr);
}

INTERCEPTOR(void *, realloc, void *ptr, uptr size) {
  __csan::Free(ptr);
  return __csan::Allocate(size, /*align=*/8);
}

void __csan_init() { __csan::Initialize(); }

void *__csan_alloc(uptr size, uptr align) {
  return __csan::Allocate(size, align);
}

void __csan_free(void *ptr) { __csan::Free(ptr); }

void __csan_mark_written(void *ptr, uptr size) {
  __csan::MarkWritten(ptr, size);
}

void __csan_mark_read(void *ptr, uptr size) { __csan::MarkRead(ptr, size); }
