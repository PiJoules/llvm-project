//===-- LLDBAssert.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "lldb/Utility/LLDBAssert.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include <mutex>

#if LLVM_SUPPORT_XCODE_SIGNPOSTS
#include <os/log.h>
#endif

#include <atomic>

namespace lldb_private {

/// The default callback prints to stderr.
static void DefaultAssertCallback(llvm::StringRef message,
                                  llvm::StringRef backtrace,
                                  llvm::StringRef prompt) {
  llvm::errs() << message << '\n';
  llvm::errs() << backtrace; // Backtrace includes a newline.
  llvm::errs() << prompt << '\n';
}

static std::atomic<LLDBAssertCallback> g_lldb_assert_callback =
    &DefaultAssertCallback;

void _lldb_assert(bool expression, const char *expr_text, const char *func,
                  const char *file, unsigned int line,
                  std::once_flag &once_flag) {
  if (LLVM_LIKELY(expression))
    return;

  std::call_once(once_flag, [&]() {
#if LLVM_SUPPORT_XCODE_SIGNPOSTS
    if (__builtin_available(macos 10.12, iOS 10, tvOS 10, watchOS 3, *)) {
      os_log_fault(OS_LOG_DEFAULT,
                   "Assertion failed: (%s), function %s, file %s, line %u\n",
                   expr_text, func, file, line);
    }
#endif

    std::string buffer;
    llvm::raw_string_ostream backtrace(buffer);
    llvm::sys::PrintStackTrace(backtrace);

    (*g_lldb_assert_callback.load())(
        llvm::formatv(
            "Assertion failed: ({0}), function {1}, file {2}, line {3}",
            expr_text, func, file, line)
            .str(),
        buffer,
        "Please file a bug report against lldb and include the backtrace, the "
        "version and as many details as possible.");
  });
}

void SetLLDBAssertCallback(LLDBAssertCallback callback) {
  g_lldb_assert_callback.exchange(callback);
}

} // namespace lldb_private
