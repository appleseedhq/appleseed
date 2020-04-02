
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#pragma once

//
// Quick reminder about Visual Studio versions:
//
//   Visual Studio 2019 Version 16.4    MSVC++ 14.24   _MSC_VER == 1924
//   Visual Studio 2019 Version 16.3    MSVC++ 14.23   _MSC_VER == 1923
//   Visual Studio 2019 Version 16.2    MSVC++ 14.22   _MSC_VER == 1922
//   Visual Studio 2019 Version 16.1    MSVC++ 14.21   _MSC_VER == 1921
//   Visual Studio 2019 Version 16.0    MSVC++ 14.2    _MSC_VER == 1920
//   Visual Studio 2017 version 15.9    MSVC++ 14.16   _MSC_VER == 1916
//   Visual Studio 2017 version 15.8    MSVC++ 14.15   _MSC_VER == 1915
//   Visual Studio 2017 version 15.7    MSVC++ 14.14   _MSC_VER == 1914
//   Visual Studio 2017 version 15.6    MSVC++ 14.13   _MSC_VER == 1913
//   Visual Studio 2017 version 15.5    MSVC++ 14.12   _MSC_VER == 1912
//   Visual Studio 2017 version 15.3    MSVC++ 14.11   _MSC_VER == 1911
//   Visual Studio 2017 version 15.3    MSVC++ 14.11   _MSC_VER == 1911
//   Visual Studio 2017 version 15.0    MSVC++ 14.1    _MSC_VER == 1910
//   Visual Studio 2015 version 14.0    MSVC++ 14.0    _MSC_VER == 1900 (oldest supported version)
//   Visual Studio 2013 version 12.0    MSVC++ 12.0    _MSC_VER == 1800 (unsupported)
//   Visual Studio 2012 version 11.0    MSVC++ 11.0    _MSC_VER == 1700 (unsupported)
//

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/compilerfeatures.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstdarg>

// Platform headers.
#if defined _MSC_VER
#include <sal.h>
#endif

namespace foundation
{

//
// Thread-local storage-class modifier.
//

// Visual C++ 2013 and earlier.
#if defined _MSC_VER && _MSC_VER < 1900
    #define APPLESEED_TLS __declspec(thread)

// gcc.
#elif defined __GNUC__
    // On gcc we use __thread because it's faster than thread_local when dynamic
    // initialization and destruction is not needed.
    // It also fixes an internal compiler error in gcc 4.8.3 when using
    // static thread local members in a template class.
    #define APPLESEED_TLS __thread

// Other compilers: use C++11's thread_local keyword.
#else
    #define APPLESEED_TLS thread_local
#endif


//
// A qualifier to force inlining of a function/method on supported compilers.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_FORCE_INLINE __forceinline

// gcc.
#elif defined __GNUC__
    #define APPLESEED_FORCE_INLINE inline __attribute__((always_inline))

// CUDA.
#elif defined __CUDACC__
    #define APPLESEED_FORCE_INLINE __forceinline__

// Other compilers: fall back to standard inlining.
#else
    #define APPLESEED_FORCE_INLINE inline
#endif


//
// A qualifier to prevent inlining of a function/method on supported compilers.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_NO_INLINE __declspec(noinline)

// gcc.
#elif defined __GNUC__
    #define APPLESEED_NO_INLINE __attribute__((noinline))

// CUDA.
#elif defined __CUDACC__
    #define APPLESEED_NO_INLINE __noinline__

// Other compilers: ignore the qualifier.
#else
    #define APPLESEED_NO_INLINE
#endif


//
// Qualifiers to specify the alignment of a variable, a structure member or a structure.
//
// APPLESEED_SIMD4_ALIGN aligns on a 16-byte boundary as required by SSE load/store instructions.
// APPLESEED_SIMD8_ALIGN aligns on a 32-byte boundary as required by AVX load/store instructions.
//
// Note that APPLESEED_SIMDx_ALIGN *always* performs the alignment, regardless of whether or not
// SSE is enabled in the build configuration.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_ALIGN(n) __declspec(align(n))
    #define APPLESEED_SIMD4_ALIGN APPLESEED_ALIGN(16)
    #define APPLESEED_SIMD8_ALIGN APPLESEED_ALIGN(32)

// gcc.
#elif defined __GNUC__
    #define APPLESEED_ALIGN(n) __attribute__((aligned(n)))
    #define APPLESEED_SIMD4_ALIGN APPLESEED_ALIGN(16)
    #define APPLESEED_SIMD8_ALIGN APPLESEED_ALIGN(32)

// Other compilers: ignore the qualifier, and leave APPLESEED_SIMDX_ALIGN undefined.
#else
    #define APPLESEED_ALIGN(n)
#endif


//
// Return the alignment requirement of a type.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_ALIGNOF(t) __alignof(t)

// gcc.
#elif defined __GNUC__
    #define APPLESEED_ALIGNOF(t) __alignof__(t)

// Other compilers: abort compilation.
#else
    #error APPLESEED_ALIGNOF is not defined for this compiler.
#endif


//
// A qualifier similar to the 'restrict' keyword in C99.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_RESTRICT __restrict

// gcc, clang or CUDA.
#elif defined __GNUC__ || defined __clang__ || defined __CUDACC__
    #define APPLESEED_RESTRICT __restrict__

// Other compilers: ignore the qualifier.
#else
    #define APPLESEED_RESTRICT
#endif


//
// A qualifier to inform the compiler that code is unreachable.
//

// Visual C++.
#if defined _MSC_VER
    #define APPLESEED_UNREACHABLE assert(!"This code was assumed to be unreachable."); __assume(0)

// gcc: supported since gcc 4.5.
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
    #define APPLESEED_UNREACHABLE assert(!"This code was assumed to be unreachable."); __builtin_unreachable()

// Other compilers: assert in debug, ignore in release.
#else
    #define APPLESEED_UNREACHABLE assert(!"This code was assumed to be unreachable.")
#endif


//
//  A macro to provide the compiler with branch prediction information.
//  Usage: replace if (cond) with if (APPLESEED_LIKELY(cond))
//  Warning: programmers are notoriously bad at guessing this.
//  It should only be used after profiling.
//

#if defined(__GNUC__)
    #define APPLESEED_LIKELY(x)   (__builtin_expect(bool(x), true))
    #define APPLESEED_UNLIKELY(x) (__builtin_expect(bool(x), false))
#else
    #define APPLESEED_LIKELY(x)   (x)
    #define APPLESEED_UNLIKELY(x) (x)
#endif


//
//  A macro to mark a variable as unused. Useful in unit tests.
//

// gcc: supported since gcc 4.6.
#if defined(__GNUC__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 6)))
    #define APPLESEED_UNUSED __attribute__((unused))

// clang.
#elif defined(__clang__)
    #define APPLESEED_UNUSED __attribute__((unused))

// Other compilers: ignore.
#else
    #define APPLESEED_UNUSED
#endif


//
// Utility macros converting their argument to a string literal:
//   APPLESEED_TO_STRING_EVAL first expands the argument definition.
//   APPLESEED_TO_STRING_NOEVAL does not expand the argument definition.
//

#define APPLESEED_TO_STRING_EVAL(x) APPLESEED_TO_STRING_NOEVAL(x)
#define APPLESEED_TO_STRING_NOEVAL(x) #x


//
// Utility macro representing an empty parameter to another macro.
// Omitting macro parameters is supported in C99, but not yet in C++98;
// using a macro expanding to nothing is a way to work around this limitation.
// Named APPLESEED_EMPTY instead of simply EMPTY to prevent possible name
// collisions.
//

#define APPLESEED_EMPTY


//
// Up to version 2012, Visual C++ doesn't provide the va_copy() macro
// introduced in C99. On Windows (32-bit and 64-bit) it is sufficient
// to use assignment between va_list variables.
// Starting with Visual C++ 2013, va_copy() is natively available.
//

#if defined _MSC_VER && _MSC_VER < 1800
    #ifndef va_copy
    #define va_copy(dst, src) ((dst) = (src))
    #endif
#endif


//
// Source code annotations.
//
// About APPLESEED_PRINTF_FMT and APPLESEED_PRINTF_FMT_ATTR() usage:
//
//   From http://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html:
//
//   The parameter string_index specifies which argument is the format string argument
//   (starting from 1), while first_to_check is the number of the first argument to
//   check against the format string. For functions where the arguments are not
//   available to be checked (such as vprintf), specify the third parameter as zero.
//   In this case the compiler only checks the format string for consistency. [...]
//
//   Since non-static C++ methods have an implicit 'this' argument, the arguments of
//   such methods should be counted from two, not one, when giving values for
//   string_index and first_to_check.
//

// Visual C++: Visual Studio 2008+ annotations.
#if defined _MSC_VER
    #define APPLESEED_PRINTF_FMT _Printf_format_string_
    #define APPLESEED_PRINTF_FMT_ATTR(string_index, first_to_check)

// gcc.
#elif defined __GNUC__
    #define APPLESEED_PRINTF_FMT
    #define APPLESEED_PRINTF_FMT_ATTR(string_index, first_to_check) __attribute__((format(printf, string_index, first_to_check)))

// Other compilers: annotations have no effect.
#else
    #define APPLESEED_PRINTF_FMT
    #define APPLESEED_PRINTF_FMT_ATTR(string_index, first_to_check)
#endif


//
// CUDA specific defines.
//

#ifdef __CUDACC__
    #define APPLESEED_HOST                  __host__
    #define APPLESEED_DEVICE                __device__
    #define APPLESEED_HOST_DEVICE           __host__ __device__
    #define APPLESEED_DEVICE_INLINE         __forceinline__ __device__
    #define APPLESEED_HOST_DEVICE_INLINE    __forceinline__ __host__ __device__
    #define APPLESEED_DEVICE_ALIGN(n)       __align__(n)
#else
    #define APPLESEED_HOST
    #define APPLESEED_HOST_DEVICE
    #define APPLESEED_HOST_DEVICE_INLINE    inline
    #define APPLESEED_DEVICE_ALIGN(n)
#endif

#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ > 0
    #define APPLESEED_DEVICE_COMPILATION    // defined when compiling CUDA device code
#endif


//
// The Compiler class provides information about the compiler used to build the library.
//

class APPLESEED_DLLSYMBOL Compiler
  : public NonCopyable
{
  public:
    // Return the name of the compiler.
    static const char* get_compiler_name();

    // Return the version of the compiler.
    static const char* get_compiler_version();
};

}   // namespace foundation
