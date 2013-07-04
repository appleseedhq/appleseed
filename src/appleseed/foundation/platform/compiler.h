
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_COMPILER_H
#define APPLESEED_FOUNDATION_PLATFORM_COMPILER_H

//
// Quick reminder about Visual Studio versions:
//
//   Visual Studio 2012   MSVC++ 11.0   _MSC_VER == 1700
//   Visual Studio 2010   MSVC++ 10.0   _MSC_VER == 1600
//   Visual Studio 2008   MSVC++ 9.0    _MSC_VER == 1500
//   Visual Studio 2005   MSVC++ 8.0    _MSC_VER == 1400
//   Visual Studio 2003   MSVC++ 7.1    _MSC_VER == 1310
//

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Source code annotations are available starting with Visual Studio 2005.
#if _MSC_VER >= 1400
#include <sal.h>
#endif

namespace foundation
{

//
// The Compiler class provides information about the compiler used to build the library.
//

class DLLSYMBOL Compiler
  : public NonCopyable
{
  public:
    // Return the name of the compiler.
    static const char* get_compiler_name();

    // Return the version of the compiler.
    static const char* get_compiler_version();
};


//
// A qualifier to force inlining of a function/method on supported compilers.
//

// Visual C++.
#if defined _MSC_VER
    #define FORCE_INLINE __forceinline

// gcc.
#elif defined __GNUC__
    #define FORCE_INLINE inline __attribute__((always_inline))

// Other compilers: fall back to standard inlining.
#else
    #define FORCE_INLINE inline
#endif


//
// A qualifier to prevent inlining of a function/method on supported compilers.
//

// Visual C++.
#if defined _MSC_VER
    #define NO_INLINE __declspec(noinline)

// gcc.
#elif defined __GNUC__
    #define NO_INLINE __attribute__((noinline))

// Other compilers: ignore the qualifier.
#else
    #define NO_INLINE
#endif


//
// A qualifier to specify the alignment of a variable, a structure member or a structure.
// Named FOUNDATION_ALIGN instead of simply ALIGN to prevent a collision with the ALIGN
// macro defined in /usr/include/i386/param.h on Mac OS X (and possibly other platforms).
//

// Visual C++.
#if defined _MSC_VER
    #define FOUNDATION_ALIGN(n) __declspec(align(n))

// gcc.
#elif defined __GNUC__
    #define FOUNDATION_ALIGN(n) __attribute__((aligned(n)))

// Other compilers: ignore the qualifier.
#else
    #define FOUNDATION_ALIGN(n)
#endif

// Specify an alignment compatible with SSE.
#define SSE_ALIGN FOUNDATION_ALIGN(16)


//
// Return the alignment requirement of a type.
//

// Visual C++.
#if defined _MSC_VER
    #define ALIGNOF(t) __alignof(t)

// gcc.
#elif defined __GNUC__
    #define ALIGNOF(t) __alignof__(t)

// Other compilers: abort compilation.
#else
    #error ALIGNOF is not defined for this compiler.
#endif


//
// A qualifier similar to the 'restrict' keyword in C99.
//

#define RESTRICT __restrict


//
// Define the OVERRIDE qualifer as a synonym for the 'override' keyword in C++11.
//

// Visual C++: supported since Visual Studio 2010.
#if _MSC_VER >= 1600
    #define OVERRIDE override

// gcc: supported since gcc 4.7.
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7)
    #define OVERRIDE override

// Other compilers: the OVERRIDE qualifier has no effect.
#else
    #define OVERRIDE
#endif


//
// Utility macros converting their argument to a string literal:
// FOUNDATION_TO_STRING_EVAL first expands the argument definition.
// FOUNDATION_TO_STRING_NOEVAL does not expand the argument definition.
//

#define FOUNDATION_TO_STRING_EVAL(x) FOUNDATION_TO_STRING_NOEVAL(x)
#define FOUNDATION_TO_STRING_NOEVAL(x) #x


//
// Utility macro representing an empty parameter to another macro.
// Omitting macro parameters is supported in C99, but not yet in C++ 98;
// using a macro expanding to nothing is a way to workaround this limitation.
//

#define FOUNDATION_EMPTY


//
// As of version 2010, Visual C++ doesn't provide the va_copy() macro
// introduced in C99. On Windows (32-bit and 64-bit) it is sufficient
// to use assignment between va_list variables.
//

#ifdef _MSC_VER
#ifndef va_copy
#define va_copy(dst, src) ((dst) = (src))
#endif
#endif


//
// Source code annotations.
//

// Visual C++: Visual Studio 2008+ annotations.
#if _MSC_VER >= 1500
    #define PRINTF_FMT _Printf_format_string_
    #define PRINTF_FMT_ATTR(m, n)

// Visual C++: Visual Studio 2005 annotations.
#elif _MSC_VER >= 1400
    #define PRINTF_FMT __format_string
    #define PRINTF_FMT_ATTR(m, n)

// gcc.
#elif defined __GNUC__
    #define PRINTF_FMT
    #define PRINTF_FMT_ATTR(m, n) __attribute__((format(printf, m, n)))

// Other compilers: annotations have no effect.
#else
    #define PRINTF_FMT
    #define PRINTF_FMT_ATTR(m, n)
#endif

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_COMPILER_H
