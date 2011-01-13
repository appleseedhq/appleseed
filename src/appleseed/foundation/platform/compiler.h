
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// The Compiler class provides information about the compiler used to build the library.
//

class FOUNDATIONDLL Compiler
  : public NonCopyable
{
  public:
    // Return the name of the compiler.
    static const char* get_compiler_name();

    // Return the version of the compiler.
    static const char* get_compiler_version();
};


//
// Attempt to force inlining of a function.
//

// Visual C++.
#if defined _MSC_VER
#define FORCE_INLINE __forceinline

// gcc.
#elif defined __GNUC__
#define FORCE_INLINE inline __attribute__((always_inline))

// Unsupported platform, fall back to standard inlining.
#else
#define FORCE_INLINE inline
#endif


//
// Attempt to prevent inlining of a function.
//

// Visual C++.
#if defined _MSC_VER
#define NO_INLINE __declspec(noinline)

// gcc.
#elif defined __GNUC__
#define NO_INLINE __attribute__((noinline))

// Unsupported platform.
#else
#define NO_INLINE
#endif


//
// Specify the alignment of a local variable or a member of a data structure.
//

// Visual C++.
#if defined _MSC_VER
#define ALIGN_VARIABLE(n) __declspec(align(n))

// gcc.
#elif defined __GNUC__
#define ALIGN_VARIABLE(n) __attribute__((aligned(n)))

// Unsupported platform.
#else
#define ALIGN_VARIABLE(n)
#endif


//
// Macros for widely used alignments.
//

#define ALIGN_SSE_VARIABLE ALIGN_VARIABLE(16)


//
// Platform-independent restrict qualifier.
//

#define RESTRICT __restrict


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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_COMPILER_H
