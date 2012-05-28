
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef COMMON_H
#define COMMON_H

#include <xmmintrin.h>

namespace ak
{

//---------------------------------------------------------------------------------------------
// Constants.
//---------------------------------------------------------------------------------------------

const float Pi = 3.14159265f;

//---------------------------------------------------------------------------------------------
// A qualifier to specify the alignment of a variable, a structure member or a structure.
//---------------------------------------------------------------------------------------------

// Visual C++.
#if defined _MSC_VER
    #define ALIGN(n) __declspec(align(n))

// gcc.
#elif defined __GNUC__
    #define ALIGN(n) __attribute__((aligned(n)))

// Other compilers: ignore the qualifier.
#else
    #define ALIGN(n)
#endif

// Specify an alignment compatible with SSE.
#define SSE_ALIGN ALIGN(16)

//---------------------------------------------------------------------------------------------
// Fast floating point-to-integer truncation using SSE. Equivalent to static_cast<Int>(x).
//---------------------------------------------------------------------------------------------

template <typename Int>
inline Int truncate(const float x)
{
    return static_cast<Int>(_mm_cvttss_si32(_mm_load_ss(&x)));
}

//---------------------------------------------------------------------------------------------
// Return true if x is a power of two, false otherwise.
//---------------------------------------------------------------------------------------------

template <typename T>
inline bool is_pow2(const T x)
{
    return (x & (x - 1)) == 0;
}

}   // namespace ak

#endif
