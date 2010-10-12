
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_TYPES_H
#define APPLESEED_FOUNDATION_PLATFORM_TYPES_H

//
// Visual C++.
//

#if defined _MSC_VER

// Define uintptr_t.
#include <cstddef>

namespace foundation
{
    // Signed integral types.
    typedef char                int8;
    typedef short               int16;
    typedef int                 int32;
    typedef long long           int64;

    // Unsigned integral types.
    typedef unsigned char       uint8;
    typedef unsigned short      uint16;
    typedef unsigned int        uint32;
    typedef unsigned long long  uint64;
}


//
// gcc.
//

#elif defined __GNUC__

// Define uintptr_t.
#include <stdint.h>

namespace foundation
{
    // Signed integral types.
    typedef char                int8;
    typedef short               int16;
    typedef int                 int32;
    typedef long long           int64;

    // Unsigned integral types.
    typedef unsigned char       uint8;
    typedef unsigned short      uint16;
    typedef unsigned int        uint32;
    typedef unsigned long long  uint64;
}


//
// Unsupported platform.
//

#else
#error Integral types are not defined on this platform.
#endif


//
// Format string to print size_t values using std::printf() variants.
//

#define FMT_SIZE_T "%lu"

#endif  // !APPLESEED_FOUNDATION_PLATFORM_TYPES_H
