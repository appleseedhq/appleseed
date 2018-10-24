
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

// appleseed.foundation headers.
#include "foundation/platform/arch.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// Define fixed-size integral types.
//

// Visual C++.
#if defined _MSC_VER

    typedef signed char                 int8;
    typedef unsigned char               uint8;

    typedef signed short int            int16;
    typedef unsigned short int          uint16;

    typedef signed int                  int32;
    typedef unsigned int                uint32;

    typedef signed long long int        int64;
    typedef unsigned long long int      uint64;

// gcc.
#elif defined __GNUC__

    typedef signed char                 int8;
    typedef unsigned char               uint8;

    typedef signed short int            int16;
    typedef unsigned short int          uint16;

    typedef signed int                  int32;
    typedef unsigned int                uint32;

    #if defined APPLESEED_ARCH32
        typedef signed long long int    int64;
        typedef unsigned long long int  uint64;
    #elif defined APPLESEED_ARCH64
        typedef signed long int         int64;
        typedef unsigned long int       uint64;
    #else
        #error Cannot determine machine architecture.
    #endif

// CUDA device compilation.
#elif defined APPLESEED_DEVICE_COMPILATION

    typedef int8_t                      int8;
    typedef uint8_t                     uint8;

    typedef int16_t                     int16;
    typedef uint16_t                    uint16;

    typedef int32_t                     int32;
    typedef uint32_t                    uint32;

    typedef int64_t                     int64;
    typedef uint64_t                    uint64;

// Other platforms.
#else

    #error Fixed-size integral types are not defined on this platform.

#endif

static_assert(sizeof(int8)   == 1, "The size of foundation::int8 must be exactly 1 byte");
static_assert(sizeof(int16)  == 2, "The size of foundation::int16 must be exactly 2 bytes");
static_assert(sizeof(int32)  == 4, "The size of foundation::int32 must be exactly 4 bytes");
static_assert(sizeof(int64)  == 8, "The size of foundation::int64 must be exactly 8 bytes");
static_assert(sizeof(uint8)  == 1, "The size of foundation::uint8 must be exactly 1 byte");
static_assert(sizeof(uint16) == 2, "The size of foundation::uint16 must be exactly 2 bytes");
static_assert(sizeof(uint32) == 4, "The size of foundation::uint32 must be exactly 4 bytes");
static_assert(sizeof(uint64) == 8, "The size of foundation::uint64 must be exactly 8 bytes");


//
// Define a signed counterpart to std::size_t, i.e. a synonym for the non-standard ssize_t type.
//

#if defined APPLESEED_ARCH32
    typedef int32 isize_t;
#elif defined APPLESEED_ARCH64
    typedef int64 isize_t;
#else
    #error Cannot determine machine architecture.
#endif

static_assert(
    sizeof(isize_t) == sizeof(size_t),
    "foundation::isize_t must have the same size as std::size_t");


//
// Format strings to use with std::printf() variants.
//

// Visual C++.
#if defined _MSC_VER

    #define FMT_UINT64          "%llu"
    #define FMT_UINT64_HEX      "%llx"
    #define FMT_SIZE_T          "%Iu"

// gcc.
#elif defined __GNUC__

    #if defined APPLESEED_ARCH32
        #define FMT_UINT64      "%llu"
        #define FMT_UINT64_HEX  "%llx"
    #elif defined APPLESEED_ARCH64
        #define FMT_UINT64      "%lu"
        #define FMT_UINT64_HEX  "%lx"
    #else
        #error Cannot determine machine architecture.
    #endif

    #define FMT_SIZE_T "%zu"

#elif defined __CUDACC__

    #define FMT_UINT64      "%lu"
    #define FMT_UINT64_HEX  "%llx"
    #define FMT_SIZE_T      "%zu"

// Other compilers.
#else

    #error Format strings are not defined on this platform.

#endif

}   // namespace foundation
