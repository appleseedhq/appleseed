
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

#ifndef APPLESEED_USE_SSE
    #error SSE support not enabled.
#endif

// Standard headers.
#include <cstdint>

//
// x86 intrinsics headers.
//
// Reference:
//
//   https://stackoverflow.com/questions/11228855/header-files-for-x86-simd-intrinsics
//

#if defined _MSC_VER
#include <intrin.h>
#else
#include <x86intrin.h>
#endif

namespace foundation
{

struct M128Fields
{
    union
    {
        __m128          m128;
        __m128d         m128d;
        __m128i         m128i;

        float           f32[4];
        double          f64[2];

        std::int8_t     i8[16];
        std::int16_t    i16[8];
        std::int32_t    i32[4];
        std::int64_t    i64[2];
        std::uint8_t    u8[16];
        std::uint16_t   u16[8];
        std::uint32_t   u32[4];
        std::uint64_t   u64[2];
    };
};

}   // namespace foundation
