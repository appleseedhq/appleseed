
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_SSE_H
#define APPLESEED_FOUNDATION_PLATFORM_SSE_H

#ifndef APPLESEED_USE_SSE
    #error SSE support not enabled.
#endif

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// Platform headers.
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
        __m128      m128;
        __m128d     m128d;
        __m128i     m128i;

        float       f32[4];
        double      f64[2];

        int8        i8[16];
        int16       i16[8];
        int32       i32[4];
        int64       i64[2];
        uint8       u8[16];
        uint16      u16[8];
        uint32      u32[4];
        uint64      u64[2];
    };
};

}

#endif  // !APPLESEED_FOUNDATION_PLATFORM_SSE_H
