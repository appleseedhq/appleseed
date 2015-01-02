
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// Platform headers.
#include <xmmintrin.h>      // SSE1 intrinsics
#include <emmintrin.h>      // SSE2 intrinsics

namespace foundation
{

//
// 4-way SSE implementation of std::floor().
//
// Reference:
//
//   http://www.masm32.com/board/index.php?topic=9515.msg78719#msg78719
//
// Note: SSE 4.1 has an _mm_floor_ps() intrinsic (see smmintrin.h).
//

inline __m128 floorps(const __m128 x)
{
    return
        _mm_cvtepi32_ps(
            _mm_sub_epi32(
                _mm_cvttps_epi32(x),
                _mm_srli_epi32(_mm_castps_si128(x), 31)));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_SSE_H
