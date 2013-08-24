
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

#ifndef APPLESEED_FOUNDATION_MATH_FASTMATH_H
#define APPLESEED_FOUNDATION_MATH_FASTMATH_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Fast, approximate math functions.
//
// The functions
//
//     foundation::fast_pow2()
//     foundation::faster_pow2()
//     foundation::fast_log2()
//     foundation::faster_log2()
//     foundation::fast_pow()
//     foundation::faster_pow()
//     foundation::fast_log()
//     foundation::faster_log()
//     foundation::fast_exp()
//     foundation::faster_exp()
//
// were borrowed from https://code.google.com/p/fastapprox/ with minor,
// non-functional changes. The original copyright notice for this code
// follows:
//
// *=====================================================================*
// *                   Copyright (C) 2011 Paul Mineiro                   *
// * All rights reserved.                                                *
// *                                                                     *
// * Redistribution and use in source and binary forms, with             *
// * or without modification, are permitted provided that the            *
// * following conditions are met:                                       *
// *                                                                     *
// *     * Redistributions of source code must retain the                *
// *     above copyright notice, this list of conditions and             *
// *     the following disclaimer.                                       *
// *                                                                     *
// *     * Redistributions in binary form must reproduce the             *
// *     above copyright notice, this list of conditions and             *
// *     the following disclaimer in the documentation and/or            *
// *     other materials provided with the distribution.                 *
// *                                                                     *
// *     * Neither the name of Paul Mineiro nor the names                *
// *     of other contributors may be used to endorse or promote         *
// *     products derived from this software without specific            *
// *     prior written permission.                                       *
// *                                                                     *
// * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND              *
// * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,         *
// * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES               *
// * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE             *
// * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER               *
// * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,                 *
// * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES            *
// * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE           *
// * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR                *
// * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF          *
// * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT           *
// * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY              *
// * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             *
// * POSSIBILITY OF SUCH DAMAGE.                                         *
// *                                                                     *
// * Contact: Paul Mineiro <paul@mineiro.com>                            *
// *=====================================================================*/
//
// Other interesting references:
//
//   Fast floor:
//     http://www.masm32.com/board/index.php?topic=9515.msg78719#msg78719
//
//   Fast log2 and pow2:
//     Production Rendering, Springer-Verlag, 2004
//       and
//     http://www.dctsystems.co.uk/Software/power.c
//
//   Fast square root:
//     http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
//
//   Fast reciprocal square root:
//     http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
//

// Fast approximation of 2^p.
float fast_pow2(const float p);
float faster_pow2(const float p);
float old_fast_pow2(const float x);

// Fast approximation of the base-2 logarithm.
float fast_log2(const float x);
float faster_log2(const float x);
float old_fast_log2(const float x);
float old_fast_log2_refined(const float x);

// Fast approximation of x^p.
float fast_pow(const float x, const float p);
float faster_pow(const float x, const float p);
float old_fast_pow(const float a, const float b);
float old_fast_pow_refined(const float a, const float b);
void old_fast_pow(float a[4], const float b);           // a must be 16-byte aligned if APPLESEED_FOUNDATION_USE_SSE is defined
void old_fast_pow_refined(float a[4], const float b);   // a must be 16-byte aligned if APPLESEED_FOUNDATION_USE_SSE is defined

// Fast approximation of the natural logarithm.
float fast_log(const float x);
float faster_log(const float x);

// Fast approximation of e^p.
float fast_exp(const float p);
float faster_exp(const float p);
float old_fast_exp(const float x);

// Fast approximation of the square root.
float fast_sqrt(const float x);

// Fast approximation of the reciprocal square root.
float fast_rcp_sqrt(const float x);
double fast_rcp_sqrt(const double x);


//
// Implementation.
//

inline float fast_pow2(const float p)
{
    // Underflow of exponential is common practice in numerical routines, so handle it here.
    const float offset = (p < 0) ? 1.0f : 0.0f;
    const float clipp = (p < -126) ? -126.0f : p;
    const int w = static_cast<int>(clipp);
    const float z = clipp - w + offset;
    union { uint32 i; float f; } v =
        { static_cast<uint32>((1 << 23) * (clipp + 121.2740575f + 27.7280233f / (4.84252568f - z) - 1.49012907f * z)) };
    return v.f;
}

inline float faster_pow2(const float p)
{
    // Underflow of exponential is common practice in numerical routines, so handle it here.
    const float clipp = (p < -126) ? -126.0f : p;
    union { uint32 i; float f; } v = { static_cast<uint32>((1 << 23) * (clipp + 126.94269504f)) };
    return v.f;
}

inline float old_fast_pow2(const float x)
{
    float y = x - std::floor(x);
    y = (y - y * y) * 0.33971f;
    float z = x + 127.0f - y;
    z *= 8388608.0f;                            // z *= 2 ^ 23
    return binary_cast<float>(static_cast<int32>(z));
}

inline float fast_log2(const float x)
{
    union { float f; uint32 i; } vx = { x };
    union { uint32 i; float f; } mx = { (vx.i & 0x007FFFFF) | 0x3f000000 };
    float y = static_cast<float>(vx.i);
    y *= 1.1920928955078125e-7f;
    return y - 124.22551499f
             - 1.498030302f * mx.f 
             - 1.72587999f / (0.3520887068f + mx.f);
}

inline float faster_log2(const float x)
{
    union { float f; uint32 i; } vx = { x };
    float y = static_cast<float>(vx.i);
    y *= 1.1920928955078125e-7f;
    return y - 126.94269504f;
}

inline float old_fast_log2(const float x)
{
    assert(x > 0.0f);
    float y = static_cast<float>(binary_cast<int32>(x));
    y *= 0.1192092896e-6f;                      // y *= 2 ^ (-23)
    y -= 127.0f;
    return y;
}

inline float old_fast_log2_refined(const float x)
{
    assert(x > 0.0f);
    float y = static_cast<float>(binary_cast<int32>(x));
    y *= 0.1192092896e-6f;                      // y *= 2 ^ (-23)
    y -= 127.0f;
    float z = y - std::floor(y);
    z = (z - z * z) * 0.346607f;
    return y + z;
}

inline float fast_pow(const float x, const float p)
{
    return fast_pow2(p * fast_log2(x));
}

inline float faster_pow(const float x, const float p)
{
    return faster_pow2(p * faster_log2(x));
}

inline float old_fast_pow(const float a, const float b)
{
    assert(a >= 0.0f);
    return a > 0.0f ? old_fast_pow2(b * old_fast_log2(a)) : 0.0f;
}

inline float old_fast_pow_refined(const float a, const float b)
{
    assert(a >= 0.0f);
    return a > 0.0f ? old_fast_pow2(b * old_fast_log2_refined(a)) : 0.0f;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

inline void old_fast_pow(float a[4], const float b)
{
    assert(is_aligned(a, 16));

    __m128 x = _mm_cvtepi32_ps(_mm_load_si128((__m128i*)a));

    const __m128 K = _mm_set1_ps(127.0f);
    x = _mm_mul_ps(x, _mm_set1_ps(0.1192092896e-6f));     // x *= 2 ^ (-23)
    x = _mm_sub_ps(x, K);
    x = _mm_mul_ps(x, _mm_set1_ps(b));

    __m128 y = _mm_sub_ps(x, floorps(x));
    y = _mm_sub_ps(y, _mm_mul_ps(y, y));
    y = _mm_mul_ps(y, _mm_set1_ps(0.33971f));
    y = _mm_sub_ps(_mm_add_ps(x, K), y);
    y = _mm_mul_ps(y, _mm_set1_ps(8388608.0f));           // y *= 2 ^ 23

    _mm_store_si128((__m128i*)a, _mm_cvtps_epi32(y));
}

inline void old_fast_pow_refined(float a[4], const float b)
{
    assert(is_aligned(a, 16));

    __m128 x = _mm_cvtepi32_ps(_mm_load_si128((__m128i*)a));

    const __m128 K = _mm_set1_ps(127.0f);
    x = _mm_mul_ps(x, _mm_set1_ps(0.1192092896e-6f));     // x *= 2 ^ (-23)
    x = _mm_sub_ps(x, K);

    // One Newton-Raphson refinement step.
    __m128 z = _mm_sub_ps(x, floorps(x));
    z = _mm_sub_ps(z, _mm_mul_ps(z, z));
    z = _mm_mul_ps(z, _mm_set1_ps(0.346607f));
    x = _mm_add_ps(x, z);

    x = _mm_mul_ps(x, _mm_set1_ps(b));

    __m128 y = _mm_sub_ps(x, floorps(x));
    y = _mm_sub_ps(y, _mm_mul_ps(y, y));
    y = _mm_mul_ps(y, _mm_set1_ps(0.33971f));
    y = _mm_sub_ps(_mm_add_ps(x, K), y);
    y = _mm_mul_ps(y, _mm_set1_ps(8388608.0f));           // y *= 2 ^ 23

    _mm_store_si128((__m128i*)a, _mm_cvtps_epi32(y));
}

#else

inline void old_fast_pow(float a[4], const float b)
{
    a[0] = old_fast_pow(a[0], b);
    a[1] = old_fast_pow(a[1], b);
    a[2] = old_fast_pow(a[2], b);
    a[3] = old_fast_pow(a[3], b);
}

inline void old_fast_pow_refined(float a[4], const float b)
{
    a[0] = old_fast_pow_refined(a[0], b);
    a[1] = old_fast_pow_refined(a[1], b);
    a[2] = old_fast_pow_refined(a[2], b);
    a[3] = old_fast_pow_refined(a[3], b);
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

inline float fast_log(const float x)
{
    return 0.69314718f * fast_log2(x);
}

inline float faster_log(const float x)
{
    // What follows is the inlined version of 0.69314718f * faster_log2(x).
    union { float f; uint32 i; } vx = { x };
    float y = static_cast<float>(vx.i);
    y *= 8.2629582881927490e-8f;
    return y - 87.989971088f;
}

inline float fast_exp(const float p)
{
    return fast_pow2(1.442695040f * p);
}

inline float faster_exp(const float p)
{
    return faster_pow2(1.442695040f * p);
}

inline float old_fast_exp(const float x)
{
    return old_fast_pow2(x * 1.442695f);        // 2 ^ (x / ln(2))
}

inline float fast_sqrt(const float x)
{
    assert(x >= 0.0f);
    int32 i = binary_cast<int32>(x);
    i -= 1 << 23;                               // remove last bit to not let it go to mantissa
    i = i >> 1;                                 // divide by 2
    i += 1 << 29;                               // add 64 to exponent
    return binary_cast<float>(i);
}

inline float fast_rcp_sqrt(const float x)
{
    assert(x >= 0.0f);
    const float xhalf = 0.5f * x;
    int32 i = binary_cast<int32>(x);
    i = 0x5F375A86L - (i >> 1);                 // initial guess
    float z = binary_cast<float>(i);
    z = z * (1.5f - xhalf * z * z);             // Newton step, repeating increases accuracy
    return z;
}

inline double fast_rcp_sqrt(const double x)
{
    assert(x >= 0.0);
    const double xhalf = 0.5 * x;
    int64 i = binary_cast<int64>(x);
    i = 0x5FE6EC85E7DE30DALL - (i >> 1);        // initial guess
    double z = binary_cast<double>(i);
    z = z * (1.5 - xhalf * z * z);              // Newton step, repeating increases accuracy
    return z;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FASTMATH_H
