
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

#ifndef APPLESEED_FOUNDATION_MATH_FASTMATH_H
#define APPLESEED_FOUNDATION_MATH_FASTMATH_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Fast power and logarithm approximations.
//

// Fast approximation of the base 2 logarithm.
float fast_log2(const float x);

// Fast approximation of the base 2 logarithm, more accurate than fast_log2().
float fast_log2_refined(const float x);

// Fast approximation of 2^x.
float fast_pow2(const float x);

// Fast approximation of a^b.
float fast_pow(const float a, const float b);
void fast_pow(float a[4], const float b);

// Fast approximation of a^b, more accurate than fast_pow().
float fast_pow_refined(const float a, const float b);
void fast_pow_refined(float a[4], const float b);

// Fast approximation of the square root.
float fast_sqrt(const float x);

// Fast approximation of the reciprocal square root.
float fast_rcp_sqrt(const float x);
double fast_rcp_sqrt(const double x);


//
// Fast power and logarithm approximations implementation.
//
// References:
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

inline float fast_log2(const float x)
{
    assert(x > 0.0f);
    float y = static_cast<float>(binary_cast<int32>(x));
    y *= 0.1192092896e-6f;                  // y *= pow(2.0f, -23)
    y -= 127.0f;
    return y;
}

inline float fast_log2_refined(const float x)
{
    assert(x > 0.0f);
    float y = static_cast<float>(binary_cast<int32>(x));
    y *= 0.1192092896e-6f;                  // y *= pow(2.0f, -23)
    y -= 127.0f;
    float z = y - std::floor(y);
    z = (z - z * z) * 0.346607f;
    return y + z;
}

inline float fast_pow2(const float x)
{
    float y = x - std::floor(x);
    y = (y - y * y) * 0.33971f;
    float z = x + 127.0f - y;
    z *= 8388608.0f;                        // z *= pow(2.0f, 23)
    return binary_cast<float>(static_cast<int32>(z));
}

inline float fast_pow(const float a, const float b)
{
    assert(a >= 0.0f);
    return a > 0.0f ? fast_pow2(b * fast_log2(a)) : 0.0f;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

inline void fast_pow(float a[4], const float b)
{
    sse4f x = _mm_cvtepi32_ps(_mm_load_si128((__m128i*)a));

    const sse4f K = set1ps(127.0f);
    x = mulps(x, set1ps(0.1192092896e-6f));     // x *= pow(2.0f, -23)
    x = subps(x, K);
    x = mulps(x, set1ps(b));

    sse4f y = subps(x, FOUNDATION_FLOOR_SSE(x));
    y = subps(y, mulps(y, y));
    y = mulps(y, set1ps(0.33971f));
    y = subps(addps(x, K), y);
    y = mulps(y, set1ps(8388608.0f));           // y *= pow(2.0f, 23)

    _mm_store_si128((__m128i*)a, _mm_cvtps_epi32(y));
}

#else

inline void fast_pow(float a[4], const float b)
{
    a[0] = fast_pow(a[0], b);
    a[1] = fast_pow(a[1], b);
    a[2] = fast_pow(a[2], b);
    a[3] = fast_pow(a[3], b);
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

inline float fast_pow_refined(const float a, const float b)
{
    assert(a >= 0.0f);
    return a > 0.0f ? fast_pow2(b * fast_log2_refined(a)) : 0.0f;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

inline void fast_pow_refined(float a[4], const float b)
{
    sse4f x = _mm_cvtepi32_ps(_mm_load_si128((__m128i*)a));

    const sse4f K = set1ps(127.0f);
    x = mulps(x, set1ps(0.1192092896e-6f));     // x *= pow(2.0f, -23)
    x = subps(x, K);

    // One Newton-Raphson refinement step.
    sse4f z = subps(x, FOUNDATION_FLOOR_SSE(x));
    z = subps(z, mulps(z, z));
    z = mulps(z, set1ps(0.346607f));
    x = addps(x, z);

    x = mulps(x, set1ps(b));

    sse4f y = subps(x, FOUNDATION_FLOOR_SSE(x));
    y = subps(y, mulps(y, y));
    y = mulps(y, set1ps(0.33971f));
    y = subps(addps(x, K), y);
    y = mulps(y, set1ps(8388608.0f));           // y *= pow(2.0f, 23)

    _mm_store_si128((__m128i*)a, _mm_cvtps_epi32(y));
}

#else

inline void fast_pow_refined(float a[4], const float b)
{
    a[0] = fast_pow_refined(a[0], b);
    a[1] = fast_pow_refined(a[1], b);
    a[2] = fast_pow_refined(a[2], b);
    a[3] = fast_pow_refined(a[3], b);
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

inline float fast_sqrt(const float x)
{
    assert(x >= 0.0f);
    int32 i = binary_cast<int32>(x);
    i -= 1 << 23;                           // remove last bit to not let it go to mantissa
    i = i >> 1;                             // divide by 2
    i += 1 << 29;                           // add 64 to exponent
    return binary_cast<float>(i);
}

inline float fast_rcp_sqrt(const float x)
{
    assert(x >= 0.0f);
    const float xhalf = 0.5f * x;
    int32 i = binary_cast<int32>(x);
    i = 0x5F375A86L - (i >> 1);             // gives initial guess
    float z = binary_cast<float>(i);
    z = z * (1.5f - xhalf * z * z);         // Newton step, repeating increases accuracy
    return z;
}

inline double fast_rcp_sqrt(const double x)
{
    assert(x >= 0.0);
    const double xhalf = 0.5 * x;
    int64 i = binary_cast<int64>(x);
    i = 0x5FE6EC85E7DE30DALL - (i >> 1);    // gives initial guess
    double z = binary_cast<double>(i);
    z = z * (1.5 - xhalf * z * z);          // Newton step, repeating increases accuracy
    return z;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FASTMATH_H
