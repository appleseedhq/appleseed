
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

#ifndef APPLESEED_FOUNDATION_MATH_FASTMATH_H
#define APPLESEED_FOUNDATION_MATH_FASTMATH_H

// appleseed.foundation headers.
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>
#include <cstddef>

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

// Fast approximation of the base-2 logarithm.
float fast_log2(const float x);
float faster_log2(const float x);

// Fast approximation of x^p.
float fast_pow(const float x, const float p);
float faster_pow(const float x, const float p);

// Fast approximation of the natural logarithm.
float fast_log(const float x);
float faster_log(const float x);

// Fast approximation of e^p.
float fast_exp(const float p);
float faster_exp(const float p);

// SSE variants of the functions above.
#ifdef APPLESEED_USE_SSE
__m128 fast_pow2(const __m128 p);
__m128 faster_pow2(const __m128 p);
__m128 fast_log2(const __m128 x);
__m128 faster_log2(const __m128 x);
__m128 fast_pow(const __m128 x, const __m128 p);
__m128 faster_pow(const __m128 x, const __m128 p);
__m128 fast_log(const __m128 x);
__m128 faster_log(const __m128 x);
__m128 fast_exp(const __m128 x);
__m128 faster_exp(const __m128 x);
#endif

// Vectorized variants of the functions above.
// When APPLESEED_USE_SSE is defined, all array arguments must be 16-byte aligned.
void fast_pow2(float p[4]);
void faster_pow2(float p[4]);
void fast_log2(float x[4]);
void faster_log2(float x[4]);
void fast_pow(float x[4], const float p[4]);
void faster_pow(float x[4], const float p[4]);
void fast_pow(float x[4], const float p);
void faster_pow(float x[4], const float p);
void fast_log(float x[4]);
void faster_log(float x[4]);
void fast_exp(float x[4]);
void faster_exp(float x[4]);

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
    const union { uint32 i; float f; } v =
    {
        static_cast<uint32>((1 << 23) * (clipp + 121.2740575f + 27.7280233f / (4.84252568f - z) - 1.49012907f * z))
    };

    return v.f;
}

inline float faster_pow2(const float p)
{
    // Underflow of exponential is common practice in numerical routines, so handle it here.
    const float clipp = (p < -126) ? -126.0f : p;
    const union { uint32 i; float f; } v =
    {
        static_cast<uint32>((1 << 23) * (clipp + 126.94269504f))
    };

    return v.f;
}

inline float fast_log2(const float x)
{
    assert(x >= 0.0f);

    const union { float f; uint32 i; } vx = { x };
    const union { uint32 i; float f; } mx = { (vx.i & 0x007FFFFF) | 0x3f000000 };
    const float y = static_cast<float>(vx.i) * 1.1920928955078125e-7f;

    return y - 124.22551499f
             - 1.498030302f * mx.f
             - 1.72587999f / (0.3520887068f + mx.f);
}

inline float faster_log2(const float x)
{
    assert(x >= 0.0f);

    const union { float f; uint32 i; } vx = { x };
    const float y = static_cast<float>(vx.i) * 1.1920928955078125e-7f;

    return y - 126.94269504f;
}

inline float fast_pow(const float x, const float p)
{
    return fast_pow2(p * fast_log2(x));
}

inline float faster_pow(const float x, const float p)
{
    return faster_pow2(p * faster_log2(x));
}

inline float fast_log(const float x)
{
    return 0.69314718f * fast_log2(x);
}

inline float faster_log(const float x)
{
    // Inlined version of 0.69314718f * faster_log2(x).

    assert(x >= 0.0f);

    const union { float f; uint32 i; } vx = { x };
    const float y = static_cast<float>(vx.i) * 8.2629582881927490e-8f;

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

#ifdef APPLESEED_USE_SSE

inline __m128 fast_pow2(const __m128 p)
{
    const __m128 ltzero = _mm_cmplt_ps(p, _mm_set1_ps(0.0f));
    const __m128 offset = _mm_and_ps(ltzero, _mm_set1_ps(1.0f));
    const __m128 lt126 = _mm_cmplt_ps(p, _mm_set1_ps(-126.0f));
    const __m128 clipp = _mm_or_ps(_mm_andnot_ps(lt126, p), _mm_and_ps(lt126, _mm_set1_ps(-126.0f)));
    const __m128i w = _mm_cvttps_epi32(clipp);
    const __m128 z = _mm_add_ps(_mm_sub_ps(clipp, _mm_cvtepi32_ps(w)), offset);

    const union { __m128i i; __m128 f; } v =
    {
        _mm_cvttps_epi32(
            _mm_mul_ps(
                _mm_set1_ps(1 << 23),
                _mm_sub_ps(
                    _mm_add_ps(
                        _mm_add_ps(clipp, _mm_set1_ps(121.2740575f)),
                        _mm_div_ps(_mm_set1_ps(27.7280233f), _mm_sub_ps(_mm_set1_ps(4.84252568f), z))
                    ),
                    _mm_mul_ps(_mm_set1_ps(1.49012907f), z)
                )
            )
        )
    };

    return v.f;
}

inline __m128 faster_pow2(const __m128 p)
{
    const __m128 lt126 = _mm_cmplt_ps(p, _mm_set1_ps(-126.0f));
    const __m128 clipp = _mm_or_ps(_mm_andnot_ps(lt126, p), _mm_and_ps(lt126, _mm_set1_ps(-126.0f)));

    const union { __m128i i; __m128 f; } v =
    {
        _mm_cvttps_epi32(
            _mm_mul_ps(
                _mm_set1_ps(1 << 23),
                _mm_add_ps(clipp, _mm_set1_ps(126.94269504f))
            )
        )
    };

    return v.f;
}

inline __m128 fast_log2(const __m128 x)
{
    const __m128i a = _mm_set1_epi32(0x007FFFFF);
    const __m128i b = _mm_set1_epi32(0x3f000000);

    const union { __m128 f; __m128i i; } vx = { x };
    const union { __m128i i; __m128 f; } mx = { _mm_or_si128(_mm_and_si128(vx.i, a), b) };

    const __m128 y = _mm_mul_ps(_mm_cvtepi32_ps(vx.i), _mm_set1_ps(1.1920928955078125e-7f));

    return
        _mm_sub_ps(
            _mm_sub_ps(
                _mm_sub_ps(y, _mm_set1_ps(124.22551499f)),
                _mm_mul_ps(_mm_set1_ps(1.498030302f), mx.f)),
            _mm_div_ps(
                _mm_set1_ps(1.72587999f),
                _mm_add_ps(_mm_set1_ps(0.3520887068f), mx.f)));
}

inline __m128 faster_log2(const __m128 x)
{
    const union { __m128 f; __m128i i; } vx = { x };
    const __m128 y = _mm_mul_ps(_mm_cvtepi32_ps(vx.i), _mm_set1_ps(1.1920928955078125e-7f));

    return _mm_sub_ps(y, _mm_set1_ps(126.94269504f));
}

inline __m128 fast_pow(const __m128 x, const __m128 p)
{
    return fast_pow2(_mm_mul_ps(p, fast_log2(x)));
}

inline __m128 faster_pow(const __m128 x, const __m128 p)
{
    return faster_pow2(_mm_mul_ps(p, faster_log2(x)));
}

inline __m128 fast_log(const __m128 x)
{
    return _mm_mul_ps(_mm_set1_ps(0.69314718f), fast_log2(x));
}

inline __m128 faster_log(const __m128 x)
{
    // Inlined version of _mm_mul_ps(_mm_set1_ps(0.69314718f), faster_log2(x)).

    const union { __m128 f; __m128i i; } vx = { x };
    const __m128 y = _mm_mul_ps(_mm_cvtepi32_ps(vx.i), _mm_set1_ps(8.2629582881927490e-8f));

    return _mm_sub_ps(y, _mm_set1_ps(87.989971088f));
}

inline __m128 fast_exp(const __m128 x)
{
    return fast_pow2(_mm_mul_ps(_mm_set1_ps(1.442695040f), x));
}

inline __m128 faster_exp(const __m128 x)
{
    return faster_pow2(_mm_mul_ps(_mm_set1_ps(1.442695040f), x));
}

inline void fast_pow2(float p[4])
{
    assert(is_aligned(p, 16));
    _mm_store_ps(p, fast_pow2(_mm_load_ps(p)));
}

inline void faster_pow2(float p[4])
{
    assert(is_aligned(p, 16));
    _mm_store_ps(p, faster_pow2(_mm_load_ps(p)));
}

inline void fast_log2(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, fast_log2(_mm_load_ps(x)));
}

inline void faster_log2(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, faster_log2(_mm_load_ps(x)));
}

inline void fast_pow(float x[4], const float p[4])
{
    assert(is_aligned(x, 16));
    assert(is_aligned(p, 16));
    _mm_store_ps(x, fast_pow(_mm_load_ps(x), _mm_load_ps(p)));
}

inline void faster_pow(float x[4], const float p[4])
{
    assert(is_aligned(x, 16));
    assert(is_aligned(p, 16));
    _mm_store_ps(x, faster_pow(_mm_load_ps(x), _mm_load_ps(p)));
}

inline void fast_pow(float x[4], const float p)
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, fast_pow(_mm_load_ps(x), _mm_set1_ps(p)));
}

inline void faster_pow(float x[4], const float p)
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, faster_pow(_mm_load_ps(x), _mm_set1_ps(p)));
}

inline void fast_log(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, fast_log(_mm_load_ps(x)));
}

inline void faster_log(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, faster_log(_mm_load_ps(x)));
}

inline void fast_exp(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, fast_exp(_mm_load_ps(x)));
}

inline void faster_exp(float x[4])
{
    assert(is_aligned(x, 16));
    _mm_store_ps(x, faster_exp(_mm_load_ps(x)));
}

#else

inline void fast_pow2(float p[4])
{
    for (size_t i = 0; i < 4; ++i)
        p[i] = fast_pow2(p[i]);
}

inline void faster_pow2(float p[4])
{
    for (size_t i = 0; i < 4; ++i)
        p[i] = faster_pow2(p[i]);
}

inline void fast_log2(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = fast_log2(x[i]);
}

inline void faster_log2(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = faster_log2(x[i]);
}

inline void fast_pow(float x[4], const float p[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = fast_pow(x[i], p[i]);
}

inline void faster_pow(float x[4], const float p[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = faster_pow(x[i], p[i]);
}

inline void fast_pow(float x[4], const float p)
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = fast_pow(x[i], p);
}

inline void faster_pow(float x[4], const float p)
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = faster_pow(x[i], p);
}

inline void fast_log(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = fast_log(x[i]);
}

inline void faster_log(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = faster_log(x[i]);
}

inline void fast_exp(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = fast_exp(x[i]);
}

inline void faster_exp(float x[4])
{
    for (size_t i = 0; i < 4; ++i)
        x[i] = faster_exp(x[i]);
}

#endif  // APPLESEED_USE_SSE

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
