
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cmath>
#include <cstdint>

namespace foundation
{

//
// Half-precision (16-bit) floating-point type.
//
// This type should be compatible with OpenGL, OpenEXR, and IEEE 754r.
// The range is [-65504.0, 65504.0] and the precision is about 1 part
// in 2000 (3.3 decimal places).
//
// From OpenGL spec 2.1.2:
//
// A 16-bit floating-point number has a 1-bit sign (S), a 5-bit
// exponent (E), and a 10-bit mantissa (M). The value of a 16-bit
// floating-point number is determined by the following:
//
//   (-1)^S * 0.0,                          if E == 0 and M == 0,
//   (-1)^S * 2^-14 * (M/2^10),             if E == 0 and M != 0,
//   (-1)^S * 2^(E-15) * (1 + M/2^10),      if 0 < E < 31,
//   (-1)^S * INF,                          if E == 31 and M == 0, or
//   NaN                                    if E == 31 and M != 0
//

class Half
{
  public:
    // Constructors.
    Half();                                 // leave half value uninitialized
    Half(const float rhs);                  // allow implicit float-to-half conversion

    // Construct a half by directly specifying its bits.
    static Half from_bits(const std::uint16_t bits);

    // Get underlying bits.
    std::uint16_t bits() const;

    // Implicit float-to-half conversion via assignment.
    Half& operator=(const float rhs);

    // Implicit half-to-float conversion.
    operator float() const;

  private:
    APPLESEED_DLLSYMBOL static const std::uint32_t s_h2f_table[65536];
    APPLESEED_DLLSYMBOL static const std::uint16_t s_f2h_table[512];
    APPLESEED_DLLSYMBOL static std::uint16_t float_to_half_except(const std::uint32_t i);

    friend Half float_to_half(const float f);
    friend float half_to_float(const Half h);

    std::uint16_t m_bits;
};

static_assert(sizeof(Half) == 2, "The size of foundation::Half must be exactly 2 bytes");


//
// Explicit conversion functions.
//
// The functions
//
//   foundation::float_to_half()
//   foundation::half_to_float()
//
// were borrowed from Ptex with minor, non-functional changes.
// The original license follows:
//
//   PTEX SOFTWARE
//   Copyright 2014 Disney Enterprises, Inc.  All rights reserved
//   
//   Redistribution and use in source and binary forms, with or without
//   modification, are permitted provided that the following conditions are
//   met:
//   
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//   
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//   
//     * The names "Disney", "Walt Disney Pictures", "Walt Disney Animation
//       Studios" or the names of its contributors may NOT be used to
//       endorse or promote products derived from this software without
//       specific prior written permission from Walt Disney Pictures.
//   
//   Disclaimer: THIS SOFTWARE IS PROVIDED BY WALT DISNEY PICTURES AND
//   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
//   BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
//   FOR A PARTICULAR PURPOSE, NONINFRINGEMENT AND TITLE ARE DISCLAIMED.
//   IN NO EVENT SHALL WALT DISNEY PICTURES, THE COPYRIGHT HOLDER OR
//   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND BASED ON ANY
//   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
//
// The functions
//
//   foundation::float_to_half_alt()
//   foundation::fast_float_to_half()
//   foundation::half_to_float_alt()
//   foundation::float_to_half() (SSE variant)
//   foundation::half_to_float() (SSE variant)
//
// were borrowed from
//
//   Float->half variants, by Fabian "ryg" Giesen.
//   https://gist.github.com/rygorous/2156668
//
//   Half->float variants, by Fabian "ryg" Giesen.
//   https://gist.github.com/rygorous/2144712
//
// with minor, non-functional changes.
//

// Float-to-half conversion function from Ptex.
Half float_to_half(const float f);

// Alternate implementation by Fabian Giesen (called float_to_half_fast3() in his gist).
Half float_to_half_alt(const float f);

// Approximate solution by Fabian Giesen (called approx_float_to_half() in his gist).
// This is faster but converts some sNaNs to infinity and doesn't round correctly. Handle with care.
Half fast_float_to_half(const float f);

// Half-to-float conversion function from Ptex.
float half_to_float(const Half h);

// Alternate implementation by Fabian Giesen (called half_to_float_fast5() in his gist).
// Turns FP16 denormals into FP32 denormals. Will be slower if denormals actually occur.
float half_to_float_alt(const Half h);

#ifdef APPLESEED_USE_SSE

__m128i float_to_half(const __m128 f);      // round-half-up (same as ISPC)
__m128 half_to_float(const __m128i h);

#endif


//
// Half class implementation.
//

inline Half::Half()
{
}

inline Half::Half(const float rhs)
  : m_bits(float_to_half(rhs).m_bits)
{
}

inline Half Half::from_bits(const std::uint16_t bits)
{
    Half h;
    h.m_bits = bits;
    return h;
}

inline std::uint16_t Half::bits() const
{
    return m_bits;
}

inline Half& Half::operator=(const float rhs)
{
    m_bits = float_to_half(rhs).m_bits;
    return *this;
}

inline Half::operator float() const
{
    return half_to_float(*this);
}


//
// Explicit conversion functions implementation.
//

inline Half float_to_half(const float f)
{
#ifdef APPLESEED_USE_F16C

    return
        Half::from_bits(
            _mm_extract_epi16(
                _mm_cvtps_ph(_mm_set_ss(f), _MM_FROUND_TO_NEAREST_INT),
                0));

#else

    if (f == 0.0f)
        return Half::from_bits(0);

    union { std::uint32_t i; float f; } u;
    u.f = f;

    const std::uint16_t e = Half::s_f2h_table[(u.i >> 23) & 0x1ff];

    return e != 0
        ? Half::from_bits(static_cast<std::uint16_t>(e + (((u.i & 0x7fffff) + 0x1000) >> 13)))
        : Half::from_bits(Half::float_to_half_except(u.i));

#endif
}

inline Half float_to_half_alt(const float fl)
{
    union FP32 { std::uint32_t u; float f; };

    static const FP32 f32infty = { 255 << 23 };
    static const FP32 f16infty = { 31 << 23 };
    static const FP32 magic = { 15 << 23 };
    const unsigned int sign_mask = 0x80000000u;
    const unsigned int round_mask = ~0xfffu;

    std::uint16_t o = 0;

    FP32 f;
    f.f = fl;

    const unsigned int sign = f.u & sign_mask;
    f.u ^= sign;

    // NOTE all the integer compares in this function can be safely
    // compiled into signed compares since all operands are below
    // 0x80000000. Important if you want fast straight SSE2 code
    // (since there's no unsigned PCMPGTD).

    if (f.u >= f32infty.u)              // Inf or NaN (all exponent bits set)
        o = (f.u > f32infty.u) ? 0x7e00 : 0x7c00; // NaN->qNaN and Inf->Inf
    else                                // (de)normalized number or zero
    {
        f.u &= round_mask;
        f.f *= magic.f;
        f.u -= round_mask;
        if (f.u > f16infty.u)           // clamp to signed infinity if overflowed
            f.u = f16infty.u;

        o = f.u >> 13;                  // take the bits!
    }

    o |= sign >> 16;

    return Half::from_bits(o);
}

inline Half fast_float_to_half(const float fl)
{
    union FP32 { std::uint32_t u; float f; };

    static const FP32 f32infty = { 255 << 23 };
    static const FP32 f16max = { (127 + 16) << 23 };
    static const FP32 magic = { 15 << 23 };
    static const FP32 expinf = { (255 ^ 31) << 23 };
    const unsigned int sign_mask = 0x80000000u;

    std::uint16_t o = 0;

    FP32 f;
    f.f = fl;

    const unsigned int sign = f.u & sign_mask;
    f.u ^= sign;

    if (!(f.f < f32infty.u))            // Inf or NaN
        o = f.u ^ expinf.u;
    else
    {
        if (f.f > f16max.f)
            f.f = f16max.f;
        f.f *= magic.f;
    }

    o = f.u >> 13;                      // take the mantissa bits
    o |= sign >> 16;

    return Half::from_bits(o);
}

inline float half_to_float(const Half h)
{
#ifdef APPLESEED_USE_F16C_DISABLED      // currently slower than the table lookup

    return
        _mm_cvtss_f32(
            _mm_cvtph_ps(
                _mm_set1_epi16(h.m_bits)));

#else

    union { std::uint32_t i; float f; } u;
    u.i = Half::s_h2f_table[h.m_bits];
    return u.f;

#endif
}

inline float half_to_float_alt(const Half h)
{
    union FP32 { std::uint32_t u; float f; };

    static const FP32 magic = { (254 - 15) << 23 };
    static const FP32 was_infnan = { (127 + 16) << 23 };

    FP32 o;
    o.u = (h.bits() & 0x7fff) << 13;    // exponent/mantissa bits
    o.f *= magic.f;                     // exponent adjust
    if (o.f >= was_infnan.f)            // make sure Inf/NaN survive
        o.u |= 255 << 23;
    o.u |= (h.bits() & 0x8000) << 16;   // sign bit

    return o.f;
}

#ifdef APPLESEED_USE_SSE

inline __m128i float_to_half(const __m128 f)
{
#ifdef APPLESEED_USE_F16C

    return _mm_cvtps_ph(f, _MM_FROUND_TO_NEAREST_INT);

#else

    const __m128i mask_sign       = _mm_set1_epi32(0x80000000u);
    const __m128i mask_round      = _mm_set1_epi32(~0xfffu);
    const __m128i c_f32infty      = _mm_set1_epi32(255 << 23);
    const __m128i c_magic         = _mm_set1_epi32(15 << 23);
    const __m128i c_nanbit        = _mm_set1_epi32(0x200);
    const __m128i c_infty_as_fp16 = _mm_set1_epi32(0x7c00);
    const __m128i c_clamp         = _mm_set1_epi32((31 << 23) - 0x1000);

    const __m128  msign           = _mm_castsi128_ps(mask_sign);
    const __m128  justsign        = _mm_and_ps(msign, f);
    const __m128i f32infty        = c_f32infty;
    const __m128  absf            = _mm_xor_ps(f, justsign);
    const __m128  mround          = _mm_castsi128_ps(mask_round);
    const __m128i absf_int        = _mm_castps_si128(absf); // pseudo-op, but val needs to be copied once so count as mov
    const __m128i b_isnan         = _mm_cmpgt_epi32(absf_int, f32infty);
    const __m128i b_isnormal      = _mm_cmpgt_epi32(f32infty, _mm_castps_si128(absf));
    const __m128i nanbit          = _mm_and_si128(b_isnan, c_nanbit);
    const __m128i inf_or_nan      = _mm_or_si128(nanbit, c_infty_as_fp16);

    const __m128  fnosticky       = _mm_and_ps(absf, mround);
    const __m128  scaled          = _mm_mul_ps(fnosticky, _mm_castsi128_ps(c_magic));
    const __m128  clamped         = _mm_min_ps(scaled, _mm_castsi128_ps(c_clamp)); // logically, we want PMINSD on "biased", but this should gen better code
    const __m128i biased          = _mm_sub_epi32(_mm_castps_si128(clamped), _mm_castps_si128(mround));
    const __m128i shifted         = _mm_srli_epi32(biased, 13);
    const __m128i normal          = _mm_and_si128(shifted, b_isnormal);
    const __m128i not_normal      = _mm_andnot_si128(b_isnormal, inf_or_nan);
    const __m128i joined          = _mm_or_si128(normal, not_normal);

    const __m128i sign_shift      = _mm_srli_epi32(_mm_castps_si128(justsign), 16);
    const __m128i final           = _mm_or_si128(joined, sign_shift);

    // ~20 SSE2 ops
    return final;

#endif
}

inline __m128 half_to_float(const __m128i h)
{
#ifdef APPLESEED_USE_F16C

    return _mm_cvtph_ps(h);

#else

#define SSE_CONST4(name, val) static const APPLESEED_SIMD4_ALIGN unsigned int name[4] = { (val), (val), (val), (val) }

    SSE_CONST4(mask_nosign, 0x7fff);
    SSE_CONST4(magic,       (254 - 15) << 23);
    SSE_CONST4(was_infnan,  0x7bff);
    SSE_CONST4(exp_infnan,  255 << 23);

    const __m128i mnosign         = *(const __m128i*)(&mask_nosign);
    const __m128i expmant         = _mm_and_si128(mnosign, h);
    const __m128i justsign        = _mm_xor_si128(h, expmant);
    const __m128i expmant2        = expmant; // copy (just here for counting purposes)
    const __m128i shifted         = _mm_slli_epi32(expmant, 13);
    const __m128  scaled          = _mm_mul_ps(_mm_castsi128_ps(shifted), *(const __m128 *)&magic);
    const __m128i b_wasinfnan     = _mm_cmpgt_epi32(expmant2, *(const __m128i*)(&was_infnan));
    const __m128i sign            = _mm_slli_epi32(justsign, 16);
    const __m128  infnanexp       = _mm_and_ps(_mm_castsi128_ps(b_wasinfnan), *(const __m128*)(&exp_infnan));
    const __m128  sign_inf        = _mm_or_ps(_mm_castsi128_ps(sign), infnanexp);
    const __m128  final           = _mm_or_ps(scaled, sign_inf);

    // ~11 SSE2 ops.
    return final;

#undef SSE_CONST4

#endif
}

#endif  // APPLESEED_USE_SSE

}   // namespace foundation
