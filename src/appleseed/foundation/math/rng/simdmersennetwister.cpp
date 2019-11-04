
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// Interface header.
#include "simdmersennetwister.h"

// Standard headers.
#include <cstring>

/*
    Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
    University.
    Copyright (c) 2012 Mutsuo Saito, Makoto Matsumoto, Hiroshima University
    and The University of Tokyo.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.
        * Redistributions in binary form must reproduce the above
          copyright notice, this list of conditions and the following
          disclaimer in the documentation and/or other materials provided
          with the distribution.
        * Neither the names of Hiroshima University, The University of
          Tokyo nor the names of its contributors may be used to endorse
          or promote products derived from this software without specific
          prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#define SFMT_SL1	18
#define SFMT_SL2	1
#define SFMT_SR1	11
#define SFMT_SR2	1
#define SFMT_MSK1	0xdfffffefU
#define SFMT_MSK2	0xddfecb7fU
#define SFMT_MSK3	0xbffaffffU
#define SFMT_MSK4	0xbffffff6U
#define SFMT_PARITY1	0x00000001U
#define SFMT_PARITY2	0x00000000U
#define SFMT_PARITY3	0x00000000U
#define SFMT_PARITY4	0x13c9e684U

#define SFMT_ALTI_SL1	{SFMT_SL1, SFMT_SL1, SFMT_SL1, SFMT_SL1}
#define SFMT_ALTI_SR1	{SFMT_SR1, SFMT_SR1, SFMT_SR1, SFMT_SR1}
#define SFMT_ALTI_MSK	{SFMT_MSK1, SFMT_MSK2, SFMT_MSK3, SFMT_MSK4}
#define SFMT_ALTI_MSK64	{SFMT_MSK2, SFMT_MSK1, SFMT_MSK4, SFMT_MSK3}
#define SFMT_ALTI_SL2_PERM	{1,2,3,23,5,6,7,0,9,10,11,4,13,14,15,8}
#define SFMT_ALTI_SL2_PERM64 {1,2,3,4,5,6,7,31,9,10,11,12,13,14,15,0}
#define SFMT_ALTI_SR2_PERM	{7,0,1,2,11,4,5,6,15,8,9,10,17,12,13,14}
#define SFMT_ALTI_SR2_PERM64 {15,0,1,2,3,4,5,6,17,8,9,10,11,12,13,14}

namespace foundation
{
namespace
{
    inline int idxof(int i)
    {
        return i;
    }

    inline std::uint32_t func1(std::uint32_t x)
    {
        return (x ^ (x >> 27)) * (std::uint32_t)1664525UL;
    }

    inline std::uint32_t func2(std::uint32_t x)
    {
        return (x ^ (x >> 27)) * (std::uint32_t)1566083941UL;
    }

    inline void mm_recursion(
        __m128i* r,
        __m128i a,
        __m128i b,
        __m128i c,
        __m128i d,
        __m128i sse2_param_mask)
    {
        __m128i y = _mm_srli_epi32(b, SFMT_SR1);
        __m128i z = _mm_srli_si128(c, SFMT_SR2);
        __m128i v = _mm_slli_epi32(d, SFMT_SL1);
        z = _mm_xor_si128(z, a);
        z = _mm_xor_si128(z, v);
        __m128i x = _mm_slli_si128(a, SFMT_SL2);
        y = _mm_and_si128(y, sse2_param_mask);
        z = _mm_xor_si128(z, x);
        z = _mm_xor_si128(z, y);
        *r = z;
    }
}

const SimdMersenneTwister::w128 SimdMersenneTwister::m_sse2_param_mask =
{{
     SFMT_MSK1,
     SFMT_MSK2,
     SFMT_MSK3,
     SFMT_MSK4
}};

SimdMersenneTwister::SimdMersenneTwister(const std::uint32_t seed)
{
    init_state(seed);
}

SimdMersenneTwister::SimdMersenneTwister(const std::uint32_t init_key[], const int key_length)
{
    init_array_state(init_key, key_length);
}

void SimdMersenneTwister::init_state(const std::uint32_t seed)
{
    std::uint32_t *psfmt32 = &mt[0].u[0];

    psfmt32[idxof(0)] = seed;
    for (int i = 1; i < N32; i++)
    {
        psfmt32[idxof(i)] =
            1812433253UL * (psfmt32[idxof(i - 1)] ^ (psfmt32[idxof(i - 1)] >> 30)) + i;
    }

    mti = N32;
    period_certification();
}

void SimdMersenneTwister::init_array_state(const std::uint32_t init_key[], const int key_length)
{
    int lag;
    const int size = N * 4;

    if (size >= 623)
        lag = 11;
    else if (size >= 68)
        lag = 7;
    else if (size >= 39)
        lag = 5;
    else
        lag = 3;

    int mid = (size - lag) / 2;
    std::uint32_t *psfmt32 = &mt[0].u[0];
    memset(&mt[0], 0x8b, sizeof(w128) * N + sizeof(int));

    int count;
    if (key_length + 1 > N32)
        count = key_length + 1;
    else
        count = N32;

    std::uint32_t r =
        func1(psfmt32[idxof(0)] ^ psfmt32[idxof(mid)] ^ psfmt32[idxof(N32 - 1)]);

    psfmt32[idxof(mid)] += r;
    r += key_length;
    psfmt32[idxof(mid + lag)] += r;
    psfmt32[idxof(0)] = r;

    count--;
    int i, j;
    for (i = 1, j = 0; (j < count) && (j < key_length); j++)
    {
        r = func1(psfmt32[idxof(i)] ^ psfmt32[idxof((i + mid) % N32)]
                  ^ psfmt32[idxof((i + N32 - 1) % N32)]);
        psfmt32[idxof((i + mid) % N32)] += r;
        r += init_key[j] + i;
        psfmt32[idxof((i + mid + lag) % N32)] += r;
        psfmt32[idxof(i)] = r;
        i = (i + 1) % N32;
    }

    for (; j < count; j++)
    {
        r = func1(psfmt32[idxof(i)] ^ psfmt32[idxof((i + mid) % N32)]
                  ^ psfmt32[idxof((i + N32 - 1) % N32)]);
        psfmt32[idxof((i + mid) % N32)] += r;
        r += i;
        psfmt32[idxof((i + mid + lag) % N32)] += r;
        psfmt32[idxof(i)] = r;
        i = (i + 1) % N32;
    }

    for (j = 0; j < N32; j++)
    {
        r = func2(psfmt32[idxof(i)] + psfmt32[idxof((i + mid) % N32)]
                  + psfmt32[idxof((i + N32 - 1) % N32)]);
        psfmt32[idxof((i + mid) % N32)] ^= r;
        r -= i;
        psfmt32[idxof((i + mid + lag) % N32)] ^= r;
        psfmt32[idxof(i)] = r;
        i = (i + 1) % N32;
    }

    mti = N32;
    period_certification();
}

void SimdMersenneTwister::update_state()
{
    w128* pstate = mt;
    __m128i r1 = pstate[N - 2].si;
    __m128i r2 = pstate[N - 1].si;

    int i;
    for (i = 0; i < N - POS1; i++)
    {
        mm_recursion(&pstate[i].si, pstate[i].si, pstate[i + POS1].si, r1, r2, m_sse2_param_mask.si);
        r1 = r2;
        r2 = pstate[i].si;
    }

    for (; i < N; i++)
    {
        mm_recursion(&pstate[i].si, pstate[i].si, pstate[i + POS1 - N].si, r1, r2, m_sse2_param_mask.si);
        r1 = r2;
        r2 = pstate[i].si;
    }
}

void SimdMersenneTwister::period_certification()
{
    const std::uint32_t parity[4] =
    {
        SFMT_PARITY1,
        SFMT_PARITY2,
        SFMT_PARITY3,
        SFMT_PARITY4
    };

    int inner = 0;
    std::uint32_t *psfmt32 = &mt[0].u[0];

    for (int i = 0; i < 4; i++)
        inner ^= psfmt32[idxof(i)] & parity[i];

    for (int i = 16; i > 0; i >>= 1)
        inner ^= inner >> i;

    inner &= 1;
    /* check OK */
    if (inner == 1)
        return;

    /* check NG, and modification */
    for (int i = 0; i < 4; i++)
    {
        int work = 1;
        for (int j = 0; j < 32; j++)
        {
            if ((work & parity[i]) != 0)
            {
                psfmt32[idxof(i)] ^= work;
                return;
            }

            work = work << 1;
        }
    }
}

}   // namespace foundation
