
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_CORRELATEDMULTIJITTER_H
#define APPLESEED_FOUNDATION_MATH_CORRELATEDMULTIJITTER_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Correlated Multi-Jittered Sampling.
//
// Reference:
//
//   Andrew Kensler, Correlated Multi-Jittered Sampling
//   https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf
//

template <typename T>
inline void cmj_compute_mn(
    T&                  m,
    T&                  n,
    const unsigned int  sample_count);

template <typename T>
inline Vector<T, 2> cmj_generate_sample(
    unsigned int        instance, // number of the sample to generate [0, sample_count)
    const unsigned int  m,
    const unsigned int  n,
    const unsigned int  sample_count, // number of sample in the sequence
    const unsigned int  pattern); // pattern to use for generating the full sequence

template <typename T>
inline Vector<T, 2> cmj_generate_sample(
    unsigned int        instance,
    const unsigned int  sample_count,
    const unsigned int  pattern);

inline unsigned int cmj_permute(
    unsigned int        i,
    const unsigned int  l,
    const unsigned int  p);

inline float cmj_randfloat(
    unsigned int        i,
    const unsigned int  p);


//
// Correlated Multi-Jittered Sampling implementation.
//

template <typename T>
void cmj_compute_mn(
    T&                  m,
    T&                  n,
    const unsigned int  sample_count)
{
    assert(sample_count > 1);
    m = static_cast<int>(std::sqrt(static_cast<float>(sample_count)));
    n = (sample_count + m - 1) / m;
}

template <typename T>
Vector<T, 2> cmj_generate_sample(
    unsigned int        instance,
    const unsigned int  m,
    const unsigned int  n,
    const unsigned int  sample_count,
    const unsigned int  pattern)
{
    assert(m >= 1 && n >= 1 && sample_count > 1);
    assert(instance >= 0 && instance < sample_count);
    instance = cmj_permute(instance, sample_count, pattern * 0x51633e2d);
    const int sx = cmj_permute(instance % m, m, pattern * 0x68bc21eb);
    const int sy = cmj_permute(instance / m, n, pattern * 0x02e5be93);
    const T jx = cmj_randfloat(instance, pattern * 0x967a889b);
    const T jy = cmj_randfloat(instance, pattern * 0x368cc8b7);
    Vector<T, 2> r;
    r[0] = (sx + (sy + jx) / n) / m;
    r[1] = (instance + jy) / sample_count;
    return r;
}

template <typename T>
Vector<T, 2> cmj_generate_sample(
    unsigned int        instance,
    const unsigned int  sample_count,
    const unsigned int  pattern)
{
    unsigned int m, n;
    cmj_compute_mn(m, n, sample_count);
    return cmj_generate_sample<T>(instance, m, n, sample_count, pattern);
}

unsigned int cmj_permute(
    unsigned int        i,
    const unsigned int  l,
    const unsigned int  p)
{
    unsigned int w = l - 1;
    if ((l & w) == 0)
    {
        // l is a power of two (fast).
        i ^= p;
        i *= 0xe170893d;
        i ^= p >> 16;
        i ^= (i & w) >> 4;
        i ^= p >> 8;
        i *= 0x0929eb3f;
        i ^= p >> 23;
        i ^= (i & w) >> 1;
        i *= 1 | p >> 27;
        i *= 0x6935fa69;
        i ^= (i & w) >> 11;
        i *= 0x74dcb303;
        i ^= (i & w) >> 2;
        i *= 0x9e501cc3;
        i ^= (i & w) >> 2;
        i *= 0xc860a3df;
        i &= w;
        i ^= i >> 5;
        return (i + p) & w;
    }
    else
    {
        w |= w >> 1;
        w |= w >> 2;
        w |= w >> 4;
        w |= w >> 8;
        w |= w >> 16;
        do
        {
            i ^= p; i *= 0xe170893d;
            i ^= p >> 16;
            i ^= (i & w) >>  4;
            i ^= p >>  8;  i *= 0x0929eb3f;
            i ^= p >> 23;
            i ^= (i & w) >> 1; i *= 1 | p >> 27;
            i *= 0x6935fa69;
            i ^= (i & w) >> 11;  i *= 0x74dcb303;
            i ^= (i & w) >>  2;  i *= 0x9e501cc3;
            i ^= (i & w) >>  2;  i *= 0xc860a3df;
            i &= w;
            i ^= i >> 5;
        } while (i >= l);
        return (i + p) % l;
    }
}

float cmj_randfloat(unsigned int i, const unsigned int p)
{
    i ^= p;
    i ^= i >> 17;
    i ^= i >> 10; i *= 0xb36534e5;
    i ^= i >> 12;
    i ^= i >> 21; i *= 0x93fc4795;
    i ^= 0xdf6e307f;
    i ^= i >> 17; i *= 1 | p >> 18;
    return i * (1.0f / 4294967808.0f);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_CORRELATEDMULTIJITTER_H
