
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

#ifndef APPLESEED_FOUNDATION_MATH_QMC_H
#define APPLESEED_FOUNDATION_MATH_QMC_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// References:
//
//   http://www-stat.stanford.edu/~owen/reports/siggraph03.pdf
//   https://lirias.kuleuven.be/bitstream/123456789/131168/1/mcm2005_bartv.pdf
//
// todo:
//
//   implement specializations of Halton and Hammersley sequences generators for bases (2,3).
//   implement incremental radical inverse (for successive input values).
//   implement vectorized radical inverse functions with SSE2.
//   implement Sobol sequence generator.
//


//
// Base-2 radical inverse functions.
//
// All return values are in the interval [0, 1).
//

// Radical inverse in base 2.
template <typename T>
T radical_inverse_base2(
    size_t              n);             // input digits

// Radical inverse in base 2 with digits scrambling.
template <typename T>
T radical_inverse_base2(
    const size_t        r,              // scrambling value
    size_t              n);             // input digits

// Folded radical inverse in base 2.
template <typename T>
T folded_radical_inverse_base2(
    size_t              n);             // input digits


//
// Arbitrary-base radical inverse functions.
//
// All return values are in the interval [0, 1).
//

// Radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T radical_inverse(
    const size_t        base,           // base (prime number)
    size_t              n);             // input digits

// Folded radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T folded_radical_inverse(
    const size_t        base,           // base (prime number)
    size_t              n);             // input digits

// Radical inverse in arbitrary base digits permutation. No fast code path for base 2.
template <typename T>
T permuted_radical_inverse(
    const size_t        base,           // base (prime number)
    const size_t        perm[],         // digit permutation table (base entries)
    size_t              n);             // input digits


//
// Halton sequences of arbitrary dimensions.
//
// All return values are in the interval [0, 1)^Dim.
//

// Return the n'th sample of a Halton sequence.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        n);             // input digits

// Return the n'th sample of a Halton sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per dimension
    const size_t        n);             // input digits

// Return the n'th sample of a Halton-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        n);             // input digits


//
// The first N points of the 4D Halton sequence.
//

const size_t PrecomputedHaltonSequenceSize = 256;
extern const double PrecomputedHaltonSequence[4 * PrecomputedHaltonSequenceSize];


//
// Hammersley sequences of arbitrary dimensions.
//
// All return values are in the interval [0, 1)^Dim.
//
// todo: replace count argument by rcp_count?
//
//

// Return the n'th sample of a Hammersley sequence
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        n,              // input digits
    const size_t        count);         // total number of samples in sequence

// Return the n'th sample of a Hammersley sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per base
    const size_t        n,              // input digits
    const size_t        count);         // total number of samples in sequence

// Return the n'th sample of a Hammersley-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        n,              // input digits
    const size_t        count);         // total number of samples in sequence



//
// Base-2 radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse_base2(
    size_t              n)
{
    n = (n >> 16) | (n << 16);                                  // 16-bit swap
    n = ((n & 0xFF00FF00) >> 8) | ((n & 0x00FF00FF) << 8);      // 8-bit swap
    n = ((n & 0xF0F0F0F0) >> 4) | ((n & 0x0F0F0F0F) << 4);      // 4-bit swap
    n = ((n & 0xCCCCCCCC) >> 2) | ((n & 0x33333333) << 2);      // 2-bit swap
    n = ((n & 0xAAAAAAAA) >> 1) | ((n & 0x55555555) << 1);      // 1-bit swap

    return static_cast<T>(n) / static_cast<T>(0x100000000LL);
}

template <typename T>
inline T radical_inverse_base2(
    const size_t        r,
    size_t              n)
{
    n = (n >> 16) | (n << 16);                                  // 16-bit swap
    n = ((n & 0xFF00FF00) >> 8) | ((n & 0x00FF00FF) << 8);      // 8-bit swap
    n = ((n & 0xF0F0F0F0) >> 4) | ((n & 0x0F0F0F0F) << 4);      // 4-bit swap
    n = ((n & 0xCCCCCCCC) >> 2) | ((n & 0x33333333) << 2);      // 2-bit swap
    n = ((n & 0xAAAAAAAA) >> 1) | ((n & 0x55555555) << 1);      // 1-bit swap

    n ^= r;

    return static_cast<T>(n) / static_cast<T>(0x100000000LL);
}

template <typename T>
inline T folded_radical_inverse_base2(
    size_t              n)
{
    T x = T(0.0);
    T b = T(0.5);
    size_t offset = 0;

    while (x + b > x)
    {
        if ((n + offset) & 1) x += b;
        b *= T(0.5);
        n >>= 1;
        ++offset;
    }

    return x;
}


//
// Arbitrary-base radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse(
    const size_t        base,
    size_t              n)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;

    T x = T(0.0);
    T b = rcp_base;

    while (n)
    {
        x += (n % base) * b;
        b *= rcp_base;
        n /= base;
    }

    return x;
}

template <typename T>
inline T folded_radical_inverse(
    const size_t        base,
    size_t              n)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;

    T x = T(0.0);
    T b = rcp_base;
    size_t offset = 0;

    while (x + (base - 1) * b > x)
    {
        x += ((n + offset) % base) * b;
        b *= rcp_base;
        n /= base;
        ++offset;
    }

    return x;
}

template <typename T>
inline T permuted_radical_inverse(
    const size_t        base,
    const size_t        perm[],
    size_t              n)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;
    T x = T(0.0);
    T b = rcp_base;

    while (n)
    {
        x += perm[n % base] * b;
        b *= rcp_base;
        n /= base;
    }

    return x;
}


//
// Halton sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t        bases[],
    const size_t        n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(n)
                : radical_inverse<T>(base, n);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(n)
                : permuted_radical_inverse<T>(base, perms, n);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],
    const size_t        n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? folded_radical_inverse_base2<T>(n)
                : folded_radical_inverse<T>(base, n);
    }

    return p;
}


//
// Hammersley sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        n,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(n)
                : radical_inverse<T>(base, n);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        n,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(n)
                : permuted_radical_inverse<T>(base, perms, n);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],
    const size_t        n,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? folded_radical_inverse_base2<T>(n)
                : folded_radical_inverse<T>(bases[i - 1], n);
    }

    return p;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_QMC_H
