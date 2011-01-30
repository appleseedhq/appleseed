
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

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
//   implement arbitrary dimension best-candidate sequence generators.
//   implement specializations of Halton and Hammersley sequences generators for bases (2,3).
//   implement incremental radical inverse (for successive input values).
//   implement vectorized radical inverse functions with SSE2.
//   implement Sobol sequence generator.
//   reimplement best_candidate_sequence using a better acceleration structure than grid.
//


//
// Radical inverse functions.
//

// Compute the radical inverse of a given positive integer.
// The returned value is in the interval [0,1).
template <typename T>
T radical_inverse(
    size_t          base,       // base (prime number)
    size_t          n);         // input digits

// Fast implementation of the radical inverse in base 2.
// The returned value is in the interval [0,1).
template <typename T>
T radical_inverse_base2(
    size_t          n);         // input digits

// Radical inverse in base 2 with digits scrambling.
template <typename T>
T radical_inverse_base2(
    const size_t    r,          // scrambling value
    size_t          n);         // input digits

// Compute the folded radical inverse of a given positive integer.
// The returned value is in the interval [0,1).
template <typename T>
T folded_radical_inverse(
    size_t          base,       // base (prime number)
    size_t          n);         // input digits

// Fast implementation of the folded radical inverse in base 2.
// The returned value is in the interval [0,1).
template <typename T>
T folded_radical_inverse_base2(
    size_t          n);         // input digits

// Compute the radical inverse of a given positive integer with
// digit permutation. The returned value is in the interval [0,1).
template <typename T>
T permuted_radical_inverse(
    size_t          base,       // base (prime number)
    const size_t    perm[],     // digit permutation table (base entries)
    size_t          n);         // input digits

// No fast implementation of the radical inverse with permutation
// in base 2, because the only valid permutation in base 2 is the
// identity permutation (since 0 must map to 0, thus 1 maps to 1).


//
// Halton sequences.
//

// Return the n'th sample of a Halton sequence of arbitrary dimension.
// Samples are contained in the unit hypercube [0,1)^Dim.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t    bases[],    // bases (Dim entries, prime numbers)
    size_t          n);         // input digits

// Return the n'th sample of a Halton sequence of arbitrary dimension
// with digit permutation. Samples are contained in the unit hypercube
// [0,1)^Dim.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t    bases[],    // bases (Dim entries, prime numbers)
    const size_t    perms[],    // permutation tables, one per dimension
    size_t          n);         // input digits

// Return the n'th sample of a Halton-Zaremba sequence (Halton sequence
// using folded radical inverse). Samples are contained in the unit
// hypercube [0,1)^Dim.
template <typename T, size_t Dim>
Vector<T, Dim> halton_zaremba_sequence(
    const size_t    bases[],    // bases (Dim entries, prime numbers)
    size_t          n);         // input digits


//
// Hammersley sequences.
//

// Return the n'th sample of a Hammersley sequence of arbitrary dimension.
// Samples are contained in the unit hypercube [0,1)^Dim.
// todo: replace count argument by rcp_count?
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t    bases[],    // bases (Dim-1 entries, prime numbers)
    size_t          n,          // input digits
    size_t          count);     // total number of samples in sequence

// Return the n'th sample of a scrambled 2D Hammersley sequence.
// Samples are contained in the unit square [0,1)^2.
// todo: replace count argument by rcp_count?
template <typename T>
Vector<T, 2> hammersley_sequence(
    const size_t    r,          // scrambling value
    size_t          n,          // input digits
    size_t          count);     // total number of samples in sequence

// Return the n'th sample of a Hammersley sequence of arbitrary dimension
// with digits permutation. Samples are contained in the unit hypercube
// [0,1)^Dim.
// todo: replace count argument by rcp_count?
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t    bases[],    // bases (Dim-1 entries, prime numbers)
    const size_t    perms[],    // permutation tables, one per base
    size_t          n,          // input digits
    size_t          count);     // total number of samples in sequence

// Return the n'th sample of a Hammersley-Zaremba sequence (Hammersley
// sequence using folded radical inverse). Samples are contained in the
// unit hypercube [0,1)^Dim.
// todo: replace count argument by rcp_count?
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t    bases[],    // bases (Dim-1 entries, prime numbers)
    size_t          n,          // input digits
    size_t          count);     // total number of samples in sequence


//
// Best-candidate sequences.
//
// todo: move to foundation/math/poisson.h.
//

// Generate a best-candidate sequence of arbitrary dimension.
// This sequence approximates a Poisson-disk distribution.
// Warning: very slow, only for offline usage.
template <typename T, size_t Dim>
void best_candidate_sequence(
    size_t          count,      // total number of samples in sequence
    size_t          num_cand,   // number of candidates per sample
    Vector<T, Dim>  samples[]); // [out] generated samples


//
// Radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse(
    size_t          base,
    size_t          n)
{
    assert(base >= 2);

    if (base == 2)
        return radical_inverse_base2<T>(n);

    const T rcp_base = T(1.0) / base;
    T x = 0.0;
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
inline T radical_inverse_base2(
    size_t          n)
{
#if 0

    // Straightforward implementation.

    T x = 0.0;
    T b = 0.5;

    while (n)
    {
        x += (n & 1) * b;
        b *= T(0.5);
        n >>= 1;
    }

    return x;

#else

    // Directly reverse the bit sequence of n by doing successive swaps.
    n = (n >> 16) | (n << 16);      // 16-bit swap
    n = ((n & 0xFF00FF00) >> 8) |
        ((n & 0x00FF00FF) << 8);    // 8-bit swap
    n = ((n & 0xF0F0F0F0) >> 4) |
        ((n & 0x0F0F0F0F) << 4);    // 4-bit swap
    n = ((n & 0xCCCCCCCC) >> 2) |
        ((n & 0x33333333) << 2);    // 2-bit swap
    n = ((n & 0xAAAAAAAA) >> 1) |
        ((n & 0x55555555) << 1);    // 1-bit swap

    return static_cast<T>(n) / static_cast<T>(0x100000000LL);

#endif
}

template <typename T>
inline T radical_inverse_base2(
    const size_t    r,
    size_t          n)
{
    // Directly reverse the bit sequence of n by doing successive swaps.
    n = (n >> 16) | (n << 16);      // 16-bit swap
    n = ((n & 0xFF00FF00) >> 8) |
        ((n & 0x00FF00FF) << 8);    // 8-bit swap
    n = ((n & 0xF0F0F0F0) >> 4) |
        ((n & 0x0F0F0F0F) << 4);    // 4-bit swap
    n = ((n & 0xCCCCCCCC) >> 2) |
        ((n & 0x33333333) << 2);    // 2-bit swap
    n = ((n & 0xAAAAAAAA) >> 1) |
        ((n & 0x55555555) << 1);    // 1-bit swap

    // Scrambling.
    n ^= r;

    return static_cast<T>(n) / static_cast<T>(0x100000000LL);
}

template <typename T>
inline T folded_radical_inverse(
    size_t          base,
    size_t          n)
{
    assert(base >= 2);

    if (base == 2)
        return folded_radical_inverse_base2<T>(n);

    const T rcp_base = T(1.0) / base;
    T x = 0.0;
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
inline T folded_radical_inverse_base2(
    size_t          n)
{
    T x = 0.0;
    T b = 0.5;
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

template <typename T>
inline T permuted_radical_inverse(
    const size_t    base,
    const size_t    perm[],
    size_t          n)
{
    assert(base >= 2);

    if (base == 2)
    {
        // In base 2, the only valid permutation is the identity.
        return radical_inverse_base2<T>(n);
    }

    const T rcp_base = T(1.0) / base;
    T x = 0.0;
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
    const size_t    bases[],
    size_t          n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
        p[i] = radical_inverse<T>(bases[i], n);

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t    bases[],
    const size_t    perms[],
    size_t          n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        p[i] = permuted_radical_inverse<T>(bases[i], perms, n);
        perms += bases[i];
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_zaremba_sequence(
    const size_t    bases[],
    size_t          n)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
        p[i] = folded_radical_inverse<T>(bases[i], n);

    return p;
}


//
// Hammersley sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t    bases[],
    size_t          n,
    size_t          count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
        p[i] = radical_inverse<T>(bases[i - 1], n);

    return p;
}

template <typename T>
inline Vector<T, 2> hammersley_sequence(
    const size_t    r,
    size_t          n,
    size_t          count)
{
    Vector<T, 2> p;
    p[0] = static_cast<T>(n) / count;
    p[1] = radical_inverse_base2<T>(r, n);
    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t    bases[],
    const size_t    perms[],
    size_t          n,
    size_t          count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        p[i] = permuted_radical_inverse<T>(bases[i - 1], perms, n);
        perms += bases[i - 1];
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t    bases[],
    size_t          n,
    size_t          count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(n) / count;

    for (size_t i = 1; i < Dim; ++i)
        p[i] = folded_radical_inverse<T>(bases[i - 1], n);

    return p;
}


//
// Best-candidate sequences implementation.
//

template <typename T, size_t Dim>
inline void best_candidate_sequence(
    size_t          count,
    size_t          num_cand,
    Vector<T, Dim>  samples[])
{
    // Compute acceleration grid size.
    const size_t grid_size =
        truncate<size_t>(std::ceil(std::pow(count, T(1.0) / Dim) * 0.1));

    // Allocate acceleration grid.
    const size_t num_cells = pow_int(grid_size, Dim);
    std::vector<size_t>* grid = new std::vector<size_t>[num_cells];

    MersenneTwister mt;

    for (size_t n = 0; n < count; ++n)
    {
        // Compute a sample by generating a small number of
        // candidates samples and keeping only the best one.
        // todo: adjust the number of candidates at each
        // iteration, to ensure somewhat constant quality.
        T best_dist = 0.0;
        Vector<T, Dim> best_candidate;
        Vector<int, Dim> best_cand_coords;
        for (size_t c = 0; c < num_cand; ++c)
        {
            // Generate a random candidate.
            Vector<T, Dim> candidate;
            for (size_t d = 0; d < Dim; ++d)
                candidate[d] = rand_double1(mt);

            // Compute grid coordinates of this candidate.
            Vector<int, Dim> cand_coords;
            for (size_t d = 0; d < Dim; ++d)
            {
                const int coord = static_cast<int>(
                    truncate<int>(candidate[d] * grid_size));
                cand_coords[d] = clamp(coord, 0, int(grid_size - 1));
            }

            // Find in the grid the sample which is the closest
            // to the current candidate, according to the
            // euclidean norm.

            // Compute the coordinates of the neighboring of the
            // current candidate.
            Vector<int, Dim> min, max;
            for (size_t d = 0; d < Dim; ++d)
            {
                min[d] = clamp(cand_coords[d] - 1, 0, int(grid_size - 1));
                max[d] = clamp(cand_coords[d] + 1, 0, int(grid_size - 1));
//                min[d] = 0;
//                max[d] = int(grid_size - 1);
            }

            // Iterate over the neighboring of the candidate,
            // compute the euclidean distance to each of the
            // sample and keep track of the distance (really
            // the square of the distance) to the closest one.
            Vector<int, Dim> cursor = min;
            Vector<T, Dim> closest;
            T smallest_dist = std::numeric_limits<T>::max();
            while (true)
            {
                // Compute index of this cell.
                size_t cell_index = 0;
                for (size_t rd = Dim; rd > 0; --rd)
                {
                    cell_index *= grid_size;
                    cell_index += cursor[rd - 1];
                }

                // Search for the closest sample in this cell.
                const std::vector<size_t>& cell = grid[cell_index];
                for (size_t j = 0; j < cell.size(); ++j)
                {
                    // Fetch sample.
                    const size_t sample_index = cell[j];
                    assert(sample_index < n);   // only n generated samples
                    const Vector<T, Dim>& sample = samples[sample_index];

                    // Compute the minimum of (1) the square of the euclidean
                    // distance between this sample and the current candidate
                    // and (2) the square of the distance between this sample
                    // and the current candidate in every 1d projection.
                    // todo: implement torus distance to generate tileable patterns.
                    T dist = 0.0;
                    for (size_t d = 0; d < Dim; ++d)
                        dist += square(sample[d] - candidate[d]);
                    for (size_t d = 0; d < Dim; ++d)
                        dist = std::min(dist, std::abs(sample[d] - candidate[d]));

                    // Keep track of the smallest distance.
                    if (dist < smallest_dist)
                        smallest_dist = dist;
                }

                // Find next neighboring cell.
                size_t d = 0;
                while (true)
                {
                    if (cursor[d] < max[d])
                    {
                        ++cursor[d];
                        break;
                    }
                    else
                    {
                        cursor[d] = min[d];
                        if (d + 1 < Dim)
                            ++d;
                        else goto exit_visit;
                    }
                }
            }

exit_visit:

            // Keep track of the sample which is the furthest away
            // from its closest neighbor in the grid.
            if (smallest_dist == std::numeric_limits<T>::max())
            {
                // First candidate, as there is no neighboring samples.
                best_candidate = candidate;
                best_cand_coords = cand_coords;
                break;
            }
            else if (smallest_dist > best_dist)
            {
                // Found a better candidate.
                best_dist = smallest_dist;
                best_candidate = candidate;
                best_cand_coords = cand_coords;
            }
        }

        // Store the sample in the output array.
        samples[n] = best_candidate;

        // Store the sample in the acceleration grid.
        size_t sample_index = 0;
        for (size_t rd = Dim; rd > 0; --rd)
        {
            sample_index *= grid_size;
            sample_index += best_cand_coords[rd - 1];
        }
        assert(sample_index < num_cells);
        grid[sample_index].push_back(n);
    }

    // Delete acceleration grid.
    delete [] grid;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_QMC_H
