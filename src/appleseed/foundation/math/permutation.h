
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

#ifndef APPLESEED_FOUNDATION_MATH_PERMUTATION_H
#define APPLESEED_FOUNDATION_MATH_PERMUTATION_H

// appleseed.foundation headers.
#include "foundation/math/rng.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// General purpose permutation generators.
//

// Generate an identity permutation.
void identity_permutation(
    const size_t    size,       // size of the permutation
    size_t          perm[]);    // [out] permutation

// Generate a random permutation using Knuth algorithm.
template <typename RNG>
void random_permutation(
    const size_t    size,       // size of the permutation
    size_t          perm[],     // [out] permutation
    RNG&            rng);       // random number generator


//
// Permutation generators suitable for digits scrambling in QMC.
//
// These permutations P satisfy P(0) = 0, i.e. the value 0 stays at position 0.
//

// Generate a reverse permutation.
void reverse_qmc_permutation(
    const size_t    size,       // size of the permutation
    size_t          perm[]);    // [out] permutation

// Generate a Faure permutation.
void faure_qmc_permutation(
    const size_t    size,       // size of the permutation
    size_t          perm[]);    // [out] permutation

// Precomputed Faure permutation tables.
const size_t FaurePermutationTableSize = 100;
FOUNDATIONDLL extern size_t* FaurePermutations[FaurePermutationTableSize];


//
// Permutation operations.
//

// Test whether a given vector is a valid permutation.
bool is_permutation(
    const size_t    size,       // size of the vector
    const size_t    v[]);       // vector


//
// Reordering functions.
//

// Shuffle items in a vector according to a given permutation.
// Use small_item_reorder() if items are 32 bytes wide or less.
// To reorder larger items, use large_item_reorder().
template <typename T>
void small_item_reorder(
    T               items[],
    T               temp[],
    const size_t    order[],
    const size_t    count);
template <typename T>
void large_item_reorder(
    T               items[],
    size_t          tags[],
    const size_t    order[],
    const size_t    count);
template <typename T>
void large_item_reorder(
    T               items[],
    size_t          order[],
    const size_t    count);


//
// Permutation generators implementation.
//

// Generate an identity permutation.
inline void identity_permutation(
    const size_t    size,
    size_t          perm[])
{
    for (size_t i = 0; i < size; ++i)
        perm[i] = i;
}

// Generate a random permutation using Knuth algorithm.
template <typename RNG>
inline void random_permutation(
    const size_t    size,
    size_t          perm[],
    RNG&            rng)
{
    // Start with identity permutation.
    for (size_t i = 0; i < size; ++i)
        perm[i] = i;

    // Shuffle.
    for (size_t i = 0; i < size - 1; ++i)
    {
        const size_t j = rand_int1(
            rng,
            static_cast<int32>(i),
            static_cast<int32>(size - 1));

        const size_t tmp = perm[i];
        perm[i] = perm[j];
        perm[j] = tmp;
    }
}

// Generate a reverse permutation.
inline void reverse_qmc_permutation(
    const size_t    size,
    size_t          perm[])
{
    assert(size > 0);
    perm[0] = 0;
    size_t n = size;
    for (size_t i = 1; i < size; ++i)
        perm[i] = --n;
}

// Generate a Faure permutation.
inline void faure_qmc_permutation(
    const size_t    size,
    size_t          perm[])
{
    assert(size >= 2);
    if (size == 2)
    {
        // Identity permutation.
        perm[0] = 0;
        perm[1] = 1;
    }
    else if (size & 1)
    {
        // Recursively build Faure permutation for odd size.
        faure_qmc_permutation(size - 1, perm);
        const size_t k = (size - 1) >> 1;
        for (size_t i = 0; i < size - 1; ++i)
        {
            if (perm[i] >= k)
                ++perm[i];
        }

        // Insert k in the middle of the permutation.
        for (size_t i = size - 1; i >= k + 1; --i)
            perm[i] = perm[i - 1];
        perm[k] = k;
    }
    else
    {
        // Recursively build Faure permutation for even size.
        const size_t k = size >> 1;
        faure_qmc_permutation(k, perm);
        for (size_t i = 0; i < k; ++i)
        {
            perm[i] <<= 1;
            perm[i + k] = perm[i] + 1;
        }
    }
}


//
// Permutation operations implementation.
//

// Test whether a given vector is a valid permutation.
inline bool is_permutation(
    const size_t    size,
    const size_t    v[])
{
    for (size_t value = 0; value < size; ++value)
    {
        bool found_value = false;

        for (size_t rank = 0; rank < size; ++rank)
        {
            if (v[rank] == value)
            {
                found_value = true;
                break;
            }
        }

        if (!found_value)
            return false;
    }

    return true;
}


//
// Reordering functions implementation.
//

// Shuffle items in a vector according to a given permutation.
template <typename T>
inline void small_item_reorder(
    T               items[],
    T               temp[],
    const size_t    order[],
    const size_t    count)
{
    assert(items);
    assert(temp);
    assert(order);
    assert(count > 0);

    for (size_t i = 0; i < count; ++i)
        temp[i] = items[order[i]];

    for (size_t i = 0; i < count; ++i)
        items[i] = temp[i];
}
template <typename T>
inline void large_item_reorder(
    T               items[],
    size_t          tags[],
    const size_t    order[],
    const size_t    count)
{
    assert(items);
    assert(tags);
    assert(order);
    assert(count > 0);

    for (size_t i = 0; i < count; ++i)
        tags[i] = 0;

    for (size_t i = 0; i < count; ++i)
    {
        if (tags[i])
            continue;

        const T temp = items[i];
        size_t next = order[i];
        size_t j = i;

        while (next != i)
        {
            items[j] = items[next];
            j = next;
            next = order[j];
            tags[j] = 1;
        }

        items[j] = temp;
    }
}
template <typename T>
inline void large_item_reorder(
    T               items[],
    size_t          order[],
    const size_t    count)
{
    assert(items);
    assert(order);
    assert(count > 0);

    for (size_t i = 0; i < count; ++i)
    {
        if (order[i] == ~size_t(0))
            continue;

        const T temp = items[i];
        size_t next = order[i];
        size_t j = i;

        while (next != i)
        {
            items[j] = items[next];
            j = next;
            next = order[j];
            order[j] = ~size_t(0);
        }

        items[j] = temp;
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_PERMUTATION_H
