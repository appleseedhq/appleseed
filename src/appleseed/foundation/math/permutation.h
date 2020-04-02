
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/rng/distribution.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// General purpose permutation generators.
//

// Generate the identity permutation.
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


//
// Precomputed Faure permutations.
//

const size_t FaurePermutationTableSize = 100;
APPLESEED_DLLSYMBOL extern const size_t* FaurePermutations[FaurePermutationTableSize];

struct PrimePermutationPair
{
    size_t          m_prime;
    const size_t*   m_perm;
};

APPLESEED_DLLSYMBOL extern const PrimePermutationPair APPLESEED_ALIGN(16) PrimesFaurePermutations[FaurePermutationTableSize];


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
template <typename Item, typename Index>
void small_item_reorder(
    Item            items[],
    Item            temp[],
    const Index     order[],
    const Index     count);
template <typename Item, typename Index>
void large_item_reorder(
    Item            items[],
    Index           tags[],
    const Index     order[],
    const Index     count);
template <typename Item, typename Index>
void large_item_reorder(
    Item            items[],
    Index           order[],
    const Index     count);


//
// Permutation generators implementation.
//

inline void identity_permutation(
    const size_t    size,
    size_t          perm[])
{
    for (size_t i = 0; i < size; ++i)
        perm[i] = i;
}

template <typename RNG>
inline void random_permutation(
    const size_t    size,
    size_t          perm[],
    RNG&            rng)
{
    assert(size > 0);

    // Start with identity permutation.
    for (size_t i = 0; i < size; ++i)
        perm[i] = i;

    // Shuffle.
    for (size_t i = 0; i < size - 1; ++i)
    {
        const size_t j =
            rand_int1(
                rng,
                static_cast<std::int32_t>(i),
                static_cast<std::int32_t>(size - 1));

        const size_t tmp = perm[i];
        perm[i] = perm[j];
        perm[j] = tmp;
    }
}

inline void reverse_qmc_permutation(
    const size_t    size,
    size_t          perm[])
{
    assert(size > 0);

    perm[0] = 0;

    for (size_t i = 1, n = size; i < size; ++i)
        perm[i] = --n;
}


//
// Permutation operations implementation.
//

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

template <typename Item, typename Index>
inline void small_item_reorder(
    Item            items[],
    Item            temp[],
    const Index     order[],
    const Index     count)
{
    assert(items);
    assert(temp);
    assert(order);

    for (Index i = 0; i < count; ++i)
        temp[i] = items[order[i]];

    for (Index i = 0; i < count; ++i)
        items[i] = temp[i];
}

template <typename Item, typename Index>
inline void large_item_reorder(
    Item            items[],
    Index           tags[],
    const Index     order[],
    const Index     count)
{
    assert(items);
    assert(tags);
    assert(order);

    for (Index i = 0; i < count; ++i)
        tags[i] = 0;

    for (Index i = 0; i < count; ++i)
    {
        if (tags[i])
            continue;

        const Item temp = items[i];
        Index next = order[i];
        Index j = i;

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

template <typename Item, typename Index>
inline void large_item_reorder(
    Item            items[],
    Index           order[],
    const Index     count)
{
    assert(items);
    assert(order);

    for (Index i = 0; i < count; ++i)
    {
        if (order[i] == ~Index(0))
            continue;

        const Item temp = items[i];
        Index next = order[i];
        Index j = i;

        while (next != i)
        {
            items[j] = items[next];
            j = next;
            next = order[j];
            order[j] = ~Index(0);
        }

        items[j] = temp;
    }
}

}   // namespace foundation
