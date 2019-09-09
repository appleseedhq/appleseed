
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

// appleseed.foundation headers.
#include "foundation/math/permutation.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdlib>
#include <cstring>

using namespace foundation;

TEST_SUITE(Foundation_Math_Permutation)
{
    TEST_CASE(TestIsPermutationOnValidPermutation1)
    {
        const size_t Size = 5;
        const size_t Permutation[Size] = { 0, 1, 2, 3, 4 };

        EXPECT_TRUE(is_permutation(Size, Permutation));
    }

    TEST_CASE(TestIsPermutationOnValidPermutation2)
    {
        const size_t Size = 5;
        const size_t Permutation[Size] = { 3, 2, 4, 0, 1 };

        EXPECT_TRUE(is_permutation(Size, Permutation));
    }

    TEST_CASE(TestIsPermutationOnValidPermutation3)
    {
        const size_t Size = 5;
        const size_t Permutation[Size] = { 4, 3, 2, 1, 0 };

        EXPECT_TRUE(is_permutation(Size, Permutation));
    }

    TEST_CASE(TestIsPermutationOnInvalidPermutation1)
    {
        const size_t Size = 5;
        const size_t Permutation[Size] = { 0, 1, 2, 3, 3 };

        EXPECT_FALSE(is_permutation(Size, Permutation));
    }

    TEST_CASE(TestIsPermutationOnInvalidPermutation2)
    {
        const size_t Size = 5;
        const size_t Permutation[Size] = { 0, 1, 2, 3, 5 };

        EXPECT_FALSE(is_permutation(Size, Permutation));
    }

    TEST_CASE(TestIdentityPermutation)
    {
        const size_t Size = 5;
        const size_t Expected[Size] = { 0, 1, 2, 3, 4 };

        size_t perm[Size];
        identity_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestRandomPermutation)
    {
        const size_t Size = 5;

        size_t perm[Size];
        MersenneTwister rng;
        random_permutation(Size, perm, rng);

        EXPECT_TRUE(is_permutation(Size, perm));
    }

    TEST_CASE(TestReverseQMCPermutationSize1)
    {
        const size_t Size = 1;
        const size_t Expected[Size] = { 0 };

        size_t perm[Size];
        reverse_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestReverseQMCPermutationSize5)
    {
        const size_t Size = 5;
        const size_t Expected[Size] = { 0, 4, 3, 2, 1 };

        size_t perm[Size];
        reverse_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestFaureQMCPermutationSize2)
    {
        const size_t Size = 2;
        const size_t Expected[Size] = { 0, 1 };

        size_t perm[Size];
        faure_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestFaureQMCPermutationSize3)
    {
        const size_t Size = 3;
        const size_t Expected[Size] = { 0, 1, 2 };

        size_t perm[Size];
        faure_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestFaureQMCPermutationSize4)
    {
        const size_t Size = 4;
        const size_t Expected[Size] = { 0, 2, 1, 3 };

        size_t perm[Size];
        faure_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestFaureQMCPermutationSize5)
    {
        const size_t Size = 5;
        const size_t Expected[Size] = { 0, 3, 2, 1, 4 };

        size_t perm[Size];
        faure_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestFaureQMCPermutationSize6)
    {
        const size_t Size = 6;
        const size_t Expected[Size] = { 0, 2, 4, 1, 3, 5 };

        size_t perm[Size];
        faure_qmc_permutation(Size, perm);

        EXPECT_ARRAY_EQ(Expected, perm);
    }

    TEST_CASE(TestSmallItemReorder)
    {
        const size_t Size = 10;
        const size_t Order[Size] = { 1, 3, 5, 2, 7, 6, 0, 4, 9, 8 };

        size_t items[Size] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        size_t temp[Size];
        small_item_reorder(items, temp, Order, Size);

        EXPECT_ARRAY_EQ(Order, items);
    }

    TEST_CASE(TestLargeItemReorderFirstVariant)
    {
        const size_t Size = 10;
        const size_t Order[Size] = { 1, 3, 5, 2, 7, 6, 0, 4, 9, 8 };

        size_t items[Size] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        size_t temp[Size];
        large_item_reorder(items, temp, Order, Size);

        EXPECT_ARRAY_EQ(Order, items);
    }

    TEST_CASE(TestLargeItemReorderSecondVariant)
    {
        const size_t Size = 10;
        const size_t Order[Size] = { 1, 3, 5, 2, 7, 6, 0, 4, 9, 8 };

        size_t items[Size] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

        size_t order[Size];
        memcpy(order, Order, sizeof(Order));
        large_item_reorder(items, order, Size);

        EXPECT_ARRAY_EQ(Order, items);
    }
}
