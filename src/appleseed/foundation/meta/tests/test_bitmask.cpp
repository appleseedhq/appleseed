
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/bitmask.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Utility_BitMask2)
{
    TEST_CASE(Clear_SetsAllBitsToZero)
    {
        BitMask2 bitmask(2, 2);

        bitmask.clear();

        EXPECT_EQ(0, bitmask.get(0, 0));
        EXPECT_EQ(0, bitmask.get(1, 0));
        EXPECT_EQ(0, bitmask.get(0, 1));
        EXPECT_EQ(0, bitmask.get(1, 1));
    }

    TEST_CASE(Set_SetsBit)
    {
        BitMask2 bitmask(2, 2);
        bitmask.clear();

        bitmask.set(1, 1);

        EXPECT_TRUE(bitmask.is_set(1, 1));
    }

    TEST_CASE(Clear_ClearsBit)
    {
        BitMask2 bitmask(2, 2);
        bitmask.clear();

        bitmask.set(1, 1);
        bitmask.clear(1, 1);

        EXPECT_TRUE(bitmask.is_clear(1, 1));
    }

    TEST_CASE(Clear_PreservesOtherBits)
    {
        BitMask2 bitmask(2, 2);
        bitmask.clear();

        bitmask.set(0, 0);
        bitmask.clear(1, 0);

        EXPECT_TRUE(bitmask.is_set(0, 0));
    }

    bool are_equal(const BitMask2& bitmask, const bool* values)
    {
        for (size_t y = 0; y < bitmask.get_height(); ++y)
        {
            for (size_t x = 0; x < bitmask.get_width(); ++x)
            {
                if (bitmask.get(x, y) != values[y * bitmask.get_width() + x])
                    return false;
            }
        }

        return true;
    }

    TEST_CASE(StressTest)
    {
        const size_t Width = 17;
        const size_t Height = 9;

        bool values[Width * Height];

        for (size_t i = 0; i < Width * Height; ++i)
            values[i] = false;

        BitMask2 bitmask(Width, Height);
        bitmask.clear();

        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const size_t x = rand_int1(rng, 0, Width - 1);
            const size_t y = rand_int1(rng, 0, Height - 1);
            const bool value = rand_int1(rng, 0, 1) == 1;

            values[y * Width + x] = value;
            bitmask.set(x, y, value);

            const bool equal = are_equal(bitmask, values);
            ASSERT_TRUE(equal);
        }
    }
}
