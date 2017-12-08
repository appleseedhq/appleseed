
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/fp.h"
#include "foundation/math/half.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include "OpenEXR/half.h"
#include "foundation/platform/_endexrheaders.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Half)
{
    TEST_CASE(BitsToFloatMatch)
    {
        half imath_half;
        Half as_half;

        for (uint16_t i = 0; i < 0xFFFF; ++i)
        {
            imath_half.setBits(i);
            as_half.set_bits(i);

            const float x = imath_half;
            const float y = as_half;

            if (FP<float>::is_nan(x))
                EXPECT_TRUE(FP<float>::is_nan(y));
            else
                EXPECT_EQ(x, y);
        }
    }

    TEST_CASE(FloatToBitsMatch)
    {
        half imath_half;
        Half as_half;

        for (uint16_t i = 0; i < 0xFFFF; ++i)
        {
            half x;
            x.setBits(i);

            const float xf = x;

            imath_half = xf;
            as_half = xf;

            if (FP<float>::is_neg_zero(xf))
            {
                // PtexHalf and our Half class convert -0.0f to 0.
                EXPECT_EQ(as_half.bits(), 0);
            }
            else
                EXPECT_EQ(imath_half.bits(), as_half.bits());
        }
    }
}
