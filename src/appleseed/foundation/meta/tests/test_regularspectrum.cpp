
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/regularspectrum.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Image_RegularSpectrum31f)
{
    TEST_CASE(Set)
    {
        static const float ExpectedValues[31] =
        {
            42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
            42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
            42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
            42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f
        };

        const RegularSpectrum31f Expected(ExpectedValues);
        RegularSpectrum31f s;

        s.set(42.0f);

        EXPECT_EQ(Expected, s);
    }

    TEST_CASE(InPlaceAddition)
    {
        static const float InputValues[31] =
        {
             1.0f,  2.0f,  3.0f,  4.0f,  5.0f,  6.0f,  7.0f,  8.0f,
             9.0f, 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f,
            17.0f, 18.0f, 19.0f, 20.0f, 21.0f, 22.0f, 23.0f, 24.0f,
            25.0f, 26.0f, 27.0f, 28.0f, 29.0f, 30.0f, 31.0f
        };

        static const float RhsValues[31] =
        {
            31.0f, 30.0f, 29.0f, 28.0f, 27.0f, 26.0f, 25.0f, 24.0f,
            23.0f, 22.0f, 21.0f, 20.0f, 19.0f, 18.0f, 17.0f, 16.0f,
            15.0f, 14.0f, 13.0f, 12.0f, 11.0f, 10.0f,  9.0f,  8.0f,
             7.0f,  6.0f,  5.0f,  4.0f,  3.0f,  2.0f,  1.0f
        };

        const RegularSpectrum31f Expected(32.0f);
        const RegularSpectrum31f Rhs(RhsValues);
        RegularSpectrum31f s(InputValues);

        s += Rhs;

        EXPECT_FEQ(Expected, s);
    }

    TEST_CASE(InPlaceMultiplicationByScalar)
    {
        static const float InputValues[31] =
        {
             1.0f,  2.0f,  3.0f,  4.0f,  5.0f,  6.0f,  7.0f,  8.0f,
             9.0f, 10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f, 16.0f,
            17.0f, 18.0f, 19.0f, 20.0f, 21.0f, 22.0f, 23.0f, 24.0f,
            25.0f, 26.0f, 27.0f, 28.0f, 29.0f, 30.0f, 31.0f
        };

        static const float ExpectedValues[31] =
        {
             2.0f,  4.0f,  6.0f,  8.0f, 10.0f, 12.0f, 14.0f, 16.0f,
            18.0f, 20.0f, 22.0f, 24.0f, 26.0f, 28.0f, 30.0f, 32.0f,
            34.0f, 36.0f, 38.0f, 40.0f, 42.0f, 44.0f, 46.0f, 48.0f,
            50.0f, 52.0f, 54.0f, 56.0f, 58.0f, 60.0f, 62.0f
        };

        const RegularSpectrum31f Expected(ExpectedValues);
        RegularSpectrum31f s(InputValues);

        s *= 2.0f;

        EXPECT_FEQ(Expected, s);
    }

    TEST_CASE(InPlaceMultiplicationBySpectrum)
    {
        static const float InputValues[31] =
        {
              1.0f,   2.0f,   3.0f,   4.0f,   5.0f,   6.0f,   7.0f,   8.0f,
              9.0f,  10.0f,  11.0f,  12.0f,  13.0f,  14.0f,  15.0f,  16.0f,
             17.0f,  18.0f,  19.0f,  20.0f,  21.0f,  22.0f,  23.0f,  24.0f,
             25.0f,  26.0f,  27.0f,  28.0f,  29.0f,  30.0f,  31.0f
        };

        static const float RhsValues[31] =
        {
             31.0f,  30.0f,  29.0f,  28.0f,  27.0f,  26.0f,  25.0f,  24.0f,
             23.0f,  22.0f,  21.0f,  20.0f,  19.0f,  18.0f,  17.0f,  16.0f,
             15.0f,  14.0f,  13.0f,  12.0f,  11.0f,  10.0f,   9.0f,   8.0f,
              7.0f,   6.0f,   5.0f,   4.0f,   3.0f,   2.0f,   1.0f
        };

        static const float ExpectedValues[31] =
        {
             31.0f,  60.0f,  87.0f, 112.0f, 135.0f, 156.0f, 175.0f, 192.0f,
            207.0f, 220.0f, 231.0f, 240.0f, 247.0f, 252.0f, 255.0f, 256.0f,
            255.0f, 252.0f, 247.0f, 240.0f, 231.0f, 220.0f, 207.0f, 192.0f,
            175.0f, 156.0f, 135.0f, 112.0f,  87.0f,  60.0f,  31.0f
        };

        const RegularSpectrum31f Expected(ExpectedValues);
        const RegularSpectrum31f Rhs(RhsValues);
        RegularSpectrum31f s(InputValues);

        s *= Rhs;

        EXPECT_FEQ(Expected, s);
    }

    TEST_CASE(IsSaturated_GivenSpectrumWithAllComponentsSetToZero_ReturnsTrue)
    {
        const RegularSpectrum31f s(0.0f);

        EXPECT_TRUE(is_saturated(s));
    }

    TEST_CASE(IsSaturated_GivenSpectrumWithAllComponentsSetToOne_ReturnsTrue)
    {
        const RegularSpectrum31f s(1.0f);

        EXPECT_TRUE(is_saturated(s));
    }

    TEST_CASE(IsSaturated_GivenUnsaturatedSpectrum_ReturnsFalse)
    {
        RegularSpectrum31f s(0.5f);
        s[0] = 2.0f;

        EXPECT_FALSE(is_saturated(s));
    }
}
