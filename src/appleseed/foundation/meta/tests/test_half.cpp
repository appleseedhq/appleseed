
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/test.h"

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/half.h"
#include "foundation/platform/_endexrheaders.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

using namespace foundation;

TEST_SUITE(Foundation_Math_Half)
{
    TEST_CASE(HalfToFloat_FloatToHalf_Roundtrip)
    {
        for (size_t i = 0x0000; i <= 0xFFFF; ++i)
        {
            const Half expected_half = Half::from_bits(static_cast<std::uint16_t>(i));
            const float expected_float = half_to_float(expected_half);

            const Half actual_half = float_to_half(expected_float);
            const float actual_float = half_to_float(actual_half);

            if (FP<float>::is_qnan(expected_float))
                EXPECT_TRUE(FP<float>::is_qnan(actual_float));
            else if (FP<float>::is_snan(expected_float))
                EXPECT_TRUE(FP<float>::is_snan(actual_float));
            else EXPECT_EQ(expected_float, actual_float);
        }
    }

    TEST_CASE(HalfToFloat_FloatToHalfAlt_Roundtrip)
    {
        for (size_t i = 0x0000; i <= 0xFFFF; ++i)
        {
            const Half expected_half = Half::from_bits(static_cast<std::uint16_t>(i));
            const float expected_float = half_to_float(expected_half);

            const Half actual_half = float_to_half_alt(expected_float);
            const float actual_float = half_to_float(actual_half);

            if (FP<float>::is_qnan(expected_float))
                EXPECT_TRUE(FP<float>::is_qnan(actual_float));
            else if (FP<float>::is_snan(expected_float))
                EXPECT_TRUE(FP<float>::is_snan(actual_float));
            else EXPECT_EQ(expected_float, actual_float);
        }
    }

    TEST_CASE(HalfToFloat_FastFloatToHalf_Roundtrip)
    {
        for (size_t i = 0x0000; i <= 0xFFFF; ++i)
        {
            const Half expected_half = Half::from_bits(static_cast<std::uint16_t>(i));
            const float expected_float = half_to_float(expected_half);

            const Half actual_half = fast_float_to_half(expected_float);
            const float actual_float = half_to_float(actual_half);

            if (!FP<float>::is_nan(expected_float) && !FP<float>::is_inf(expected_float))
                EXPECT_EQ(expected_float, actual_float);
        }
    }

    TEST_CASE(HalfToFloat_MatchesImath)
    {
        for (size_t i = 0x0000; i <= 0xFFFF; ++i)
        {
            // Construct an Imath's half then convert it to a float.
            half imath_half;
            imath_half.setBits(static_cast<std::uint16_t>(i));
            const float imath_float = static_cast<float>(imath_half);

            // Construct an appleseed's Half then convert it to a float using half_to_float().
            const Half as_half = Half::from_bits(static_cast<std::uint16_t>(i));
            const float as_float = half_to_float(as_half);

            if (FP<float>::is_qnan(imath_float))
                EXPECT_TRUE(FP<float>::is_qnan(as_float));
            else if (FP<float>::is_snan(imath_float))
                EXPECT_TRUE(FP<float>::is_snan(as_float));
            else EXPECT_EQ(imath_float, as_float);
        }
    }

    TEST_CASE(FloatToHalf_MatchesImath)
    {
        for (size_t i = 0x0000; i <= 0xFFFF; ++i)
        {
            // Generate a float via Imath's half.
            half x;
            x.setBits(static_cast<std::uint16_t>(i));
            const float f = x;

            const half imath_half(f);
            const float imath_float = static_cast<float>(imath_half);

            const Half as_half = float_to_half(f);
            const float as_float = half_to_float(as_half);

            if (FP<float>::is_qnan(imath_float))
                EXPECT_TRUE(FP<float>::is_qnan(as_float));
            else if (FP<float>::is_snan(imath_float))
                EXPECT_TRUE(FP<float>::is_snan(as_float));
            else EXPECT_EQ(imath_float, as_float);
        }
    }
}
