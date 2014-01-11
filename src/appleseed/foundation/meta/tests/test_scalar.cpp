
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <limits>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Scalar)
{
    TEST_CASE(TestDegToRad)
    {
        EXPECT_FEQ(0.0,      deg_to_rad(0.0));
        EXPECT_FEQ(Pi / 4.0, deg_to_rad(45.0));
        EXPECT_FEQ(Pi,       deg_to_rad(180.0));
        EXPECT_FEQ(2.0 * Pi, deg_to_rad(360.0));
    }

    TEST_CASE(TestRadToDeg)
    {
        EXPECT_FEQ(0.0,      rad_to_deg(0.0));
        EXPECT_FEQ(45.0,     rad_to_deg(Pi / 4.0));
        EXPECT_FEQ(180.0,    rad_to_deg(Pi));
        EXPECT_FEQ(360.0,    rad_to_deg(2.0 * Pi));
    }

    TEST_CASE(TestNextPow2)
    {
        EXPECT_EQ(1,   next_pow2(1));
        EXPECT_EQ(2,   next_pow2(2));
        EXPECT_EQ(4,   next_pow2(3));
        EXPECT_EQ(4,   next_pow2(4));
        EXPECT_EQ(8,   next_pow2(5));
        EXPECT_EQ(8,   next_pow2(6));
        EXPECT_EQ(8,   next_pow2(7));
        EXPECT_EQ(8,   next_pow2(8));
        EXPECT_EQ(16,  next_pow2(9));
        EXPECT_EQ(256, next_pow2(256));
    }

    TEST_CASE(TestNextPow2Large)
    {
        const int32 TwoPower29 = 512 * 1024 * 1024;
        const int32 TwoPower30 = 512 * 1024 * 1024 * 2;
        EXPECT_EQ(TwoPower30, next_pow2(TwoPower29 + 1));
    }

    TEST_CASE(TestNextPow2Int64)
    {
        EXPECT_EQ(1,   next_pow2<int64>(1));
        EXPECT_EQ(2,   next_pow2<int64>(2));
        EXPECT_EQ(4,   next_pow2<int64>(3));
        EXPECT_EQ(4,   next_pow2<int64>(4));
        EXPECT_EQ(8,   next_pow2<int64>(5));
        EXPECT_EQ(8,   next_pow2<int64>(6));
        EXPECT_EQ(8,   next_pow2<int64>(7));
        EXPECT_EQ(8,   next_pow2<int64>(8));
        EXPECT_EQ(16,  next_pow2<int64>(9));
        EXPECT_EQ(256, next_pow2<int64>(256));
    }

    TEST_CASE(TestNextPow2Int64Large)
    {
        const int64 TwoPower34 = 17179869184LL;
        const int64 TwoPower35 = 17179869184LL * 2;
        EXPECT_EQ(TwoPower35, next_pow2(TwoPower34 + 1));
    }

    TEST_CASE(TestNextPow2UInt64)
    {
        EXPECT_EQ(1,   next_pow2<uint64>(1));
        EXPECT_EQ(2,   next_pow2<uint64>(2));
        EXPECT_EQ(4,   next_pow2<uint64>(3));
        EXPECT_EQ(4,   next_pow2<uint64>(4));
        EXPECT_EQ(8,   next_pow2<uint64>(5));
        EXPECT_EQ(8,   next_pow2<uint64>(6));
        EXPECT_EQ(8,   next_pow2<uint64>(7));
        EXPECT_EQ(8,   next_pow2<uint64>(8));
        EXPECT_EQ(16,  next_pow2<uint64>(9));
        EXPECT_EQ(256, next_pow2<uint64>(256));
    }

    TEST_CASE(TestNextPow2UInt64Large)
    {
        const uint64 TwoPower34 = 17179869184ULL;
        const uint64 TwoPower35 = 17179869184ULL * 2;
        EXPECT_EQ(TwoPower35, next_pow2(TwoPower34 + 1));
    }

    TEST_CASE(TestIsPow2OnPowerOfTwoValues)
    {
        EXPECT_TRUE(is_pow2(0));
        EXPECT_TRUE(is_pow2(1));
        EXPECT_TRUE(is_pow2(2));
        EXPECT_TRUE(is_pow2(4));
        EXPECT_TRUE(is_pow2(8));
        EXPECT_TRUE(is_pow2(256));
    }

    TEST_CASE(TestIsPow2OnNonPowerOfTwoValues)
    {
        EXPECT_FALSE(is_pow2(3));
        EXPECT_FALSE(is_pow2(5));
        EXPECT_FALSE(is_pow2(6));
        EXPECT_FALSE(is_pow2(7));
        EXPECT_FALSE(is_pow2(9));
        EXPECT_FALSE(is_pow2(255));
    }

    TEST_CASE(TestLog2)
    {
        EXPECT_EQ(0,  int_log2(1));
        EXPECT_EQ(1,  int_log2(2));
        EXPECT_EQ(1,  int_log2(3));
        EXPECT_EQ(2,  int_log2(4));
        EXPECT_EQ(5,  int_log2<int16>(1 << 5));
        EXPECT_EQ(5,  int_log2<int32>(1 << 5));
        EXPECT_EQ(5,  int_log2<int64>(1 << 5));
        EXPECT_EQ(5,  int_log2<uint16>(1 << 5));
        EXPECT_EQ(5,  int_log2<uint32>(1 << 5));
        EXPECT_EQ(5,  int_log2<uint64>(1 << 5));
        EXPECT_EQ(16, int_log2(1UL << 16));
        EXPECT_EQ(31, int_log2(1UL << 31));
    }

    TEST_CASE(TestFactorial)
    {
        EXPECT_EQ(1,   factorial(0));
        EXPECT_EQ(1,   factorial(1));
        EXPECT_EQ(2,   factorial(2));
        EXPECT_EQ(6,   factorial(3));
        EXPECT_EQ(24,  factorial(4));
        EXPECT_EQ(120, factorial(5));
    }

    TEST_CASE(Wrap_Given0_Returns0)
    {
        EXPECT_EQ(0.0, wrap(0.0));
    }

    TEST_CASE(Wrap_Given0Dot5_Returns0Dot5)
    {
        EXPECT_EQ(0.5, wrap(0.5));
    }

    TEST_CASE(Wrap_Given1_Returns0)
    {
        EXPECT_EQ(0.0, wrap(1.0));
    }

    TEST_CASE(Wrap_Given1Dot5_Returns0Dot5)
    {
        EXPECT_EQ(0.5, wrap(1.5));
    }

    TEST_CASE(Wrap_GivenMinus0Dot5_Returns0Dot5)
    {
        EXPECT_EQ(0.5, wrap(-0.5));
    }

    TEST_CASE(Wrap_GivenMinus1_Returns0)
    {
        EXPECT_EQ(0.0, wrap(-1.0));
    }

    TEST_CASE(Wrap_GivenMinus1Dot5_Returns0Dot5)
    {
        EXPECT_EQ(0.5, wrap(-1.5));
    }

    TEST_CASE(Truncate_FloatToSignedInteger)
    {
        EXPECT_EQ(static_cast<int>(-1.5f), truncate<int>(-1.5f));
        EXPECT_EQ(static_cast<int>(-1.0f), truncate<int>(-1.0f));
        EXPECT_EQ(static_cast<int>(-0.5f), truncate<int>(-0.5f));
        EXPECT_EQ(static_cast<int>( 0.0f), truncate<int>( 0.0f));
        EXPECT_EQ(static_cast<int>(+0.5f), truncate<int>(+0.5f));
        EXPECT_EQ(static_cast<int>(+1.0f), truncate<int>(+1.0f));
        EXPECT_EQ(static_cast<int>(+1.5f), truncate<int>(+1.5f));
    }

    TEST_CASE(Truncate_FloatToUnsignedInteger)
    {
        EXPECT_EQ(3221225472UL, truncate<uint32>(3221225472.0f));
    }

    TEST_CASE(Truncate_DoubleToUnsignedInteger)
    {
        EXPECT_EQ(3221225472UL, truncate<uint32>(3221225472.0));
    }

    TEST_CASE(Round_Given0_Returns0)
    {
        EXPECT_EQ(0, round<int>(0.0));
    }

    TEST_CASE(Round_Given3_Returns3)
    {
        EXPECT_EQ(3, round<int>(3.0));
    }

    TEST_CASE(Round_Given3Dot1_Returns3)
    {
        EXPECT_EQ(3, round<int>(3.1));
    }

    TEST_CASE(Round_Given3Dot5_Returns4)
    {
        EXPECT_EQ(4, round<int>(3.5));
    }

    TEST_CASE(Round_Given3Dot9_Returns4)
    {
        EXPECT_EQ(4, round<int>(3.9));
    }

    TEST_CASE(Round_GivenMinus3_ReturnsMinus3)
    {
        EXPECT_EQ(-3, round<int>(-3.0));
    }

    TEST_CASE(Round_GivenMinus3Dot1_ReturnsMinus3)
    {
        EXPECT_EQ(-3, round<int>(-3.1));
    }

    TEST_CASE(Round_GivenMinus3Dot5_ReturnsMinus4)
    {
        EXPECT_EQ(-4, round<int>(-3.5));
    }

    TEST_CASE(Round_GivenMinus3Dot9_ReturnsMinus4)
    {
        EXPECT_EQ(-4, round<int>(-3.9));
    }

    TEST_CASE(Mod_Given0Mod3_Returns0)
    {
        EXPECT_EQ(0, mod(0, 3));
    }

    TEST_CASE(Mod_Given1Mod3_Returns1)
    {
        EXPECT_EQ(1, mod(1, 3));
    }

    TEST_CASE(Mod_Given2Mod3_Returns2)
    {
        EXPECT_EQ(2, mod(2, 3));
    }

    TEST_CASE(Mod_Given3Mod3_Returns0)
    {
        EXPECT_EQ(0, mod(3, 3));
    }

    TEST_CASE(Mod_Given4Mod3_Returns1)
    {
        EXPECT_EQ(1, mod(4, 3));
    }

    TEST_CASE(Mod_GivenMinus1Mod3_Returns2)
    {
        EXPECT_EQ(2, mod(-1, 3));
    }

    TEST_CASE(Mod_GivenMinus2Mod3_Returns1)
    {
        EXPECT_EQ(1, mod(-2, 3));
    }

    TEST_CASE(Mod_GivenMinus3Mod3_Returns0)
    {
        EXPECT_EQ(0, mod(-3, 3));
    }

    TEST_CASE(Mod_GivenMinus4Mod3_Returns2)
    {
        EXPECT_EQ(2, mod(-4, 3));
    }

    TEST_CASE(Mod_Given0Dot5Mod2_Returns0Dot5)
    {
        EXPECT_EQ(0.5, mod(0.5, 2.0));
    }

    TEST_CASE(Mod_GivenMinus0Dot5Mod2_Returns1Dot5)
    {
        EXPECT_EQ(1.5, mod(-0.5, 2.0));
    }

    TEST_CASE(TestSmoothStep)
    {
        EXPECT_EQ(0.0, smoothstep(10.0, 20.0,  0.0));
        EXPECT_EQ(0.0, smoothstep(10.0, 20.0, 10.0));
        EXPECT_FEQ(0.5, smoothstep(10.0, 20.0, 15.0));
        EXPECT_EQ(1.0, smoothstep(10.0, 20.0, 20.0));
        EXPECT_EQ(1.0, smoothstep(10.0, 20.0, 30.0));
    }

    TEST_CASE(Mix_GivenBlendParameterLessThan0_ReturnsFirstValue)
    {
        const double result = mix(1.0, 5.0, -1.0);
        EXPECT_EQ(1.0, result);          // note: EQ, not FEQ
    }

    TEST_CASE(Mix_GivenBlendParameterEqualTo0_ReturnsFirstValue)
    {
        const double result = mix(1.0, 5.0, 0.0);
        EXPECT_EQ(1.0, result);          // note: EQ, not FEQ
    }

    TEST_CASE(Mix_GivenBlendParameterEqualTo1_ReturnsSecondValue)
    {
        const double result = mix(1.0, 5.0, 1.0);
        EXPECT_EQ(5.0, result);          // note: EQ, not FEQ
    }

    TEST_CASE(Mix_GivenBlendParameterGreatherThan1_ReturnsSecondValue)
    {
        const double result = mix(1.0, 5.0, 2.0);
        EXPECT_EQ(5.0, result);          // note: EQ, not FEQ
    }

    TEST_CASE(Mix_GivenBlendParameterBetween0And1_ReturnsCorrectlyBlendedValue)
    {
        const double result = mix(1.0, 5.0, 0.5);
        EXPECT_FEQ(3.0, result);
    }

    TEST_CASE(Mix_GivenBlendParameterOfDifferentTypeBetween0And1_ReturnsCorrectlyBlendedValue)
    {
        const double result = mix(1.0, 5.0, 0.5f);
        EXPECT_FEQ(3.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterLessThan0_ReturnsCorrectlyExtrapoledValue)
    {
        const double result = lerp(1.0, 5.0, -1.0);
        EXPECT_FEQ(-3.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterEqualTo0_ReturnsFirstValue)
    {
        const double result = lerp(1.0, 5.0, 0.0);
        EXPECT_FEQ(1.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterEqualTo1_ReturnsSecondValue)
    {
        const double result = lerp(1.0, 5.0, 1.0);
        EXPECT_FEQ(5.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterGreatherThan1_ReturnsCorrectlyExtrapoledValue)
    {
        const double result = lerp(1.0, 5.0, 2.0);
        EXPECT_FEQ(9.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterBetween0And1_ReturnsCorrectlyBlendedValue)
    {
        const double result = lerp(1.0, 5.0, 0.5);
        EXPECT_FEQ(3.0, result);
    }

    TEST_CASE(Lerp_GivenBlendParameterOfDifferentTypeBetween0And1_ReturnsCorrectlyBlendedValue)
    {
        const double result = lerp(1.0, 5.0, 0.5f);
        EXPECT_FEQ(3.0, result);
    }

    TEST_CASE(Fit_GivenIntegerInputRangeAndFloatingPointOutputRange)
    {
        const double result = fit<size_t, double>(4, 0, 19, 0.0, 1.0);
        EXPECT_FEQ(4.0 / 19.0, result);
    }

    TEST_CASE(TestFeqScalarScalarReturnsTrue)
    {
        EXPECT_TRUE(feq(  0.0,   0.0));
        EXPECT_TRUE(feq( 42.0,  42.0));
        EXPECT_TRUE(feq(-42.0, -42.0));
    }

    TEST_CASE(TestFeqScalarScalarReturnsFalse)
    {
        EXPECT_FALSE(feq(  0.0,  42.0));
        EXPECT_FALSE(feq( 42.0,   0.0));
        EXPECT_FALSE(feq( 42.0, -42.0));
        EXPECT_FALSE(feq(-42.0,  42.0));
    }

    TEST_CASE(TestFeqScalarScalarOverflows)
    {
        EXPECT_FALSE(feq(0.5 * numeric_limits<double>::max(), 0.1));
    }

    TEST_CASE(TestFeqScalarScalarUnderflows)
    {
        EXPECT_FALSE(feq(2.0 * numeric_limits<double>::min(), 10.0));
    }

    TEST_CASE(TestFzScalarReturnsTrue)
    {
        EXPECT_TRUE(fz(+0.0));
        EXPECT_TRUE(fz(-0.0));
    }

    TEST_CASE(TestFzScalarReturnsFalse)
    {
        EXPECT_FALSE(fz( 42.0));
        EXPECT_FALSE(fz(-42.0));
    }
}
