
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
    TEST_CASE(DegToRad)
    {
        EXPECT_FEQ(0.0,                deg_to_rad(0.0));
        EXPECT_FEQ(Pi<double>() / 4.0, deg_to_rad(45.0));
        EXPECT_FEQ(Pi<double>(),       deg_to_rad(180.0));
        EXPECT_FEQ(2.0 * Pi<double>(), deg_to_rad(360.0));

        EXPECT_FEQ(static_cast<float>(Pi<double>() / 4.0), deg_to_rad(45.0f));
        EXPECT_FEQ_EPS(static_cast<long double>(Pi<double>() / 4.0), deg_to_rad(45.0L), 1.0e-14L);
    }

    TEST_CASE(RadToDeg)
    {
        EXPECT_FEQ(0.0,   rad_to_deg(0.0));
        EXPECT_FEQ(45.0,  rad_to_deg(Pi<double>() / 4.0));
        EXPECT_FEQ(180.0, rad_to_deg(Pi<double>()));
        EXPECT_FEQ(360.0, rad_to_deg(2.0 * Pi<double>()));

        EXPECT_FEQ(45.0f, rad_to_deg(static_cast<float>(Pi<double>() / 4.0)));
        EXPECT_FEQ_EPS(45.0L, rad_to_deg(static_cast<long double>(Pi<double>() / 4.0)), 1.0e-14L);
    }

    TEST_CASE(PowInt_CompileTimeExponent)
    {
        EXPECT_EQ(1, (pow_int<0, int>(0)));
        EXPECT_EQ(0, (pow_int<1, int>(0)));
        EXPECT_EQ(0, (pow_int<2, int>(0)));

        EXPECT_EQ(1, (pow_int<0, int>(1)));
        EXPECT_EQ(1, (pow_int<1, int>(1)));
        EXPECT_EQ(1, (pow_int<2, int>(1)));

        EXPECT_EQ(1, (pow_int<0, int>(2)));
        EXPECT_EQ(2, (pow_int<1, int>(2)));
        EXPECT_EQ(4, (pow_int<2, int>(2)));

        EXPECT_EQ(1, (pow_int<0, unsigned int>(2)));
        EXPECT_EQ(2, (pow_int<1, unsigned int>(2)));
        EXPECT_EQ(4, (pow_int<2, unsigned int>(2)));

        EXPECT_FEQ(1.0, (pow_int<0, double>(2.0)));
        EXPECT_FEQ(2.0, (pow_int<1, double>(2.0)));
        EXPECT_FEQ(4.0, (pow_int<2, double>(2.0)));
        EXPECT_FEQ(8.0, (pow_int<3, double>(2.0)));
    }

    TEST_CASE(PowInt_RunTimeExponent)
    {
        EXPECT_EQ(1, (pow_int<int>(0, 0)));
        EXPECT_EQ(0, (pow_int<int>(0, 1)));
        EXPECT_EQ(0, (pow_int<int>(0, 2)));

        EXPECT_EQ(1, (pow_int<int>(1, 0)));
        EXPECT_EQ(1, (pow_int<int>(1, 1)));
        EXPECT_EQ(1, (pow_int<int>(1, 2)));

        EXPECT_EQ(1, (pow_int<int>(2, 0)));
        EXPECT_EQ(2, (pow_int<int>(2, 1)));
        EXPECT_EQ(4, (pow_int<int>(2, 2)));

        EXPECT_EQ(1, (pow_int<unsigned int>(2, 0)));
        EXPECT_EQ(2, (pow_int<unsigned int>(2, 1)));
        EXPECT_EQ(4, (pow_int<unsigned int>(2, 2)));

        EXPECT_FEQ(1.0, (pow_int<double>(2.0, 0)));
        EXPECT_FEQ(2.0, (pow_int<double>(2.0, 1)));
        EXPECT_FEQ(4.0, (pow_int<double>(2.0, 2)));
        EXPECT_FEQ(8.0, (pow_int<double>(2.0, 3)));
    }

    TEST_CASE(NextPow2)
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

    TEST_CASE(NextPow2_Large)
    {
        const int32 TwoPower29 = 512 * 1024 * 1024;
        const int32 TwoPower30 = 512 * 1024 * 1024 * 2;
        EXPECT_EQ(TwoPower30, next_pow2(TwoPower29 + 1));
    }

    TEST_CASE(NextPow2_Int64)
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

    TEST_CASE(NextPow2_Int64_Large)
    {
        const int64 TwoPower34 = 17179869184LL;
        const int64 TwoPower35 = 17179869184LL * 2;
        EXPECT_EQ(TwoPower35, next_pow2(TwoPower34 + 1));
    }

    TEST_CASE(NextPow2_UInt64)
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

    TEST_CASE(NextPow2_UInt64_Large)
    {
        const uint64 TwoPower34 = 17179869184ULL;
        const uint64 TwoPower35 = 17179869184ULL * 2;
        EXPECT_EQ(TwoPower35, next_pow2(TwoPower34 + 1));
    }

    TEST_CASE(IsPow2_PowerOfTwoValues)
    {
        EXPECT_TRUE(is_pow2(0));
        EXPECT_TRUE(is_pow2(1));
        EXPECT_TRUE(is_pow2(2));
        EXPECT_TRUE(is_pow2(4));
        EXPECT_TRUE(is_pow2(8));
        EXPECT_TRUE(is_pow2(256));
    }

    TEST_CASE(IsPow2_NonPowerOfTwoValues)
    {
        EXPECT_FALSE(is_pow2(3));
        EXPECT_FALSE(is_pow2(5));
        EXPECT_FALSE(is_pow2(6));
        EXPECT_FALSE(is_pow2(7));
        EXPECT_FALSE(is_pow2(9));
        EXPECT_FALSE(is_pow2(255));
    }

    TEST_CASE(Log2Int)
    {
        EXPECT_EQ(0,  log2_int(1));
        EXPECT_EQ(1,  log2_int(2));
        EXPECT_EQ(1,  log2_int(3));
        EXPECT_EQ(2,  log2_int(4));
        EXPECT_EQ(5,  log2_int<int16>(1UL << 5));
        EXPECT_EQ(5,  log2_int<int32>(1UL << 5));
        EXPECT_EQ(5,  log2_int<int64>(1UL << 5));
        EXPECT_EQ(5,  log2_int<uint16>(1UL << 5));
        EXPECT_EQ(5,  log2_int<uint32>(1UL << 5));
        EXPECT_EQ(5,  log2_int<uint64>(1UL << 5));
        EXPECT_EQ(16, log2_int(1UL << 16));
        EXPECT_EQ(31, log2_int(1UL << 31));
    }

    TEST_CASE(NextMultiple)
    {
        EXPECT_EQ(0,  next_multiple(0, 5));
        EXPECT_EQ(5,  next_multiple(1, 5));
        EXPECT_EQ(5,  next_multiple(2, 5));
        EXPECT_EQ(5,  next_multiple(3, 5));
        EXPECT_EQ(5,  next_multiple(4, 5));
        EXPECT_EQ(5,  next_multiple(5, 5));
        EXPECT_EQ(10, next_multiple(6, 5));
    }

    TEST_CASE(PrevMultiple)
    {
        EXPECT_EQ(0, prev_multiple(0, 5));
        EXPECT_EQ(0, prev_multiple(1, 5));
        EXPECT_EQ(0, prev_multiple(2, 5));
        EXPECT_EQ(0, prev_multiple(3, 5));
        EXPECT_EQ(0, prev_multiple(4, 5));
        EXPECT_EQ(5, prev_multiple(5, 5));
        EXPECT_EQ(5, prev_multiple(6, 5));
    }

    TEST_CASE(Factorial)
    {
        EXPECT_EQ(1,   factorial(0));
        EXPECT_EQ(1,   factorial(1));
        EXPECT_EQ(2,   factorial(2));
        EXPECT_EQ(6,   factorial(3));
        EXPECT_EQ(24,  factorial(4));
        EXPECT_EQ(120, factorial(5));
    }

    TEST_CASE(Binomial)
    {
        EXPECT_EQ(1,   binomial(0, 0));

        EXPECT_EQ(1,   binomial(1, 0));
        EXPECT_EQ(1,   binomial(1, 1));

        EXPECT_EQ(1,   binomial(2, 0));
        EXPECT_EQ(2,   binomial(2, 1));
        EXPECT_EQ(1,   binomial(2, 2));

        EXPECT_EQ(1,   binomial(3, 0));
        EXPECT_EQ(3,   binomial(3, 1));
        EXPECT_EQ(3,   binomial(3, 2));
        EXPECT_EQ(1,   binomial(3, 3));

        EXPECT_EQ(1,   binomial(4, 0));
        EXPECT_EQ(4,   binomial(4, 1));
        EXPECT_EQ(6,   binomial(4, 2));
        EXPECT_EQ(4,   binomial(4, 3));
        EXPECT_EQ(1,   binomial(4, 4));

        EXPECT_EQ(1,   binomial(5, 0));
        EXPECT_EQ(5,   binomial(5, 1));
        EXPECT_EQ(10,  binomial(5, 2));
        EXPECT_EQ(10,  binomial(5, 3));
        EXPECT_EQ(5,   binomial(5, 4));
        EXPECT_EQ(1,   binomial(5, 5));
    }

    TEST_CASE(Wrap)
    {
        EXPECT_EQ(0.0, wrap(0.0));
        EXPECT_EQ(0.5, wrap(0.5));
        EXPECT_EQ(0.0, wrap(1.0));
        EXPECT_EQ(0.5, wrap(1.5));
        EXPECT_EQ(0.5, wrap(-0.5));
        EXPECT_EQ(0.0, wrap(-1.0));
        EXPECT_EQ(0.5, wrap(-1.5));
    }

    TEST_CASE(Truncate_FloatToSignedInteger)
    {
        EXPECT_EQ(-1, truncate<int>(-1.5f));
        EXPECT_EQ(-1, truncate<int>(-1.0f));
        EXPECT_EQ( 0, truncate<int>(-0.5f));
        EXPECT_EQ( 0, truncate<int>( 0.0f));
        EXPECT_EQ( 0, truncate<int>(+0.5f));
        EXPECT_EQ(+1, truncate<int>(+1.0f));
        EXPECT_EQ(+1, truncate<int>(+1.5f));
    }

    TEST_CASE(Truncate_FloatToUnsignedInteger)
    {
        EXPECT_EQ(3221225472UL, truncate<uint32>(3221225472.0f));
    }

    TEST_CASE(Truncate_DoubleToUnsignedInteger)
    {
        EXPECT_EQ(3221225472UL, truncate<uint32>(3221225472.0));
    }

    TEST_CASE(Round_FloatToSignedInteger)
    {
        EXPECT_EQ( 0, round<int>(0.0f));
        EXPECT_EQ( 3, round<int>(3.0f));
        EXPECT_EQ( 3, round<int>(3.1f));
        EXPECT_EQ( 4, round<int>(3.5f));
        EXPECT_EQ( 4, round<int>(3.9f));
        EXPECT_EQ(-3, round<int>(-3.0f));
        EXPECT_EQ(-3, round<int>(-3.1f));
        EXPECT_EQ(-4, round<int>(-3.5f));
        EXPECT_EQ(-4, round<int>(-3.9f));
    }

    TEST_CASE(FastFloor)
    {
        EXPECT_EQ(-2.0f, fast_floor(-1.5f));
        EXPECT_EQ(-1.0f, fast_floor(-1.0f));
        EXPECT_EQ(-1.0f, fast_floor(-0.5f));
        EXPECT_EQ( 0.0f, fast_floor( 0.0f));
        EXPECT_EQ( 0.0f, fast_floor(+0.5f));
        EXPECT_EQ(+1.0f, fast_floor(+1.0f));
        EXPECT_EQ(+1.0f, fast_floor(+1.5f));
    }

    TEST_CASE(FastCeil)
    {
        EXPECT_EQ(-1.0f, fast_ceil(-1.5f));
        EXPECT_EQ(-1.0f, fast_ceil(-1.0f));
        EXPECT_EQ( 0.0f, fast_ceil(-0.5f));
        EXPECT_EQ( 0.0f, fast_ceil( 0.0f));
        EXPECT_EQ(+1.0f, fast_ceil(+0.5f));
        EXPECT_EQ(+1.0f, fast_ceil(+1.0f));
        EXPECT_EQ(+2.0f, fast_ceil(+1.5f));
    }

    TEST_CASE(Frac)
    {
        EXPECT_FEQ(0.0, frac(+0.0));
        EXPECT_FEQ(0.5, frac(+0.5));
        EXPECT_FEQ(0.0, frac(+1.0));
        EXPECT_FEQ(0.2, frac(+4.2));
        EXPECT_FEQ(0.0, frac(-0.0));
        EXPECT_FEQ(0.5, frac(-0.5));
        EXPECT_FEQ(0.0, frac(-1.0));
        EXPECT_FEQ(0.8, frac(-4.2));
    }

    TEST_CASE(Mod)
    {
        EXPECT_EQ(0, mod(0, 3));
        EXPECT_EQ(1, mod(1, 3));
        EXPECT_EQ(2, mod(2, 3));
        EXPECT_EQ(0, mod(3, 3));
        EXPECT_EQ(1, mod(4, 3));
        EXPECT_EQ(2, mod(-1, 3));
        EXPECT_EQ(1, mod(-2, 3));
        EXPECT_EQ(0, mod(-3, 3));
        EXPECT_EQ(2, mod(-4, 3));
        EXPECT_EQ(0.5, mod(0.5, 2.0));
        EXPECT_EQ(1.5, mod(-0.5, 2.0));
    }

    TEST_CASE(RotL32)
    {
        EXPECT_EQ(0xDEADBEEFUL, rotl32(0xDEADBEEFUL, 0));
        EXPECT_EQ(0xEADBEEFDUL, rotl32(0xDEADBEEFUL, 4));
        EXPECT_EQ(0xEF56DF77UL, rotl32(0xDEADBEEFUL, 31));
    }

    TEST_CASE(RotL64)
    {
        EXPECT_EQ(0xDEADBEEF12345678ULL, rotl64(0xDEADBEEF12345678ULL, 0));
        EXPECT_EQ(0xEADBEEF12345678DULL, rotl64(0xDEADBEEF12345678ULL, 4));
        EXPECT_EQ(0x6F56DF77891A2B3CULL, rotl64(0xDEADBEEF12345678ULL, 63));
    }

    TEST_CASE(RotR32)
    {
        EXPECT_EQ(0xDEADBEEFUL, rotr32(0xDEADBEEFUL, 0));
        EXPECT_EQ(0xFDEADBEEUL, rotr32(0xDEADBEEFUL, 4));
        EXPECT_EQ(0xBD5B7DDFUL, rotr32(0xDEADBEEFUL, 31));
    }

    TEST_CASE(RotR64)
    {
        EXPECT_EQ(0xDEADBEEF12345678ULL, rotr64(0xDEADBEEF12345678ULL, 0));
        EXPECT_EQ(0x8DEADBEEF1234567ULL, rotr64(0xDEADBEEF12345678ULL, 4));
        EXPECT_EQ(0xBD5B7DDE2468ACF1ULL, rotr64(0xDEADBEEF12345678ULL, 63));
    }

    TEST_CASE(SmoothStep)
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

    TEST_CASE(InverseLerp_GivenBlendParameterLessThanFirstValue_ReturnsCorrectlyExtrapoledValue)
    {
        const double result = inverse_lerp(1.0, 5.0, 0.0);
        EXPECT_FEQ(-0.25, result);
    }

    TEST_CASE(InverseLerp_GivenBlendParameterEqualToFirstValue_Returns0)
    {
        const double result = inverse_lerp(1.0, 5.0, 1.0);
        EXPECT_FEQ(0.0, result);
    }

    TEST_CASE(InverseLerp_GivenBlendParameterEqualToSecondValue_Returns1)
    {
        const double result = inverse_lerp(1.0, 5.0, 5.0);
        EXPECT_FEQ(1.0, result);
    }

    TEST_CASE(InverseLerp_GivenBlendParameterGreaterThanSecondValue_ReturnsCorrectlyExtrapoledValue)
    {
        const double result = inverse_lerp(1.0, 5.0, 6.0);
        EXPECT_FEQ(1.25, result);
    }

    TEST_CASE(InverseLerp_GivenBlendParameterBetweenFirstValueAndSecondValue_ReturnsCorrectlyBlendedValue)
    {
        const double result = inverse_lerp(1.0, 5.0, 3.0);
        EXPECT_FEQ(0.5, result);
    }

    TEST_CASE(InverseLerp_GivenBlendParameterOfDifferentTypeBetweenFirstValueAndSecondValue_ReturnsCorrectlyBlendedValue)
    {
        const double result = inverse_lerp(1.0, 5.0, 3.0f);
        EXPECT_FEQ(0.5, result);
    }

    TEST_CASE(Fit_GivenIntegerInputRangeAndFloatingPointOutputRange)
    {
        const double result = fit<size_t, double>(4, 0, 19, 0.0, 1.0);
        EXPECT_FEQ(4.0 / 19.0, result);
    }

    TEST_CASE(Feq_ScalarScalar_ReturnsTrue)
    {
        EXPECT_TRUE(feq(  0.0,   0.0));
        EXPECT_TRUE(feq( 42.0,  42.0));
        EXPECT_TRUE(feq(-42.0, -42.0));
    }

    TEST_CASE(Feq_ScalarScalar_ReturnsFalse)
    {
        EXPECT_FALSE(feq(  0.0,  42.0));
        EXPECT_FALSE(feq( 42.0,   0.0));
        EXPECT_FALSE(feq( 42.0, -42.0));
        EXPECT_FALSE(feq(-42.0,  42.0));
    }

    TEST_CASE(Feq_ScalarScalar_Overflows)
    {
        EXPECT_FALSE(feq(0.5 * numeric_limits<double>::max(), 0.1));
    }

    TEST_CASE(Feq_ScalarScalar_Underflows)
    {
        EXPECT_FALSE(feq(2.0 * numeric_limits<double>::min(), 10.0));
    }

    TEST_CASE(Fz_Scalar_ReturnsTrue)
    {
        EXPECT_TRUE(fz(+0.0));
        EXPECT_TRUE(fz(-0.0));
    }

    TEST_CASE(Fz_Scalar_ReturnsFalse)
    {
        EXPECT_FALSE(fz( 42.0));
        EXPECT_FALSE(fz(-42.0));
    }
}
