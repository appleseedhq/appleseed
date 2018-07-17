
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
#include "foundation/math/fp.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_FP_Float)
{
    const volatile float Zero = 0.0f;

    TEST_CASE(PosInf_ReturnsPositiveInfinite)
    {
        const float PositiveInfinity = 1.0f / Zero;

        EXPECT_EQ(PositiveInfinity, FP<float>::pos_inf());
    }

    TEST_CASE(NegInf_ReturnsNegativeInfinite)
    {
        const float NegativeInfinity = -1.0f / Zero;

        EXPECT_EQ(NegativeInfinity, FP<float>::neg_inf());
    }

    TEST_CASE(Sign_GivenPositiveZero_ReturnsZero)
    {
        EXPECT_EQ(0, FP<float>::sign(+0.0f));
    }

    TEST_CASE(Sign_GivenNegativeZero_ReturnsOne)
    {
        EXPECT_EQ(1, FP<float>::sign(-0.0f));
    }

    TEST_CASE(Sign_GivenStrictlyPositiveValue_ReturnsZero)
    {
        EXPECT_EQ(0, FP<float>::sign(+42.0f));
    }

    TEST_CASE(Sign_GivenStrictlyNegativeValue_ReturnsOne)
    {
        EXPECT_EQ(1, FP<float>::sign(-42.0f));
    }

    TEST_CASE(IsSNan_GivenSNan_ReturnsTrue)
    {
        EXPECT_TRUE(FP<float>::is_snan(FP<float>::snan()));
    }

    TEST_CASE(IsQNan_GivenQNan_ReturnsTrue)
    {
        EXPECT_TRUE(FP<float>::is_qnan(FP<float>::qnan()));
    }

    TEST_CASE(Shift_GivenZeroAndShiftOfZero_ReturnsZero)
    {
        EXPECT_EQ(+0.0f, shift(+0.0f, 0));
        EXPECT_EQ(-0.0F, shift(-0.0f, 0));
    }

    TEST_CASE(Shift_GivenZeroAndShiftOfPlusOne_ReturnsPosMin)
    {
        EXPECT_EQ(FP<float>::pos_min(), shift(0.0f, +1));
    }

    TEST_CASE(Shift_GivenZeroAndShiftOfMinusOne_ReturnsNegMin)
    {
        EXPECT_EQ(FP<float>::neg_min(), shift(0.0f, -1));
    }

    TEST_CASE(Shift_GivenOneAndShiftOfPlusOne_ReturnsValueSlightlyGreaterThanOne)
    {
        const float result = shift(1.0f, +1);

        EXPECT_GT(1.0f, result);
        EXPECT_FEQ(1.0f, result);
    }

    TEST_CASE(Shift_GivenOneAndShiftOfMinusOne_ReturnsValueSlightlyLesserThanOne)
    {
        const float result = shift(1.0f, -1);

        EXPECT_LT(1.0f, result);
        EXPECT_FEQ(1.0f, result);
    }

    TEST_CASE(Shift_GivenPosMinAndShiftOfMinusOne_ReturnsNegMin)
    {
        EXPECT_EQ(FP<float>::neg_min(), shift(FP<float>::pos_min(), -1));
    }

    TEST_CASE(Shift_GivenPosMinSuccessorAndShiftOfMinusTwo_ReturnsNegMin)
    {
        const float succ_pos_min = shift(FP<float>::pos_min(), 1);

        EXPECT_EQ(FP<float>::neg_min(), shift(succ_pos_min, -2));
    }

    TEST_CASE(Shift_GivenNegMinAndShiftOfPlusOne_ReturnsPosMin)
    {
        EXPECT_EQ(FP<float>::pos_min(), shift(FP<float>::neg_min(), +1));
    }

    TEST_CASE(Shift_GivenNegMinPredecessorAndShiftOfPlusTwo_ReturnsPosMin)
    {
        const float pred_neg_min = shift(FP<float>::neg_min(), -1);

        EXPECT_EQ(FP<float>::pos_min(), shift(pred_neg_min, +2));
    }

    TEST_CASE(Shift_GivenPosInfAndShiftOnePlusOne_ReturnsPosInf)
    {
        EXPECT_EQ(FP<float>::pos_inf(), shift(FP<float>::pos_inf(), +1));
    }

    TEST_CASE(Shift_GivenNegInfAndShiftOneMinusOne_ReturnsNegInf)
    {
        EXPECT_EQ(FP<float>::neg_inf(), shift(FP<float>::neg_inf(), -1));
    }

    TEST_CASE(Sign_GivenNegativeAndPositiveFloats_ReturnsOneAndZero)
    {
        const float Negative = -0.33f;
        const float Positive = +0.33f;
        EXPECT_EQ(1, FP<float>::sign(Negative));
        EXPECT_EQ(0, FP<float>::sign(Positive));
    }

    TEST_CASE(Exponent_GivenFloat_ReturnsValidExponent)
    {
        // https://www.h-schmidt.net/FloatConverter/IEEE754.html used for
        // exponent calculation.
        const float Value = -0.01612f;

        EXPECT_EQ(121, FP<float>::exponent(Value));
    }

    TEST_CASE(Mantissa_GivenFloat_ReturnsValidMantissa)
    {
        // https://www.h-schmidt.net/FloatConverter/IEEE754.html used for
        // mantissa calculation.
        const float Value = 0.1234598821f;

        EXPECT_EQ(8181897, FP<float>::mantissa(Value));
    }

    TEST_CASE(Construct_GivenSignExponentMantissaOfAFloat_ReturnsTheSameNumber)
    {
        const float Initial = -0.15f;

        EXPECT_EQ(
            Initial,
            FP<float>::construct(
                FP<float>::sign(Initial),
                FP<float>::exponent(Initial),
                FP<float>::mantissa(Initial)));
    }
}

TEST_SUITE(Foundation_Math_FP_Double)
{
    const volatile double Zero = 0.0;

    TEST_CASE(PosInf_ReturnsPositiveInfinite)
    {
        const double PositiveInfinity = 1.0 / Zero;

        EXPECT_EQ(PositiveInfinity, FP<double>::pos_inf());
    }

    TEST_CASE(NegInf_ReturnsNegativeInfinite)
    {
        const double NegativeInfinity = -1.0 / Zero;

        EXPECT_EQ(NegativeInfinity, FP<double>::neg_inf());
    }

    TEST_CASE(Sign_GivenPositiveZero_ReturnsZero)
    {
        EXPECT_EQ(0, FP<double>::sign(+0.0));
    }

    TEST_CASE(Sign_GivenNegativeZero_ReturnsOne)
    {
        EXPECT_EQ(1, FP<double>::sign(-0.0));
    }

    TEST_CASE(Sign_GivenStrictlyPositiveValue_ReturnsZero)
    {
        EXPECT_EQ(0, FP<double>::sign(+42.0));
    }

    TEST_CASE(Sign_GivenStrictlyNegativeValue_ReturnsOne)
    {
        EXPECT_EQ(1, FP<double>::sign(-42.0));
    }

    TEST_CASE(IsSNan_GivenSNan_ReturnsTrue)
    {
        EXPECT_TRUE(FP<double>::is_snan(FP<double>::snan()));
    }

    TEST_CASE(IsQNan_GivenQNan_ReturnsTrue)
    {
        EXPECT_TRUE(FP<double>::is_qnan(FP<double>::qnan()));
    }

    TEST_CASE(Sign_GivenNegativeAndPositiveFloats_ReturnsOneAndZero)
    {
        const double Negative = -0.33;
        const double Positive = +0.33;
        EXPECT_EQ(1, FP<double>::sign(Negative));
        EXPECT_EQ(0, FP<double>::sign(Positive));
    }

    TEST_CASE(Exponent_GivenFloat_ReturnsValidExponent)
    {
        const double Value = -0.01612;

        EXPECT_EQ(1017, FP<double>::exponent(Value));
    }

    TEST_CASE(Mantissa_GivenFloat_ReturnsValidMantissa)
    {
        const double Value = 0.1234598821;

        EXPECT_EQ(4392622436961750, FP<double>::mantissa(Value));
    }

    TEST_CASE(Construct_GivenSignExponentMantissaOfAFloat_ReturnsTheSameNumber)
    {
        const double Initial = -0.15;

        EXPECT_EQ(
            Initial,
            FP<double>::construct(
                FP<double>::sign(Initial),
                FP<double>::exponent(Initial),
                FP<double>::mantissa(Initial)));
    }
}
