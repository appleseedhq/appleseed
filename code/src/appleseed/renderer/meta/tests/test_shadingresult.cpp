
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

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/fp.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Shading_ShadingResult)
{
    struct IsValidFixture
    {
        ShadingResult m_result;

        IsValidFixture()
        {
            // Initialize m_result to linear RGB transparent black.
            m_result.m_main[0] = 0.0f;
            m_result.m_main[1] = 0.0f;
            m_result.m_main[2] = 0.0f;
            m_result.m_main[3] = 0.0f;
        }
    };

    TEST_CASE_F(IsValid_PositiveZero_ReturnsTrue, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::pos_zero();

        EXPECT_TRUE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_NegativeZero_ReturnsTrue, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::neg_zero();

        EXPECT_TRUE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_PositiveMin_ReturnsTrue, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::pos_min();

        EXPECT_TRUE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_NegativeMin_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::neg_min();

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_PositiveOne_ReturnsTrue, IsValidFixture)
    {
        m_result.m_main[0] = +1.0f;

        EXPECT_TRUE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_NegativeOne_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = -1.0f;

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_PositiveInfinity_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::pos_inf();

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_NegativeInfinity_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::neg_inf();

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_QNaN_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::qnan();

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE_F(IsValid_SNaN_ReturnsFalse, IsValidFixture)
    {
        m_result.m_main[0] = FP<float>::snan();

        EXPECT_FALSE(m_result.is_valid());
    }

    TEST_CASE(CompositeOver_TenPercentOpaqueWhiteOverFullyTransparentBlack)
    {
        ShadingResult a;
        a.m_main.set(0.1f);

        ShadingResult b;
        b.m_main.set(0.0f);

        a.composite_over(b);

        EXPECT_FEQ(0.1f, a.m_main[0]);
    }

    TEST_CASE(CompositeOver_FullyTransparentBlackOverTenPercentOpaqueWhite)
    {
        ShadingResult a;
        a.m_main.set(0.0f);

        ShadingResult b;
        b.m_main.set(0.1f);

        a.composite_over(b);

        EXPECT_FEQ(0.1f, a.m_main[0]);
    }
}
