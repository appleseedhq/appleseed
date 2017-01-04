
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
#include "renderer/kernel/shading/shadingfragment.h"
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
    struct IsValidLinearRGBFixture
    {
        ShadingResult m_result;

        IsValidLinearRGBFixture()
        {
            // Initialize m_result to linear RGB transparent black.
            m_result.m_color_space = ColorSpaceLinearRGB;
            m_result.m_main.m_color[0] = 0.0f;
            m_result.m_main.m_color[1] = 0.0f;
            m_result.m_main.m_color[2] = 0.0f;
            m_result.m_main.m_alpha[0] = 0.0f;
        }
    };

    TEST_CASE_F(IsValidLinearRGB_PositiveZero_ReturnsTrue, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::pos_zero();

        EXPECT_TRUE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_NegativeZero_ReturnsTrue, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::neg_zero();

        EXPECT_TRUE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_PositiveMin_ReturnsTrue, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::pos_min();

        EXPECT_TRUE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_NegativeMin_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::neg_min();

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_PositiveOne_ReturnsTrue, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = +1.0f;

        EXPECT_TRUE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_NegativeOne_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = -1.0f;

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_PositiveInfinity_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::pos_inf();

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_NegativeInfinity_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::neg_inf();

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_QNaN_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::qnan();

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    TEST_CASE_F(IsValidLinearRGB_SNaN_ReturnsFalse, IsValidLinearRGBFixture)
    {
        m_result.m_main.m_color[0] = FP<float>::snan();

        EXPECT_FALSE(m_result.is_valid_linear_rgb());
    }

    struct TransformToLinearRGBFixture
    {
        const LightingConditions m_lighting_conditions;

        TransformToLinearRGBFixture()
          : m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
        }
    };

    TEST_CASE_F(TransformToLinearRGB_GivenLinearRGB_DoesNothing, TransformToLinearRGBFixture)
    {
        ShadingResult result;
        result.m_color_space = ColorSpaceLinearRGB;
        result.m_main.m_color[0] = 0.0f;
        result.m_main.m_color[1] = 0.0f;
        result.m_main.m_color[2] = 0.0f;

        result.transform_to_linear_rgb(m_lighting_conditions);

        EXPECT_EQ(ColorSpaceLinearRGB, result.m_color_space);
        EXPECT_EQ(0.0f, result.m_main.m_color[0]);
        EXPECT_EQ(0.0f, result.m_main.m_color[1]);
        EXPECT_EQ(0.0f, result.m_main.m_color[2]);
    }

    TEST_CASE_F(TransformToLinearRGB_FromSRGB, TransformToLinearRGBFixture)
    {
        ShadingResult result;
        result.m_color_space = ColorSpaceSRGB;
        result.m_main.m_color[0] = 0.0f;
        result.m_main.m_color[1] = 0.0f;
        result.m_main.m_color[2] = 0.0f;

        result.transform_to_linear_rgb(m_lighting_conditions);

        EXPECT_EQ(ColorSpaceLinearRGB, result.m_color_space);
        EXPECT_EQ(0.0f, result.m_main.m_color[0]);
        EXPECT_EQ(0.0f, result.m_main.m_color[1]);
        EXPECT_EQ(0.0f, result.m_main.m_color[2]);
    }

    TEST_CASE_F(TransformToLinearRGB_FromCIEXYZ, TransformToLinearRGBFixture)
    {
        ShadingResult result;
        result.m_color_space = ColorSpaceCIEXYZ;
        result.m_main.m_color[0] = 0.0f;
        result.m_main.m_color[1] = 0.0f;
        result.m_main.m_color[2] = 0.0f;

        result.transform_to_linear_rgb(m_lighting_conditions);

        EXPECT_EQ(ColorSpaceLinearRGB, result.m_color_space);
        EXPECT_EQ(0.0f, result.m_main.m_color[0]);
        EXPECT_EQ(0.0f, result.m_main.m_color[1]);
        EXPECT_EQ(0.0f, result.m_main.m_color[2]);
    }

    TEST_CASE_F(TransformToLinearRGB_FromSpectrum, TransformToLinearRGBFixture)
    {
        ShadingResult result;
        result.m_color_space = ColorSpaceSpectral;
        result.m_main.m_color.set(0.0f);

        result.transform_to_linear_rgb(m_lighting_conditions);

        EXPECT_EQ(ColorSpaceLinearRGB, result.m_color_space);
        EXPECT_EQ(0.0f, result.m_main.m_color[0]);
        EXPECT_EQ(0.0f, result.m_main.m_color[1]);
        EXPECT_EQ(0.0f, result.m_main.m_color[2]);
    }

    TEST_CASE(CompositeOverLinearRGB_TenPercentOpaqueWhiteOverFullyTransparentBlack)
    {
        ShadingResult a;
        a.m_color_space = ColorSpaceLinearRGB;
        a.m_main.m_color.set(0.1f);
        a.m_main.m_alpha.set(0.1f);

        ShadingResult b;
        b.m_color_space = ColorSpaceLinearRGB;
        b.m_main.m_color.set(0.0f);
        b.m_main.m_alpha.set(0.0f);

        a.composite_over_linear_rgb(b);

        EXPECT_FEQ(0.1f, a.m_main.m_color[0]);
        EXPECT_FEQ(0.1f, a.m_main.m_alpha[0]);
    }

    TEST_CASE(CompositeOverLinearRGB_FullyTransparentBlackOverTenPercentOpaqueWhite)
    {
        ShadingResult a;
        a.m_color_space = ColorSpaceLinearRGB;
        a.m_main.m_color.set(0.0f);
        a.m_main.m_alpha.set(0.0f);

        ShadingResult b;
        b.m_color_space = ColorSpaceLinearRGB;
        b.m_main.m_color.set(0.1f);
        b.m_main.m_alpha.set(0.1f);

        a.composite_over_linear_rgb(b);

        EXPECT_FEQ(0.1f, a.m_main.m_color[0]);
        EXPECT_FEQ(0.1f, a.m_main.m_alpha[0]);
    }
}
