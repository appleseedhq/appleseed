
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Shading_ShadingResult)
{
    struct Fixture
    {
        const LightingConditions m_lighting_conditions;

        Fixture()
          : m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
        }
    };

    TEST_CASE_F(TransformToLinearRGB_GivenLinearRGB_DoesNothing, Fixture)
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

    TEST_CASE_F(TransformToLinearRGB_FromSRGB, Fixture)
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

    TEST_CASE_F(TransformToLinearRGB_FromCIEXYZ, Fixture)
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

    TEST_CASE_F(TransformToLinearRGB_FromSpectrum, Fixture)
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
