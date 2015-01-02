
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// appleseed.renderer headers.
#include "renderer/utility/dynamicspectrum.h"
#include "renderer/utility/iostreamop.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_DynamicSpectrum31f)
{
    static const float SpectrumValues[31] =
    {
        42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
        42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
        42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f,
        42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f
    };

    TEST_CASE(DefaultConstructor_CreatesRGB)
    {
        const DynamicSpectrum31f s;

        EXPECT_EQ(3, s.size());
    }

    TEST_CASE(ConstructorTakingAnArrayOfValues_CreatesSpectrum)
    {
        const DynamicSpectrum31f s(SpectrumValues);

        EXPECT_EQ(31, s.size());
    }

    TEST_CASE(ConstructorTakingSingleValue_CreatesRGB)
    {
        const DynamicSpectrum31f s(42.0f);

        EXPECT_EQ(3, s.size());
    }

    TEST_CASE(ConstructorTakingColor_CreatesRGB)
    {
        const DynamicSpectrum31f s(Color3f(1.0f, 2.0f, 3.0f));

        EXPECT_EQ(3, s.size());
        EXPECT_EQ(1.0f, s[0]);
        EXPECT_EQ(2.0f, s[1]);
        EXPECT_EQ(3.0f, s[2]);
    }

    TEST_CASE(Resize)
    {
        DynamicSpectrum31f s(Color3f(0.0f));

        s.resize(31);

        EXPECT_EQ(31, s.size());
    }

    TEST_CASE(Set_GivenRGB_PreservesRGB)
    {
        DynamicSpectrum31f s(Color3f(0.0f));

        s.set(42.0f);

        EXPECT_EQ(3, s.size());
    }

    TEST_CASE(Set_GivenSpectrum_PreservesSpectrum)
    {
        DynamicSpectrum31f s(SpectrumValues);

        s.set(42.0f);

        EXPECT_EQ(31, s.size());
    }

    TEST_CASE(Set_GivenRGB_SetsValues)
    {
        DynamicSpectrum31f s(Color3f(42.0f));

        s.set(36.0f);

        EXPECT_EQ(36.0f, s[0]);
        EXPECT_EQ(36.0f, s[1]);
        EXPECT_EQ(36.0f, s[2]);
    }

    TEST_CASE(Set_GivenSpectrum_SetsValues)
    {
        DynamicSpectrum31f s(SpectrumValues);

        s.set(36.0f);

        for (size_t i = 0; i < 31; ++i)
            EXPECT_EQ(36.0f, s[i]);
    }

    TEST_CASE(Upgrade_GivenRGB_MakesSpectrum)
    {
        const DynamicSpectrum31f source(Color3f(0.5f));
        DynamicSpectrum31f dest;

        DynamicSpectrum31f::upgrade(source, dest);

        EXPECT_EQ(31, dest.size());
    }

    TEST_CASE(Upgrade_GivenSpectrum_CopiesSpectrum)
    {
        const DynamicSpectrum31f source(SpectrumValues);
        DynamicSpectrum31f dest;

        DynamicSpectrum31f::upgrade(source, dest);

        EXPECT_EQ(dest, source);
    }

    TEST_CASE(OperatorNotEqual_RGBNotEqualSpectrum_ReturnsTrue)
    {
        const DynamicSpectrum31f lhs(Color3f(42.0f));
        const DynamicSpectrum31f rhs(SpectrumValues);

        EXPECT_TRUE(lhs != rhs);
    }

    TEST_CASE(OperatorPlus_RGBPlusRGB)
    {
        const DynamicSpectrum31f lhs(Color3f(0.5f));
        const DynamicSpectrum31f rhs(Color3f(0.2f));

        const DynamicSpectrum31f result = lhs + rhs;

        EXPECT_EQ(3, result.size());
        EXPECT_FEQ(0.7f, result[0]);
        EXPECT_FEQ(0.7f, result[1]);
        EXPECT_FEQ(0.7f, result[2]);
    }

    TEST_CASE(OperatorPlus_RGBPlusSpectrum)
    {
        const DynamicSpectrum31f lhs(Color3f(0.5f));
        const DynamicSpectrum31f rhs(SpectrumValues);

        const DynamicSpectrum31f result = lhs + rhs;

        EXPECT_EQ(31, result.size());
    }

    TEST_CASE(OperatorPlus_SpectrumPlusRGB)
    {
        const DynamicSpectrum31f lhs(SpectrumValues);
        const DynamicSpectrum31f rhs(Color3f(0.5f));

        const DynamicSpectrum31f result = lhs + rhs;

        EXPECT_EQ(31, result.size());
    }
}
