
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
#include "renderer/utility/dynamicspectrum.h"
#include "renderer/utility/iostreamop.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
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
        static const float ExpectedValues[31] =
        {
            0.530693471f, 0.531045496f, 0.531118512f, 0.531190515f,
            0.531234503f, 0.531205475f, 0.531243503f, 0.531251013f,
            0.531204522f, 0.531026006f, 0.530674517f, 0.530518472f,
            0.530655980f, 0.530689478f, 0.530879974f, 0.531168520f,
            0.531268001f, 0.531226516f, 0.531256020f, 0.531213522f,
            0.531239510f, 0.531276524f, 0.531270504f, 0.531207979f,
            0.531177998f, 0.531280994f, 0.530978501f, 0.530169487f,
            0.529729486f, 0.530041993f, 0.530124009f
        };
        const DynamicSpectrum31f Expected(ExpectedValues);

        const DynamicSpectrum31f source(Color3f(0.5f));
        DynamicSpectrum31f dest;

        DynamicSpectrum31f::upgrade(source, dest);

        EXPECT_EQ(Expected, dest);
    }

    TEST_CASE(Upgrade_GivenRGB_MakesSpectrum_InPlace)
    {
        static const float ExpectedValues[31] =
        {
            0.530693471f, 0.531045496f, 0.531118512f, 0.531190515f,
            0.531234503f, 0.531205475f, 0.531243503f, 0.531251013f,
            0.531204522f, 0.531026006f, 0.530674517f, 0.530518472f,
            0.530655980f, 0.530689478f, 0.530879974f, 0.531168520f,
            0.531268001f, 0.531226516f, 0.531256020f, 0.531213522f,
            0.531239510f, 0.531276524f, 0.531270504f, 0.531207979f,
            0.531177998f, 0.531280994f, 0.530978501f, 0.530169487f,
            0.529729486f, 0.530041993f, 0.530124009f
        };
        const DynamicSpectrum31f Expected(ExpectedValues);

        DynamicSpectrum31f s(Color3f(0.5f));

        DynamicSpectrum31f::upgrade(s, s);

        EXPECT_EQ(Expected, s);
    }

    TEST_CASE(Upgrade_GivenSpectrum_CopiesSpectrum)
    {
        const DynamicSpectrum31f source(SpectrumValues);
        DynamicSpectrum31f dest;

        DynamicSpectrum31f::upgrade(source, dest);

        EXPECT_EQ(dest, source);
    }

    TEST_CASE(Downgrade_GivenSpectrum_MakesRGB)
    {
        const DynamicSpectrum31f Expected(Color3f(41.9590912f, 42.0810776f, 41.3171921f));

        const DynamicSpectrum31f source(SpectrumValues);
        DynamicSpectrum31f dest;

        const LightingConditions lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg);
        DynamicSpectrum31f::downgrade(lighting_conditions, source, dest);

        EXPECT_FEQ(Expected, dest);
    }

    TEST_CASE(Downgrade_GivenSpectrum_MakesRGB_InPlace)
    {
        const DynamicSpectrum31f Expected(Color3f(41.9590912f, 42.0810776f, 41.3171921f));

        DynamicSpectrum31f s(SpectrumValues);

        const LightingConditions lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg);
        DynamicSpectrum31f::downgrade(lighting_conditions, s, s);

        EXPECT_FEQ(Expected, s);
    }

    TEST_CASE(Downgrade_GivenRGB_CopiesRGB)
    {
        const DynamicSpectrum31f source(Color3f(0.5f));
        DynamicSpectrum31f dest;

        const LightingConditions lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg);
        DynamicSpectrum31f::downgrade(lighting_conditions, source, dest);

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

    TEST_CASE(Lerp)
    {
        static const float AValues[31] =
        {
            44.3686705734f, 40.3421349896f, 33.453223163f,
            8.31074685536f, 6.94405423347f, 5.64898798988f,
            43.8460619098f, 6.40074755165f, 0.33867778534f,
            0.0715336120232f, 2.12863156003f, 34.5451712652f,
            21.5243428209f, 3.9138078502f, 21.1764794452f,
            22.2335340883f, 30.6847305494f, 48.3241077594f,
            45.3785266829f, 22.5251255181f, 24.8279963757f,
            38.0971410264f, 30.7149063876f, 10.8512568244f,
            12.1817780577f, 15.7592770342f, 19.9367690804f,
            39.4424421483f, 11.6243008551f, 32.2975710956f,
            18.1880153577f
        };

        static const float BValues[31] =
        {
            7.16371136279f, 4.92346021263f, 45.2532024467f,
            24.5796560419f, 10.8153340444f, 29.0175099963f,
            1.4203235082f, 46.6306846903f, 11.4450238478f,
            42.4604343809f, 9.91985098646f, 48.0334593452f,
            33.3467096652f, 15.6591254479f, 29.359242383f,
            20.3241978105f, 10.0449337818f, 38.5267480163f,
            41.0477659538f, 30.5889348172f, 30.0057689985f,
            0.615133030455f, 37.019556993f, 34.9476624082f,
            5.07521228466f, 31.417480479f, 34.7101131884f,
            18.6648099603f, 45.6438172418f, 14.4759442377f,
            40.1987008497f,
        };

        static const float TValues[31] =
        {
            0.0488609444107f,  0.966903688431f,  0.168464414014f,
            0.572471672986f,  0.947986505265f,  0.409792262575f,
            0.254850010845f,  0.739773335505f,  0.746530559414f,
            0.193545818426f,  0.106295610775f,  0.510361083187f,
            0.851752588571f,  0.137322819871f,  0.496098577508f,
            0.521458215829f,  0.685975962407f,  0.566740097315f,
            0.418909956427f,  0.764345316007f,  0.0944871156372f,
            0.950190112224f,  0.491818015485f,  0.594393913149f,
            0.606335001724f,  0.310539095405f,  0.746640551004f,
            0.98488453207f,  0.558276210639f,  0.050465128121f,
            0.954259619265f
        };

        const DynamicSpectrum31f a(AValues);
        const DynamicSpectrum31f b(BValues);
        const DynamicSpectrum31f t(TValues);
        const DynamicSpectrum31f result = lerp(a, b, t);

        for (size_t i = 0, e = result.size(); i < e; ++i)
            EXPECT_FEQ(lerp(a[i], b[i], t[i]), result[i]);
    }

    TEST_CASE(MinValue_RGB)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            DynamicSpectrum31f s;
            s.resize(3);

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 2.0f;
            s[i] = 1.0f;

            EXPECT_EQ(1.0f, min_value(s));
        }
    }

    TEST_CASE(MinValue_Spectrum)
    {
        for (size_t i = 0; i < 31; ++i)
        {
            DynamicSpectrum31f s;
            s.resize(31);

            // Don't use set() to avoid altering the padding value (the 32th value in DynamicSpectrum<>::m_samples).
            for (size_t j = 0; j < 31; ++j)
                s[j] = j == i ? 1.0f : 2.0f;

            EXPECT_EQ(1.0f, min_value(s));
        }
    }

    TEST_CASE(MaxValue_RGB)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            DynamicSpectrum31f s;
            s.resize(3);

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 1.0f;
            s[i] = 2.0f;

            EXPECT_EQ(2.0f, max_value(s));
        }
    }

    TEST_CASE(MaxValue_Spectrum)
    {
        for (size_t i = 0; i < 31; ++i)
        {
            DynamicSpectrum31f s;
            s.resize(31);

            // Don't use set() to avoid altering the padding value (the 32th value in DynamicSpectrum<>::m_samples).
            for (size_t j = 0; j < 31; ++j)
                s[j] = j == i ? 2.0f : 1.0f;

            EXPECT_EQ(2.0f, max_value(s));
        }
    }

    TEST_CASE(Sqrt)
    {
        static const float Values[31] =
        {
            33.7794576606f, 34.6017115499f, 10.8598925557f,
            10.1564868071f, 2.12352584175f, 17.3759605036f,
            6.23873436098f, 22.077745133f, 22.2779760462f,
            36.9120118385f, 7.21339038295f, 10.6783551356f,
            14.935931608f, 39.2048955192f, 14.1621055891f,
            4.52243405258f, 30.0648763532f, 32.401711383f,
            15.9086715908f, 0.975102075444f, 8.33461383584f,
            41.0116977052f, 7.06685022531f, 27.8164731673f,
            12.9754651075f, 39.8339872284f, 5.99074310976f,
            20.1765086006f, 0.619432298957f, 8.978846717f,
            25.1470816934f
        };

        const DynamicSpectrum31f x(Values);
        const DynamicSpectrum31f result = sqrt(x);

        for (size_t i = 0, e = x.size(); i < e; ++i)
            EXPECT_FEQ(sqrt(Values[i]), result[i]);
    }
}
