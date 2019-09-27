
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// appleseed.renderer headers.
#include "renderer/utility/dynamicspectrum.h"
#include "renderer/utility/iostreamop.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_DynamicSpectrum31f)
{
    struct RGBFixture
    {
        const DynamicSpectrum31f::Mode m_old_mode;

        RGBFixture()
          : m_old_mode(DynamicSpectrum31f::set_mode(DynamicSpectrum31f::RGB))
        {
        }

        ~RGBFixture()
        {
            DynamicSpectrum31f::set_mode(m_old_mode);
        }
    };

    struct SpectralFixture
    {
        const DynamicSpectrum31f::Mode m_old_mode;

        SpectralFixture()
          : m_old_mode(DynamicSpectrum31f::set_mode(DynamicSpectrum31f::Spectral))
        {
        }

        ~SpectralFixture()
        {
            DynamicSpectrum31f::set_mode(m_old_mode);
        }
    };

    TEST_CASE_F(Lerp_Spectral, SpectralFixture)
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
            0.0488609444107f, 0.966903688431f, 0.168464414014f,
            0.572471672986f, 0.947986505265f, 0.409792262575f,
            0.254850010845f, 0.739773335505f, 0.746530559414f,
            0.193545818426f, 0.106295610775f, 0.510361083187f,
            0.851752588571f, 0.137322819871f, 0.496098577508f,
            0.521458215829f, 0.685975962407f, 0.566740097315f,
            0.418909956427f, 0.764345316007f, 0.0944871156372f,
            0.950190112224f, 0.491818015485f, 0.594393913149f,
            0.606335001724f, 0.310539095405f, 0.746640551004f,
            0.98488453207f, 0.558276210639f, 0.050465128121f,
            0.954259619265f
        };

        const auto a(DynamicSpectrum31f::from_array(AValues));
        const auto b(DynamicSpectrum31f::from_array(BValues));
        const auto t(DynamicSpectrum31f::from_array(TValues));
        const auto result = lerp(a, b, t);

        for (size_t i = 0, e = result.size(); i < e; ++i)
            EXPECT_FEQ(lerp(a[i], b[i], t[i]), result[i]);
    }

    TEST_CASE_F(MinValue_RGB, RGBFixture)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            DynamicSpectrum31f s;

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 2.0f;
            s[i] = 1.0f;

            EXPECT_EQ(1.0f, min_value(s));
        }
    }

    TEST_CASE_F(MinValue_Spectral, SpectralFixture)
    {
        for (size_t i = 0; i < 31; ++i)
        {
            DynamicSpectrum31f s;

            // Don't use set() to avoid altering the padding value (the 32th value in DynamicSpectrum<>::m_samples).
            for (size_t j = 0; j < 31; ++j)
                s[j] = j == i ? 1.0f : 2.0f;

            EXPECT_EQ(1.0f, min_value(s));
        }
    }

    TEST_CASE_F(MaxValue_RGB, RGBFixture)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            DynamicSpectrum31f s;

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 1.0f;
            s[i] = 2.0f;

            EXPECT_EQ(2.0f, max_value(s));
        }
    }

    TEST_CASE_F(MaxValue_Spectral, SpectralFixture)
    {
        for (size_t i = 0; i < 31; ++i)
        {
            DynamicSpectrum31f s;

            // Don't use set() to avoid altering the padding value (the 32th value in DynamicSpectrum<>::m_samples).
            for (size_t j = 0; j < 31; ++j)
                s[j] = j == i ? 2.0f : 1.0f;

            EXPECT_EQ(2.0f, max_value(s));
        }
    }

    TEST_CASE_F(Sqrt_Spectral, SpectralFixture)
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

        const auto x(DynamicSpectrum31f::from_array(Values));
        const auto result = sqrt(x);

        for (size_t i = 0, e = x.size(); i < e; ++i)
            EXPECT_FEQ(std::sqrt(Values[i]), result[i]);
    }
}
