
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/utility/iostreamop.h"
#include "renderer/utility/rgbspectrum.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_RGBSpectrumf)
{
    TEST_CASE(Lerp)
    {
        static const float AValues[3] =
        {
            44.3686705734f, 40.3421349896f, 33.453223163f
        };

        static const float BValues[31] =
        {
            7.16371136279f, 4.92346021263f, 45.2532024467f
        };

        static const float TValues[31] =
        {
            0.0488609444107f, 0.966903688431f, 0.168464414014f
        };

        const auto a(RGBSpectrumf::from_array(AValues));
        const auto b(RGBSpectrumf::from_array(BValues));
        const auto t(RGBSpectrumf::from_array(TValues));
        const auto result = lerp(a, b, t);

        for (size_t i = 0, e = result.size(); i < e; ++i)
            EXPECT_FEQ(lerp(a[i], b[i], t[i]), result[i]);
    }

    TEST_CASE(MinValue)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            RGBSpectrumf s;

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 2.0f;
            s[i] = 1.0f;

            EXPECT_EQ(1.0f, min_value(s));
        }
    }

    TEST_CASE(MaxValue)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            RGBSpectrumf s;

            // Don't use set() to avoid altering the fourth value.
            s[0] = s[1] = s[2] = 1.0f;
            s[i] = 2.0f;

            EXPECT_EQ(2.0f, max_value(s));
        }
    }

    TEST_CASE(Sqrt)
    {
        static const float Values[3] =
        {
            33.7794576606f, 34.6017115499f, 10.8598925557f
        };

        const auto x(RGBSpectrumf::from_array(Values));
        const auto result = sqrt(x);

        for (size_t i = 0, e = x.size(); i < e; ++i)
            EXPECT_FEQ(std::sqrt(Values[i]), result[i]);
    }
}
