
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Image_ColorSpace)
{
    struct LinearRGBTosRGBFixture
    {
        Color3f m_input;
        Color3f m_output;

        LinearRGBTosRGBFixture()
          : m_input(0.5f, 0.7f, 0.2f)
        {
        }
    };

    BENCHMARK_CASE_F(LinearRGBTosRGBConversion, LinearRGBTosRGBFixture)
    {
        m_output = linear_rgb_to_srgb(m_input);
    }

    BENCHMARK_CASE_F(FastLinearRGBTosRGBConversion, LinearRGBTosRGBFixture)
    {
        m_output = fast_linear_rgb_to_srgb(m_input);
    }

    struct SpectrumToCIEXYZFixture
    {
        const LightingConditions    m_lighting_conditions;
        RegularSpectrum31f          m_input;
        Color3f                     m_output;

        SpectrumToCIEXYZFixture()
          : m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
            MersenneTwister rng;

            for (size_t w = 0; w < RegularSpectrum31f::Samples; ++w)
                m_input[w] = rand_float1(rng);
        }
    };

    BENCHMARK_CASE_F(SpectrumToCIEXYZ, SpectrumToCIEXYZFixture)
    {
        m_output = spectrum_to_ciexyz<float>(m_lighting_conditions, m_input);
    }

    struct LinearRGBToSpectrumFixture
    {
        Color3f                     m_input;
        RegularSpectrum31f          m_output;

        LinearRGBToSpectrumFixture()
          : m_input(0.5f, 0.7f, 0.2f)
        {
        }
    };

    BENCHMARK_CASE_F(LinearRGBIlluminanceToSpectrum, LinearRGBToSpectrumFixture)
    {
        linear_rgb_illuminance_to_spectrum(m_input, m_output);
    }
}
