
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/utility/benchmark.h"

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(Renderer_Utility_DynamicSpectrum31f)
{
    template <DynamicSpectrum31f::Mode Mode>
    struct Fixture
    {
        const DynamicSpectrum31f::Mode  m_old_mode;
        DynamicSpectrum31f              m_black;
        DynamicSpectrum31f              m_white;
        bool                            m_is_zero_result;
        float                           m_max_value_result;

        Fixture()
          : m_old_mode(DynamicSpectrum31f::set_mode(Mode))
          , m_is_zero_result(true)
          , m_max_value_result(0.0f)
        {
            // Must be initialized after setting the dynamic spectrum mode.
            m_black = DynamicSpectrum31f(0.0f);
            m_white = DynamicSpectrum31f(1.0f);
        }

        ~Fixture()
        {
            DynamicSpectrum31f::set_mode(m_old_mode);
        }
    };

    BENCHMARK_CASE_F(IsZero_Black_RGB, Fixture<DynamicSpectrum31f::RGB>)
    {
        m_is_zero_result ^= is_zero(m_black);
    }

    BENCHMARK_CASE_F(MaxValue_Black_RGB, Fixture<DynamicSpectrum31f::RGB>)
    {
        m_max_value_result += max_value(m_black);
    }

    BENCHMARK_CASE_F(IsZero_White_RGB, Fixture<DynamicSpectrum31f::RGB>)
    {
        m_is_zero_result ^= is_zero(m_white);
    }

    BENCHMARK_CASE_F(MaxValue_White_RGB, Fixture<DynamicSpectrum31f::RGB>)
    {
        m_max_value_result += max_value(m_white);
    }

    BENCHMARK_CASE_F(IsZero_Black_Spectral, Fixture<DynamicSpectrum31f::Spectral>)
    {
        m_is_zero_result ^= is_zero(m_black);
    }

    BENCHMARK_CASE_F(MaxValue_Black_Spectral, Fixture<DynamicSpectrum31f::Spectral>)
    {
        m_max_value_result += max_value(m_black);
    }

    BENCHMARK_CASE_F(IsZero_White_Spectral, Fixture<DynamicSpectrum31f::Spectral>)
    {
        m_is_zero_result ^= is_zero(m_white);
    }

    BENCHMARK_CASE_F(MaxValue_White_Spectral, Fixture<DynamicSpectrum31f::Spectral>)
    {
        m_max_value_result += max_value(m_white);
    }
}
