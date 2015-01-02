
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.foundation headers.
#include "foundation/math/filter.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Filter_BoxFilter2)
{
    struct Fixture
    {
        BoxFilter2<float>       m_filter;
        float                   m_dummy;

        Fixture()
          : m_filter(2.0f, 2.0f)
        {
        }
    };

    BENCHMARK_CASE_F(Evaluate, Fixture)
    {
        m_dummy = 0.0f;

        for (int y = -2; y <= +2; ++y)
        {
            for (int x = -2; x <= +2; ++x)
            {
                m_dummy += m_filter.evaluate(static_cast<float>(x), static_cast<float>(y));
            }
        }
    }
}

BENCHMARK_SUITE(Foundation_Math_Filter_GaussianFilter2)
{
    struct Fixture
    {
        GaussianFilter2<float>  m_filter;
        float                   m_dummy;

        Fixture()
          : m_filter(2.0f, 2.0f, 8.0f)
        {
        }
    };

    BENCHMARK_CASE_F(Evaluate, Fixture)
    {
        m_dummy = 0.0f;

        for (int y = -2; y <= +2; ++y)
        {
            for (int x = -2; x <= +2; ++x)
            {
                m_dummy += m_filter.evaluate(static_cast<float>(x), static_cast<float>(y));
            }
        }
    }
}

BENCHMARK_SUITE(Foundation_Math_Filter_MitchellFilter2)
{
    struct Fixture
    {
        MitchellFilter2<float>  m_filter;
        float                   m_dummy;

        Fixture()
          : m_filter(2.0f, 2.0f, 1.0f / 3, 1.0f / 3)
        {
        }
    };

    BENCHMARK_CASE_F(Evaluate, Fixture)
    {
        m_dummy = 0.0f;

        for (int y = -2; y <= +2; ++y)
        {
            for (int x = -2; x <= +2; ++x)
            {
                m_dummy += m_filter.evaluate(static_cast<float>(x), static_cast<float>(y));
            }
        }
    }
}
