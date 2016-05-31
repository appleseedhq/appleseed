
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(IntegerDivision)
{
    const size_t N = 1000;

    struct Fixture
    {
        size_t  m_num[N];
        size_t  m_den[N];
        double  m_one_over_den[N];
        size_t  m_result;

        Fixture()
          : m_result(0)
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
            {
                m_num[i] = rand_int1(rng, 0, 100);
                m_den[i] = rand_int1(rng, 1, 100);
                m_one_over_den[i] = 1.0 / m_den[i];
            }
        }
    };

    BENCHMARK_CASE_F(IntegerDivision, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result += m_num[i] / m_den[i];
    }

    BENCHMARK_CASE_F(MultiplicationByReciprocal, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result += truncate<size_t>(m_num[i] * m_one_over_den[i]);
    }
}
