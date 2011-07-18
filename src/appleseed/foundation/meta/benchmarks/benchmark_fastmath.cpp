
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/math/fastmath.h"
#include "foundation/math/rng.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstring>

using namespace foundation;
using namespace std;

BENCHMARK_SUITE(Foundation_Math_FastMath)
{
    const float Exponent = 2.4f;
    const size_t N = 100;

    struct Fixture
    {
        ALIGN_SSE_VARIABLE float m_values[N];
        ALIGN_SSE_VARIABLE float m_output[N];

        Fixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
                m_values[i] = rand_float1(rng);
        }
    };

    BENCHMARK_CASE_F(StdPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = pow(m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(ScalarFastPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_pow(m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(VectorFastPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_pow(&m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(ScalarFastPowRefined, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_pow_refined(m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(VectorFastPowRefined, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_pow_refined(&m_output[i], Exponent);
    }
}
