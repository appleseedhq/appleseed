
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
#include "foundation/math/fastmath.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
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
    const size_t N = 100;

    struct Fixture
    {
        APPLESEED_SIMD4_ALIGN float m_values[N];
        APPLESEED_SIMD4_ALIGN float m_output[N];

        Fixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
                m_values[i] = rand_float1(rng);
        }
    };

    //
    // Pow2(x).
    //

    BENCHMARK_CASE_F(StdPow2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = pow(2.0f, m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFastPow2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_pow2(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterPow2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_pow2(m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFastPow2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_pow2(&m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFasterPow2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            faster_pow2(&m_output[i]);
    }

    //
    // Log2(x).
    //

    BENCHMARK_CASE_F(ScalarFastLog2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_log2(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterLog2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_log2(m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFastLog2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_log2(&m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFasterLog2, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            faster_log2(&m_output[i]);
    }

    //
    // Pow(x).
    //

    const float Exponent = 2.4f;

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

    BENCHMARK_CASE_F(ScalarFasterPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_pow(m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(VectorFastPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_pow(&m_output[i], Exponent);
    }

    BENCHMARK_CASE_F(VectorFasterPow, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            faster_pow(&m_output[i], Exponent);
    }

    //
    // Log(x).
    //

    BENCHMARK_CASE_F(StdLog, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = log(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFastLog, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_log(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterLog, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_log(m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFastLog, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_log(&m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFasterLog, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            faster_log(&m_output[i]);
    }

    //
    // Exp(x).
    //

    BENCHMARK_CASE_F(StdExp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = exp(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFastExp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_exp(m_output[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterExp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_exp(m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFastExp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            fast_exp(&m_output[i]);
    }

    BENCHMARK_CASE_F(VectorFasterExp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; i += 4)
            faster_exp(&m_output[i]);
    }

    //
    // Rcp(x).
    //

    BENCHMARK_CASE_F(Rcp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = 1.0f / m_output[i];
    }

    BENCHMARK_CASE_F(FastRcp, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_rcp(m_output[i]);
    }

    //
    // Sqrt(x).
    //

    BENCHMARK_CASE_F(Sqrt, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = sqrt(m_output[i]);
    }

    BENCHMARK_CASE_F(FastSqrt, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_sqrt(m_output[i]);
    }

    //
    // RcpSqrt(x).
    //

    BENCHMARK_CASE_F(RcpSqrt, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = 1.0f / sqrt(m_output[i]);
    }

    BENCHMARK_CASE_F(FastRcpSqrt, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_rcp_sqrt(m_output[i]);
    }

    BENCHMARK_CASE_F(FasterRcpSqrt, Fixture)
    {
        memcpy(m_output, m_values, N * sizeof(float));

        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_rcp_sqrt(m_output[i]);
    }
}
