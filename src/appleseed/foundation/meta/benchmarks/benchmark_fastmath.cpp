
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
    // 2^x
    //

    BENCHMARK_CASE_F(StdPow2, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::pow(2.0f, m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFastPow2, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_pow2(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterPow2, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_pow2(m_values[i]);
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
    // log2(x)
    //

    BENCHMARK_CASE_F(ScalarFastLog2, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_log2(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterLog2, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_log2(m_values[i]);
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
    // x^e
    //

    const float Exponent = 2.4f;

    BENCHMARK_CASE_F(StdPow, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::pow(m_values[i], Exponent);
    }

    BENCHMARK_CASE_F(ScalarFastPow, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_pow(m_values[i], Exponent);
    }

    BENCHMARK_CASE_F(ScalarFasterPow, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_pow(m_values[i], Exponent);
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
    // log(x)
    //

    BENCHMARK_CASE_F(StdLog, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::log(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFastLog, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_log(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterLog, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_log(m_values[i]);
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
    // exp(x)
    //

    BENCHMARK_CASE_F(StdExp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::exp(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFastExp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_exp(m_values[i]);
    }

    BENCHMARK_CASE_F(ScalarFasterExp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_exp(m_values[i]);
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
    // acos(x)
    //

    BENCHMARK_CASE_F(Acos, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::acos(m_values[i]);
    }

    BENCHMARK_CASE_F(FastAcos, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_acos(m_values[i]);
    }

    //
    // 1/x
    //

    BENCHMARK_CASE_F(Rcp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = 1.0f / m_values[i];
    }

    BENCHMARK_CASE_F(FastRcp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_rcp(m_values[i]);
    }

    //
    // sqrt(x)
    //

    BENCHMARK_CASE_F(Sqrt, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = std::sqrt(m_values[i]);
    }

    BENCHMARK_CASE_F(FastSqrt, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_sqrt(m_values[i]);
    }

    //
    // 1/sqrt(x)
    //

    BENCHMARK_CASE_F(RcpSqrt, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = 1.0f / std::sqrt(m_values[i]);
    }

    BENCHMARK_CASE_F(FastRcpSqrt, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = fast_rcp_sqrt(m_values[i]);
    }

    BENCHMARK_CASE_F(FasterRcpSqrt, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_output[i] = faster_rcp_sqrt(m_values[i]);
    }
}
