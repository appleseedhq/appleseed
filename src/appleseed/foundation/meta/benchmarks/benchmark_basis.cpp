
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Aytek Aman, The appleseedhq Organization
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
#include "foundation/math/basis.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Basis)
{
    const size_t N = 100000;

    template <typename T>
    struct BuildFixture
    {
        Vector<T, 3>  m_normal[N];
        Basis3<T>     m_result[N];

        BuildFixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
            {
                m_normal[i] = rand_vector1<Vector<T, 3> >(rng);
                m_normal[i] = normalize(m_normal[i]);
            }
        }

        static Basis3<T> build_original(const Vector<T, 3>& normal)
        {
            const Vector<T, 3> n = normal;

            Vector<T, 3> u, v;

            if (std::abs(n[0]) < std::abs(n[1]))
            {
                if (std::abs(n[0]) < std::abs(n[2]))
                {
                    // n[0] is the smallest component.
                    u[1] = -n[2];
                    u[0] = T(0.0);
                    u[2] = n[1];
                }
                else
                {
                    // n[2] is the smallest component.
                    u[0] = -n[1];
                    u[1] = n[0];
                    u[2] = T(0.0);
                }
            }
            else
            {
                if (std::abs(n[1]) < std::abs(n[2]))
                {
                    // n[1] is the smallest component.
                    u[0] = -n[2];
                    u[1] = T(0.0);
                    u[2] = n[0];
                }
                else
                {
                    // n[2] is the smallest component.
                    u[0] = -n[1];
                    u[1] = n[0];
                    u[2] = T(0.0);
                }
            }

            // u is orthogonal to n, but not unit-length. Normalize it.
            u = normalize(u);

            // Compute v.
            v = cross(u, n);

            return Basis3<T>(n, u, v);
        }
    };

    BENCHMARK_CASE_F(BranchlessBuild_SinglePrecision, BuildFixture<float>)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = Basis3<float>(m_normal[i]);
    }

    BENCHMARK_CASE_F(OriginalBuild_SinglePrecision, BuildFixture<float>)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = build_original(m_normal[i]);
    }

    BENCHMARK_CASE_F(BranchlessBuild_DoublePrecision, BuildFixture<double>)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = Basis3<double>(m_normal[i]);
    }

    BENCHMARK_CASE_F(OriginalBuild_DoublePrecision, BuildFixture<double>)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = build_original(m_normal[i]);
    }
}
