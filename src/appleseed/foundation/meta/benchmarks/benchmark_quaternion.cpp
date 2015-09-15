
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Quaternion)
{
    struct Fixture
    {
        static const size_t N = 16;

        Quaterniond m_q1[N];
        Quaterniond m_q2[N];
        double      m_t[N];

        Quaterniond m_result[N];

        Fixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
            {
                const Vector3d v1 = normalize(rand_vector1<Vector3d>(rng));
                const Vector3d v2 = normalize(rand_vector1<Vector3d>(rng));
                const double a1 = rand_double1(rng) * TwoPi;
                const double a2 = rand_double1(rng) * TwoPi;
                m_q1[i] = Quaterniond::rotation(v1, a1);
                m_q2[i] = Quaterniond::rotation(v2, a2);
                m_t[i] = rand_double1(rng);
            }
        }
    };

    BENCHMARK_CASE_F(Slerp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = slerp(m_q1[i], m_q2[i], m_t[i]);
    }

    BENCHMARK_CASE_F(FastSlerp, Fixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = fast_slerp(m_q1[i], m_q2[i], m_t[i]);
    }
}
