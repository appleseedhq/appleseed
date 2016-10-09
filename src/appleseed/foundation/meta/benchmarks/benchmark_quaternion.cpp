
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/matrix.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Quaternion)
{
    template <typename RNG>
    Quaterniond make_random_unit_quat(RNG& rng)
    {
        const Vector3d axis = sample_sphere_uniform(rand_vector2<Vector2d>(rng));
        const double angle = rand_double2(rng) * TwoPi<double>();
        return Quaterniond::make_rotation(axis, angle);
    }

    struct SlerpFixture
    {
        static const size_t N = 16;

        Quaterniond m_q1[N];
        Quaterniond m_q2[N];
        double      m_t[N];

        Quaterniond m_result[N];

        SlerpFixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
            {
                m_q1[i] = make_random_unit_quat(rng);
                m_q2[i] = make_random_unit_quat(rng);
                m_t[i] = rand_double1(rng);
            }
        }
    };

    BENCHMARK_CASE_F(Slerp, SlerpFixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = slerp(m_q1[i], m_q2[i], m_t[i]);
    }

    BENCHMARK_CASE_F(FastSlerp, SlerpFixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_result[i] = fast_slerp(m_q1[i], m_q2[i], m_t[i]);
    }

    struct RotateFixture
    {
        static const size_t N = 16;

        Quaterniond m_q[N];
        Matrix3d    m_m[N];
        Vector3d    m_v;

        RotateFixture()
        {
            MersenneTwister rng;

            for (size_t i = 0; i < N; ++i)
            {
                m_q[i] = make_random_unit_quat(rng);
                m_m[i] = Matrix3d::make_rotation(m_q[i]);
            }

            m_v = rand_vector2<Vector3d>(rng);
        }
    };

    BENCHMARK_CASE_F(Rotate, RotateFixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_v = rotate(m_q[i], m_v);
    }

    BENCHMARK_CASE_F(RotateWith3x3Matrix, RotateFixture)
    {
        for (size_t i = 0; i < N; ++i)
            m_v = m_m[i] * m_v;
    }
}
