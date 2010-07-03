
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/aabb.h"
#include "foundation/math/intersection.h"
#include "foundation/math/ray.h"
#include "foundation/math/rng.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>
#include <limits>

using namespace foundation;
using namespace std;

namespace
{
    template <typename T>
    struct FixtureBase
    {
        typedef Vector<T, 3> VectorType;
        typedef Ray<T, 3> RayType;
        typedef RayInfo<T, 3> RayInfoType;

        template <size_t N>
        static Vector<T, N> get_random_vector(
            MersenneTwister&    rng,
            const T             min,
            const T             max)
        {
            Vector<T, N> v;

            for (size_t i = 0; i < N; ++i)
                v[i] = static_cast<T>(rand_double1(rng, min, max));

            return v;
        }

        static void get_random_ray(
            MersenneTwister&    rng,
            const T             radius,
            RayType&            ray,
            RayInfoType&        ray_info)
        {
            Vector<T, 2> s;
            s[0] = static_cast<T>(rand_double2(rng));
            s[1] = static_cast<T>(rand_double2(rng));

            const VectorType v = sample_sphere_uniform(s);

            ray = RayType(v * radius, -v);
            ray_info = RayInfoType(ray);
        }

        static void get_random_ray(
            MersenneTwister&    rng,
            const T             radius,
            RayType&            ray)
        {
            RayInfoType ray_info_unused;
            get_random_ray(rng, radius, ray, ray_info_unused);
        }
    };
}

FOUNDATION_BENCHMARK_SUITE(Foundation_Math_Intersection_RayAABB)
{
    template <typename T>
    struct Fixture
      : public FixtureBase<T>
    {
        typedef AABB<T, 3> AABBType;

        static const size_t RayCount = 1000;

        AABBType        m_aabb;
        RayType         m_ray[RayCount];
        RayInfoType     m_ray_info[RayCount];

        bool            m_hit;

        Fixture()
          : m_aabb(VectorType(-1.0), VectorType(1.0))
          , m_hit(false)
        {
            MersenneTwister rng;

            for (size_t i = 0; i < RayCount; ++i)
                get_random_ray(rng, T(10.0), m_ray[i], m_ray_info[i]);
        }
    };

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision, Fixture<float>)
    {
        for (size_t i = 0; i < RayCount; ++i)
            m_hit ^= intersect(m_ray[i], m_ray_info[i], m_aabb);
    }

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision, Fixture<double>)
    {
        for (size_t i = 0; i < RayCount; ++i)
            m_hit ^= intersect(m_ray[i], m_ray_info[i], m_aabb);
    }

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Clip_SinglePrecision, Fixture<float>)
    {
        for (size_t i = 0; i < RayCount; ++i)
        {
            m_ray[i].m_tmin = 0.0f;
            m_ray[i].m_tmax = numeric_limits<float>::max();

            m_hit ^= clip(m_ray[i], m_ray_info[i], m_aabb);
        }
    }

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Clip_DoublePrecision, Fixture<double>)
    {
        for (size_t i = 0; i < RayCount; ++i)
        {
            m_ray[i].m_tmin = 0.0;
            m_ray[i].m_tmax = numeric_limits<double>::max();

            m_hit ^= clip(m_ray[i], m_ray_info[i], m_aabb);
        }
    }
}

namespace
{
    template <typename TriangleType, typename T, int TargetHitRate>
    struct RayTriangleFixture
      : public FixtureBase<T>
    {
        static const size_t RayCount = 1000;

        TriangleType    m_triangle;
        RayType         m_ray[RayCount];

        bool            m_hit;
        T               m_t;
        T               m_u;
        T               m_v;

        RayTriangleFixture()
          : m_hit(false)
        {
            MersenneTwister rng;

            m_triangle = get_random_triangle(rng, T(-1.0), T(1.0));

            generate_rays(rng);
        }

        void generate_rays(MersenneTwister& rng)
        {
            size_t hit_count = 0;
            double current_hit_rate;

            for (size_t i = 0; i < RayCount; ++i)
            {
                current_hit_rate = i > 0 ? (100.0 * hit_count) / i : 0.0;

                if (current_hit_rate < static_cast<double>(TargetHitRate))
                {
                    generate_hitting_ray(rng, m_ray[i]);
                    ++hit_count;
                }
                else
                {
                    generate_missing_ray(rng, m_ray[i]);
                }
            }
        }

        void generate_hitting_ray(MersenneTwister& rng, RayType& ray)
        {
            do 
            {
                get_random_ray(rng, T(10.0), ray);
            } while (!m_triangle.intersect(ray));
        }

        void generate_missing_ray(MersenneTwister& rng, RayType& ray)
        {
            do 
            {
                get_random_ray(rng, T(10.0), ray);
            } while (m_triangle.intersect(ray));
        }

        static TriangleType get_random_triangle(
            MersenneTwister&    rng,
            const T             min,
            const T             max)
        {
            const VectorType v0 = get_random_vector<3>(rng, min, max);
            const VectorType v1 = get_random_vector<3>(rng, min, max);
            const VectorType v2 = get_random_vector<3>(rng, min, max);
            return TriangleType(v0, v1, v2);
        }

        FOUNDATION_FORCE_INLINE void payload()
        {
            for (size_t i = 0; i < RayCount; ++i)
                m_hit ^= m_triangle.intersect(m_ray[i], m_t, m_u, m_v);
        }
    };
}

FOUNDATION_BENCHMARK_SUITE(Foundation_Math_Intersection_RayTriangleMT)
{
    template <typename T, int TargetHitRate>
    struct Fixture
      : public RayTriangleFixture<TriangleMT<T>, T, TargetHitRate>
    {
    };

    // We need these typedefs because we can't use commas in macro parameters.
    typedef Fixture<float, 0>       FixtureFloat0;
    typedef Fixture<float, 33>      FixtureFloat33;
    typedef Fixture<float, 66>      FixtureFloat66;
    typedef Fixture<float, 100>     FixtureFloat100;
    typedef Fixture<double, 0>      FixtureDouble0;
    typedef Fixture<double, 33>     FixtureDouble33;
    typedef Fixture<double, 66>     FixtureDouble66;
    typedef Fixture<double, 100>    FixtureDouble100;

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs0Percent, FixtureFloat0) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs33Percents, FixtureFloat33) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs66Percents, FixtureFloat66) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs100Percents, FixtureFloat100) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs0Percent, FixtureDouble0) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs33Percents, FixtureDouble33) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs66Percents, FixtureDouble66) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs100Percents, FixtureDouble100) { payload(); }
}

FOUNDATION_BENCHMARK_SUITE(Foundation_Math_Intersection_RayTriangleSSK)
{
    template <typename T, int TargetHitRate>
    struct Fixture
      : public RayTriangleFixture<TriangleSSK<T>, T, TargetHitRate>
    {
    };

    // We need these typedefs because we can't use commas in macro parameters.
    typedef Fixture<float, 0>       FixtureFloat0;
    typedef Fixture<float, 33>      FixtureFloat33;
    typedef Fixture<float, 66>      FixtureFloat66;
    typedef Fixture<float, 100>     FixtureFloat100;
    typedef Fixture<double, 0>      FixtureDouble0;
    typedef Fixture<double, 33>     FixtureDouble33;
    typedef Fixture<double, 66>     FixtureDouble66;
    typedef Fixture<double, 100>    FixtureDouble100;

    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs0Percent, FixtureFloat0) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs33Percents, FixtureFloat33) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs66Percents, FixtureFloat66) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_SinglePrecision_HitRateIs100Percents, FixtureFloat100) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs0Percent, FixtureDouble0) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs33Percents, FixtureDouble33) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs66Percents, FixtureDouble66) { payload(); }
    FOUNDATION_BENCHMARK_CASE_WITH_FIXTURE(Intersect_DoublePrecision_HitRateIs100Percents, FixtureDouble100) { payload(); }
};
