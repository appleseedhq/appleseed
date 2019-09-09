
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/sampling/qmcsamplingcontext.h"
#include "foundation/math/sampling/rngsamplingcontext.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace
{
    struct SamplingContextFixture
    {
        typedef MersenneTwister RNG;

        RNG         m_rng;
        Vector2d    m_v;

        SamplingContextFixture()
          : m_v(0.0)
        {
        }
    };
}

BENCHMARK_SUITE(Foundation_Math_Sampling_RNGSamplingContext)
{
    BENCHMARK_CASE_F(BenchmarkTrajectory, SamplingContextFixture)
    {
        const size_t InitialInstance = 1234567;
        RNGSamplingContext<RNG> context(
            m_rng,
            1,
            InitialInstance,
            InitialInstance);

        for (size_t i = 0; i < 32; ++i)
        {
            context.split_in_place(2, 1);
            m_v += context.next2<Vector2d>();
        }
    }
}

BENCHMARK_SUITE(Foundation_Math_Sampling_QMCSamplingContext)
{
    BENCHMARK_CASE_F(BenchmarkTrajectory_RNGMode, SamplingContextFixture)
    {
        const size_t InitialInstance = 1234567;
        QMCSamplingContext<RNG> context(
            m_rng,
            QMCSamplingContext<RNG>::RNGMode,
            1,
            InitialInstance,
            InitialInstance);

        for (size_t i = 0; i < 32; ++i)
        {
            context.split_in_place(2, 1);
            m_v += context.next2<Vector2d>();
        }
    }

    BENCHMARK_CASE_F(BenchmarkTrajectory_QMCMode, SamplingContextFixture)
    {
        const size_t InitialInstance = 1234567;
        QMCSamplingContext<RNG> context(
            m_rng,
            QMCSamplingContext<RNG>::QMCMode,
            1,
            InitialInstance,
            InitialInstance);

        for (size_t i = 0; i < 32; ++i)
        {
            context.split_in_place(2, 1);
            m_v += context.next2<Vector2d>();
        }
    }
}

BENCHMARK_SUITE(Foundation_Math_Sampling_Mappings)
{
    const size_t SampleCount = 16;

    struct Fixture
    {
        Vector2d    m_samples[SampleCount];
        Vector2d    m_dummy2d;
        Vector3d    m_dummy3d;

        Fixture()
          : m_dummy2d(0.0)
          , m_dummy3d(0.0)
        {
            MersenneTwister rng;

            for (size_t i = 0; i < SampleCount; ++i)
            {
                m_samples[i].x = rand_double2(rng);
                m_samples[i].y = rand_double2(rng);
            }
        }
    };

    BENCHMARK_CASE_F(Benchmark_SampleDiskUniform, Fixture)
    {
        for (size_t i = 0; i < SampleCount; ++i)
            m_dummy2d += sample_disk_uniform(m_samples[i]);
    }

    BENCHMARK_CASE_F(Benchmark_SampleDiskUniformAlt, Fixture)
    {
        for (size_t i = 0; i < SampleCount; ++i)
            m_dummy2d += sample_disk_uniform_alt(m_samples[i]);
    }

    BENCHMARK_CASE_F(Benchmark_SampleTriangleUniform, Fixture)
    {
        for (size_t i = 0; i < SampleCount; ++i)
            m_dummy3d += sample_triangle_uniform(m_samples[i]);
    }

    BENCHMARK_CASE_F(Benchmark_SampleTriangleUniformHeitz, Fixture)
    {
        for (size_t i = 0; i < SampleCount; ++i)
            m_dummy3d += sample_triangle_uniform_heitz(m_samples[i]);
    }
}
