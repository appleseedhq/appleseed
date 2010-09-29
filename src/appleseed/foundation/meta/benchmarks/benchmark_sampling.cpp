
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
#include "foundation/math/rng.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

BENCHMARK_SUITE(Foundation_Math_Sampling_QMCSamplingContext)
{
    using namespace foundation;

    typedef MersenneTwister RNG;
    typedef QMCSamplingContext<RNG> QMCSamplingContext;

    const size_t SampleCount = 64;

    struct Fixture
    {
        RNG                 m_rng;
        QMCSamplingContext  m_context;
        Vector2d            m_v;

        Fixture()
          : m_context(m_rng, 2, SampleCount)
        {
        }
    };

    BENCHMARK_CASE_WITH_FIXTURE(BenchmarkNextVector2, Fixture)
    {
        m_context.set_instance(0);
        m_v += m_context.next_vector2<2>();
        m_v += m_context.next_vector2<2>();
        m_v += m_context.next_vector2<2>();
        m_v += m_context.next_vector2<2>();
    }
}
