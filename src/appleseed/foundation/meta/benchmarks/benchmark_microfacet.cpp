
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/microfacet.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/lcg.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Microfacet)
{
    template <typename MDFType>
    struct FixtureBase
    {
        LCG         m_rng;
        Vector3f    m_outgoing;     // view direction, unit-length
        float       m_dummy;
        Vector3f    m_dummy_vec;

        FixtureBase()
          : m_outgoing(normalize(Vector3f(0.1f, 0.2f, 0.3f)))
          , m_dummy(0.0f)
          , m_dummy_vec(0.0f)
        {
        }

        void sample(const float alpha_x, const float alpha_y)
        {
            Vector3f s;
            s[0] = rand_float2(m_rng);
            s[1] = rand_float2(m_rng);
            s[2] = rand_float2(m_rng);

            m_dummy_vec +=
                MDFType().sample(
                    m_outgoing,
                    s,
                    alpha_x,
                    alpha_y,
                    0.0f);
        }

        void evaluate(const float alpha_x, const float alpha_y)
        {
            Vector2f s;
            s[0] = rand_float2(m_rng);
            s[1] = rand_float2(m_rng);

            m_dummy +=
                MDFType().D(
                    normalize(Vector3f(s[0], 0.5f, s[1])),
                    alpha_x,
                    alpha_y,
                    0.0f);
        }
    };

    //
    // Blinn-Phong MDF.
    //

    BENCHMARK_CASE_F(BlinnMDF_Sample, FixtureBase<BlinnMDF>)
    {
        sample(10.0f, 10.0f);
    }

    BENCHMARK_CASE_F(BlinnMDF_Evaluate, FixtureBase<BlinnMDF>)
    {
        evaluate(10.0f, 10.0f);
    }

    //
    // Beckmann MDF.
    //

    BENCHMARK_CASE_F(BeckmannMDF_Sample, FixtureBase<BeckmannMDF>)
    {
        sample(0.5f, 0.5f);
    }

    BENCHMARK_CASE_F(BeckmannMDF_Evaluate, FixtureBase<BeckmannMDF>)
    {
        evaluate(0.5f, 0.5f);
    }

    //
    // Ward MDF.
    //

    BENCHMARK_CASE_F(WardMDF_Sample, FixtureBase<WardMDF>)
    {
        sample(0.5f, 0.5f);
    }

    BENCHMARK_CASE_F(WardMDF_Evaluate, FixtureBase<WardMDF>)
    {
        evaluate(0.5f, 0.5f);
    }

    //
    // GGX MDF.
    //

    BENCHMARK_CASE_F(GGXMDF_Sample, FixtureBase<GGXMDF>)
    {
        sample(0.5f, 0.5f);
    }

    BENCHMARK_CASE_F(GGXMDF_Evaluate, FixtureBase<GGXMDF>)
    {
        evaluate(0.5f, 0.5f);
    }
}
