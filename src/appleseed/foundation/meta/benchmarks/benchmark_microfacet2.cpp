
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/microfacet2.h"
//#include "foundation/math/rng.h"
//#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_Microfacet2)
{
    /*
    template <typename DummyType>
    struct FixtureBase
    {
        LCG         m_rng;
        DummyType   m_dummy;

        FixtureBase()
          : m_dummy(0.0)
        {
        }
    };

    //
    // Blinn-Phong MDF.
    //

    template <typename DummyType>
    struct BlinnMDFFixture
      : public FixtureBase<DummyType>
    {
        BlinnMDF<double> m_mdf;

        BlinnMDFFixture()
          : m_mdf(10.0)
        {
        }
    };

    BENCHMARK_CASE_F(BlinnMDF_Sample, BlinnMDFFixture<Vector3d>)
    {
        Vector2d s;
        s[0] = rand_double2(m_rng);
        s[1] = rand_double2(m_rng);

        m_dummy += m_mdf.sample(s);
    }

    BENCHMARK_CASE_F(BlinnMDF_Evaluate, BlinnMDFFixture<double>)
    {
        const double s = rand_double2(m_rng);

        m_dummy += m_mdf.evaluate(s);
    }

    //
    // Beckmann MDF.
    //

    template <typename DummyType>
    struct BeckmannMDFFixture
      : public FixtureBase<DummyType>
    {
        BeckmannMDF<double> m_mdf;

        BeckmannMDFFixture()
          : m_mdf(0.5)
        {
        }
    };

    BENCHMARK_CASE_F(BeckmannMDF_Sample, BeckmannMDFFixture<Vector3d>)
    {
        Vector2d s;
        s[0] = rand_double2(m_rng);
        s[1] = rand_double2(m_rng);

        m_dummy += m_mdf.sample(s);
    }

    BENCHMARK_CASE_F(BeckmannMDF_Evaluate, BeckmannMDFFixture<double>)
    {
        const double s = rand_double2(m_rng);

        m_dummy += m_mdf.evaluate(s);
    }

    //
    // Ward MDF.
    //

    template <typename DummyType>
    struct WardMDFFixture
      : public FixtureBase<DummyType>
    {
        WardMDF<double> m_mdf;

        WardMDFFixture()
          : m_mdf(0.5)
        {
        }
    };

    BENCHMARK_CASE_F(WardMDF_Sample, WardMDFFixture<Vector3d>)
    {
        Vector2d s;
        s[0] = rand_double2(m_rng);
        s[1] = rand_double2(m_rng);

        m_dummy += m_mdf.sample(s);
    }

    BENCHMARK_CASE_F(WardMDF_Evaluate, WardMDFFixture<double>)
    {
        const double s = rand_double2(m_rng);

        m_dummy += m_mdf.evaluate(s);
    }

    //
    // GGX MDF.
    //

    template <typename DummyType>
    struct GGXMDFFixture
      : public FixtureBase<DummyType>
    {
        GGXMDF<double> m_mdf;

        GGXMDFFixture()
          : m_mdf(0.5)
        {
        }
    };

    BENCHMARK_CASE_F(GGXMDF_Sample, GGXMDFFixture<Vector3d>)
    {
        Vector2d s;
        s[0] = rand_double2(m_rng);
        s[1] = rand_double2(m_rng);

        m_dummy += m_mdf.sample(s);
    }

    BENCHMARK_CASE_F(GGXMDF_Evaluate, GGXMDFFixture<double>)
    {
        const double s = rand_double2(m_rng);

        m_dummy += m_mdf.evaluate(s);
    }
    */
}
