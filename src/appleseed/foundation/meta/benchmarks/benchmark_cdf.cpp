
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/math/cdf.h"
#include "foundation/math/rng.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cassert>

BENCHMARK_SUITE(Foundation_Math_CDF)
{
    using namespace foundation;
    using namespace std;

    struct Fixture
    {
        typedef CDF<int, double> CDFType;

        CDFType m_cdf;
        double  m_x;

        Fixture()
          : m_x(0.0)
        {
            MersenneTwister rng;

            for (size_t i = 0; i < 1000; ++i)
                m_cdf.insert(i, rand_double1(rng));

            assert(m_cdf.valid());

            m_cdf.prepare();
        }
    };

    BENCHMARK_CASE_F(DoublePrecisionSampling, Fixture)
    {
        m_x += m_cdf.sample(0.5).second;
    }
}
