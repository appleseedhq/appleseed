
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
#include "foundation/math/qmc.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

BENCHMARK_SUITE(Foundation_Math_QMC)
{
    using namespace foundation;

    template <typename T>
    struct Fixture
    {
        static size_t Bases[2];

        T m_x;

        Fixture()
          : m_x(T(0.0))
        {
        }
    };

    template <typename T>
    size_t Fixture<T>::Bases[2] = { 2, 3 };

    const size_t Digits = 0x55555555;

    BENCHMARK_CASE_F(SinglePrecisionRadicalInverseBase2, Fixture<float>)
    {
        m_x += radical_inverse_base2<float>(Digits);
    }

    BENCHMARK_CASE_F(DoublePrecisionRadicalInverseBase2, Fixture<double>)
    {
        m_x += radical_inverse_base2<double>(Digits);
    }

    BENCHMARK_CASE_F(SinglePrecisionRadicalInverseBase3, Fixture<float>)
    {
        m_x += radical_inverse<float>(3, Digits);
    }

    BENCHMARK_CASE_F(DoublePrecisionRadicalInverseBase3, Fixture<double>)
    {
        m_x += radical_inverse<double>(3, Digits);
    }

    BENCHMARK_CASE_F(SinglePrecisionHaltonSequenceBases2And3, Fixture<Vector2f>)
    {
        m_x += halton_sequence<float, 2>(Bases, Digits);
    }

    BENCHMARK_CASE_F(DoublePrecisionHaltonSequenceBases2And3, Fixture<Vector2d>)
    {
        m_x += halton_sequence<double, 2>(Bases, Digits);
    }

    BENCHMARK_CASE_F(SinglePrecisionHammersleySequenceBases2And3, Fixture<Vector2f>)
    {
        m_x += hammersley_sequence<float, 2>(Bases, 255, 256);
    }

    BENCHMARK_CASE_F(DoublePrecisionHammersleySequenceBases2And3, Fixture<Vector2d>)
    {
        m_x += hammersley_sequence<double, 2>(Bases, 255, 256);
    }
}
