
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
#include "foundation/math/permutation.h"
#include "foundation/math/primes.h"
#include "foundation/math/qmc.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Math_QMC)
{
    template <typename T>
    struct Fixture
    {
        T m_x;

        void radical_inverse_base2_payload()
        {
            m_x = T(0.0);

            for (size_t i = 0; i < 128; ++i)
                m_x += radical_inverse_base2<T>(i);
        }

        void radical_inverse_payload()
        {
            m_x = T(0.0);

            for (size_t s = 0, d = 1; d <= 32; ++d)
            {
                for (size_t i = 0; i < 4; ++i, ++s)
                {
                    m_x +=
                        radical_inverse<T>(
                            Primes[d],
                            s);
                }
            }
        }

        void permuted_radical_inverse_payload()
        {
            m_x = T(0.0);

            for (size_t s = 0, d = 1; d <= 32; ++d)
            {
                for (size_t i = 0; i < 4; ++i, ++s)
                {
                    m_x +=
                        permuted_radical_inverse<T>(
                            Primes[d],
                            FaurePermutations[d],
                            s);
                }
            }
        }
    };

    BENCHMARK_CASE_F(RadicalInverseBase2_SinglePrecision, Fixture<float>)
    {
        radical_inverse_base2_payload();
    }

    BENCHMARK_CASE_F(RadicalInverse_SinglePrecision, Fixture<float>)
    {
        radical_inverse_payload();
    }

    BENCHMARK_CASE_F(PermutedRadicalInverse_SinglePrecision, Fixture<float>)
    {
        permuted_radical_inverse_payload();
    }

    BENCHMARK_CASE_F(RadicalInverseBase2_DoublePrecision, Fixture<double>)
    {
        radical_inverse_base2_payload();
    }

    BENCHMARK_CASE_F(RadicalInverse_DoublePrecision, Fixture<double>)
    {
        radical_inverse_payload();
    }

    BENCHMARK_CASE_F(PermutedRadicalInverse_DoublePrecision, Fixture<double>)
    {
        permuted_radical_inverse_payload();
    }

    BENCHMARK_CASE_F(HaltonSequence_Bases2And3_SinglePrecision, Fixture<Vector2f>)
    {
        static const size_t Bases[] = { 2, 3 };

        m_x = Vector2f(0.0f);

        for (size_t i = 0; i < 64; ++i)
            m_x += halton_sequence<float, 2>(Bases, i);
    }

    BENCHMARK_CASE_F(HaltonSequence_Bases2And3_DoublePrecision, Fixture<Vector2d>)
    {
        static const size_t Bases[] = { 2, 3 };

        m_x = Vector2d(0.0);

        for (size_t i = 0; i < 64; ++i)
            m_x += halton_sequence<double, 2>(Bases, i);
    }

    BENCHMARK_CASE_F(HammersleySequence_Bases2And3_SinglePrecision, Fixture<Vector2f>)
    {
        static const size_t Bases[] = { 2, 3 };

        m_x = Vector2f(0.0f);

        for (size_t i = 0; i < 64; ++i)
            m_x += hammersley_sequence<float, 2>(Bases, i, 128);
    }

    BENCHMARK_CASE_F(HammersleySequence_Bases2And3_DoublePrecision, Fixture<Vector2d>)
    {
        static const size_t Bases[] = { 2, 3 };

        m_x = Vector2d(0.0);

        for (size_t i = 0; i < 64; ++i)
            m_x += hammersley_sequence<double, 2>(Bases, i, 128);
    }
}
