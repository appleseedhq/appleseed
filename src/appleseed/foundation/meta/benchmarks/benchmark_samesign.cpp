
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
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/benchmark.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstdint>

using namespace foundation;

BENCHMARK_SUITE(SameSign)
{
    //
    // Research for this post on Stack Overflow:
    //
    //   How to efficiently compare the sign of two floating-point values while handling negative zeros
    //   http://stackoverflow.com/questions/2922619/how-to-efficiently-compare-the-sign-of-two-floating-point-values-while-handling-n
    //

    //
    // Empty function.
    //

    APPLESEED_NO_INLINE bool empty_function(const float a, const float b)
    {
        return false;
    }

    APPLESEED_NO_INLINE bool empty_function(const float a, const float b, const float c)
    {
        return false;
    }

    //
    // Naive variant.
    //

    APPLESEED_NO_INLINE bool same_sign_naive(const float a, const float b)
    {
        if (std::abs(a) == 0.0f || std::abs(b) == 0.0f)
            return true;

        return (a >= 0.0f) == (b >= 0.0f);
    }

    APPLESEED_NO_INLINE bool same_sign_naive(const float a, const float b, const float c)
    {
        return same_sign_naive(a, b) && same_sign_naive(a, c) && same_sign_naive(b, c);
    }

    //
    // Integer arithmetic-based variant.
    //

    APPLESEED_NO_INLINE bool same_sign_integer(const float a, const float b)
    {
        const int ia = binary_cast<int>(a);
        const int ib = binary_cast<int>(b);

        const int az = (ia & 0x7FFFFFFF) == 0;
        const int bz = (ib & 0x7FFFFFFF) == 0;
        const int ab = (ia ^ ib) >= 0;

        return (az | bz | ab) != 0;
    }

    APPLESEED_NO_INLINE bool same_sign_integer(const float a, const float b, const float c)
    {
        const std::int32_t ia = binary_cast<std::int32_t>(a);
        const std::int32_t ib = binary_cast<std::int32_t>(b);
        const std::int32_t ic = binary_cast<std::int32_t>(c);

        const std::int32_t az = (ia & 0x7FFFFFFFL) == 0;
        const std::int32_t bz = (ib & 0x7FFFFFFFL) == 0;
        const std::int32_t cz = (ic & 0x7FFFFFFFL) == 0;

        const std::int32_t ab = (ia ^ ib) >= 0;
        const std::int32_t ac = (ia ^ ic) >= 0;
        const std::int32_t bc = (ib ^ ic) >= 0;

        const std::int32_t b1 = ab | az | bz;
        const std::int32_t b2 = ac | az | cz;
        const std::int32_t b3 = bc | bz | cz;

        return (b1 & b2 & b3) != 0;
    }

    //
    // Multiplication-based variant.
    //

    APPLESEED_NO_INLINE bool same_sign_multiplication(const float a, const float b)
    {
        return a * b >= 0.0f;
    }

    APPLESEED_NO_INLINE bool same_sign_multiplication(const float a, const float b, const float c)
    {
        return a * b >= 0.0f && a * c >= 0.0f && b * c >= 0.0f;
    }

#ifdef APPLESEED_USE_SSE

    //
    // SSE implementation of the 3-component multiplication-based variant.
    //

    APPLESEED_NO_INLINE bool same_sign_multiplication_sse(const float a, const float b, const float c)
    {
        APPLESEED_SIMD4_ALIGN float u[4] = { a, a, b, c };

        const __m128 mu = _mm_load_ps(u);
        const __m128 mv = _mm_shuffle_ps(mu, mu, _MM_SHUFFLE(2, 3, 3, 2));
        const __m128 product = _mm_mul_ps(mu, mv);
        const __m128 zero = _mm_set1_ps(0.0f);
        const __m128 cmp = _mm_cmpge_ps(product, zero);
        const int mask = _mm_movemask_ps(cmp);

        return mask == 0xF;
    }

#endif

    struct Fixture
    {
        static const size_t InvocationCount = 100;
        static const size_t ValueCount = InvocationCount + 2;

        bool    m_result;
        float   m_values[ValueCount];

        Fixture()
          : m_result(false)
        {
            MersenneTwister rng;

            for (size_t i = 0; i < ValueCount; ++i)
            {
                switch (rand_int1(rng, 0, 3))
                {
                  case 0: m_values[i] = -1.0f; break;
                  case 1: m_values[i] = -0.0f; break;
                  case 2: m_values[i] = +0.0f; break;
                  case 3: m_values[i] = +1.0f; break;
                  assert_otherwise;
                }
            }
        }
    };

    BENCHMARK_CASE_F(EmptyFunction2, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= empty_function(m_values[i], m_values[i + 1]);
    }

    BENCHMARK_CASE_F(EmptyFunction3, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= empty_function(m_values[i], m_values[i + 1], m_values[i + 2]);
    }

    BENCHMARK_CASE_F(SameSignNaive2, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_naive(m_values[i], m_values[i + 1]);
    }

    BENCHMARK_CASE_F(SameSignNaive3, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_naive(m_values[i], m_values[i + 1], m_values[i + 2]);
    }

    BENCHMARK_CASE_F(SameSignInteger2, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_integer(m_values[i], m_values[i + 1]);
    }

    BENCHMARK_CASE_F(SameSignInteger3, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_integer(m_values[i], m_values[i + 1], m_values[i + 2]);
    }

    BENCHMARK_CASE_F(SameSignMultiplication2, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_multiplication(m_values[i], m_values[i + 1]);
    }

    BENCHMARK_CASE_F(SameSignMultiplication3, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_multiplication(m_values[i], m_values[i + 1], m_values[i + 2]);
    }

#ifdef APPLESEED_USE_SSE

    BENCHMARK_CASE_F(SameSignMultiplicationSSE3, Fixture)
    {
        for (size_t i = 0; i < InvocationCount; ++i)
            m_result ^= same_sign_multiplication_sse(m_values[i], m_values[i + 1], m_values[i + 2]);
    }

#endif
}
