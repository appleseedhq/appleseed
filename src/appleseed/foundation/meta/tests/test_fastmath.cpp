
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "foundation/math/fastmath.h"
#include "foundation/math/scalar.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_FastMath)
{
    template <typename T>
    T compute_error(const T ref_value, const T value)
    {
        return
            ref_value == T(0.0)
                ? abs(ref_value - value)
                : abs(ref_value - value) / ref_value;
    }

    template <typename T, typename Function>
    T compute_max_relative_error(
        Function&       func,
        const T         exponent,
        const T         low,
        const T         high,
        const size_t    step_count)
    {
        T max_error = 0.0;

        for (size_t i = 0; i < step_count; ++i)
        {
            const T x =
                fit<T>(
                    static_cast<T>(i),
                    static_cast<T>(0),
                    static_cast<T>(step_count - 1),
                    low,
                    high);

            const T ref_value = pow(x, exponent);
            const T value = func(x, exponent);

            const T error = compute_error(ref_value, value);
            max_error = max(error, max_error);
        }

        return max_error;
    }

    float std_pow(const float x, const float y)
    {
        return pow(x, y);
    }

    float scalar_fast_pow(const float x, const float y)
    {
        return fast_pow(x, y);
    }

    float scalar_fast_pow_refined(const float x, const float y)
    {
        return fast_pow_refined(x, y);
    }

    TEST_CASE(ComputeMaxRelativeError_GivenStdPowFunction_ReturnsZero)
    {
        const float error =
            compute_max_relative_error(
                std_pow,
                2.4f,
                0.0f,
                1.0f,
                1000);

        EXPECT_EQ(0.0f, error);
    }

    TEST_CASE(FastPow)
    {
        const float error =
            compute_max_relative_error(
                scalar_fast_pow,
                2.4f,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.14f, error);
    }

    TEST_CASE(FastPowRefined)
    {
        const float error =
            compute_max_relative_error(
                scalar_fast_pow_refined,
                2.4f,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.016f, error);
    }

    template <typename T, typename Function>
    T compute_max_relative_error_sse(
        Function&       func,
        const T         exponent,
        const T         low,
        const T         high,
        const size_t    step_count)
    {
        T max_error = 0.0;

        for (size_t i = 0; i < step_count; i += 4)
        {
            ALIGN_SSE_VARIABLE T x[4];

            for (size_t j = 0; j < 4; ++j)
            {
                x[j] =
                    fit<T>(
                        static_cast<T>(i + j),
                        static_cast<T>(0),
                        static_cast<T>(step_count - 1),
                        low,
                        high);
            }

            T ref_values[4];

            for (size_t j = 0; j < 4; ++j)
                ref_values[j] = pow(x[j], exponent);

            func(x, exponent);

            for (size_t j = 0; j < 4; ++j)
            {
                const T error = compute_error(ref_values[j], x[j]);
                max_error = max(error, max_error);
            }
        }

        return max_error;
    }

    void vector_fast_pow(float x[4], const float y)
    {
        fast_pow(x, y);
    }

    void vector_fast_pow_refined(float x[4], const float y)
    {
        fast_pow_refined(x, y);
    }

    TEST_CASE(FastPowSSE)
    {
        const float error =
            compute_max_relative_error_sse(
                vector_fast_pow,
                2.4f,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.14f, error);
    }

    TEST_CASE(FastPowRefinedSSE)
    {
        const float error =
            compute_max_relative_error_sse(
                vector_fast_pow_refined,
                2.4f,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.016f, error);
    }
}
