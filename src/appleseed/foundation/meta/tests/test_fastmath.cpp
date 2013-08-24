
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/utility/countof.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_FastMath)
{
    template <typename T>
    T compute_relative_error(const T ref, const T value)
    {
        return
            ref == T(0.0)
                ? abs(ref - value)
                : abs((ref - value) / ref);
    }

    template <typename T, typename Function>
    T compute_max_relative_error(
        const Function& ref,
        const Function& func,
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

            const T ref_value = ref(x);
            const T value = func(x);
            const T error = compute_relative_error(ref_value, value);

            max_error = max(error, max_error);
        }

        return max_error;
    }

    template <typename T, typename Function>
    T compute_max_relative_error_sse(
        const Function& ref,
        const Function& func,
        const T         low,
        const T         high,
        const size_t    step_count)
    {
        T max_error = T(0.0);

        for (size_t i = 0; i < step_count; i += 4)
        {
            SSE_ALIGN T x[4];

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

            SSE_ALIGN T ref_values[4] = { x[0], x[1], x[2], x[3] };
            ref(ref_values);

            SSE_ALIGN T values[4] = { x[0], x[1], x[2], x[3] };
            func(values);

            for (size_t j = 0; j < 4; ++j)
            {
                const T error = compute_relative_error(ref_values[j], values[j]);
                max_error = max(error, max_error);
            }
        }

        return max_error;
    }

    template <typename Function>
    struct FuncDef
    {
        string      m_name;
        string      m_legend;
        string      m_color;
        Function    m_function;
    };

    template <typename T, typename Function>
    void plot_functions(
        const string&           filename,
        const FuncDef<Function> functions[],
        const size_t            function_count,
        const T                 low,
        const T                 high,
        const size_t            step_count)
    {
        MapleFile file(filename);

        vector<MaplePlotDef> plot_defs;
        plot_defs.reserve(function_count);

        for (size_t f = 0; f < function_count; ++f)
        {
            vector<float> xs, ys;

            xs.reserve(step_count);
            ys.reserve(step_count);

            for (size_t i = 0; i < step_count; ++i)
            {
                const T x =
                    fit<T>(
                        static_cast<T>(i),
                        static_cast<T>(0),
                        static_cast<T>(step_count - 1),
                        low,
                        high);

                xs.push_back(x);
                ys.push_back(functions[f].m_function(x));
            }

            file.define(functions[f].m_name, xs, ys);

            MaplePlotDef plot_def(functions[f].m_name);
            plot_def.set_legend(functions[f].m_legend);
            plot_def.set_color(functions[f].m_color);
            plot_defs.push_back(plot_def);
        }

        file.plot(plot_defs);
    }

    // Pow2(x).

    float scalar_std_pow2(const float x)
    {
        return pow(2.0f, x);
    }

    TEST_CASE(PlotPow2Functions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "scalar_std_pow2", "std::pow[2]", "black", scalar_std_pow2 },
            { "scalar_fast_pow2", "foundation::fast_pow2", "green", fast_pow2 },
            { "scalar_faster_pow2", "foundation::faster_pow2", "red", faster_pow2 }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_pow2.mpl",
            functions,
            countof(functions),
            0.0f,
            1.0f,
            1000);
    }

    // Log2(x).

    float scalar_std_log2(const float x)
    {
        return log(x) / log(2.0f);
    }

    TEST_CASE(PlotLog2Functions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "scalar_std_log2", "std::log[2]", "black", scalar_std_log2 },
            { "scalar_fast_log2", "foundation::fast_log2", "green", fast_log2 },
            { "scalar_faster_log2", "foundation::faster_log2", "red", faster_log2 }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_log2.mpl",
            functions,
            countof(functions),
            0.1f,
            1.0f,
            1000);
    }

    // Pow(x).

    const float Exponent = 2.4f;

    float scalar_std_pow(const float x)
    {
        return pow(x, Exponent);
    }

    float scalar_fast_pow(const float x)
    {
        return fast_pow(x, Exponent);
    }

    float scalar_faster_pow(const float x)
    {
        return faster_pow(x, Exponent);
    }

    void vector_std_pow(float x[4])
    {
        x[0] = pow(x[0], Exponent);
        x[1] = pow(x[1], Exponent);
        x[2] = pow(x[2], Exponent);
        x[3] = pow(x[3], Exponent);
    }

    void vector_old_fast_pow(float x[4])
    {
        old_fast_pow(x, Exponent);
    }

    void vector_old_fast_pow_refined(float x[4])
    {
        old_fast_pow_refined(x, Exponent);
    }

    TEST_CASE(ComputeMaxRelativeError_GivenScalarStdPowFunction_ReturnsZero)
    {
        const float error =
            compute_max_relative_error(
                scalar_std_pow,
                scalar_std_pow,
                0.1f,
                1.0f,
                1000);

        EXPECT_EQ(0.0f, error);
    }

    TEST_CASE(ScalarFastPow)
    {
        const float error =
            compute_max_relative_error(
                scalar_std_pow,
                scalar_fast_pow,
                0.1f,
                1.0f,
                1000);

        EXPECT_LT(0.00031f, error);
    }

    TEST_CASE(VectorOldFastPow)
    {
        const float error =
            compute_max_relative_error_sse(
                vector_std_pow,
                vector_old_fast_pow,
                0.1f,
                1.0f,
                1000);

        EXPECT_LT(0.14f, error);
    }

    TEST_CASE(VectorOldFastPowRefined)
    {
        const float error =
            compute_max_relative_error_sse(
                vector_std_pow,
                vector_old_fast_pow_refined,
                0.1f,
                1.0f,
                1000);

        EXPECT_LT(0.016f, error);
    }

    TEST_CASE(PlotPowFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "scalar_std_pow", "std::pow", "black", scalar_std_pow },
            { "scalar_fast_pow", "foundation::fast_pow", "green", scalar_fast_pow },
            { "scalar_faster_pow", "foundation::faster_pow", "red", scalar_faster_pow }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_pow.mpl",
            functions,
            countof(functions),
            0.1f,
            1.0f,
            1000);
    }

    // Log(x).

    TEST_CASE(PlotLogFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "scalar_std_log", "std::log", "black", log },
            { "scalar_fast_log", "foundation::fast_log", "green", fast_log },
            { "scalar_faster_log", "foundation::faster_log", "red", faster_log }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_log.mpl",
            functions,
            countof(functions),
            0.1f,
            1.0f,
            1000);
    }

    // Exp(x).

    TEST_CASE(PlotExpFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "scalar_std_exp", "std::exp", "black", exp },
            { "scalar_fast_exp", "foundation::fast_exp", "green", fast_exp },
            { "scalar_faster_exp", "foundation::faster_exp", "red", faster_exp }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_exp.mpl",
            functions,
            countof(functions),
            0.0f,
            1.0f,
            1000);
    }
}
