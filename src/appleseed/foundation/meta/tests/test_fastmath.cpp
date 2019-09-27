
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
#include "foundation/math/fastmath.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/countof.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Math_FastMath)
{
    template <typename T>
    T compute_relative_error(const T ref, const T value)
    {
        return
            ref == T(0.0)
                ? std::abs(value)
                : std::abs((ref - value) / ref);
    }

    template <typename T, typename Function>
    T compute_avg_relative_error_scalar(
        const Function& ref,
        const Function& func,
        const T         low,
        const T         high,
        const size_t    step_count)
    {
        double sum_error = 0.0;

        for (size_t i = 0; i < step_count; ++i)
        {
            const T x = fit<size_t, T>(i, 0, step_count - 1, low, high);
            const T ref_value = ref(x);
            const T value = func(x);
            const T error = compute_relative_error(ref_value, value);

            sum_error += static_cast<double>(error);
        }

        return static_cast<T>(sum_error / step_count);
    }

    template <typename T, typename Function>
    T compute_avg_relative_error_vector(
        const Function& ref,
        const Function& func,
        const T         low,
        const T         high,
        const size_t    step_count)
    {
        double sum_error = 0.0;

        for (size_t i = 0; i < step_count; i += 4)
        {
            APPLESEED_SIMD4_ALIGN T x[4];

            for (size_t j = 0; j < 4; ++j)
                x[j] = fit<size_t, T>(i + j, 0, step_count - 1, low, high);

            APPLESEED_SIMD4_ALIGN T ref_values[4] = { x[0], x[1], x[2], x[3] };
            ref(ref_values);

            APPLESEED_SIMD4_ALIGN T values[4] = { x[0], x[1], x[2], x[3] };
            func(values);

            for (size_t j = 0; j < 4; ++j)
            {
                const T error = compute_relative_error(ref_values[j], values[j]);
                sum_error += static_cast<double>(error);
            }
        }

        return static_cast<T>(sum_error / step_count);
    }

    template <typename Function>
    struct FuncDef
    {
        std::string      m_title;
        std::string      m_color;
        Function         m_function;
    };

    template <typename T, typename Function>
    void plot_functions(
        const std::string&      filepath,
        const FuncDef<Function> functions[],
        const size_t            function_count,
        const T                 low,
        const T                 high,
        const size_t            step_count)
    {
        GnuplotFile plotfile;

        for (size_t f = 0; f < function_count; ++f)
        {
            std::vector<Vector<T, 2>> points(step_count);

            for (size_t i = 0; i < step_count; ++i)
            {
                const T x = fit<size_t, T>(i, 0, step_count - 1, low, high);
                const T y = functions[f].m_function(x);
                points[i] = Vector2f(x, y);
            }

            plotfile
                .new_plot()
                .set_points(points)
                .set_title(functions[f].m_title)
                .set_color(functions[f].m_color);
        }

        plotfile.write(filepath);
    }

    //
    // 2^x
    //

    float scalar_std_pow2(const float x)
    {
        return std::pow(2.0f, x);
    }

    void vector_std_pow2(float x[4])
    {
        for (size_t i = 0; i < 4; ++i)
            x[i] = std::pow(2.0f, x[i]);
    }

    TEST_CASE(ScalarFastPow2)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_pow2,
                fast_pow2,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0000235f, error);
    }

    TEST_CASE(ScalarFasterPow2)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_pow2,
                faster_pow2,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0154f, error);
    }

    TEST_CASE(VectorFastPow2)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_pow2,
                fast_pow2,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0000235f, error);
    }

    TEST_CASE(VectorFasterPow2)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_pow2,
                faster_pow2,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0154f, error);
    }

    TEST_CASE(PlotPow2Functions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::pow[2]", "black", scalar_std_pow2 },
            { "foundation::fast_pow2", "green", fast_pow2 },
            { "foundation::faster_pow2", "red", faster_pow2 }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_pow2.gnuplot",
            functions,
            countof(functions),
            -2.0f,
            +10.0f,
            10000);
    }

    //
    // log2(x)
    //

    float scalar_std_log2(const float x)
    {
        return std::log(x) / std::log(2.0f);
    }

    void vector_std_log2(float x[4])
    {
        for (size_t i = 0; i < 4; ++i)
            x[i] = std::log(x[i]) / std::log(2.0f);
    }

    TEST_CASE(ScalarFastLog2)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_log2,
                fast_log2,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000262f, error);
    }

    TEST_CASE(ScalarFasterLog2)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_log2,
                faster_log2,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.193f, error);
    }

    TEST_CASE(VectorFastLog2)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_log2,
                fast_log2,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000262f, error);
    }

    TEST_CASE(VectorFasterLog2)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_log2,
                faster_log2,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.193f, error);
    }

    TEST_CASE(PlotLog2Functions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::log[2]", "black", scalar_std_log2 },
            { "foundation::fast_log2", "green", fast_log2 },
            { "foundation::faster_log2", "red", faster_log2 }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_log2.gnuplot",
            functions,
            countof(functions),
            1.0e-2f,
            1.0f,
            1000);
    }

    //
    // x^e
    //

    const float Exponent = 2.4f;

    float scalar_std_pow(const float x)
    {
        return std::pow(x, Exponent);
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
        for (size_t i = 0; i < 4; ++i)
            x[i] = std::pow(x[i], Exponent);
    }

    void vector_fast_pow(float x[4])
    {
        fast_pow(x, Exponent);
    }

    void vector_faster_pow(float x[4])
    {
        faster_pow(x, Exponent);
    }

    TEST_CASE(ScalarFastPow)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_pow,
                scalar_fast_pow,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000147f, error);
    }

    TEST_CASE(ScalarFasterPow)
    {
        const float error =
            compute_avg_relative_error_scalar(
                scalar_std_pow,
                scalar_faster_pow,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.0376f, error);
    }

    TEST_CASE(VectorFastPow)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_pow,
                vector_fast_pow,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000147f, error);
    }

    TEST_CASE(VectorFasterPow)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_pow,
                vector_faster_pow,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.0376f, error);
    }

    TEST_CASE(PlotPowFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::pow", "black", scalar_std_pow },
            { "foundation::fast_pow", "green", scalar_fast_pow },
            { "foundation::faster_pow", "red", scalar_faster_pow }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_pow.gnuplot",
            functions,
            countof(functions),
            1.0e-2f,
            1.0f,
            1000);
    }

    //
    // log(x)
    //

    void vector_std_log(float x[4])
    {
        for (size_t i = 0; i < 4; ++i)
            x[i] = std::log(x[i]);
    }

    TEST_CASE(ScalarFastLog)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                std::log,
                fast_log,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000262f, error);
    }

    TEST_CASE(ScalarFasterLog)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                std::log,
                faster_log,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.193f, error);
    }

    TEST_CASE(VectorFastLog)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_log,
                fast_log,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.000262f, error);
    }

    TEST_CASE(VectorFasterLog)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_log,
                faster_log,
                1.0e-2f,
                1.0f,
                1000);

        EXPECT_LT(0.193f, error);
    }

    TEST_CASE(PlotLogFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::log", "black", std::log },
            { "foundation::fast_log", "green", fast_log },
            { "foundation::faster_log", "red", faster_log }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_log.gnuplot",
            functions,
            countof(functions),
            1.0e-2f,
            1.0f,
            1000);
    }

    //
    // exp(x)
    //

    void vector_std_exp(float x[4])
    {
        for (size_t i = 0; i < 4; ++i)
            x[i] = std::exp(x[i]);
    }

    TEST_CASE(ScalarFastExp)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                std::exp,
                fast_exp,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0000253f, error);
    }

    TEST_CASE(ScalarFasterExp)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                std::exp,
                faster_exp,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0159f, error);
    }

    TEST_CASE(VectorFastExp)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_exp,
                fast_exp,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0000253f, error);
    }

    TEST_CASE(VectorFasterExp)
    {
        const float error =
            compute_avg_relative_error_vector(
                vector_std_exp,
                faster_exp,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.0159f, error);
    }

    TEST_CASE(PlotExpFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::exp", "black", std::exp },
            { "foundation::fast_exp", "green", fast_exp },
            { "foundation::faster_exp", "red", faster_exp }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_exp.gnuplot",
            functions,
            countof(functions),
            -2.0f,
            +10.0f,
            10000);
    }

    // 
    // acos(x)
    //

    TEST_CASE(PlotAcosFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::acos", "black", std::acos },
            { "foundation::fast_acos", "green", fast_acos }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_acos.gnuplot",
            functions,
            countof(functions),
            -1.0f,
            +1.0f,
            1000);
    }

    //
    // 1/x
    //

    TEST_CASE(ScalarFastRcp)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                rcp<float>,
                fast_rcp,
                1.0e-3f,
                1.0f,
                1000);

        EXPECT_LT(1.0e-4f, error);
    }

    TEST_CASE(PlotRcpFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "foundation::rcp", "black", rcp<float> },
            { "foundation::fast_rcp", "green", fast_rcp }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_rcp.gnuplot",
            functions,
            countof(functions),
            1.0e-3f,
            1.0f,
            1000);
    }

    //
    // sqrt(x)
    //

    TEST_CASE(ScalarFastSqrt)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                std::sqrt,
                fast_sqrt,
                0.0f,
                1.0f,
                1000);

        EXPECT_LT(0.02f, error);
    }

    TEST_CASE(PlotSqrtFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "std::sqrt", "black", std::sqrt },
            { "foundation::fast_sqrt", "green", fast_sqrt }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_sqrt.gnuplot",
            functions,
            countof(functions),
            0.0f,
            1.0f,
            1000);
    }

    //
    // 1/sqrt(x)
    //

    float rcp_sqrt(const float x)
    {
        return 1.0f / std::sqrt(x);
    }

    TEST_CASE(ScalarFastRcpSqrt)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                rcp_sqrt,
                fast_rcp_sqrt,
                1.0e-3f,
                1.0f,
                1000);

#ifdef APPLESEED_USE_SSE
        EXPECT_LT(4.0e-8f, error);
#else
        EXPECT_LT(1.0e-3f, error);
#endif
    }

    TEST_CASE(ScalarFasterRcpSqrt)
    {
        const float error =
            compute_avg_relative_error_scalar<float, float (*)(float)>(
                rcp_sqrt,
                faster_rcp_sqrt,
                1.0e-3f,
                1.0f,
                1000);

#ifdef APPLESEED_USE_SSE
        EXPECT_LT(1.0e-4f, error);
#else
        EXPECT_LT(1.0e-3f, error);
#endif
    }

    TEST_CASE(PlotRcpSqrtFunctions)
    {
        const FuncDef<float (*)(float)> functions[] =
        {
            { "1.0/std::sqrt", "black", rcp_sqrt },
            { "foundation::fast_rcp_sqrt", "green", fast_rcp_sqrt },
            { "foundation::faster_rcp_sqrt", "red", faster_rcp_sqrt }
        };

        plot_functions(
            "unit tests/outputs/test_fastmath_rcpsqrt.gnuplot",
            functions,
            countof(functions),
            1.0e-3f,
            1.0f,
            1000);
    }
}
