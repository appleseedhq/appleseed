
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
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Microfacet)
{
    template <typename MDF>
    bool is_positive(
        const MDF&    mdf,
        const float   alpha_x,
        const float   alpha_y,
        const size_t  sample_count,
        const float   gamma = 0.0f)
    {
        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, sample_count, i);

            const Vector3f h = sample_hemisphere_uniform(s);
            const float value = mdf.D(h, alpha_x, alpha_y, gamma);

            if (value < 0.0f)
                return false;
        }

        return true;
    }

    template <typename MDF>
    float integrate(
        const MDF&    mdf,
        const float   alpha,
        const size_t  sample_count,
        const float   gamma = 0.0f)
    {
        float integral = 0.0f;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float theta = radical_inverse_base2<float>(i) * HalfPi<float>();
            const Vector3f h(0.0f, cos(theta), 0.0f);
            const float value = mdf.D(h, alpha, alpha, gamma);

            integral += value * h.y * sin(theta);
        }

        integral *= HalfPi<float>() / sample_count;     // integration over theta
        integral *= TwoPi<float>();                     // integration over phi

        return integral;
    }

    //
    // Weak white furnace test.
    //
    // Reference:
    //
    //   Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
    //   http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
    //

    struct WeakWhiteFurnaceTestResult
    {
        float m_min_G1;
        float m_max_G1;
        float m_min_result;
        float m_max_result;
    };

    template <typename MDF>
    void weak_white_furnace_test(
        size_t                      num_runs,
        const float                 alpha_x,
        const float                 alpha_y,
        const float                 angle_step,
        WeakWhiteFurnaceTestResult& result,
        const float                 gamma = 0.0f)
    {
        result.m_min_G1 =  numeric_limits<float>::max();
        result.m_max_G1 = -numeric_limits<float>::max();
        result.m_min_result =  numeric_limits<float>::max();
        result.m_max_result = -numeric_limits<float>::max();

        MDF mdf;

        for (size_t i = 0; i < num_runs; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, num_runs, i);
            const Vector3f v = sample_hemisphere_uniform(s);
            const float G1 = mdf.G1(v, Vector3f(0.0f, 1.0, 0.0f), alpha_x, alpha_y, gamma);

            result.m_min_G1 = std::min(result.m_min_G1, G1);
            result.m_max_G1 = std::max(result.m_max_G1, G1);

            const float cos_thetha_o_4 = std::abs(4.0f * v.y);

            float integral = 0.0f;

            for (float theta = 0.0f; theta < Pi<float>(); theta += angle_step)
            {
                const float cos_theta = std::cos(theta);
                const float sin_theta = std::sin(theta);

                for (float phi = 0.0f; phi < TwoPi<float>(); phi += angle_step)
                {
                    const float cos_phi = std::cos(phi);
                    const float sin_phi = std::sin(phi);

                    const Vector3f l =
                        Vector3f::make_unit_vector(
                            cos_theta,
                            sin_theta,
                            cos_phi,
                            sin_phi);

                    const Vector3f h = normalize(v + l);

                    if (h.y > 0.0f)
                        integral += sin_theta * mdf.D(h, alpha_x, alpha_y, gamma) * G1 / cos_thetha_o_4;
                }
            }

            // Result should be 1.
            integral *= square(angle_step);

            result.m_min_result = std::min(result.m_min_result, integral);
            result.m_max_result = std::max(result.m_max_result, integral);
        }
    }

#define EXPECT_WEAK_WHITE_FURNACE_PASS(result) \
    EXPECT_NEQ(result.m_min_G1, result.m_max_G1); \
    EXPECT_FEQ_EPS(1.0f, result.m_min_result, WeakWhiteFurnaceEps); \
    EXPECT_FEQ_EPS(1.0f, result.m_max_result, WeakWhiteFurnaceEps);


    //
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
    const size_t FunctionPlotSampleCount = 256;
    const size_t FunctionSamplingSampleCount = 64;
    const float IntegrationEps = 1.0e-3f;
    const size_t WeakWhiteFurnaceRuns = 128;
    const float WeakWhiteFurnaceAngleStep = 0.0125f;
    const float WeakWhiteFurnaceEps = 0.05f;


    //
    // Blinn-Phong MDF.
    //

    TEST_CASE(BlinnMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BlinnMDF mdf;

        EXPECT_TRUE(is_positive(mdf, 10.0f, 10.0f, PositivityTestSampleCount));
    }

    TEST_CASE(BlinnMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF mdf;

        const float limit = mdf.D(Vector3f(0.0f), 10.0f, 10.0f, 0.0f);

        EXPECT_FEQ(0.0f, limit);
    }

    TEST_CASE(BlinnMDF_Integral_EqualsOne)
    {
        const BlinnMDF mdf;

        const float integral = integrate(mdf, 10.0f, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }


    //
    // Beckmann MDF.
    //

    TEST_CASE(BeckmannMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BeckmannMDF mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5f, 0.5f, PositivityTestSampleCount));
    }

    TEST_CASE(BeckmannMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF mdf;

        const float limit = mdf.D(Vector3f(0.0f), 0.5f, 0.5f, 0.0f);

        EXPECT_FEQ(0.0f, limit);
    }

    TEST_CASE(BeckmannMDF_Integral_EqualsOne)
    {
        const BeckmannMDF mdf;

        const float integral = integrate(mdf, 0.5f, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<BeckmannMDF >(
            WeakWhiteFurnaceRuns,
            0.6f,
            0.6f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    TEST_CASE(BeckmannMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<BeckmannMDF >(
            WeakWhiteFurnaceRuns,
            0.25f,
            0.5f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }


    //
    // GGX MDF.
    //

    TEST_CASE(GGXMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const GGXMDF mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5f, 0.5f, PositivityTestSampleCount));
    }

    TEST_CASE(GGXMDF_Evaluate_GivenCosThetaIsZero_ReturnsLimitValue)
    {
        const float AlphaG = 0.5f;
        const GGXMDF mdf;
        const float ExpectedLimit = AlphaG * AlphaG * RcpPi<float>();

        const float limit = mdf.D(Vector3f(0.0f), AlphaG, AlphaG, 0.0f);

        EXPECT_FEQ(ExpectedLimit, limit);
    }

    TEST_CASE(GGXMDF_Integral_EqualsOne)
    {
        const GGXMDF mdf;

        const float integral = integrate(mdf, 0.5f, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GGXMDF >(
            WeakWhiteFurnaceRuns,
            0.35f,
            0.35f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    TEST_CASE(GGXMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GGXMDF >(
            WeakWhiteFurnaceRuns,
            0.25f,
            0.5f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }


    //
    // Ward MDF.
    //

    TEST_CASE(WardMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const WardMDF mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5f, 0.5f, PositivityTestSampleCount));
    }

    TEST_CASE(WardMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const WardMDF mdf;

        const float limit = mdf.D(Vector3f(0.0f), 0.5f, 0.5f, 0.0f);

        EXPECT_FEQ(0.0f, limit);
    }


    //
    // GTR1 MDF.
    //

    TEST_CASE(GTR1MDF_Evaluate_ReturnsNonNegativeValues)
    {
        const GTR1MDF mdf;

        EXPECT_TRUE(is_positive(mdf, 10.0f, 10.0f, PositivityTestSampleCount));
    }

    TEST_CASE(GTR1MDF_Integral_EqualsOne)
    {
        const GTR1MDF mdf;

        const float integral = integrate(mdf, 10.0f, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }

    //
    // STD MDF.
    //

    TEST_CASE(StdMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const StdMDF mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5f, 0.5f, PositivityTestSampleCount, 2.0f));
    }

    TEST_CASE(StdMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const StdMDF mdf;

        const float limit = mdf.D(Vector3f(0.0f), 0.5f, 0.5f, 2.0f);

        EXPECT_FEQ(0.0f, limit);
    }

    TEST_CASE(StdMDF_Integral_EqualsOne)
    {
        const StdMDF mdf;

        const float integral = integrate(mdf, 0.5f, IntegrationSampleCount, 2.0);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }

    TEST_CASE(StdMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<StdMDF >(
            WeakWhiteFurnaceRuns,
            0.6f,
            0.6f,
            WeakWhiteFurnaceAngleStep,
            result,
            2.0f);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    // Expect StdMDF and GGXMDF to be the same for gamma = 2

    TEST_CASE(StdMDF_2_GGXMDF_G1_comparison)
    {
        const StdMDF std;
        const GGXMDF ggx;

        const size_t num_sample = 128;
        for (size_t i = 0; i < num_sample; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, num_sample, i);
            const Vector3f v = sample_hemisphere_uniform(s);
            const float std_G1 = std.G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, 2.0f);
            const float ggx_G1 = ggx.G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, 2.0f);
            EXPECT_FEQ_EPS(std_G1, ggx_G1, 0.01f);
        }
    }

    // Check for lambda overflow

    TEST_CASE(StdMDF_lambda_overflow)
    {
        const StdMDF std;

        const float gamma_max = 40.0f;
        const float gamma_step = 2.0f;
        const size_t num_sample = 128;
        for (float i = 10.0f; i <= gamma_max; i += gamma_step)
        {
            for (size_t j = 0; j < num_sample; ++j)
            {
                static const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, num_sample, j);
                const Vector3f v = sample_hemisphere_uniform(s);
                const float std_G1 = std.G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, i);
                EXPECT_TRUE(foundation::FP<float>::is_finite(std_G1)); // check that G1 doesn't produce NaN
            }
        }

    }

    // Check for D overflow

    TEST_CASE(StdMDF_D_overflow)
    {
        const StdMDF std;

        const float gamma_max = 40.0f;
        const float gamma_step = 2.0f;
        const size_t num_sample = 128;
        for (float i = 10.0f; i <= gamma_max; i += gamma_step)
        {
            for (size_t j = 0; j < num_sample; ++j)
            {
                static const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, num_sample, j);
                const Vector3f h = sample_hemisphere_uniform(s);
                const float std_D = std.D(h, 0.5f, 0.5f, i);
                EXPECT_TRUE(foundation::FP<float>::is_finite(std_D)); // check that D doesn't produce NaN
            }
        }

    }

#undef EXPECT_WEAK_WHITE_FURNACE_PASS
}
