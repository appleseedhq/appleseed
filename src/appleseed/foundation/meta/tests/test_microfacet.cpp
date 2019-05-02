
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
#include "foundation/math/fp.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

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
        const size_t                num_runs,
        const float                 alpha_x,
        const float                 alpha_y,
        const float                 gamma,
        const float                 angle_step,
        WeakWhiteFurnaceTestResult& result)
    {
        result.m_min_G1 =  numeric_limits<float>::max();
        result.m_max_G1 = -numeric_limits<float>::max();
        result.m_min_result =  numeric_limits<float>::max();
        result.m_max_result = -numeric_limits<float>::max();

        for (size_t i = 0; i < num_runs; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, num_runs, i);
            const Vector3f v = sample_hemisphere_uniform(s);
            const float G1 = MDF::G1(v, Vector3f(0.0f, 1.0, 0.0f), alpha_x, alpha_y, gamma);

            result.m_min_G1 = min(result.m_min_G1, G1);
            result.m_max_G1 = max(result.m_max_G1, G1);

            const float cos_thetha_o_4 = abs(4.0f * v.y);

            float integral = 0.0f;

            for (float theta = 0.0f; theta < Pi<float>(); theta += angle_step)
            {
                const float cos_theta = cos(theta);
                const float sin_theta = sin(theta);

                for (float phi = 0.0f; phi < TwoPi<float>(); phi += angle_step)
                {
                    const float cos_phi = cos(phi);
                    const float sin_phi = sin(phi);

                    const Vector3f l =
                        Vector3f::make_unit_vector(
                            cos_theta,
                            sin_theta,
                            cos_phi,
                            sin_phi);

                    const Vector3f h = normalize(v + l);

                    if (h.y > 0.0f)
                        integral += sin_theta * MDF::D(h, alpha_x, alpha_y, gamma) * G1 / cos_thetha_o_4;
                }
            }

            // Result should be 1.
            integral *= square(angle_step);

            result.m_min_result = min(result.m_min_result, integral);
            result.m_max_result = max(result.m_max_result, integral);
        }
    }

#define EXPECT_WEAK_WHITE_FURNACE_PASS(result)                              \
    do                                                                      \
    {                                                                       \
        EXPECT_NEQ(result.m_min_G1, result.m_max_G1);                       \
        EXPECT_FEQ_EPS(1.0f, result.m_min_result, WeakWhiteFurnaceEps);     \
        EXPECT_FEQ_EPS(1.0f, result.m_max_result, WeakWhiteFurnaceEps);     \
    } while (false)


    //
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
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
        const float limit = BlinnMDF::D(Vector3f(0.0f), 10.0f, 10.0f, 0.0f);

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
        const float limit = BeckmannMDF::D(Vector3f(0.0f), 0.5f, 0.5f, 0.0f);

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
        weak_white_furnace_test<BeckmannMDF>(
            WeakWhiteFurnaceRuns,
            0.6f,
            0.6f,
            0.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
    }

    TEST_CASE(BeckmannMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<BeckmannMDF>(
            WeakWhiteFurnaceRuns,
            0.25f,
            0.5f,
            0.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
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
        const float ExpectedLimit = AlphaG * AlphaG * RcpPi<float>();

        const float limit = GGXMDF::D(Vector3f(0.0f), AlphaG, AlphaG, 0.0f);

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
        weak_white_furnace_test<GGXMDF>(
            WeakWhiteFurnaceRuns,
            0.35f,
            0.35f,
            0.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
    }

    TEST_CASE(GGXMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GGXMDF>(
            WeakWhiteFurnaceRuns,
            0.25f,
            0.5f,
            0.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
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
        const float limit = WardMDF::D(Vector3f(0.0f), 0.5f, 0.5f, 0.0f);

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

    TEST_CASE(GTR1MDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GTR1MDF>(
            WeakWhiteFurnaceRuns,
            0.21f,
            0.21f,
            0.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
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
        const float limit = StdMDF::D(Vector3f(0.0f), 0.5f, 0.5f, 2.0f);

        EXPECT_FEQ(0.0f, limit);
    }

    TEST_CASE(StdMDF_Integral_EqualsOne)
    {
        const StdMDF mdf;

        const float integral = integrate(mdf, 0.5f, IntegrationSampleCount, 2.0);

        EXPECT_FEQ_EPS(1.0f, integral, IntegrationEps);
    }

    TEST_CASE(StdMDF_2_GGXMDF_G1_comparison)
    {
        const size_t SampleCount = 128;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2f s = hammersley_sequence<float, 2>(Bases, SampleCount, i);
            const Vector3f v = sample_hemisphere_uniform(s);
            const float std_G1 = StdMDF::G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, 2.0f);
            const float ggx_G1 = GGXMDF::G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, 2.0f);
            EXPECT_FEQ_EPS(std_G1, ggx_G1, 0.01f);
        }
    }

    TEST_CASE(StdMDF_lambda_overflow)
    {
        const float GammaMax = 40.0f;
        const float GammaStep = 2.0f;
        const size_t SampleCount = 128;

        for (float i = 2.0f; i <= GammaMax; i += GammaStep)
        {
            for (size_t j = 0; j < SampleCount; ++j)
            {
                static const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, SampleCount, j);
                const Vector3f v = sample_hemisphere_uniform(s);
                const float std_G1 = StdMDF::G1(v, Vector3f(0.0f, 1.0f, 0.0f), 0.5f, 0.5f, i);
                EXPECT_TRUE(FP<float>::is_finite(std_G1));  // check that G1 doesn't produce NaN
            }
        }

    }

    TEST_CASE(StdMDF_D_overflow)
    {
        const float GammaMax = 40.0f;
        const float GammaStep = 2.0f;
        const size_t SampleCount = 128;

        for (float i = 2.0f; i <= GammaMax; i += GammaStep)
        {
            for (size_t j = 0; j < SampleCount; ++j)
            {
                static const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, SampleCount, j);
                const Vector3f h = sample_hemisphere_uniform(s);
                const float std_D = StdMDF::D(h, 0.5f, 0.5f, i);
                EXPECT_TRUE(FP<float>::is_finite(std_D));   // check that D doesn't produce NaN
            }
        }
    }

    TEST_CASE(StdMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<StdMDF>(
            WeakWhiteFurnaceRuns,
            0.6f,
            0.6f,
            2.0f,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result);
    }

#undef EXPECT_WEAK_WHITE_FURNACE_PASS
}
