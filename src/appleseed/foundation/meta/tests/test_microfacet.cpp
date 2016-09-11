
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
        const MDF&                      mdf,
        const typename MDF::ValueType   alpha_x,
        const typename MDF::ValueType   alpha_y,
        const size_t                    sample_count)
    {
        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, sample_count, i);

            const Vector<typename MDF::ValueType, 3> h = sample_hemisphere_uniform(s);
            const double value = mdf.D(h, alpha_x, alpha_y);

            if (value < 0.0)
                return false;
        }

        return true;
    }

    template <typename MDF>
    typename MDF::ValueType integrate(
        const MDF&                      mdf,
        const typename MDF::ValueType   alpha,
        const size_t                    sample_count)
    {
        typedef typename MDF::ValueType ValueType;

        ValueType integral = ValueType(0.0);

        for (size_t i = 0; i < sample_count; ++i)
        {
            const ValueType theta = radical_inverse_base2<ValueType>(i) * ValueType(HalfPi);
            const Vector<ValueType, 3> h(ValueType(0.0), cos(theta), ValueType(0.0));
            const ValueType value = mdf.D(h, alpha, alpha);

            integral += value * h.y * sin(theta);
        }

        integral *= ValueType(HalfPi) / sample_count;   // integration over theta
        integral *= ValueType(TwoPi);                   // integration over phi

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
        double m_min_G1;
        double m_max_G1;
        double m_min_result;
        double m_max_result;
    };

    template <typename MDF>
    void weak_white_furnace_test(
        size_t                          num_runs,
        const double                    alpha_x,
        const double                    alpha_y,
        const double                    angle_step,
        WeakWhiteFurnaceTestResult& result)
    {
        result.m_min_G1 =  numeric_limits<double>::max();
        result.m_max_G1 = -numeric_limits<double>::max();
        result.m_min_result =  numeric_limits<double>::max();
        result.m_max_result = -numeric_limits<double>::max();

        MDF mdf;

        for (size_t i = 0; i < num_runs; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, num_runs, i);
            const Vector3d v = sample_hemisphere_uniform(s);
            const double G1 = mdf.G1(v, Vector3d(0.0, 1.0, 0.0), alpha_x, alpha_y);

            result.m_min_G1 = std::min(result.m_min_G1, G1);
            result.m_max_G1 = std::max(result.m_max_G1, G1);

            const double cos_thetha_o_4 = std::abs(4.0 * v.y);

            double integral = 0.0;

            for (double theta = 0.0; theta < Pi; theta += angle_step)
            {
                const double cos_theta = std::cos(theta);
                const double sin_theta = std::sin(theta);

                for (double phi = 0.0; phi < TwoPi; phi += angle_step)
                {
                    const double cos_phi = std::cos(phi);
                    const double sin_phi = std::sin(phi);

                    const Vector3d l =
                        Vector3d::make_unit_vector(
                            cos_theta,
                            sin_theta,
                            cos_phi,
                            sin_phi);

                    const Vector3d h = normalize(v + l);

                    if (h.y > 0.0)
                        integral += sin_theta * mdf.D(h, alpha_x, alpha_y) * G1 / cos_thetha_o_4;
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
    EXPECT_FEQ_EPS(1.0, result.m_min_result, WeakWhiteFurnaceEps); \
    EXPECT_FEQ_EPS(1.0, result.m_max_result, WeakWhiteFurnaceEps);

    //
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
    const size_t FunctionPlotSampleCount = 256;
    const size_t FunctionSamplingSampleCount = 64;
    const double IntegrationEps = 1.0e-3;
    const size_t WeakWhiteFurnaceRuns = 128;
    const double WeakWhiteFurnaceAngleStep = 0.0125;
    const double WeakWhiteFurnaceEps = 0.05;


    //
    // Blinn-Phong MDF.
    //

    TEST_CASE(BlinnMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BlinnMDF<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 10.0, 10.0, PositivityTestSampleCount));
    }

    TEST_CASE(BlinnMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF<double> mdf;
        const double limit = mdf.D(Vector3d(0.0), 10.0, 10.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BlinnMDF_Integral_EqualsOne)
    {
        const BlinnMDF<double> mdf;

        const double integral = integrate(mdf, 10.0, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }


    //
    // Beckmann MDF.
    //

    TEST_CASE(BeckmannMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BeckmannMDF<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5, 0.5, PositivityTestSampleCount));
    }

    TEST_CASE(BeckmannMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF<double> mdf;

        const double limit = mdf.D(Vector3d(0.0), 0.5, 0.5);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BeckmannMDF_Integral_EqualsOne)
    {
        const BeckmannMDF<double> mdf;

        const double integral = integrate(mdf, 0.5, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<BeckmannMDF<double> >(
            WeakWhiteFurnaceRuns,
            0.6,
            0.6,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    TEST_CASE(BeckmannMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<BeckmannMDF<double> >(
            WeakWhiteFurnaceRuns,
            0.25,
            0.5,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }


    //
    // GGX MDF.
    //

    TEST_CASE(GGXMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const GGXMDF<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5, 0.5, PositivityTestSampleCount));
    }

    TEST_CASE(GGXMDF_Evaluate_GivenCosThetaIsZero_ReturnsLimitValue)
    {
        const double AlphaG = 0.5;
        const GGXMDF<double> mdf;
        const double ExpectedLimit = AlphaG * AlphaG * RcpPi;

        const double limit = mdf.D(Vector3d(0.0), AlphaG, AlphaG);

        EXPECT_FEQ(ExpectedLimit, limit);
    }

    TEST_CASE(GGXMDF_Integral_EqualsOne)
    {
        const GGXMDF<double> mdf;

        const double integral = integrate(mdf, 0.5, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_Isotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GGXMDF<double> >(
            WeakWhiteFurnaceRuns,
            0.35,
            0.35,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    TEST_CASE(GGXMDF_Anisotropic_WeakWhiteFurnace)
    {
        WeakWhiteFurnaceTestResult result;
        weak_white_furnace_test<GGXMDF<double> >(
            WeakWhiteFurnaceRuns,
            0.25,
            0.5,
            WeakWhiteFurnaceAngleStep,
            result);

        EXPECT_WEAK_WHITE_FURNACE_PASS(result)
    }

    //
    // Ward MDF.
    //

    TEST_CASE(WardMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const WardMDF<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5, 0.5, PositivityTestSampleCount));
    }

    TEST_CASE(WardMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const WardMDF<double> mdf;

        const double limit = mdf.D(Vector3d(0.0), 0.5, 0.5);

        EXPECT_FEQ(0.0, limit);
    }


    //
    // Berry MDF.
    //

    TEST_CASE(BerryMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BerryMDF<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 10.0, 10.0, PositivityTestSampleCount));
    }

    TEST_CASE(BerryMDF_Integral_EqualsOne)
    {
        const BerryMDF<double> mdf;

        const double integral = integrate(mdf, 10.0, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }
}
