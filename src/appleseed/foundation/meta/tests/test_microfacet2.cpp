
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/microfacet2.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Microfacet2)
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
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, sample_count);

            const Vector<typename MDF::ValueType, 3> h = sample_hemisphere_uniform(s);
            const double value = mdf.D(h, alpha_x, alpha_y);

            if (value < 0.0)
                return false;
        }

        return true;
    }

    template <typename MDF>
    typename MDF::ValueType integrate_quadrature(
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

    template <typename T>
    struct UniformHemisphereSampler
    {
        Vector<T, 3> sample(
            const Vector<T, 2>& s,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return sample_hemisphere_uniform(s);
        }

        T pdf(
            const Vector<T, 3>& v,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return RcpTwoPi;
        }
    };

    template <typename T>
    struct CosineHemisphereSampler
    {
        Vector<T, 3> sample(
            const Vector<T, 2>& s,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return sample_hemisphere_cosine(s);
        }

        T pdf(
            const Vector<T, 3>& v,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return v.y * RcpPi;
        }
    };

    template <typename T, typename MDF>
    struct ImportanceSampler
    {
        const MDF&  m_mdf;

        explicit ImportanceSampler(const MDF&  mdf)
          : m_mdf(mdf)
        {
        }

        Vector<T, 3> sample(
            const Vector<T, 2>& s,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return m_mdf.sample(s, alpha_x, alpha_y);
        }

        T pdf(
            const Vector<T, 3>& v,
            const T             alpha_x,
            const T             alpha_y) const
        {
            return m_mdf.pdf(v, alpha_x, alpha_y);
        }
    };

    template <typename MDF, typename Sampler>
    double integrate_sampling(
        const MDF&      mdf,
        const double    alpha_x,
        const double    alpha_y,
        const Sampler&  sampler,
        const size_t    sample_count)
    {
        double integral = 0.0;

        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, sample_count);

            const Vector3d w = sampler.sample(s, alpha_x, alpha_y);
            const double pdf = sampler.pdf(w, alpha_x, alpha_y);
            const double value = mdf.D(w, alpha_x, alpha_y);
            const double sample = value / pdf;

            integral += sample * w.y;
        }

        integral /= static_cast<double>(sample_count);

        return integral;
    }


    //
    // Weak white furnace test.
    //
    // References:
    //
    //   Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs
    //   http://hal.inria.fr/docs/00/96/78/44/PDF/RR-8468.pdf
    //

    template <typename MDF, typename G>
    double weak_white_furnace_test(
        const MDF&      mdf,
        const G&        g,
        const double    theta_o,
        const double    phi_o,
        const double    alpha_x,
        const double    alpha_y,
        const double    angle_step)
    {
        const Vector3d v = Vector3d::unit_vector(theta_o, phi_o);
        const double cos_thetha_o_4 = abs(4.0 * v.y);
        const double G1 = g.G1(v, Vector3d(0, 1, 0), alpha_x, alpha_y);

        double integral = 0.0;

        for (double theta = 0; theta < Pi; theta += angle_step)
        {
            const double cos_theta = cos(theta);
            const double sin_theta = sin(theta);

            for (double phi = 0; phi < TwoPi; phi += angle_step)
            {
                const double cos_phi = cos(phi);
                const double sin_phi = sin(phi);

                const Vector3d l = Vector3d::unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
                const Vector3d h = normalize(v + l);

                if (h.y > 0.0)
                {
                    const double D = mdf.D(h, alpha_x, alpha_y);
                    integral += sin_theta * D;
                }
            }
        }

        // Result should be 1.
        return integral * G1 * square(angle_step) / cos_thetha_o_4;
    }


    //
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
    const size_t FunctionPlotSampleCount = 256;
    const size_t FunctionSamplingSampleCount = 64;
    const double IntegrationEps = 1.0e-3;
    const double WeakWhiteFurnaceAngleStep = 0.05;
    const double WeakWhiteFurnaceEps = 1.0e-2;


    //
    // Blinn-Phong MDF.
    //

    TEST_CASE(BlinnMDF2_Evaluate_ReturnsNonNegativeValues)
    {
        const BlinnMDF2<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 10.0, 10.0, PositivityTestSampleCount));
    }

    TEST_CASE(BlinnMDF2_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF2<double> mdf;
        const double limit = mdf.D(Vector3d(0.0), 10.0, 10.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BlinnMDF2_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF2<double> mdf;
        const double limit = mdf.pdf(Vector3d(0.0), 10.0, 10.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BlinnMDF2_IntegratedViaQuadrature_EqualsOne)
    {
        const BlinnMDF2<double> mdf;

        const double integral = integrate_quadrature(mdf, 10.0, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF2_IntegratedViaUniformSampling_EqualsOne)
    {
        const BlinnMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                10.0,
                10.0,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF2_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const BlinnMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                10.0,
                10.0,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF2_IntegratedViaImportanceSampling_EqualsOne)
    {
        const BlinnMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                10.0,
                10.0,
                ImportanceSampler<double, BlinnMDF2<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }


    //
    // Beckmann MDF.
    //

    TEST_CASE(BeckmannMDF2_Evaluate_ReturnsNonNegativeValues)
    {
        const BeckmannMDF2<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5, 0.5, PositivityTestSampleCount));
    }

    TEST_CASE(BeckmannMDF2_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF2<double> mdf;

        const double limit = mdf.D(Vector3d(0.0), 0.5, 0.5);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BeckmannMDF2_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF2<double> mdf;

        const double limit = mdf.pdf(Vector3d(0.0), 0.5, 0.5);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BeckmannMDF2_IntegratedViaQuadrature_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral = integrate_quadrature(mdf, 0.5, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_IntegratedViaUniformSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_Anisotropic_IntegratedViaUniformSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.6,
                0.2,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_Anisotropic_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.2,
                0.6,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_IntegratedViaImportanceSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                ImportanceSampler<double, BeckmannMDF2<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_Anisotropic_IntegratedViaImportanceSampling_EqualsOne)
    {
        const BeckmannMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.7,
                0.2,
                ImportanceSampler<double, BeckmannMDF2<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF2_Isotropic_SmithWeakWhiteFurnace)
    {
        const BeckmannSmithMaskingShadowing<double> g;
        const BeckmannMDF2<double> mdf;

        const double integral =
            weak_white_furnace_test(
                mdf,
                g,
                Pi / 7.0,
                0,
                0.25,
                0.25,
                WeakWhiteFurnaceAngleStep);

        EXPECT_FEQ_EPS(1.0, integral, WeakWhiteFurnaceEps);
    }

    TEST_CASE(BeckmannMDF2_Anisotropic_SmithWeakWhiteFurnace)
    {
        const BeckmannSmithMaskingShadowing<double> g;
        const BeckmannMDF2<double> mdf;

        const double integral =
            weak_white_furnace_test(
                mdf,
                g,
                Pi / 5.0,
                Pi / 8.0,
                0.25,
                0.5,
                WeakWhiteFurnaceAngleStep);

        EXPECT_FEQ_EPS(1.0, integral, WeakWhiteFurnaceEps);
    }

    //
    // GGX MDF.
    //

    TEST_CASE(GGXMDF2_Evaluate_ReturnsNonNegativeValues)
    {
        const GGXMDF2<double> mdf;

        EXPECT_TRUE(is_positive(mdf, 0.5, 0.5, PositivityTestSampleCount));
    }

    TEST_CASE(GGXMDF2_Evaluate_GivenCosThetaIsZero_ReturnsLimitValue)
    {
        const double AlphaG = 0.5;
        const GGXMDF2<double> mdf;
        const double ExpectedLimit = AlphaG * AlphaG * RcpPi;

        const double limit = mdf.D(Vector3d(0.0), AlphaG, AlphaG);

        EXPECT_FEQ(ExpectedLimit, limit);
    }

    TEST_CASE(GGXMDF2_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const GGXMDF2<double> mdf;

        const double limit = mdf.pdf(Vector3d(0.0), 0.5, 0.5);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(GGXMDF2_IntegratedViaQuadrature_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral = integrate_quadrature(mdf, 0.5, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_IntegratedViaUniformSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_Anisotropic_IntegratedViaUniformSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.6,
                0.2,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_Anisotropic_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.2,
                0.6,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_IntegratedViaImportanceSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.5,
                0.5,
                ImportanceSampler<double, GGXMDF2<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_Anisotropic_IntegratedViaImportanceSampling_EqualsOne)
    {
        const GGXMDF2<double> mdf;

        const double integral =
            integrate_sampling(
                mdf,
                0.7,
                0.2,
                ImportanceSampler<double, GGXMDF2<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF2_Isotropic_SmithWeakWhiteFurnace)
    {
        const GGXSmithMaskingShadowing<double> g;
        const GGXMDF2<double> mdf;

        const double integral =
            weak_white_furnace_test(
                mdf,
                g,
                Pi / 6.0,
                Pi / 3.0,
                0.35,
                0.35,
                WeakWhiteFurnaceAngleStep);

        EXPECT_FEQ_EPS(1.0, integral, WeakWhiteFurnaceEps);
    }

    TEST_CASE(GGXMDF2_Anisotropic_SmithWeakWhiteFurnace)
    {
        const GGXSmithMaskingShadowing<double> g;
        const GGXMDF2<double> mdf;

        const double integral =
            weak_white_furnace_test(
                mdf,
                g,
                Pi / 5.0,
                Pi / 8.0,
                0.25,
                0.5,
                WeakWhiteFurnaceAngleStep);

        EXPECT_FEQ_EPS(1.0, integral, WeakWhiteFurnaceEps);
    }
}
