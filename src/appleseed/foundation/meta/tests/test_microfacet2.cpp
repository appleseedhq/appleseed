
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
#include "foundation/utility/makevector.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/test.h"

// boost headers.
#include <boost/mpl/assert.hpp>
#include <boost/mpl/not.hpp>

// Standard headers.
#include <cmath>
#include <cstddef>
#include <string>

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
            Vector<typename MDF::ValueType,3> h = sample_hemisphere_uniform(s);
            const double value = mdf.D(h, alpha_x, alpha_y);

            if (value < 0.0)
                return false;
        }

        return true;
    }

    template <typename MDF>
    typename MDF::ValueType integrate_quadrature(
        const MDF&                      mdf,
        const typename MDF::ValueType   alpha_x,
        const typename MDF::ValueType   alpha_y,
        const size_t                    sample_count)
    {
        BOOST_MPL_ASSERT((boost::mpl::not_<typename MDF::IsAnisotropicType>));

        typedef typename MDF::ValueType RealType;
        
        Vector<RealType,3> h(0.0);
        RealType integral = 0.0;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const RealType theta = radical_inverse_base2<double>(i) * HalfPi;
            h.y = cos(theta);
            const RealType sin_theta = sin(theta);

            const RealType value = mdf.D(h, alpha_x, alpha_y);

            integral += value * h.y * sin_theta;
        }

        integral *= RealType(HalfPi) / sample_count;  // integration over theta
        integral *= TwoPi;                  // integration over phi

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
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
    const size_t FunctionPlotSampleCount = 256;
    const size_t FunctionSamplingSampleCount = 64;
    const double IntegrationEps = 1.0e-3;

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

        const double integral = integrate_quadrature(mdf, 10.0, 10.0, IntegrationSampleCount);

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

        const double integral = integrate_quadrature(mdf, 0.5, 0.5, IntegrationSampleCount);

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

        const double integral = integrate_quadrature(mdf, 0.5, 0.5, IntegrationSampleCount);

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
}
