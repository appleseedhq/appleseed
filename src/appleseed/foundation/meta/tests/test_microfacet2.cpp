
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
        Vector<typename MDF::ValueType,3> h(0);
        
        for (size_t i = 0; i < sample_count; ++i)
        {
            const double theta = radical_inverse_base2<double>(i) * HalfPi;
            h.y = cos(theta);
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
        typedef typename MDF::ValueType RealType;
        
        Vector<RealType,3> h(0);
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
        static Vector<T, 3> sample(const Vector<T, 2>& s)
        {
            return sample_hemisphere_uniform(s);
        }

        static T pdf(const Vector<T, 3>& v)
        {
            return RcpTwoPi;
        }
    };

    template <typename T>
    struct CosineHemisphereSampler
    {
        static Vector<T, 3> sample(const Vector<T, 2>& s)
        {
            return sample_hemisphere_cosine(s);
        }

        static T pdf(const Vector<T, 3>& v)
        {
            return v.y * RcpPi;
        }
    };

    /*
    template <typename T, typename MDF>
    struct ImportanceSampler
    {
        const MDF& m_mdf;

        explicit ImportanceSampler(const MDF& mdf)
          : m_mdf(mdf)
        {
        }

        Vector<T, 3> sample(const Vector<T, 2>& s) const
        {
            return m_mdf.sample(s);
        }

        T pdf(const Vector<T, 3>& v) const
        {
            return m_mdf.evaluate_pdf(v.y);
        }
    };
    */
    /*
    template <typename MDF, typename Sampler>
    double integrate_sampling(
        const MDF&      mdf,
        const Sampler&  sampler,
        const size_t    sample_count)
    {
        double integral = 0.0;

        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, sample_count);

            const Vector3d w = sampler.sample(s);
            const double pdf = sampler.pdf(w);
            const double cos_theta = w.y;

            const double value = mdf.evaluate(cos_theta);
            const double sample = value / pdf;

            integral += sample * cos_theta;
        }

        integral /= static_cast<double>(sample_count);

        return integral;
    }
    */
    /*
    template <typename MDF>
    void plot(
        MapleFile&      file,
        const string&   name,
        const MDF&      mdf,
        const size_t    point_count,
        const size_t    sample_count)
    {
        vector<double> angles(point_count);
        vector<double> densities(point_count);

        for (size_t i = 0; i < point_count; ++i)
        {
            const double angle =
                fit(
                    static_cast<double>(i), 0.0, static_cast<double>(point_count - 1),
                    -HalfPi, +HalfPi);
            const double cos_angle = cos(angle);

            angles[i] = rad_to_deg(angle);
            densities[i] = mdf.evaluate(cos_angle) * cos_angle;
        }

        vector<double> angle_samples(sample_count);
        vector<double> density_samples(sample_count);

        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, sample_count);
            const Vector3d w = mdf.sample(s);
            const double cos_angle = w.y;
            const double angle = acos(cos_angle) * (w.x < 0.0 ? -1.0 : 1.0);

            angle_samples[i] = rad_to_deg(angle);
            density_samples[i] = mdf.evaluate(cos_angle) * cos_angle;
        }

        file.define(name, angles, densities);
        file.define(name + "_samples", angle_samples, density_samples);

        file.plot(
            make_vector(
                MaplePlotDef(name)
                    .set_legend("Microfacet Distribution Function (" + name + ")"),
                MaplePlotDef(name + "_samples")
                    .set_legend("Integration Samples")
                    .set_style("point")
                    .set_color("red")));
    }
    */
    //
    // Test settings.
    //

    const size_t PositivityTestSampleCount = 256;
    const size_t IntegrationSampleCount = 8192;
    const size_t FunctionPlotSampleCount = 256;
    const size_t FunctionSamplingSampleCount = 64;
    const double IntegrationEps = 1.0e-3;

    /*
    //
    // Blinn-Phong MDF.
    //

    TEST_CASE(BlinnMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BlinnMDF<double> mdf(10.0);

        EXPECT_TRUE(is_positive(mdf, PositivityTestSampleCount));
    }

    TEST_CASE(BlinnMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF<double> mdf(10.0);

        const double limit = mdf.evaluate(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BlinnMDF_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const BlinnMDF<double> mdf(10.0);

        const double limit = mdf.evaluate_pdf(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BlinnMDF_IntegratedViaQuadrature_EqualsOne)
    {
        const BlinnMDF<double> mdf(10.0);

        const double integral = integrate_quadrature(mdf, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF_IntegratedViaUniformSampling_EqualsOne)
    {
        const BlinnMDF<double> mdf(10.0);

        const double integral =
            integrate_sampling(
                mdf,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const BlinnMDF<double> mdf(10.0);

        const double integral =
            integrate_sampling(
                mdf,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF_IntegratedViaImportanceSampling_EqualsOne)
    {
        const BlinnMDF<double> mdf(10.0);

        const double integral =
            integrate_sampling(
                mdf,
                ImportanceSampler<double, BlinnMDF<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BlinnMDF_GeneratePlotFiles)
    {
        MapleFile file("unit tests/outputs/test_microfacet_blinn.mpl");

        plot(file, "blinn_1",  BlinnMDF<double>( 1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "blinn_10", BlinnMDF<double>(10.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "blinn_50", BlinnMDF<double>(50.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }
    */
    
    /*
    //
    // Beckmann MDF.
    //

    TEST_CASE(BeckmannMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const BeckmannMDF<double> mdf(0.5);

        EXPECT_TRUE(is_positive(mdf, PositivityTestSampleCount));
    }

    TEST_CASE(BeckmannMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double limit = mdf.evaluate(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BeckmannMDF_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double limit = mdf.evaluate_pdf(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(BeckmannMDF_IntegratedViaQuadrature_EqualsOne)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double integral = integrate_quadrature(mdf, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_IntegratedViaUniformSampling_EqualsOne)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_IntegratedViaImportanceSampling_EqualsOne)
    {
        const BeckmannMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                ImportanceSampler<double, BeckmannMDF<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(BeckmannMDF_GeneratePlotFiles)
    {
        MapleFile file("unit tests/outputs/test_microfacet_beckmann.mpl");

        plot(file, "beckmann_0_1", BeckmannMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "beckmann_0_5", BeckmannMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "beckmann_1_0", BeckmannMDF<double>(1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }
    */
    
    /*
    //
    // Ward MDF.
    //

    TEST_CASE(WardMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const WardMDF<double> mdf(0.5);

        EXPECT_TRUE(is_positive(mdf, PositivityTestSampleCount));
    }

    TEST_CASE(WardMDF_Evaluate_GivenCosThetaIsZero_ReturnsZero)
    {
        const WardMDF<double> mdf(0.5);

        const double limit = mdf.evaluate(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(WardMDF_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const WardMDF<double> mdf(0.5);

        const double limit = mdf.evaluate_pdf(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(WardMDF_GeneratePlotFiles)
    {
        MapleFile file("unit tests/outputs/test_microfacet_ward.mpl");

        plot(file, "ward_0_1", WardMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "ward_0_5", WardMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "ward_1_0", WardMDF<double>(1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }
    */
    
    /*
    //
    // GGX MDF.
    //

    TEST_CASE(GGXMDF_Evaluate_ReturnsNonNegativeValues)
    {
        const GGXMDF<double> mdf(0.5);

        EXPECT_TRUE(is_positive(mdf, PositivityTestSampleCount));
    }

    TEST_CASE(GGXMDF_Evaluate_GivenCosThetaIsZero_ReturnsLimitValue)
    {
        const double AlphaG = 0.5;
        const GGXMDF<double> mdf(AlphaG);
        const double ExpectedLimit = AlphaG * AlphaG * RcpPi;

        const double limit = mdf.evaluate(0.0);

        EXPECT_FEQ(ExpectedLimit, limit);
    }

    TEST_CASE(GGXMDF_EvaluatePDF_GivenCosThetaIsZero_ReturnsZero)
    {
        const GGXMDF<double> mdf(0.5);

        const double limit = mdf.evaluate_pdf(0.0);

        EXPECT_FEQ(0.0, limit);
    }

    TEST_CASE(GGXMDF_IntegratedViaQuadrature_EqualsOne)
    {
        const GGXMDF<double> mdf(0.5);

        const double integral = integrate_quadrature(mdf, IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_IntegratedViaUniformSampling_EqualsOne)
    {
        const GGXMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                UniformHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_IntegratedViaCosineWeightedSampling_EqualsOne)
    {
        const GGXMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                CosineHemisphereSampler<double>(),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_IntegratedViaImportanceSampling_EqualsOne)
    {
        const GGXMDF<double> mdf(0.5);

        const double integral =
            integrate_sampling(
                mdf,
                ImportanceSampler<double, GGXMDF<double> >(mdf),
                IntegrationSampleCount);

        EXPECT_FEQ_EPS(1.0, integral, IntegrationEps);
    }

    TEST_CASE(GGXMDF_GeneratePlotFiles)
    {
        MapleFile file("unit tests/outputs/test_microfacet_ggx.mpl");

        plot(file, "ggx_0_1", GGXMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "ggx_0_5", GGXMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot(file, "ggx_2_0", GGXMDF<double>(2.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }
    */
}
