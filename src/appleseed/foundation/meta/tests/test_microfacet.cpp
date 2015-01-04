
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <sstream>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Microfacet)
{
    template <typename MDF>
    bool is_positive(
        const MDF&      mdf,
        const size_t    sample_count)
    {
        for (size_t i = 0; i < sample_count; ++i)
        {
            const double theta = radical_inverse_base2<double>(i) * HalfPi;
            const double cos_theta = cos(theta);

            const double value = mdf.evaluate(cos_theta);

            if (value < 0.0)
                return false;
        }

        return true;
    }

    template <typename MDF>
    double integrate_quadrature(
        const MDF&      mdf,
        const size_t    sample_count)
    {
        double integral = 0.0;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const double theta = radical_inverse_base2<double>(i) * HalfPi;
            const double cos_theta = cos(theta);
            const double sin_theta = sin(theta);

            const double value = mdf.evaluate(cos_theta);

            integral += value * cos_theta * sin_theta;
        }

        integral *= HalfPi / sample_count;  // integration over theta
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

    template <typename MDF>
    void plot(
        const string&   mdf_name,
        const double    mdf_param,
        const MDF&      mdf,
        const size_t    point_count,
        const size_t    sample_count)
    {
        vector<Vector2d> densities(point_count);

        for (size_t i = 0; i < point_count; ++i)
        {
            const double angle = fit<size_t, double>(i, 0, point_count - 1, -HalfPi, +HalfPi);
            const double cos_angle = cos(angle);
            const double density = mdf.evaluate(cos_angle) * cos_angle;
            densities[i] = Vector2d(rad_to_deg(angle), density);
        }

        vector<Vector2d> samples(sample_count);

        for (size_t i = 0; i < sample_count; ++i)
        {
            static const size_t Bases[] = { 2 };
            const Vector2d s = hammersley_sequence<double, 2>(Bases, i, sample_count);
            const Vector3d w = mdf.sample(s);
            const double cos_angle = w.y;
            const double angle = acos(cos_angle) * (w.x < 0.0 ? -1.0 : 1.0);
            const double density = mdf.evaluate(cos_angle) * cos_angle;
            samples[i] = Vector2d(rad_to_deg(angle), density);
        }

        GnuplotFile plotfile;
        plotfile.set_title(mdf_name + " Microfacet Distribution Function");
        plotfile.set_xlabel("Angle (degrees)");

        plotfile
            .new_plot()
            .set_points(densities)
            .set_title("Density")
            .set_color("black")
            .set_style("lines");

        plotfile
            .new_plot()
            .set_points(samples)
            .set_title("Integration Samples")
            .set_color("red")
            .set_style("points pointtype 13");

        stringstream filename;
        filename << "test_microfacet";
        filename << "_" << lower_case(mdf_name);
        filename << "_" << replace(pretty_scalar(mdf_param, 1), ".", "_");
        filename << ".gnuplot";

        plotfile.write("unit tests/outputs/" + filename.str());
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
        plot("Blinn",  1.0, BlinnMDF<double>( 1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Blinn", 10.0, BlinnMDF<double>(10.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Blinn", 50.0, BlinnMDF<double>(50.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }


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
        plot("Beckmann", 0.1, BeckmannMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Beckmann", 0.5, BeckmannMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Beckmann", 1.0, BeckmannMDF<double>(1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }


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
        plot("Ward", 0.1, WardMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Ward", 0.5, WardMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("Ward", 1.0, WardMDF<double>(1.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }


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
        plot("GGX", 0.1, GGXMDF<double>(0.1), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("GGX", 0.5, GGXMDF<double>(0.5), FunctionPlotSampleCount, FunctionSamplingSampleCount);
        plot("GGX", 2.0, GGXMDF<double>(2.0), FunctionPlotSampleCount, FunctionSamplingSampleCount);
    }
}
