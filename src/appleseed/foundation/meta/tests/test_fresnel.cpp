
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
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Math_Fresnel)
{
    TEST_CASE(FresnelReflectanceDielectricSchlick_GivenCosThetaIsZero_ReturnsOne)
    {
        const RegularSpectrum31f NormalReflectance(42.0f);

        RegularSpectrum31f result;
        fresnel_reflectance_dielectric_schlick(result, NormalReflectance, 0.0);

        EXPECT_EQ(RegularSpectrum31f(1.0f), result);
    }

    TEST_CASE(FresnelReflectanceDielectricSchlick_GivenCosThetaIsOne_ReturnsNormalReflectance)
    {
        const RegularSpectrum31f NormalReflectance(42.0f);

        RegularSpectrum31f result;
        fresnel_reflectance_dielectric_schlick(result, NormalReflectance, 1.0);

        EXPECT_EQ(NormalReflectance, result);
    }

    TEST_CASE(FresnelReflectanceDielectric_WhenSwappingEtaAndRcpEta_ReturnsIdenticalValues)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double eta = rand_double1(rng, 0.5, 2.0);

            double cos_theta_i, sin_theta_t2;
            do
            {
                cos_theta_i = rand_double1(rng);
                sin_theta_t2 = (1.0 - square(cos_theta_i)) / square(eta);
            }
            while (sin_theta_t2 > 1.0);
            const double cos_theta_t = std::sqrt(1.0 - sin_theta_t2);

            double fr_eta, fr_rcp_eta;
            fresnel_reflectance_dielectric(fr_eta, eta, cos_theta_i, cos_theta_t);
            fresnel_reflectance_dielectric(fr_rcp_eta, 1.0 / eta, cos_theta_i, cos_theta_t);

            EXPECT_FEQ_EPS(fr_eta, fr_rcp_eta, 1.0e-12);
        }
    }

    TEST_CASE(FresnelTransmittanceDielectric_WhenSwappingEtaAndRcpEtaAndSwappingNormal_ReturnsIdenticalValues)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double eta = rand_double1(rng, 0.5, 2.0);

            double cos_theta_i, sin_theta_t2;
            do
            {
                cos_theta_i = rand_double1(rng);
                const double sin_theta_i2 = 1.0 - square(cos_theta_i);
                sin_theta_t2 = sin_theta_i2 * square(eta);
            }
            while (sin_theta_t2 > 1.0);

            const double cos_theta_t = std::sqrt(1.0 - sin_theta_t2);

            double tr_eta, tr_rcp_eta;
            fresnel_transmittance_dielectric(tr_eta, eta, cos_theta_i);
            fresnel_transmittance_dielectric(tr_rcp_eta, 1.0 / eta, cos_theta_t);

            EXPECT_FEQ_EPS(tr_eta, tr_rcp_eta, 1.0e-8);
        }
    }

    TEST_CASE(PlotFresnelReflectanceTransmittanceDielectric)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Fresnel Reflectance/Transmittance for a Dielectric, eta = 1.0 / 1.5");
        plotfile.set_xlabel("Theta");
        plotfile.set_ylabel("Reflectance");

        const double Eta = 1.0 / 1.5;
        const size_t PointCount = 256;
        std::vector<Vector2d> reflectance_points, transmittance_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double theta_i = fit<size_t, double>(i, 0, PointCount - 1, 0.0, 90.0);
            const double cos_theta_i = std::cos(deg_to_rad(theta_i));

            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double sin_theta_t2 = sin_theta_i2 * square(Eta);
            const double cos_theta_t2 = 1.0 - sin_theta_t2;
            assert(cos_theta_t2 >= 0.0);
            const double cos_theta_t = std::sqrt(cos_theta_t2);

            double reflectance;
            fresnel_reflectance_dielectric(reflectance, Eta, cos_theta_i, cos_theta_t);

            double transmittance;
            fresnel_transmittance_dielectric(transmittance, Eta, cos_theta_i);

            reflectance_points.emplace_back(theta_i, reflectance);
            transmittance_points.emplace_back(theta_i, transmittance);
        }

        plotfile
            .new_plot()
            .set_points(reflectance_points)
            .set_title("Reflectance");

        plotfile
            .new_plot()
            .set_points(transmittance_points)
            .set_title("Transmittance");

        plotfile.write("unit tests/outputs/test_fresnel_reflectance_transmittance_dielectric.gnuplot");
    }

    TEST_CASE(PlotFresnelReflectanceDielectricSchlick)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Fresnel Reflectance for a Dielectric, Schlick Approximation, eta = 1.0 / 1.5");
        plotfile.set_xlabel("Theta");
        plotfile.set_ylabel("Reflectance");

        const double Eta = 1.0 / 1.5;
        const size_t PointCount = 256;
        std::vector<Vector2d> ref_refl_points, schlick_refl_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double theta_i = fit<size_t, double>(i, 0, PointCount - 1, 0.0, 90.0);
            const double cos_theta_i = std::cos(deg_to_rad(theta_i));

            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double sin_theta_t2 = sin_theta_i2 * square(Eta);
            const double cos_theta_t2 = 1.0 - sin_theta_t2;
            assert(cos_theta_t2 >= 0.0);
            const double cos_theta_t = std::sqrt(cos_theta_t2);

            double ref_refl;
            fresnel_reflectance_dielectric(ref_refl, Eta, cos_theta_i, cos_theta_t);

            double r0;
            normal_reflectance_dielectric(r0, Eta);

            double schlick_refl;
            fresnel_reflectance_dielectric_schlick(schlick_refl, r0, cos_theta_i);

            ref_refl_points.emplace_back(theta_i, ref_refl);
            schlick_refl_points.emplace_back(theta_i, schlick_refl);
        }

        plotfile
            .new_plot()
            .set_points(ref_refl_points)
            .set_title("Exact");

        plotfile
            .new_plot()
            .set_points(schlick_refl_points)
            .set_title("Schlick");

        plotfile.write("unit tests/outputs/test_fresnel_reflectance_dielectric_schlick.gnuplot");
    }

    double integrate_diffuse_fresnel_reflectance(const double eta)
    {
        const size_t SampleCount = 1024;
        MersenneTwister rng;

        double integral = 0.0;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const Vector3d wi = sample_hemisphere_cosine(rand_vector2<Vector2d>(rng));
            const double cos_theta_i = wi[1];

            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double sin_theta_t2 = sin_theta_i2 * square(eta);
            const double cos_theta_t2 = 1.0 - sin_theta_t2;

            double reflectance;
            if (cos_theta_t2 >= 0.0)
            {
                const double cos_theta_t = std::sqrt(cos_theta_t2);
                fresnel_reflectance_dielectric(reflectance, eta, cos_theta_i, cos_theta_t);
            }
            else
            {
                // Total internal reflection.
                reflectance = 1.0;
            }

            const double value = reflectance * cos_theta_i;
            const double pdf = cos_theta_i * RcpPi<double>();

            integral += value / pdf;
        }

        // todo: either we are computing the *average* internal diffuse reflectance,
        // and we should divide by 2 * Pi, or we are just computing the integral, in
        // which case we shouldn't divide at all. In any case, this allows to match
        // the polynomial approximation perfectly.
        integral *= RcpPi<double>();

        return integral / SampleCount;
    }

    TEST_CASE(PlotInternalDiffuseFresnelReflectance)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Internal Diffuse Fresnel Reflectance");
        plotfile.set_xlabel("Eta");
        plotfile.set_ylabel("Fdr");

        const size_t PointCount = 256;
        std::vector<Vector2d> integral_points, approx_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double eta = fit<size_t, double>(i, 0, PointCount - 1, 0.5, 2.0);

            integral_points.emplace_back(
                eta,
                integrate_diffuse_fresnel_reflectance(eta));

            approx_points.emplace_back(
                eta,
                fresnel_internal_diffuse_reflectance(eta));
        }

        plotfile
            .new_plot()
            .set_points(integral_points)
            .set_title("Numerical Integration")
            .set_color("blue");

        plotfile
            .new_plot()
            .set_points(approx_points)
            .set_title("Polynomial Approximation")
            .set_color("orange");

        plotfile.write("unit tests/outputs/test_fresnel_internal_diffuse_reflectance.gnuplot");
    }

    TEST_CASE(FresnelReflectanceDielectric_Limits)
    {
        const double Eta = 1.0;
        const double Eps = 1.0e-14;
        double fr;

        fresnel_reflectance_dielectric(fr, Eta, 0.0, 0.0); EXPECT_EQ(1.0, fr);
        fresnel_reflectance_dielectric(fr, Eta, 0.0, Eps); EXPECT_FEQ(1.0, fr);
        fresnel_reflectance_dielectric(fr, Eta, 0.0, 1.0); EXPECT_EQ(1.0, fr);
        fresnel_reflectance_dielectric(fr, Eta, Eps, 0.0); EXPECT_FEQ(1.0, fr);
        fresnel_reflectance_dielectric(fr, Eta, 1.0, 0.0); EXPECT_EQ(1.0, fr);
        fresnel_reflectance_dielectric(fr, Eta, 1.0, 1.0); EXPECT_EQ(0.0, fr);
    }

    TEST_CASE(FresnelTransmittanceDielectric_Limits)
    {
        const double Eta = 1.0;
        const double Eps = 1.0e-14;
        double tr;

        fresnel_transmittance_dielectric(tr, Eta, 0.0); EXPECT_EQ(0.0, tr);
        fresnel_transmittance_dielectric(tr, Eta, Eps); EXPECT_FEQ(0.0, tr);
        fresnel_transmittance_dielectric(tr, Eta, 1.0); EXPECT_EQ(1.0, tr);
    }

    // Source: http://refractiveindex.info
    // Wavelengths: 645, 526 and 444 nm.
    static const double CopperN[3] = {0.2804, 0.8541, 1.3284};
    static const double CopperK[3] = {3.5586, 2.4519, 2.2949};

    TEST_CASE(FresnelReflectanceConductor_GivenCosThetaIsZero_ReturnsOne)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            double result;
            fresnel_reflectance_conductor(result, CopperN[i], CopperK[i], 0.0);
            EXPECT_FEQ(1.0, result);
        }
    }

    TEST_CASE(FresnelReflectanceConductor_GivenCosThetaIsZero_ReturnsNormalReflectance)
    {
        for (size_t i = 0; i < 3; ++i)
        {
            double result;
            fresnel_reflectance_conductor(
                result,
                CopperN[i],
                CopperK[i],
                1.0);

            const double expected =
                    (square(CopperN[i] - 1.0) + square(CopperK[i])) /
                    (square(CopperN[i] + 1.0) + square(CopperK[i]));
            EXPECT_FEQ(expected, result);
        }
    }

    TEST_CASE(PlotFresnelConductorReflectance)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Fresnel Reflectance for a Conductor (copper)");
        plotfile.set_xlabel("Theta");
        plotfile.set_ylabel("Reflectance");
        plotfile.set_xrange(0.0, 90.0);
        plotfile.set_yrange(0.3, 1.0);

        const size_t PointCount = 256;
        std::vector<Vector2d> red_points, green_points, blue_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double theta_i = fit<size_t, double>(i, 0, PointCount - 1, 0.0, 90.0);
            const double cos_theta_i = std::cos(deg_to_rad(theta_i));

            double result;
            fresnel_reflectance_conductor(
                result,
                CopperN[0],
                CopperK[0],
                cos_theta_i);
            red_points.emplace_back(theta_i, result);

            fresnel_reflectance_conductor(
                result,
                CopperN[1],
                CopperK[1],
                cos_theta_i);
            green_points.emplace_back(theta_i, result);

            fresnel_reflectance_conductor(
                result,
                CopperN[2],
                CopperK[2],
                cos_theta_i);
            blue_points.emplace_back(theta_i, result);
        }

        plotfile
            .new_plot()
            .set_points(red_points)
            .set_title("R")
            .set_color("red");

        plotfile
            .new_plot()
            .set_points(green_points)
            .set_title("G")
            .set_color("green");

        plotfile
            .new_plot()
            .set_points(blue_points)
            .set_title("B")
            .set_color("blue");

        plotfile.write("unit tests/outputs/test_fresnel_conductor_reflectance.gnuplot");
    }

    TEST_CASE(ArtistFriendlyFresnelConductorReparamRoundtrip)
    {
        // Presets from alShaders:
        // https://bitbucket.org/anderslanglands/alshaders/
        static const double R[24] =
        {
            0.914, 0.921, 0.921,
            0.548, 0.549, 0.570,
            0.985, 0.649, 0.546,
            0.990, 0.791, 0.346,
            0.679, 0.642, 0.582,
            0.970, 0.959, 0.924,
            0.550, 0.501, 0.447,
            0.504, 0.495, 0.475
        };

        static const double G[24] =
        {
            0.971, 0.979, 0.989,
            0.579, 0.598, 0.620,
            0.996, 0.918, 0.859,
            0.990, 0.980, 0.792,
            0.785, 0.789, 0.783,
            0.999, 0.999, 0.998,
            0.689, 0.683, 0.693,
            0.403, 0.419, 0.422
        };

        for (size_t i = 0; i < 24; ++i)
        {
            double n, k;
            artist_friendly_fresnel_conductor_reparameterization(
                R[i],
                G[i],
                n,
                k);

            double r, g;
            artist_friendly_fresnel_conductor_inverse_reparameterization(
                n,
                k,
                r,
                g);

            EXPECT_FEQ(R[i], r);
            EXPECT_FEQ(G[i], g);
        }
    }

    TEST_CASE(PlotArtistFriendlyFresnelConductor)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Fresnel Reflectance for a Conductor, Artist friendly reparameterization.");
        plotfile.set_xlabel("Theta");
        plotfile.set_ylabel("Reflectance");
        plotfile.set_xrange(0.0, 90.0);
        plotfile.set_yrange(0.3, 1.0);

        const double normal_reflectance = 0.4;
        static const double edge_tint[3] = {1.0, 0.5, 0.1};

        const size_t PointCount = 256;
        std::vector<Vector2d> red_points, green_points, blue_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const double theta_i = fit<size_t, double>(i, 0, PointCount - 1, 0.0, 90.0);
            const double cos_theta_i = std::cos(deg_to_rad(theta_i));

            double result;
            artist_friendly_fresnel_reflectance_conductor(
                result,
                normal_reflectance,
                edge_tint[0],
                cos_theta_i);
            red_points.emplace_back(theta_i, result);

            artist_friendly_fresnel_reflectance_conductor(
                result,
                normal_reflectance,
                edge_tint[1],
                cos_theta_i);
            green_points.emplace_back(theta_i, result);

            artist_friendly_fresnel_reflectance_conductor(
                result,
                normal_reflectance,
                edge_tint[2],
                cos_theta_i);
            blue_points.emplace_back(theta_i, result);
        }

        plotfile
            .new_plot()
            .set_points(red_points)
            .set_title("R")
            .set_color("red");

        plotfile
            .new_plot()
            .set_points(green_points)
            .set_title("G")
            .set_color("green");

        plotfile
            .new_plot()
            .set_points(blue_points)
            .set_title("B")
            .set_color("blue");

        plotfile.write("unit tests/outputs/test_artist_friendly_fresnel_conductor_reflectance.gnuplot");
    }
}
