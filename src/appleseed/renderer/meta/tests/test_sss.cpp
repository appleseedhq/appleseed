
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingpointbuilder.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Modeling_BSSRDF_SSS)
{
    //
    // BSSRDF reparameterization.
    //

    template <typename ComputeRdFun>
    double rd_alpha_prime_roundtrip(
        const double rd,
        const double eta)
    {
        const ComputeRdFun f(eta);
        const double alpha_prime = compute_alpha_prime(f, rd);
        return f(alpha_prime);
    }

    const double AlphaPrimeRoundtripTestEps = 0.00001;

    TEST_CASE(Rd_AlphaPrime_Roundtrip)
    {
        EXPECT_FEQ_EPS(0.0, rd_alpha_prime_roundtrip<ComputeRd>(0.0, 1.6), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.1, rd_alpha_prime_roundtrip<ComputeRd>(0.1, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.2, rd_alpha_prime_roundtrip<ComputeRd>(0.2, 1.2), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.4, rd_alpha_prime_roundtrip<ComputeRd>(0.4, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.6, rd_alpha_prime_roundtrip<ComputeRd>(0.6, 1.4), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.8, rd_alpha_prime_roundtrip<ComputeRd>(0.8, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(1.0, rd_alpha_prime_roundtrip<ComputeRd>(1.0, 1.5), AlphaPrimeRoundtripTestEps);
    }

    TEST_CASE(Rd_AlphaPrime_BetterDipole_Roundtrip)
    {
        EXPECT_FEQ_EPS(0.0, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.0, 1.6), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.1, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.1, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.2, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.2, 1.2), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.4, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.4, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.6, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.6, 1.4), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(0.8, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(0.8, 1.3), AlphaPrimeRoundtripTestEps);
        EXPECT_FEQ_EPS(1.0, rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(1.0, 1.5), AlphaPrimeRoundtripTestEps);
    }

    //
    // Gaussian profile.
    //

    TEST_CASE(GaussianProfileIntegration_UniformSampling)
    {
        const double V = 1.0;
        const double RIntegralThreshold = 0.999;
        const double RMax2Constant = -2.0 * log(1.0 - RIntegralThreshold);
        const double RMax2 = V * RMax2Constant;

        const size_t SampleCount = 10000;
        MersenneTwister rng;

        double integral = 0.0;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double u = rand_double2(rng);
            const double r = u * sqrt(RMax2);
            const double pdf_radius = 1.0 / sqrt(RMax2);
            const double pdf_angle = RcpTwoPi;
            const double pdf = pdf_radius * pdf_angle;
            const double value = r * gaussian_profile(r, V, RIntegralThreshold);
            integral += value / pdf;
        }

        integral /= SampleCount;

        EXPECT_FEQ_EPS(1.0, integral, 0.01);
    }

    TEST_CASE(GaussianProfileIntegration_ImportanceSampling)
    {
        const double V = 1.0;
        const double RIntegralThreshold = 0.999;
        const double RMax2Constant = -2.0 * log(1.0 - RIntegralThreshold);
        const double RMax2 = V * RMax2Constant;

        const size_t SampleCount = 1000;
        MersenneTwister rng;

        double integral = 0.0;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double u = rand_double2(rng);
            const double r = gaussian_profile_sample(u, V, RMax2);
            const double pdf = gaussian_profile_pdf(r, V, RIntegralThreshold);
            const double value = gaussian_profile(r, V, RIntegralThreshold);
            integral += value / pdf;
        }

        integral /= SampleCount;

        EXPECT_FEQ(1.0, integral);
    }

    //
    // Normalized diffusion profile.
    //

    const double NormalizedDiffusionTestEps = 0.0001;

    TEST_CASE(NormalizedDiffusionS)
    {
        static const double Expected[] =
        {
            4.68592, 4.11466, 3.77984, 3.60498, 3.52856, 3.5041, 3.50008,
            3.50002, 3.5024, 3.52074, 3.58352, 3.73426, 4.03144, 4.54858,
            5.37416, 6.6117, 8.37968, 10.8116, 14.056, 18.2763, 23.6511
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const double a = fit<size_t, double>(i, 0, countof(Expected) - 1, 0.0, 1.0);
            const double s = normalized_diffusion_s(a);

            EXPECT_FEQ_EPS(Expected[i], s, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionR)
    {
        static const double Expected[] =
        {
            2.53511, 0.674967, 0.327967, 0.192204, 0.124137, 0.0852575,
            0.0611367, 0.0452741, 0.0343737, 0.0266197, 0.0209473, 0.0167009,
            0.0134603, 0.0109471, 0.00897108, 0.00739936, 0.00613676, 0.00511384,
            0.00427902, 0.0035934, 0.00302721
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const double A = 0.5;                           // surface albedo
            const double L = 1.0;                           // mean free path
            const double S = 3.583521;                      // scaling factor for A = 0.5
            const double r = static_cast<double>(i) * 0.1 + 0.05;
            const double value = normalized_diffusion_profile(r, L, S, A);

            EXPECT_FEQ_EPS(Expected[i], value, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionCDF)
    {
        static const double Expected[] =
        {
            0.282838, 0.598244, 0.760091, 0.85267, 0.908478, 0.942885, 0.964293,
            0.97766, 0.98602, 0.99125, 0.994523, 0.996572, 0.997854, 0.998657,
            0.999159, 0.999474, 0.999671, 0.999794, 0.999871, 0.999919, 0.999949,
            0.999968, 0.99998, 0.999988, 0.999992, 0.999995, 0.999997, 0.999998,
            0.999999, 0.999999, 1
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const double L = 1.0;                           // mean free path
            const double S = 14.056001;                     // scaling factor for A = 0.9
            const double r = static_cast<double>(i) * 0.1 + 0.05;
            const double cdf = normalized_diffusion_cdf(r, L, S);

            EXPECT_FEQ_EPS(Expected[i], cdf, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionSample)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double u = rand_double2(rng);
            const double a = rand_double1(rng);
            const double l = rand_double1(rng, 0.001, 10.0);
            const double s = normalized_diffusion_s(a);
            const double r = normalized_diffusion_sample(u, l, s, 0.00001);
            const double e = normalized_diffusion_cdf(r, l, s);

            EXPECT_FEQ_EPS(u, e, 0.005);
        }
    }

    TEST_CASE(NormalizedDiffusionMaxRadius)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double a = rand_double1(rng);
            const double l = rand_double1(rng, 0.001, 10.0);
            const double s = normalized_diffusion_s(a);
            const double r = normalized_diffusion_max_radius(l, s);
            const double value = normalized_diffusion_profile(r, l, s, a);

            EXPECT_LT(0.00001, value);
        }
    }

    TEST_CASE(NormalizedDiffusionProfileIntegration)
    {
        const double A = 0.5;
        const double L = 100;
        const double s = normalized_diffusion_s(A);

        const size_t SampleCount = 1000;
        MersenneTwister rng;

        double integral = 0.0;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const double u = rand_double2(rng);
            const double r = normalized_diffusion_sample(u, L, s);
            const double pdf_radius = normalized_diffusion_pdf(r, L, s);
            const double pdf_angle = RcpTwoPi;
            const double pdf = pdf_radius * pdf_angle;
            const double value = r * normalized_diffusion_profile(r, L, s, A);
            integral += value / pdf;
        }

        integral /= SampleCount;

        EXPECT_FEQ(A, integral);
    }

    TEST_CASE(PlotNormalizedDiffusionS)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Scaling Factor For Searchlight Configuration With dmfp Parameterization");
        plotfile.set_xlabel("A");
        plotfile.set_ylabel("s(A)");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 20.0);

        const size_t N = 1000;
        vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const double a = fit<size_t, double>(i, 0, N - 1, 0.0, 1.0);
            const double s = normalized_diffusion_s(a);
            points.push_back(Vector2d(a, s));
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_s.gnuplot");
    }

    TEST_CASE(PlotNormalizedDiffusionR)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Reflectance Profile For Searchlight Configuration With dmfp Parameterization");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("r R(r)");
        plotfile.set_xrange(0.0, 8.0);
        plotfile.set_yrange(0.001, 1.0);
        plotfile.set_logscale_y();

        for (size_t i = 9; i >= 1; --i)
        {
            const double a = static_cast<double>(i) / 10.0;
            const double s = normalized_diffusion_s(a);

            const size_t N = 1000;
            vector<Vector2d> points;

            for (size_t j = 0; j < N; ++j)
            {
                const double r = max(fit<size_t, double>(j, 0, N - 1, 0.0, 8.0), 0.0001);
                const double y = r * normalized_diffusion_profile(r, 1.0, s, a);
                points.push_back(Vector2d(r, y));
            }

            static const char* Colors[9] =
            {
                "gray",
                "orange",
                "black",
                "brown",
                "cyan",
                "magenta",
                "blue",
                "green",
                "red"
            };

            plotfile
                .new_plot()
                .set_points(points)
                .set_title("A = " + to_string(a))
                .set_color(Colors[i - 1]);
        }

        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_r.gnuplot");
    }

    TEST_CASE(PlotNormalizedDiffusionCDF)
    {
        GnuplotFile plotfile;
        plotfile.set_title("CDF");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("u");
        plotfile.set_xrange(0.0, 40.0);
        plotfile.set_yrange(0.0, 1.0);

        const size_t N = 1000;
        vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const double r = fit<size_t, double>(i, 0, N - 1, 0.0, 40.0);
            const double u = normalized_diffusion_cdf(r, 1.0);
            points.push_back(Vector2d(r, u));
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_cdf.gnuplot");
    }

    //
    // Directional dipole profile.
    //

    void init_dirpole_bssrdf_values_sigmas(
        const double                        sigma_a,
        const double                        sigma_s,
        const double                        eta,
        const double                        g,
        DirectionalDipoleBSSRDFInputValues& values)
    {
        values.m_weight = 1.0;
        values.m_inside_ior = eta;
        values.m_outside_ior = 1.0;
        values.m_anisotropy = g;
        values.m_sigma_a = Color3f(static_cast<float>(sigma_a));
        values.m_sigma_s = Color3f(static_cast<float>(sigma_s));
    }

    void init_dirpole_bssrdf_values_rd_dmfp(
        const double                        rd,
        const double                        dmfp,
        const double                        eta,
        const double                        g,
        DirectionalDipoleBSSRDFInputValues& values)
    {
        values.m_weight = 1.0;
        values.m_inside_ior = eta;
        values.m_outside_ior = 1.0;
        values.m_anisotropy = g;

        compute_absorption_and_scattering(
            Spectrum(Color3f(static_cast<float>(rd))),
            Spectrum(Color3f(static_cast<float>(dmfp))),
            values.m_inside_ior / values.m_outside_ior,
            values.m_anisotropy,
            values.m_sigma_a,
            values.m_sigma_s);
    }

    void plot_dirpole_rd(
        const char*     filename,
        const char*     title,
        const double    sigma_a,
        const double    ymin,
        const double    ymax)
    {
        GnuplotFile plotfile;
        plotfile.set_title(title);
        plotfile.set_xlabel("x [cm]");
        plotfile.set_ylabel("Rd(x)");
        plotfile.set_logscale_y();
        plotfile.set_xrange(-16.0, 16.0);   // cm
        plotfile.set_yrange(ymin, ymax);

        auto_release_ptr<BSSRDF> bssrdf(
            DirectionalDipoleBSSRDFFactory().create("dirpole", ParamArray()));

        DirectionalDipoleBSSRDFInputValues values;
        init_dirpole_bssrdf_values_sigmas(sigma_a, 1.0, 1.0, 0.0, values);

        const Vector3d normal(0.0, 1.0, 0.0);

        ShadingPoint outgoing;
        ShadingPointBuilder outgoing_builder(outgoing);
        outgoing_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
        outgoing_builder.set_point(Vector3d(0.0, 0.0, 0.0));
        outgoing_builder.set_shading_basis(Basis3d(normal));

        ShadingPoint incoming;
        ShadingPointBuilder incoming_builder(incoming);
        incoming_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
        incoming_builder.set_shading_basis(Basis3d(normal));

        const size_t N = 1000;
        vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const double x = fit<size_t, double>(i, 0, N - 1, -16.0, 16.0);
            incoming_builder.set_point(Vector3d(x, 0.0, 0.0));

            Spectrum result;
            bssrdf->evaluate(
                &values,
                outgoing,
                normal,
                incoming,
                normal,
                result);

            points.push_back(Vector2d(x, result[0] * Pi / abs(x)));
        }

        plotfile
            .new_plot()
            .set_points(points);

        plotfile.write(filename);
    }

    TEST_CASE(PlotDirectionalDipoleRd)
    {
        plot_dirpole_rd(
            "unit tests/outputs/test_sss_dirpole_sg_a_001.gnuplot",
            "Directional Dipole Diffuse Reflectance (sigma_a == 0.01)",
            0.01,           // sigma_a in cm
            1.0e-5,         // ymin
            1.0e+1);        // ymax

        plot_dirpole_rd(
            "unit tests/outputs/test_sss_dirpole_sg_a_01.gnuplot",
            "Directional Dipole Diffuse Reflectance (sigma_a == 0.1)",
            0.1,            // sigma_a in cm
            1.0e-8,         // ymin
            1.0e+1);        // ymax

        plot_dirpole_rd(
            "unit tests/outputs/test_sss_dirpole_sg_a_1.gnuplot",
            "Directional Dipole Diffuse Reflectance (sigma_a == 1.0)",
            1.0,            // sigma_a in cm
            1.0e-16,        // ymin
            1.0e+1);        // ymax
    }
}
