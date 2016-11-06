
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/betterdipolebssrdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/dipolebssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#endif
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/bssrdf/standarddipolebssrdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/utility/iostreamop.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/gnuplotfile.h"
#ifdef APPLESEED_WITH_PARTIO
#include "foundation/utility/partiofile.h"
#endif
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
    // Utilities.
    //

    template <typename BSSRDFFactory>
    class DipoleBSSRDFEvaluator
    {
      public:
        DipoleBSSRDFEvaluator()
          : m_bssrdf(BSSRDFFactory().create("bssrdf", ParamArray()))
        {
            ShadingPointBuilder outgoing_builder(m_outgoing_point);
            outgoing_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
            outgoing_builder.set_point(Vector3d(0.0, 0.0, 0.0));
            outgoing_builder.set_geometric_normal(Vector3d(0.0, 1.0, 0.0));
            outgoing_builder.set_side(ObjectInstance::FrontSide);
            outgoing_builder.set_shading_basis(Basis3d(Vector3d(0.0, 1.0, 0.0)));
        }

        void set_values_from_sigmas(
            const float     sigma_a,
            const float     sigma_s,
            const float     eta,
            const float     g)
        {
            m_values.m_weight = 1.0;
            m_values.m_g = g;
            m_values.m_ior = eta;

            m_bssrdf->get_inputs().find("sigma_a").bind(new ScalarSource(sigma_a));
            m_bssrdf->get_inputs().find("sigma_a").source()->evaluate_uniform(m_values.m_sigma_a);

            m_bssrdf->get_inputs().find("sigma_s").bind(new ScalarSource(sigma_s));
            m_bssrdf->get_inputs().find("sigma_s").source()->evaluate_uniform(m_values.m_sigma_s);

            m_bssrdf->prepare_inputs(m_outgoing_point, &m_values);
        }

        void set_values_from_rd_mfp(
            const float     rd,
            const float     mfp,
            const float     eta)
        {
            m_values.m_weight = 1.0;
            m_values.m_reflectance.set(rd);
            m_values.m_reflectance_multiplier = 1.0;
            m_values.m_mfp.set(mfp);
            m_values.m_mfp_multiplier = 1.0;
            m_values.m_g = 0.0;
            m_values.m_ior = eta;

            m_bssrdf->prepare_inputs(m_outgoing_point, &m_values);
        }

        const float get_sigma_tr() const
        {
            return m_values.m_precomputed.m_sigma_tr[0];
        }

        float evaluate(const float r) const
        {
            ShadingPoint incoming_point;
            ShadingPointBuilder incoming_builder(incoming_point);
            incoming_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
            incoming_builder.set_shading_basis(Basis3d(Vector3d(0.0, 1.0, 0.0)));
            incoming_builder.set_point(Vector3d(r, 0.0, 0.0));

            const Vector3f Up(0.0f, 1.0f, 0.0f);

            Spectrum result;
            m_bssrdf->evaluate(
                &m_values,
                m_outgoing_point,
                Up,
                incoming_point,
                Up,
                result);

            return result[0];
        }

      private:
        auto_release_ptr<BSSRDF>    m_bssrdf;
        ShadingPoint                m_outgoing_point;
        DipoleBSSRDFInputValues     m_values;
    };

    template <typename BSSRDFEvaluator>
    float integrate_dipole(
        MersenneTwister&        rng,
        const BSSRDFEvaluator&  bssrdf_eval,
        const size_t            sample_count)
    {
        const float sigma_tr = bssrdf_eval.get_sigma_tr();
        float integral = 0.0f;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float u = static_cast<float>(rand_double2(rng));
            const float r = sample_exponential_distribution(u, sigma_tr);

            const float pdf_radius = exponential_distribution_pdf(r, sigma_tr);
            const float pdf_angle = RcpTwoPi<float>();
            const float pdf = pdf_radius * pdf_angle;

            const float value = bssrdf_eval.evaluate(r);
            integral += value / pdf;
        }

        return integral / sample_count;
    }

    template <typename BSSRDFFactory>
    float integrate_dipole_rd_mfp(
        MersenneTwister&        rng,
        const float             rd,
        const float             mfp,
        const float             eta,
        const size_t            sample_count)
    {
        DipoleBSSRDFEvaluator<BSSRDFFactory> bssrdf_eval;
        bssrdf_eval.set_values_from_rd_mfp(rd, mfp, eta);

        return integrate_dipole(rng, bssrdf_eval, sample_count);
    }

    template <typename BSSRDFFactory>
    float integrate_dipole_alpha_prime(
        MersenneTwister&        rng,
        const float             alpha_prime,
        const float             eta,
        const size_t            sample_count)
    {
        const float sigma_s_prime = alpha_prime;
        const float sigma_a = 1.0f - alpha_prime;

        DipoleBSSRDFEvaluator<BSSRDFFactory> bssrdf_eval;
        bssrdf_eval.set_values_from_sigmas(sigma_a, sigma_s_prime, eta, 0.0f);

        return integrate_dipole(rng, bssrdf_eval, sample_count);
    }

    //
    // BSSRDF reparameterization.
    //

    template <typename ComputeRdFun>
    float rd_alpha_prime_roundtrip(
        const float             rd,
        const float             eta)
    {
        const ComputeRdFun f(eta);
        const float alpha_prime = compute_alpha_prime(f, rd);
        return f(alpha_prime);
    }

    const float AlphaPrimeRoundtripTestEps = 0.001f;

    static const float AlphaPrimes[] =
    {
        0.025f, 0.1f, 0.2f, 0.4f, 0.6f, 0.8f, 0.99f
    };

    static const float IORs[] =
    {
        1.6f, 1.3f, 1.2f, 1.3f, 1.4f, 1.3f, 1.5f
    };

    TEST_CASE(BSSRDFReparam_StandardDipoleRoundtrip)
    {
        for (size_t i = 0, e = countof(AlphaPrimes); i < e; ++i)
            EXPECT_FEQ_EPS(AlphaPrimes[i], rd_alpha_prime_roundtrip<ComputeRdStandardDipole>(AlphaPrimes[i], IORs[i]), AlphaPrimeRoundtripTestEps);
    }

    TEST_CASE(BSSRDFReparam_BetterDipoleRoundtrip)
    {
        for (size_t i = 0, e = countof(AlphaPrimes); i < e; ++i)
            EXPECT_FEQ_EPS(AlphaPrimes[i], rd_alpha_prime_roundtrip<ComputeRdBetterDipole>(AlphaPrimes[i], IORs[i]), AlphaPrimeRoundtripTestEps);
    }

    TEST_CASE(BSSRDFReparamStandardDipole_RoundTrip)
    {
        //
        // 1. Start with sigma_a and sigma_s.
        //    Compute diffuse surface reflectance and mean free path.
        //
        //    Using skin2 material parameters from
        //
        //      A Practical Model for Subsurface Light Transport
        //      https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
        //

        const Color3f sigma_a(0.013f, 0.070f, 0.145f);      // in mm^-1
        const Color3f sigma_s(1.09f, 1.59f, 1.79f);         // in mm^-1
        const Spectrum sigma_t(sigma_a + sigma_s);

        const Spectrum sigma_a_spectrum(sigma_a);
        const Spectrum sigma_s_spectrum(sigma_s);

        const Color3f alpha_prime = sigma_s / (sigma_a + sigma_s);
        const float eta = 1.0f;
        const ComputeRdStandardDipole rd_fun(eta);
        const Color3f rd(
            rd_fun(alpha_prime[0]),
            rd_fun(alpha_prime[1]),
            rd_fun(alpha_prime[2]));

        // rd = [185, 138, 112] in 8-bit linear RGB

        Spectrum sigma_tr;
        effective_extinction_coefficient(
            sigma_a_spectrum,
            sigma_s_spectrum,
            0.0f,       // anisotropy
            sigma_tr);

        const Spectrum mfp = Spectrum(1.0f) / sigma_t;

        //
        // 2. Start from diffuse surface reflectance and mean free path.
        //    Compute sigma_a and sigma_s.
        //

        Spectrum new_sigma_a_spectrum, new_sigma_s_spectrum;
        const Spectrum rd_spectrum(rd);
        compute_absorption_and_scattering_mfp(
            rd_fun,
            rd_spectrum,
            mfp,
            new_sigma_a_spectrum,
            new_sigma_s_spectrum);

        EXPECT_FEQ_EPS(sigma_a_spectrum, new_sigma_a_spectrum, 1.0e-5f);
        EXPECT_FEQ_EPS(sigma_s_spectrum, new_sigma_s_spectrum, 1.0e-5f);
    }

    TEST_CASE(Plot_CompareStandardAndBetterDipolesReparameterizations)
    {
        GnuplotFile plotfile;
        plotfile.set_title("BSSRDF Reparameterization");
        plotfile.set_xlabel("Rd");
        plotfile.set_ylabel("Alpha'");
        plotfile.set_xrange(-0.05, 1.0);
        plotfile.set_yrange(0.0, 1.05);

        const float Eta = 1.0f / 1.3f;
        const ComputeRdStandardDipole std_rd_fun(Eta);
        const ComputeRdBetterDipole better_rd_fun(Eta);

        const size_t PointCount = 1000;
        vector<Vector2d> std_points, better_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float rd = fit<size_t, float>(i, 0, PointCount - 1, 0.0f, 1.0f);

            std_points.push_back(
                Vector2d(
                    rd,
                    compute_alpha_prime(std_rd_fun, rd)));

            better_points.push_back(
                Vector2d(
                    rd,
                    compute_alpha_prime(better_rd_fun, rd)));
        }

        plotfile
            .new_plot()
            .set_points(std_points)
            .set_title("Standard Dipole")
            .set_color("orange");

        plotfile
            .new_plot()
            .set_points(better_points)
            .set_title("Better Dipole")
            .set_color("blue");

        plotfile.write("unit tests/outputs/test_sss_reparam_compare.gnuplot");
    }

    TEST_CASE(CompareAnalyticalAndNumericalIntegrals_StandardDipole)
    {
        const float Eta = 1.0f / 1.0f;
        const ComputeRdStandardDipole rd_fun(Eta);

        const size_t TestCount = 100;
        const size_t SampleCount = 10000;

        for (size_t i = 0; i < TestCount; ++i)
        {
            const float Eps = 1.0e-5f;
            const float alpha_prime = fit<size_t, float>(i, 0, TestCount - 1, 0.0f + Eps, 1.0f - Eps);

            const float rd_a = RcpPi<float>() * rd_fun(alpha_prime);

            MersenneTwister rng;
            const float rd_n =
                integrate_dipole_alpha_prime<StandardDipoleBSSRDFFactory>(
                    rng,
                    alpha_prime,
                    Eta,
                    SampleCount);

            EXPECT_FEQ_EPS(rd_a, rd_n, 0.02f);
        }
    }

    TEST_CASE(Plot_CompareAnalyticalAndNumericalIntegrals_StandardDipole)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Integration of the Standard Dipole Profile");
        plotfile.set_xlabel("Alpha'");
        plotfile.set_ylabel("Rd");

        const float Eta = 1.0f / 1.0f;
        const ComputeRdStandardDipole rd_fun(Eta);

        const size_t PointCount = 1000;
        const size_t SampleCount = 1000;
        vector<Vector2d> ai_points, ni_points;
        MersenneTwister rng;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float Eps = 1.0e-6f;
            const float alpha_prime = fit<size_t, float>(i, 0, PointCount - 1, 0.0f + Eps, 1.0f - Eps);

            ai_points.push_back(
                Vector2d(
                    alpha_prime,
                    RcpPi<float>() * rd_fun(alpha_prime)));

            ni_points.push_back(
                Vector2d(
                    alpha_prime,
                    integrate_dipole_alpha_prime<StandardDipoleBSSRDFFactory>(
                        rng,
                        alpha_prime,
                        Eta,
                        SampleCount)));
        }

        plotfile
            .new_plot()
            .set_points(ai_points)
            .set_title("Analytical Integration")
            .set_color("gray");

        plotfile
            .new_plot()
            .set_points(ni_points)
            .set_title("Numerical Integration")
            .set_color("blue");

        plotfile.write("unit tests/outputs/test_sss_stddipole_integrals.gnuplot");
    }

    TEST_CASE(Plot_CompareAnalyticalAndNumericalIntegrals_BetterDipole)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Integration of the Better Dipole Profile");
        plotfile.set_xlabel("Alpha'");
        plotfile.set_ylabel("Rd");

        const float Eta = 1.0f / 1.0f;
        const ComputeRdBetterDipole rd_fun(Eta);

        const size_t PointCount = 1000;
        const size_t SampleCount = 1000;
        vector<Vector2d> ai_points, ni_points;
        MersenneTwister rng;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float Eps = 1.0e-6f;
            const float alpha_prime = fit<size_t, float>(i, 0, PointCount - 1, 0.0f + Eps, 1.0f - Eps);

            ai_points.push_back(
                Vector2d(
                    alpha_prime,
                    RcpPi<float>() * rd_fun(alpha_prime)));

            ni_points.push_back(
                Vector2d(
                    alpha_prime,
                    integrate_dipole_alpha_prime<BetterDipoleBSSRDFFactory>(
                        rng,
                        alpha_prime,
                        Eta,
                        SampleCount)));
        }

        plotfile
            .new_plot()
            .set_points(ai_points)
            .set_title("Analytical Integration")
            .set_color("gray");

        plotfile
            .new_plot()
            .set_points(ni_points)
            .set_title("Numerical Integration")
            .set_color("blue");

        plotfile.write("unit tests/outputs/test_sss_betterdipole_integrals.gnuplot");
    }

    //
    // Gaussian profile.
    //

    TEST_CASE(GaussianProfileIntegration_UniformSampling)
    {
        const float V = 1.0f;
        const float RIntegralThreshold = 0.999f;
        const float RMax2Constant = -2.0f * log(1.0f - RIntegralThreshold);
        const float RMax2 = V * RMax2Constant;

        const size_t SampleCount = 10000;
        MersenneTwister rng;

        float integral = 0.0f;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const float u = static_cast<float>(rand_double2(rng));
            const float r = u * sqrt(RMax2);
            const float pdf_radius = 1.0f / sqrt(RMax2);
            const float pdf_angle = RcpTwoPi<float>();
            const float pdf = pdf_radius * pdf_angle;
            const float value = r * gaussian_profile(r, V, RIntegralThreshold);
            integral += value / pdf;
        }

        integral /= SampleCount;

        EXPECT_FEQ_EPS(1.0f, integral, 0.02f);
    }

    TEST_CASE(GaussianProfileIntegration_ImportanceSampling)
    {
        const float V = 1.0f;
        const float RIntegralThreshold = 0.999f;
        const float RMax2Constant = -2.0f * log(1.0f - RIntegralThreshold);
        const float RMax2 = V * RMax2Constant;

        const size_t SampleCount = 1000;
        MersenneTwister rng;

        float integral = 0.0f;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const float u = static_cast<float>(rand_double2(rng));
            const float r = gaussian_profile_sample(u, V, RMax2);
            const float pdf = gaussian_profile_pdf(r, V, RIntegralThreshold);
            const float value = gaussian_profile(r, V, RIntegralThreshold);
            integral += value / pdf;
        }

        integral /= SampleCount;

        EXPECT_FEQ(1.0f, integral);
    }

    //
    // Normalized diffusion profile.
    //

    const float NormalizedDiffusionTestEps = 0.0001f;

    TEST_CASE(NormalizedDiffusionS)
    {
        static const float Expected[] =
        {
            4.68592f, 4.11466f, 3.77984f, 3.60498f, 3.52856f, 3.5041f,  3.50008f,
            3.50002f, 3.5024f,  3.52074f, 3.58352f, 3.73426f, 4.03144f, 4.54858f,
            5.37416f, 6.6117f,  8.37968f, 10.8116f, 14.056f,  18.2763f, 23.6511f
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const float a = fit<size_t, float>(i, 0, countof(Expected) - 1, 0.0f, 1.0f);
            const float s = normalized_diffusion_s_dmfp(a);

            EXPECT_FEQ_EPS(Expected[i], s, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionR)
    {
        static const float Expected[] =
        {
            2.53511f,    0.674967f,  0.327967f,   0.192204f,   0.124137f,   0.0852575f,
            0.0611367f,  0.0452741f, 0.0343737f,  0.0266197f,  0.0209473f,  0.0167009f,
            0.0134603f,  0.0109471f, 0.00897108f, 0.00739936f, 0.00613676f, 0.00511384f,
            0.00427902f, 0.0035934f, 0.00302721f
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const float A = 0.5f;                           // surface albedo
            const float L = 1.0f;                           // mean free path
            const float S = 3.583521f;                      // scaling factor for A = 0.5
            const float r = static_cast<float>(i) * 0.1f + 0.05f;
            const float value = normalized_diffusion_profile(r, L, S, A);

            EXPECT_FEQ_EPS(Expected[i], value, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionCDF)
    {
        static const float Expected[] =
        {
            0.282838f, 0.598244f, 0.760091f, 0.85267f,  0.908478f, 0.942885f, 0.964293f,
            0.97766f,  0.98602f,  0.99125f,  0.994523f, 0.996572f, 0.997854f, 0.998657f,
            0.999159f, 0.999474f, 0.999671f, 0.999794f, 0.999871f, 0.999919f, 0.999949f,
            0.999968f, 0.99998f,  0.999988f, 0.999992f, 0.999995f, 0.999997f, 0.999998f,
            0.999999f, 0.999999f, 1.0f
        };

        for (size_t i = 0, e = countof(Expected); i < e; ++i)
        {
            const float L = 1.0f;                           // mean free path
            const float S = 14.056001f;                     // scaling factor for A = 0.9
            const float r = static_cast<float>(i) * 0.1f + 0.05f;
            const float cdf = normalized_diffusion_cdf(r, L, S);

            EXPECT_FEQ_EPS(Expected[i], cdf, NormalizedDiffusionTestEps);
        }
    }

    TEST_CASE(NormalizedDiffusionSample)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float u = static_cast<float>(rand_double2(rng));
            const float a = static_cast<float>(rand_double1(rng));
            const float l = static_cast<float>(rand_double1(rng, 0.001, 10.0));

            const float s = normalized_diffusion_s_dmfp(a);
            const float r = normalized_diffusion_sample(u, l, s, 0.00001f);
            const float e = normalized_diffusion_cdf(r, l, s);

            EXPECT_FEQ_EPS(u, e, 0.005f);
        }
    }

    TEST_CASE(NormalizedDiffusionMaxRadius)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float a = static_cast<float>(rand_double1(rng));
            const float l = static_cast<float>(rand_double1(rng, 0.001, 10.0));

            const float s = normalized_diffusion_s_dmfp(a);
            const float r = normalized_diffusion_max_radius(l, s);
            const float value = normalized_diffusion_profile(r, l, s, a);

            EXPECT_LT(0.00001f, value);
        }
    }

    TEST_CASE(NormalizedDiffusionProfileIntegration)
    {
        const float A = 0.5f;
        const float L = 100.0f;
        const float s = normalized_diffusion_s_dmfp(A);

        const size_t SampleCount = 1000;
        MersenneTwister rng;

        float integral = 0.0f;

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const float u = static_cast<float>(rand_double2(rng));
            const float r = normalized_diffusion_sample(u, L, s);
            const float pdf_radius = normalized_diffusion_pdf(r, L, s);
            const float pdf_angle = RcpTwoPi<float>();
            const float pdf = pdf_radius * pdf_angle;
            const float value = r * normalized_diffusion_profile(r, L, s, A);
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
            const float a = fit<size_t, float>(i, 0, N - 1, 0.0f, 1.0f);
            const float s = normalized_diffusion_s_dmfp(a);
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
            const float a = static_cast<float>(i) / 10.0f;
            const float s = normalized_diffusion_s_dmfp(a);

            const size_t N = 1000;
            vector<Vector2d> points;

            for (size_t j = 0; j < N; ++j)
            {
                const float r = max(fit<size_t, float>(j, 0, N - 1, 0.0f, 8.0f), 0.0001f);
                const float y = r * normalized_diffusion_profile(r, 1.0f, s, a);
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
            const float r = fit<size_t, float>(i, 0, N - 1, 0.0f, 40.0f);
            const float u = normalized_diffusion_cdf(r, 1.0f);
            points.push_back(Vector2d(r, u));
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_cdf.gnuplot");
    }

#ifdef APPLESEED_WITH_PARTIO

    TEST_CASE(PartioNormalizedDiffusionSample)
    {
        const float A = 1.0f;
        const float L = 1;
        const float s = normalized_diffusion_s_dmfp(A);

        const size_t SampleCount = 1000;
        MersenneTwister rng;

        PartioFile particles;
        Partio::ParticleAttribute pos_attr = particles.add_vector_attribute("position");
        Partio::ParticleAttribute col_attr = particles.add_color_attribute("color");

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const float u = rand_double2(rng);
            const float r = normalized_diffusion_sample(u, L, s);
            const float phi = TwoPi<float>() * rand_double2(rng);
            const Vector3d sample(r * cos(phi), 0.0f, r * sin(phi));

            Partio::ParticleIndex p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, sample);
            particles.set_color_attribute(p, col_attr, Color3d(1.0, 1.0, 1.0));
        }

        // Add a circle of particles of radius max_radius.
        const size_t CircleCount = 100;
        const float max_radius = normalized_diffusion_max_radius(L, s);

        for (size_t i = 0; i < CircleCount; ++i)
        {
            const float phi = fit<size_t, float>(i, 0, CircleCount - 1, 0.0f, TwoPi<float>());
            const Vector3d c(max_radius * cos(phi), 0.0f, max_radius * sin(phi));

            Partio::ParticleIndex p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, c);
            particles.set_color_attribute(p, col_attr, Color3d(0.0, 0.0, 1.0));
        }

        // Add particles with the value of the profile.
        const size_t ProfileCount = 500;
        for (size_t i = 0; i < ProfileCount; ++i)
        {
            const float r = fit<size_t, float>(i, 0, ProfileCount - 1, 0.0f, max_radius);
            const float v = normalized_diffusion_profile(r, L, s, A) * r;

            Partio::ParticleIndex p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, Vector3d(r, v, 0.0f));
            particles.set_color_attribute(p, col_attr, Color3d(1.0, 0.0, 0.0));

            p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, Vector3d(-r, v, 0.0f));
            particles.set_color_attribute(p, col_attr, Color3d(1.0, 0.0, 0.0));

            p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, Vector3d(0.0f, v, r));
            particles.set_color_attribute(p, col_attr, Color3d(1.0, 0.0, 0.0));

            p = particles.add_particle();
            particles.set_vector_attribute(p, pos_attr, Vector3d(0.0f, v, -r));
            particles.set_color_attribute(p, col_attr, Color3d(1.0, 0.0, 0.0));
        }

        particles.write("unit tests/outputs/test_sss_nd_sampling_particles.bgeo");
    }

#endif

    //
    // Standard dipole profile.
    //

    TEST_CASE(StdDipoleMaxRadius)
    {
        DipoleBSSRDFEvaluator<StandardDipoleBSSRDFFactory> bssrdf_eval;
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float rd = static_cast<float>(rand_double1(rng));
            const float mfp = static_cast<float>(rand_double1(rng, 0.001, 10.0));
            bssrdf_eval.set_values_from_rd_mfp(rd, mfp, 1.0f);

            const float r = dipole_max_radius(bssrdf_eval.get_sigma_tr());
            const float result = bssrdf_eval.evaluate(r);

            EXPECT_LT(0.00001f, result);
        }
    }

    TEST_CASE(PlotStdDipoleIntegralRd)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Standard Dipole Integral");
        plotfile.set_xlabel("Rd");
        plotfile.set_ylabel("Integral");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 1.25);

        const size_t N = 256;
        vector<Vector2d> points;
        MersenneTwister rng;

        for (size_t i = 0; i < N; ++i)
        {
            const float rd = fit<size_t, float>(i, 0, N - 1, 0.01f, 1.0f);
            const float x =
                integrate_dipole_rd_mfp<StandardDipoleBSSRDFFactory>(
                    rng,
                    rd,
                    1.0f,
                    1.0f,
                    1000);

            points.push_back(Vector2d(rd, x));
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_stddipole_integral_rd.gnuplot");
    }

    //
    // Directional dipole profile.
    //

    void plot_dirpole_rd(
        const char*     filename,
        const char*     title,
        const float     sigma_a,
        const double    ymin,
        const double    ymax)
    {
        GnuplotFile plotfile;
        plotfile.set_title(title);
        plotfile.set_xlabel("r [cm]");
        plotfile.set_ylabel("Rd(r)");
        plotfile.set_logscale_y();
        plotfile.set_xrange(-16.0, 16.0);   // cm
        plotfile.set_yrange(ymin, ymax);

        DipoleBSSRDFEvaluator<DirectionalDipoleBSSRDFFactory> bssrdf_eval;
        bssrdf_eval.set_values_from_sigmas(sigma_a, 1.0f, 1.0f, 0.0f);

        const size_t N = 1000;
        vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const float r = fit<size_t, float>(i, 0, N - 1, -16.0f, 16.0f);
            const float result = bssrdf_eval.evaluate(r);
            points.push_back(Vector2d(r, result * Pi<float>() / abs(r)));
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
            "Directional Dipole Diffuse Reflectance (sigma_a = 0.01)",
            0.01f,          // sigma_a in cm
            1.0e-5,         // ymin
            1.0e+1);        // ymax

        plot_dirpole_rd(
            "unit tests/outputs/test_sss_dirpole_sg_a_01.gnuplot",
            "Directional Dipole Diffuse Reflectance (sigma_a = 0.1)",
            0.1f,           // sigma_a in cm
            1.0e-8,         // ymin
            1.0e+1);        // ymax

        plot_dirpole_rd(
            "unit tests/outputs/test_sss_dirpole_sg_a_1.gnuplot",
            "Directional Dipole Diffuse Reflectance (sigma_a = 1.0)",
            1.0f,           // sigma_a in cm
            1.0e-16,        // ymin
            1.0e+1);        // ymax
    }

    TEST_CASE(DirpoleMaxRadius)
    {
        DipoleBSSRDFEvaluator<DirectionalDipoleBSSRDFFactory> bssrdf_eval;
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float rd = static_cast<float>(rand_double1(rng));
            const float mfp = static_cast<float>(rand_double1(rng, 0.001, 100.0));
            bssrdf_eval.set_values_from_rd_mfp(rd, mfp, 1.0f);

            const float r = dipole_max_radius(bssrdf_eval.get_sigma_tr());
            const float result = bssrdf_eval.evaluate(r);

            EXPECT_LT(0.00001f, result);
        }
    }
}
