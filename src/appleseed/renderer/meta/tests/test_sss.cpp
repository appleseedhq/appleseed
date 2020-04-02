
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#include "renderer/modeling/bssrdf/separablebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/bssrdf/standarddipolebssrdf.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/iostreamop.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/memory/arena.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/string/string.h"
#include "foundation/utility/gnuplotfile.h"
#include "foundation/utility/poison.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_BSSRDF_SSS)
{
    //
    // Utilities.
    //

    template <typename BSSRDFFactory, typename BSSRDFInputValues>
    class BSSRDFEvaluator
    {
      public:
        explicit BSSRDFEvaluator(Project& project)
          : m_project(project)
          , m_bssrdf(
              static_cast<SeparableBSSRDF*>(
                  BSSRDFFactory().create("bssrdf", ParamArray()).release()))
        {
            ShadingPointBuilder outgoing_builder(m_outgoing_point);
            outgoing_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
            outgoing_builder.set_point(Vector3d(0.0, 0.0, 0.0));
            outgoing_builder.set_geometric_normal(Vector3d(0.0, 1.0, 0.0));
            outgoing_builder.set_shading_basis(Basis3d(Vector3d(0.0, 1.0, 0.0)));
            outgoing_builder.set_side(ObjectInstance::FrontSide);
        }

        ~BSSRDFEvaluator()
        {
            m_recorder.on_frame_end(m_project);
        }

        void set_values_from_sigmas(
            const float     sigma_a,
            const float     sigma_s)
        {
            m_bssrdf->get_inputs().find("sigma_a").bind(new ScalarSource(sigma_a));
            m_bssrdf->get_inputs().find("sigma_s").bind(new ScalarSource(sigma_s));

            m_bssrdf->on_frame_begin(m_project, nullptr, m_recorder);

            m_values.m_weight = 1.0f;
            debug_poison(m_values.m_reflectance);
            debug_poison(m_values.m_reflectance_multiplier);
            debug_poison(m_values.m_mfp);
            debug_poison(m_values.m_mfp_multiplier);
            m_values.m_sigma_a.set(sigma_a);
            m_values.m_sigma_s.set(sigma_s);
            m_values.m_g = 0.0f;
            m_values.m_ior = 1.0f;
            m_values.m_fresnel_weight = 1.0f;

            m_bssrdf->prepare_inputs(m_arena, m_outgoing_point, &m_values);
        }

        void set_values_from_rd_mfp(
            const float     rd,
            const float     mfp)
        {
            m_bssrdf->on_frame_begin(m_project, nullptr, m_recorder);

            do_set_values_from_rd_mfp(m_values, rd, mfp);

            m_bssrdf->prepare_inputs(m_arena, m_outgoing_point, &m_values);
        }

        const float get_sigma_tr() const
        {
            return m_values.m_precomputed.m_sigma_tr[0];
        }

        float sample_profile(const float u) const
        {
            return m_bssrdf->sample_profile(&m_values, 0, u);
        }

        float evaluate_profile_pdf(const float disk_radius) const
        {
            return m_bssrdf->evaluate_profile_pdf(&m_values, disk_radius);
        }

        float evaluate_profile(const float disk_radius) const
        {
            ShadingPoint incoming_point;
            ShadingPointBuilder incoming_builder(incoming_point);
            incoming_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
            incoming_builder.set_point(Vector3d(disk_radius, 0.0, 0.0));
            incoming_builder.set_shading_basis(Basis3d(Vector3d(0.0, 1.0, 0.0)));

            Spectrum value;
            m_bssrdf->evaluate_profile(
                &m_values,
                m_outgoing_point,
                Vector3f(0.0f, 1.0f, 0.0f),
                incoming_point,
                Vector3f(0.0f, 1.0f, 0.0f),
                value);

            return value[0];
        }

        float evaluate(const float radius) const
        {
            ShadingPoint incoming_point;
            ShadingPointBuilder incoming_builder(incoming_point);
            incoming_builder.set_primitive_type(ShadingPoint::PrimitiveTriangle);
            incoming_builder.set_shading_basis(Basis3d(Vector3d(0.0, 1.0, 0.0)));
            incoming_builder.set_point(Vector3d(radius, 0.0, 0.0));

            Spectrum result;
            m_bssrdf->evaluate(
                &m_values,
                m_outgoing_point,
                Vector3f(0.0f, 1.0f, 0.0f),
                incoming_point,
                Vector3f(0.0f, 1.0f, 0.0f),
                ScatteringMode::All,
                result);

            return result[0];
        }

      private:
        Arena                               m_arena;
        Project&                            m_project;
        auto_release_ptr<SeparableBSSRDF>   m_bssrdf;
        ShadingPoint                        m_outgoing_point;
        BSSRDFInputValues                   m_values;
        OnFrameBeginRecorder                m_recorder;

        template <typename InputValues>
        static void do_set_values_from_rd_mfp(
            InputValues&                    values,
            const float                     rd,
            const float                     mfp)
        {
            values.m_weight = 1.0f;
            values.m_reflectance.set(rd);
            values.m_reflectance_multiplier = 1.0f;
            values.m_mfp.set(mfp);
            values.m_mfp_multiplier = 1.0f;
            values.m_ior = 1.0f;
            values.m_fresnel_weight = 1.0f;
        }

        static void do_set_values_from_rd_mfp(
            DipoleBSSRDFInputValues&        values,
            const float                     rd,
            const float                     mfp)
        {
            values.m_weight = 1.0f;
            values.m_reflectance.set(rd);
            values.m_reflectance_multiplier = 1.0f;
            values.m_mfp.set(mfp);
            values.m_mfp_multiplier = 1.0f;
            debug_poison(values.m_sigma_a);
            debug_poison(values.m_sigma_s);
            values.m_g = 0.0f;
            values.m_ior = 1.0f;
            values.m_fresnel_weight = 1.0f;
        }
    };

    template <typename BSSRDFEvaluatorType>
    float integrate_bssrdf_profile(const BSSRDFEvaluatorType& bssrdf_eval, const size_t sample_count)
    {
        MersenneTwister rng;

        const float range_size = 1.0f / static_cast<float>(sample_count);

        float integral = 0.0f;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float s = i * range_size + rand_float2(rng) * range_size;
            const float radius = bssrdf_eval.sample_profile(s);
            const float pdf = bssrdf_eval.evaluate_profile_pdf(radius);
            if (pdf > 0.0f)
            {
                const float value = bssrdf_eval.evaluate_profile(radius);
                integral += value / pdf;
            }
        }

        return integral / sample_count;
    }

    template <typename BSSRDFEvaluatorType>
    float integrate_bssrdf(const BSSRDFEvaluatorType& bssrdf_eval, const size_t sample_count)
    {
        MersenneTwister rng;

        float integral = 0.0f;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float s = rand_float2(rng);
            const float radius = bssrdf_eval.sample_profile(s);
            const float pdf = bssrdf_eval.evaluate_profile_pdf(radius);
            if (pdf > 0.0f)
            {
                const float value = bssrdf_eval.evaluate(radius);
                integral += value / pdf;
            }
        }

        return integral / sample_count;
    }

    //
    // BSSRDF reparameterization.
    //

    static const float RDs[] =
    {
        0.025f, 0.1f, 0.2f, 0.4f, 0.6f, 0.8f, 0.99f
    };

    static const float IORs[] =
    {
        1.6f, 1.3f, 1.2f, 1.3f, 1.4f, 1.3f, 1.5f
    };

    TEST_CASE(BSSRDFReparam_StandardDipole_RdAlphaPrimeRdRoundtrip)
    {
        for (size_t i = 0, e = countof(RDs); i < e; ++i)
        {
            const ComputeRdStandardDipole f(IORs[i]);
            const float rd = RDs[i];
            const float alpha_prime = compute_alpha_prime(f, rd);
            const float new_rd = f(alpha_prime);
            EXPECT_FEQ_EPS(rd, new_rd, 1.0e-3f);
        }
    }

    TEST_CASE(BSSRDFReparam_BetterDipole_RdAlphaPrimeRdRoundtrip)
    {
        for (size_t i = 0, e = countof(RDs); i < e; ++i)
        {
            const ComputeRdBetterDipole f(IORs[i]);
            const float rd = RDs[i];
            const float alpha_prime = compute_alpha_prime(f, rd);
            const float new_rd = f(alpha_prime);
            EXPECT_FEQ_EPS(rd, new_rd, 1.0e-3f);
        }
    }

    TEST_CASE(BSSRDFReparam_StandardDipole_SigmasRdMfpSigmasRoundTrip)
    {
        //
        // 1. (sigma_a, sigma_s) -> (diffuse surface reflectance, mean free path).
        //
        //    Using skin2 material parameters from
        //
        //      A Practical Model for Subsurface Light Transport
        //      https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
        //

        const Color3f sigma_a(0.013f, 0.070f, 0.145f);      // in mm^-1
        const Color3f sigma_s(1.09f, 1.59f, 1.79f);         // in mm^-1
        const Spectrum sigma_t(sigma_a + sigma_s, g_std_lighting_conditions, Spectrum::Reflectance);

        const Spectrum sigma_a_spectrum(sigma_a, g_std_lighting_conditions, Spectrum::Reflectance);
        const Spectrum sigma_s_spectrum(sigma_s, g_std_lighting_conditions, Spectrum::Reflectance);

        const Color3f alpha_prime = sigma_s / (sigma_a + sigma_s);
        const float eta = 1.0f;
        const ComputeRdStandardDipole rd_fun(eta);
        const Color3f rd(
            rd_fun(alpha_prime[0]),
            rd_fun(alpha_prime[1]),
            rd_fun(alpha_prime[2]));

        // rd = [185, 138, 112] in 8-bit linear RGB.

        Spectrum sigma_tr;
        effective_extinction_coefficient(
            sigma_a_spectrum,
            sigma_s_spectrum,
            0.0f,       // anisotropy
            sigma_tr);

        const Spectrum mfp = Spectrum(1.0f) / sigma_t;

        //
        // 2. (diffuse surface reflectance, mean free path) -> (sigma_a, sigma_s).
        //

        Spectrum new_sigma_a_spectrum, new_sigma_s_spectrum;
        const Spectrum rd_spectrum(rd, g_std_lighting_conditions, Spectrum::Reflectance);
        compute_absorption_and_scattering_mfp(
            rd_fun,
            rd_spectrum,
            mfp,
            new_sigma_a_spectrum,
            new_sigma_s_spectrum);

        //
        // 3. Compare sigmas.
        //

        EXPECT_FEQ_EPS(sigma_a_spectrum, new_sigma_a_spectrum, 1.0e-5f);
        EXPECT_FEQ_EPS(sigma_s_spectrum, new_sigma_s_spectrum, 1.0e-5f);
    }

    TEST_CASE(Plot_StandardAndBetterDipolesReparameterizations)
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
        std::vector<Vector2d> std_points, better_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float rd = fit<size_t, float>(i, 0, PointCount - 1, 0.0f, 1.0f);

            std_points.emplace_back(
                rd,
                compute_alpha_prime(std_rd_fun, rd));

            better_points.emplace_back(
                rd,
                compute_alpha_prime(better_rd_fun, rd));
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

    TEST_CASE(Plot_AnalyticalAndNumericalIntegrals_StandardDipole)
    {
        const size_t PointCount = 200;
        const size_t SampleCount = 1000;

        GnuplotFile plotfile;
        plotfile.set_title("Integration of the Standard Dipole Profile");
        plotfile.set_xlabel("Alpha'");
        plotfile.set_ylabel("Rd");

        auto_release_ptr<Project> project(ProjectFactory::create("project"));

        const ComputeRdStandardDipole rd_fun(1.0f);
        std::vector<Vector2d> ai_points, ni_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float Eps = 1.0e-6f;
            const float alpha_prime = fit<size_t, float>(i, 0, PointCount - 1, 0.0f + Eps, 1.0f - Eps);

            // Analytical integration.
            const float rd_a = rd_fun(alpha_prime);
            ai_points.emplace_back(alpha_prime, rd_a);

            // Numerical integration.
            const float sigma_s_prime = alpha_prime;
            const float sigma_a = 1.0f - alpha_prime;
            BSSRDFEvaluator<StandardDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_sigmas(sigma_a, sigma_s_prime);
            const float rd_n = integrate_bssrdf(bssrdf_eval, SampleCount);
            ni_points.emplace_back(alpha_prime, rd_n);
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

    TEST_CASE(Plot_AnalyticalAndNumericalIntegrals_BetterDipole)
    {
        const size_t PointCount = 200;
        const size_t SampleCount = 1000;

        GnuplotFile plotfile;
        plotfile.set_title("Integration of the Better Dipole Profile");
        plotfile.set_xlabel("Alpha'");
        plotfile.set_ylabel("Rd");

        auto_release_ptr<Project> project(ProjectFactory::create("project"));

        const ComputeRdBetterDipole rd_fun(1.0f);
        std::vector<Vector2d> ai_points, ni_points;

        for (size_t i = 0; i < PointCount; ++i)
        {
            const float Eps = 1.0e-6f;
            const float alpha_prime = fit<size_t, float>(i, 0, PointCount - 1, 0.0f + Eps, 1.0f - Eps);

            // Analytical integration.
            const float rd_a = rd_fun(alpha_prime);
            ai_points.emplace_back(alpha_prime, rd_a);

            // Numerical integration.
            const float sigma_s_prime = alpha_prime;
            const float sigma_a = 1.0f - alpha_prime;
            BSSRDFEvaluator<BetterDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_sigmas(sigma_a, sigma_s_prime);
            const float rd_n = integrate_bssrdf(bssrdf_eval, SampleCount);
            ni_points.emplace_back(alpha_prime, rd_n);
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
    // Dwivedi sampling.
    //
    // Reference:
    //
    //   Johannes Meng, Johannes Hanika, Carsten Dachsbacher
    //   Improving the Dwivedi Sampling Scheme,
    //   Journal Computer Graphics Forum Vol. 35 Issue 4, pp. 37-44, July 2016.
    //   https://jo.dreggn.org/home/2016_dwivedi.pdf [1]
    //   https://jo.dreggn.org/home/2016_dwivedi_additional.pdf (supplement material) [2]
    //

    TEST_CASE(DwivediSampling_ComputeRcpDiffusionLength_SatisfiesEigenvalueEquation)
    {
        const size_t ValueCount = 32;
        for (size_t i = 0; i < ValueCount; ++i)
        {
            const float albedo = 1.0f * i / (ValueCount - 1);
            const float estimated_value = compute_rcp_diffusion_length(albedo);
            const float x = std::tanh(estimated_value / std::max(albedo, 0.01f));
            EXPECT_FEQ_EPS(estimated_value, x, 1.0e-2f);
        }
    }

    float integrate_cosine_dwivedi(const float mu, const size_t sample_count)
    {
        MersenneTwister rng;

        const float range_size = 1.0f / static_cast<float>(sample_count);

        float integral = 0.0f;

        for (size_t i = 0; i < sample_count; ++i)
        {
            const float s = i * range_size + rand_float2(rng) * range_size;
            const float cosine = sample_cosine_dwivedi(mu, s);
            const float pdf = evaluate_cosine_dwivedi(mu, cosine);
            integral += 0.5f * (cosine + 1.0f) / pdf;
        }

        return integral / sample_count;
    }

    TEST_CASE(DwivediSampling_SampleCosineDwivedi)
    {
        for (size_t i = 0; i <= 10; ++i)
        {
            const float mu = std::max(rcp(compute_rcp_diffusion_length(0.1f * i)), 1.001f);
            const float integral = integrate_cosine_dwivedi(mu, 10000);

            EXPECT_FEQ_EPS(1.0f, integral, 1.0e-2f);
        }
    }

    bool do_randomwalk_classical(
        MersenneTwister&    rng,
        const size_t        max_iterations,
        size_t&             iterations_count)
    {
        const float Extinction = 1.0f;

        Vector3f current_direction = sample_hemisphere_uniform(rand_vector2<Vector2f>(rng));
        Vector3f current_point = Vector3f(0.0f);
        iterations_count = 0;

        while (true)
        {
            iterations_count++;
            if (iterations_count > max_iterations) return false;
            const float distance = sample_exponential_distribution(rand_float2(rng), Extinction);
            current_point += current_direction * distance;
            if (current_point.y <= 0.0f)
                return true;
            else
            {
                current_direction = sample_sphere_uniform(rand_vector2<Vector2f>(rng));
            }
        }
    }

    bool do_randomwalk_dwivedi(
        MersenneTwister&    rng,
        const size_t        max_iterations,
        const float         mu,
        size_t&             iterations_count)
    {
        const float Extinction = 1.0f;

        Vector3f current_direction = sample_hemisphere_uniform(rand_vector2<Vector2f>(rng));
        float current_cosine = 0.0f;
        Vector3f current_point = Vector3f(0.0f);
        iterations_count = 0;

        while (true)
        {
            if (iterations_count++ > max_iterations) return false;

            float extinction = Extinction;
            extinction *= 1.0f - current_cosine * mu;
            const float distance = sample_exponential_distribution(rand_float2(rng), extinction);
            current_point += current_direction * distance;
            if (current_point.y <= 0.0f)
                return true;
            else
            {
                current_cosine = sample_cosine_dwivedi(rcp(mu), rand_float2(rng));
                const float sine = std::sqrt(1.0f - square(current_cosine));
                Vector2f xz = sine * sample_circle_uniform(rand_float2(rng));
                current_direction.x = xz[0];
                current_direction.y = -current_cosine;
                current_direction.z = xz[1];
            }
        }
    }

    TEST_CASE(Plot_Randomwalk_DiffusionLengthApproximations)
    {
        //
        // Plot of approximation formulas for diffusion length. Compare with [2] Figure 1.
        //

        const size_t SampleCount = 1000;
        std::vector<Vector2d> points_low_albedo;
        std::vector<Vector2d> points_high_albedo;

        GnuplotFile plotfile;
        plotfile.set_title("Approximations for diffusion length");
        plotfile.set_xlabel("diffusion length");
        plotfile.set_ylabel("albedo");
        plotfile.set_xrange(0.0, 1.1);
        plotfile.set_yrange(0.1, 100.0);
        plotfile.set_logscale_y();

        for (size_t i = 0; i < SampleCount; ++i)
        {
            const float albedo = fit<size_t, float>(i, 0, SampleCount - 1, 0.0f, 1.0f);
            if (albedo < 0.9f)
            {
                const float kappa_low = compute_rcp_diffusion_length_low_albedo(albedo);
                points_low_albedo.emplace_back(albedo, rcp(kappa_low));
            }
            if (albedo > 0.1f)
            {
                const float kappa_high = compute_rcp_diffusion_length_high_albedo(albedo);
                points_high_albedo.emplace_back(albedo, rcp(kappa_high));
            }
        }
        plotfile.new_plot().set_points(points_low_albedo).set_title("Low").set_color("red");
        plotfile.new_plot().set_points(points_high_albedo).set_title("High").set_color("blue");
        plotfile.write("unit tests/outputs/test_sss_randomwalk_diffusion_length_approximations.gnuplot");
    }

    TEST_CASE(Plot_Randomwalk_MethodsComparison)
    {
        MersenneTwister rng;
        const size_t SamplesCount = 8000;
        const size_t MaxIterations = 50;
        size_t transmitted_count_classical = 0;
        size_t transmitted_count_dwivedi = 0;
        std::vector<size_t> iterations_hist_classical(MaxIterations, 0);
        std::vector<size_t> iterations_hist_dwivedi(MaxIterations, 0);
        std::vector<Vector2d> points;

        GnuplotFile plotfile;
        plotfile.set_title("Histogram of randomwalk iterations");
        plotfile.set_xlabel("number of iterations");
        plotfile.set_ylabel("number of paths");
        plotfile.set_xrange(2.0, 1.0 * MaxIterations);
        plotfile.set_yrange(1.0, 1.0 * SamplesCount);
        plotfile.set_logscale_y();

        // Classical.
        for (size_t i = 0; i < SamplesCount; ++i)
        {
            size_t iterations_count;
            const bool transmitted = do_randomwalk_classical(
                rng,
                MaxIterations,
                iterations_count);

            if (transmitted)
            {
                iterations_hist_classical[iterations_count - 1]++;
                transmitted_count_classical++;
            }
        }
        for (size_t i = 1; i < MaxIterations; ++i)
        {
            const double x = 1.0 * (i + 1);
            const double y = 1.0 * iterations_hist_classical[i];
            points.emplace_back(x, y);
        }
        plotfile.new_plot().set_points(points).set_title("Classical");
        points.clear();

        // Dwivedi.
        for (size_t i = 0; i < SamplesCount; ++i)
        {
            size_t iterations_count;
            const bool transmitted = do_randomwalk_dwivedi(
                rng,
                MaxIterations,
                0.5f,
                iterations_count);

            if (transmitted)
            {
                iterations_hist_dwivedi[iterations_count - 1]++;
                transmitted_count_dwivedi++;
            }
        }
        for (size_t i = 1; i < MaxIterations; ++i)
        {
            const double x = 1.0 * (i + 1);
            const double y = 1.0 * iterations_hist_dwivedi[i];
            points.emplace_back(x, y);
        }
        plotfile.new_plot().set_points(points).set_title("Dwivedi (mu=0.5)");
        plotfile.write("unit tests/outputs/test_sss_randomwalk_methods_comparison.gnuplot");
    }

    //
    // Gaussian BSSRDF.
    //

    TEST_CASE(GaussianBSSRDF_IntegrateProfile)
    {
        const float Rd = 0.5f;
        const float Mfp = 0.1f;
        const size_t SampleCount = 10000;

        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<GaussianBSSRDFFactory, GaussianBSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_rd_mfp(Rd, Mfp);

        const float integral = integrate_bssrdf_profile(bssrdf_eval, SampleCount);

        EXPECT_FEQ_EPS(Rd, integral, 1.0e-2f);
    }

    const float MaxRadiusTolerance = 0.007f;

    //
    // Normalized Diffusion BSSRDF.
    //

    TEST_CASE(NormalizedDiffusion_PlotS_mfp)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Scaling Factor For Searchlight Configuration With mfp Parameterization");
        plotfile.set_xlabel("A");
        plotfile.set_ylabel("s(A)");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 6.0);

        const size_t N = 1000;
        std::vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const float a = fit<size_t, float>(i, 0, N - 1, 0.0f, 1.0f);
            const float s = normalized_diffusion_s_mfp(a);
            points.emplace_back(a, s);
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_s_mfp.gnuplot");
    }

    TEST_CASE(NormalizedDiffusion_PlotS_dmfp)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Scaling Factor For Searchlight Configuration With dmfp Parameterization");
        plotfile.set_xlabel("A");
        plotfile.set_ylabel("s(A)");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 20.0);

        const size_t N = 1000;
        std::vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const float a = fit<size_t, float>(i, 0, N - 1, 0.0f, 1.0f);
            const float s = normalized_diffusion_s_dmfp(a);
            points.emplace_back(a, s);
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_s_dmfp.gnuplot");
    }

    TEST_CASE(NormalizedDiffusion_PlotR_mfp)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Reflectance Profile For Searchlight Configuration With mfp Parameterization");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("r R(r)");
        plotfile.set_xrange(0.0, 8.0);
        plotfile.set_yrange(0.001, 0.1);
        plotfile.set_logscale_y();

        for (size_t i = 9; i >= 1; --i)
        {
            const float a = static_cast<float>(i) / 10.0f;
            const float s = normalized_diffusion_s_mfp(a);

            const size_t N = 1000;
            std::vector<Vector2d> points;

            for (size_t j = 0; j < N; ++j)
            {
                const float r = std::max(fit<size_t, float>(j, 0, N - 1, 0.0f, 8.0f), 0.0001f);
                const float y = r * normalized_diffusion_profile(r, 1.0f, s, a);
                points.emplace_back(r, y);
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

        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_r_mfp.gnuplot");
    }

    TEST_CASE(NormalizedDiffusion_PlotR_dmfp)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Reflectance Profile For Searchlight Configuration With dmfp Parameterization");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("r R(r)");
        plotfile.set_xrange(0.0, 8.0);
        plotfile.set_yrange(0.001, 0.1);
        plotfile.set_logscale_y();

        for (size_t i = 9; i >= 1; --i)
        {
            const float a = static_cast<float>(i) / 10.0f;
            const float s = normalized_diffusion_s_dmfp(a);

            const size_t N = 1000;
            std::vector<Vector2d> points;

            for (size_t j = 0; j < N; ++j)
            {
                const float r = std::max(fit<size_t, float>(j, 0, N - 1, 0.0f, 8.0f), 0.0001f);
                const float y = r * normalized_diffusion_profile(r, 1.0f, s, a);
                points.emplace_back(r, y);
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

        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_r_dmfp.gnuplot");
    }

    TEST_CASE(NormalizedDiffusion_PlotCDF)
    {
        GnuplotFile plotfile;
        plotfile.set_title("CDF");
        plotfile.set_xlabel("r");
        plotfile.set_ylabel("u");
        plotfile.set_xrange(0.0, 40.0);
        plotfile.set_yrange(0.0, 1.0);

        const size_t N = 1000;
        std::vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const float r = fit<size_t, float>(i, 0, N - 1, 0.0f, 40.0f);
            const float u = normalized_diffusion_cdf(r, 1.0f);
            points.emplace_back(r, u);
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_normalized_diffusion_cdf.gnuplot");
    }

    TEST_CASE(NormalizedDiffusion_MaxRadius)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float a = rand_float1(rng);
            const float l = rand_float1(rng, 0.001f, 10.0f);

            const float s = normalized_diffusion_s_dmfp(a);
            const float r = normalized_diffusion_max_radius(l, s);
            const float value = normalized_diffusion_profile(r, l, s, a);

            EXPECT_LT(MaxRadiusTolerance, value);
        }
    }

    TEST_CASE(NormalizedDiffusion_IntegrateProfile)
    {
        const float Rd = 0.5f;
        const float Mfp = 0.1f;
        const size_t SampleCount = 10000;

        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<NormalizedDiffusionBSSRDFFactory, NormalizedDiffusionBSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_rd_mfp(Rd, Mfp);

        const float integral = integrate_bssrdf_profile(bssrdf_eval, SampleCount);

        EXPECT_FEQ_EPS(Rd, integral, 1.0e-2f);
    }

    //
    // Standard dipole BSSRDF.
    //

    TEST_CASE(StandardDipole_MaxRadius)
    {
        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float rd = rand_float1(rng);
            const float mfp = rand_float1(rng, 0.001f, 10.0f);

            BSSRDFEvaluator<StandardDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_rd_mfp(rd, mfp);

            const float r = dipole_max_radius(bssrdf_eval.get_sigma_tr());
            const float result = bssrdf_eval.evaluate(r);

            EXPECT_LT(MaxRadiusTolerance, result);
        }
    }

    TEST_CASE(StandardDipole_IntegrateProfile)
    {
        const float Rd = 0.5f;
        const float Mfp = 0.1f;
        const size_t SampleCount = 10000;

        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<StandardDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_rd_mfp(Rd, Mfp);

        const float integral = integrate_bssrdf_profile(bssrdf_eval, SampleCount);

        EXPECT_FEQ_EPS(Rd, integral, 1.0e-2f);
    }

    TEST_CASE(StandardDipole_PlotRdIntegral)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Standard Dipole Integral");
        plotfile.set_xlabel("Rd");
        plotfile.set_ylabel("Integral");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 1.25);

        auto_release_ptr<Project> project(ProjectFactory::create("project"));

        const size_t N = 200;
        std::vector<Vector2d> points;
        MersenneTwister rng;

        for (size_t i = 0; i < N; ++i)
        {
            const float rd = fit<size_t, float>(i, 0, N - 1, 0.01f, 1.0f);

            BSSRDFEvaluator<StandardDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_rd_mfp(rd, 1.0f);

            const float integral = integrate_bssrdf(bssrdf_eval, 1000);
            points.emplace_back(rd, integral);
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_standarddipole_rd_integral.gnuplot");
    }

    //
    // Better dipole BSSRDF.
    //

    TEST_CASE(BetterDipole_MaxRadius)
    {
        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float rd = rand_float1(rng);
            const float mfp = rand_float1(rng, 0.001f, 10.0f);

            BSSRDFEvaluator<BetterDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_rd_mfp(rd, mfp);

            const float r = dipole_max_radius(bssrdf_eval.get_sigma_tr());
            const float result = bssrdf_eval.evaluate(r);

            EXPECT_LT(MaxRadiusTolerance, result);
        }
    }

    TEST_CASE(BetterDipole_IntegrateProfile)
    {
        const float Rd = 0.5f;
        const float Mfp = 0.1f;
        const size_t SampleCount = 10000;

        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<BetterDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_rd_mfp(Rd, Mfp);

        const float integral = integrate_bssrdf_profile(bssrdf_eval, SampleCount);

        EXPECT_FEQ_EPS(Rd, integral, 0.015f);
    }

    TEST_CASE(BetterDipole_PlotRdIntegral)
    {
        GnuplotFile plotfile;
        plotfile.set_title("Better Dipole Integral");
        plotfile.set_xlabel("Rd");
        plotfile.set_ylabel("Integral");
        plotfile.set_xrange(0.0, 1.0);
        plotfile.set_yrange(0.0, 1.25);

        auto_release_ptr<Project> project(ProjectFactory::create("project"));

        const size_t N = 200;
        std::vector<Vector2d> points;
        MersenneTwister rng;

        for (size_t i = 0; i < N; ++i)
        {
            const float rd = fit<size_t, float>(i, 0, N - 1, 0.01f, 1.0f);

            BSSRDFEvaluator<BetterDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_rd_mfp(rd, 1.0f);

            const float integral = integrate_bssrdf(bssrdf_eval, 1000);
            points.emplace_back(rd, integral);
        }

        plotfile.new_plot().set_points(points);
        plotfile.write("unit tests/outputs/test_sss_betterdipole_rd_integral.gnuplot");
    }

    //
    // Directional dipole BSSRDF.
    //

    TEST_CASE(DirectionalDipole_MaxRadius)
    {
        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const float rd = rand_float1(rng);
            const float mfp = rand_float1(rng, 0.001f, 100.0f);

            BSSRDFEvaluator<DirectionalDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
            bssrdf_eval.set_values_from_rd_mfp(rd, mfp);

            const float r = dipole_max_radius(bssrdf_eval.get_sigma_tr());
            const float result = bssrdf_eval.evaluate(r);

            EXPECT_LT(MaxRadiusTolerance, result);
        }
    }

#if 0

    // There is still a misunderstanding about the directional dipole or a bug in our implementation
    // which makes this test fail. Commenting it until we figure things out.
    TEST_CASE(DirectionalDipole_IntegrateProfile)
    {
        const float Rd = 0.5f;
        const float Mfp = 0.1f;
        const size_t SampleCount = 10000;

        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<DirectionalDipoleBSSRDFFactory, DipoleBSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_rd_mfp(Rd, Mfp);

        const float integral = integrate_bssrdf_profile(bssrdf_eval, SampleCount);

        EXPECT_FEQ_EPS(Rd, integral, 1.0e-2f);
    }

#endif

    //
    // Comparison of the dipole-based models.
    //

    template <typename BSSRDFFactory, typename BSSRDFInputValues>
    void plot_rd_curve(
        GnuplotFile&    plotfile,
        const char*     title,
        const float     sigma_a,
        const float     sigma_s)
    {
        auto_release_ptr<Project> project(ProjectFactory::create("project"));
        BSSRDFEvaluator<BSSRDFFactory, BSSRDFInputValues> bssrdf_eval(project.ref());
        bssrdf_eval.set_values_from_sigmas(sigma_a, sigma_s);

        const size_t N = 1000;
        std::vector<Vector2d> points;

        for (size_t i = 0; i < N; ++i)
        {
            const float r = fit<size_t, float>(i, 0, N - 1, -16.0f, 16.0f);
            const float rd = bssrdf_eval.evaluate(r);   // integral of Lambertian BRDF equals 1
            points.emplace_back(r, rd);
        }

        plotfile
            .new_plot()
            .set_title(title)
            .set_points(points);
    }

    void plot_rd_curves(
        const char*     filename,
        const char*     title,
        const float     sigma_a,
        const float     sigma_s,
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

        plot_rd_curve<StandardDipoleBSSRDFFactory, DipoleBSSRDFInputValues>(
            plotfile,
            "Standard Dipole",
            sigma_a,
            sigma_s);

        plot_rd_curve<BetterDipoleBSSRDFFactory, DipoleBSSRDFInputValues>(
            plotfile,
            "Better Dipole",
            sigma_a,
            sigma_s);

        plot_rd_curve<DirectionalDipoleBSSRDFFactory, DipoleBSSRDFInputValues>(
            plotfile,
            "Directional Dipole",
            sigma_a,
            sigma_s);

        plotfile.write(filename);
    }

    TEST_CASE(DipoleBasedModels_PlotRdCurves)
    {
        plot_rd_curves(
            "unit tests/outputs/test_sss_rd_curves_sigma_a_001.gnuplot",
            "Diffuse Reflectance Curves (sigma_a = 0.01 cm^-1)",
            0.01f,          // sigma_a in cm^-1
            1.0f,           // sigma_s in cm^-1
            1.0e-5,         // ymin
            1.0e+1);        // ymax

        plot_rd_curves(
            "unit tests/outputs/test_sss_rd_curves_sigma_a_01.gnuplot",
            "Diffuse Reflectance Curves (sigma_a = 0.1 cm^-1)",
            0.1f,           // sigma_a in cm^-1
            1.0f,           // sigma_s in cm^-1
            1.0e-8,         // ymin
            1.0e+1);        // ymax

        plot_rd_curves(
            "unit tests/outputs/test_sss_rd_curves_sigma_a_1.gnuplot",
            "Diffuse Reflectance Curves (sigma_a = 1.0 cm^-1)",
            1.0f,           // sigma_a in cm^-1
            1.0f,           // sigma_s in cm^-1
            1.0e-16,        // ymin
            1.0e+1);        // ymax
    }
}
