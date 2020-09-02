
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

// Interface header.
#include "kelemenbrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/gnuplotfile.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;

namespace renderer
{

namespace
{
    class WardMDFAdapter
    {
      public:
        explicit WardMDFAdapter(const float alpha)
          : m_alpha(alpha)
        {
        }

        Vector3f sample(const Vector2f& s) const
        {
            return
                WardMDF::sample(
                    Vector3f(0.0f, 0.0f, 0.0f),
                    s,
                    m_alpha,
                    m_alpha);
        }

        float evaluate(const float cos_alpha) const
        {
            return
                WardMDF::D(
                    Vector3f(0.0f, cos_alpha, 0.0f),
                    m_alpha,
                    m_alpha);
        }

        float evaluate_pdf(const float cos_alpha) const
        {
            return
                WardMDF::pdf(
                    Vector3f(0.0f, 0.0f, 0.0f),
                    Vector3f(0.0f, cos_alpha, 0.0f),
                    m_alpha,
                    m_alpha);
        }

      private:
        const float m_alpha;
    };


    //
    // Kelemen BRDF.
    //
    // Reference:
    //
    //   A Microfacet Based Coupled Specular-Matte BRDF Model with Importance Sampling
    //   http://sirkan.iit.bme.hu/~szirmay/scook.pdf
    //

    const char* Model = "kelemen_brdf";

    const size_t AlbedoTableSize = 32;
    const size_t AlbedoSampleCount = 1024;

    class KelemenBRDFImpl
      : public BSDF
    {
      public:
        KelemenBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse | ScatteringMode::Glossy, params)
        {
            m_inputs.declare("matte_reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("matte_reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("specular_reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("specular_reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("roughness", InputFormat::Float);
        }

        ~KelemenBRDFImpl() override
        {
            assert(m_mdf.get() == 0);
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (!m_inputs.source("specular_reflectance")->is_uniform() ||
                !m_inputs.source("specular_reflectance_multiplier")->is_uniform() ||
                !m_inputs.source("roughness")->is_uniform())
            {
                RENDERER_LOG_ERROR(
                    "the \"specular_reflectance\", \"specular_reflectance_multiplier\" "
                    "and \"roughness\" parameters of the Kelemen BRDF model must be uniform.");
                return false;
            }

            InputValues values;
            m_inputs.evaluate_uniforms(&values);

            // Construct the Microfacet Distribution Function.
            m_mdf.reset(new MDFType(std::max(values.m_roughness, 1.0e-6f)));

            // Precompute the specular albedo curve.
            Spectrum rs(values.m_rs);
            rs *= values.m_rs_multiplier;
            compute_specular_albedo(*m_mdf.get(), rs, m_a_spec);

            // Precompute the average specular albedo.
            Spectrum a_spec_avg;
            compute_average_specular_albedo(m_a_spec, a_spec_avg);

            // Precompute the normalization constant for the matte component.
            Spectrum s_denom(1.0f);
            s_denom -= a_spec_avg;
            s_denom *= Pi<float>();
            m_s.set(1.0f);
            m_s /= s_denom;

            // Generate plot files for debug purposes.
            // plot_specular_albedo_curves();

            return true;
        }

        void on_frame_end(
            const Project&              project,
            const BaseGroup*            parent) override
        {
            m_mdf.reset();

            BSDF::on_frame_end(project, parent);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Define aliases to match notations in the paper.
            const Vector3f& V = outgoing.get_value();
            const Vector3f& N = local_geometry.m_shading_basis.get_normal();
            const float dot_VN = std::abs(dot(V, N));

            // Compute specular albedo for outgoing angle.
            Spectrum specular_albedo_V;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);

            // Compute matte albedo.
            Spectrum matte_albedo(1.0f);
            matte_albedo -= specular_albedo_V;
            matte_albedo *= values->m_rm;
            matte_albedo *= values->m_rm_multiplier;

            // Compute component weights.
            float matte_weight = ScatteringMode::has_diffuse(modes) ? average_value(matte_albedo) : 0.0f;
            float specular_weight = ScatteringMode::has_glossy(modes) ? 1.0f - matte_weight : 0.0f;
            const float total_weight = matte_weight + specular_weight;
            if (total_weight == 0.0f)
                return;
            const float rcp_total_weight = 1.0f / total_weight;
            matte_weight *= rcp_total_weight;
            specular_weight *= rcp_total_weight;

            // Generate a uniform sample in [0,1)^3.
            sampling_context.split_in_place(3, 1);
            const Vector3f s = sampling_context.next2<Vector3f>();

            ScatteringMode::Mode mode;
            Vector3f H, incoming;
            float dot_LN, dot_HN, dot_HV;

            // Select a component and sample it to compute the incoming direction.
            if (s[2] < matte_weight)
            {
                mode = ScatteringMode::Diffuse;

                // Compute the incoming direction.
                const Vector3f wi = sample_hemisphere_cosine(Vector2f(s[0], s[1]));
                incoming = local_geometry.m_shading_basis.transform_to_parent(wi);

                // Compute the halfway vector.
                H = normalize(incoming + V);

                dot_LN = wi.y;
                dot_HN = std::abs(dot(H, N));
                dot_HV = std::abs(dot(H, V));
            }
            else
            {
                mode = ScatteringMode::Glossy;

                // Sample the microfacet distribution to get an halfway vector H.
                const Vector3f local_H = m_mdf->sample(Vector2f(s[0], s[1]));
                H = local_geometry.m_shading_basis.transform_to_parent(local_H);

                // The incoming direction is the reflection of V around H.
                dot_HV = dot(H, V);
                incoming = (dot_HV + dot_HV) * H - V;

                dot_LN = dot(incoming, N);
                dot_HN = local_H.y;
                dot_HV = std::abs(dot_HV);

                // No reflection below the shading surface.
                if (dot_LN < 0.0f)
                    return;
            }

            float pdf_matte = 0.0f, pdf_specular = 0.0f;

            if (ScatteringMode::has_diffuse(modes) && matte_weight > 0.0f)
            {
                // Compute the specular albedo for the incoming angle.
                Spectrum specular_albedo_L;
                evaluate_a_spec(m_a_spec, dot_LN, specular_albedo_L);

                // Matte component (last equation of section 2.2f).
                Spectrum matte_comp(1.0f);
                matte_comp -= specular_albedo_L;
                matte_comp *= matte_albedo;
                matte_comp *= m_s;
                sample.m_value.m_diffuse = matte_comp;

                sample.m_aov_components.m_albedo = values->m_rm;

                // Evaluate the PDF of the incoming direction for the matte component.
                pdf_matte = dot_LN * RcpPi<float>();
                assert(pdf_matte >= 0.0f);
            }

            if (ScatteringMode::has_glossy(modes) && specular_weight > 0.0f)
            {
                // Specular component (equation 3).
                Spectrum rs(values->m_rs);
                rs *= values->m_rs_multiplier;
                evaluate_fr_spec(*m_mdf.get(), rs, dot_HV, dot_HN, sample.m_value.m_glossy);

                // Evaluate the PDF of the incoming direction for the specular component.
                const float pdf_H = m_mdf->evaluate_pdf(dot_HN);
                pdf_specular = pdf_H / (4.0f * dot_HV);
                assert(pdf_specular >= 0.0f);
            }

            const float probability = matte_weight * pdf_matte + specular_weight * pdf_specular;
            assert(probability >= 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(mode, probability);
                sample.m_incoming = Dual3f(incoming);
                sample.m_value.m_beauty = sample.m_value.m_diffuse;
                sample.m_value.m_beauty += sample.m_value.m_glossy;
                sample.m_min_roughness = values->m_roughness;
                sample.compute_glossy_reflected_differentials(local_geometry, values->m_roughness, outgoing);
            }
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            // Define aliases to match the notations in the paper.
            const Vector3f& V = outgoing;
            const Vector3f& L = incoming;
            const Vector3f& N = local_geometry.m_shading_basis.get_normal();
            const float dot_VN = std::abs(dot(V, N));
            const float dot_LN = std::abs(dot(L, N));

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the halfway vector.
            const Vector3f H = normalize(L + V);
            const float dot_HN = std::abs(dot(H, N));
            const float dot_HL = std::min(dot(H, L), 1.0f);

            // Compute the specular albedo for the outgoing angle.
            Spectrum specular_albedo_V;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);

            // Compute the matte albedo.
            Spectrum matte_albedo(1.0f);
            matte_albedo -= specular_albedo_V;
            matte_albedo *= values->m_rm;
            matte_albedo *= values->m_rm_multiplier;

            // Compute component weights.
            float matte_weight = ScatteringMode::has_diffuse(modes) ? average_value(matte_albedo) : 0.0f;
            float specular_weight = ScatteringMode::has_glossy(modes) ? 1.0f - matte_weight : 0.0f;
            const float total_weight = matte_weight + specular_weight;
            if (total_weight == 0.0f)
                return 0.0f;
            const float rcp_total_weight = 1.0f / total_weight;
            matte_weight *= rcp_total_weight;
            specular_weight *= rcp_total_weight;

            float pdf_matte = 0.0f, pdf_specular = 0.0f;

            if (ScatteringMode::has_diffuse(modes))
            {
                // Compute the specular albedo for the incoming angle.
                Spectrum specular_albedo_L;
                evaluate_a_spec(m_a_spec, dot_LN, specular_albedo_L);

                // Compute the matte component (last equation of section 2.2).
                Spectrum matte_comp(1.0f);
                matte_comp -= specular_albedo_L;
                matte_comp *= matte_albedo;
                matte_comp *= m_s;
                value.m_diffuse = matte_comp;

                // Evaluate the PDF of the incoming direction for the matte component.
                pdf_matte = dot_LN * RcpPi<float>();
                assert(pdf_matte >= 0.0f);
            }

            if (ScatteringMode::has_glossy(modes))
            {
                // Compute the specular component (equation 3).
                Spectrum rs(values->m_rs);
                rs *= values->m_rs_multiplier;
                evaluate_fr_spec(*m_mdf.get(), rs, dot_HL, dot_HN, value.m_glossy);

                // Evaluate the PDF of the incoming direction for the specular component.
                const float pdf_H = m_mdf->evaluate_pdf(dot_HN);
                pdf_specular = pdf_H / (4.0f * dot_HL);
                assert(pdf_specular >= 0.0f);
            }

            value.m_beauty = value.m_diffuse;
            value.m_beauty += value.m_glossy;

            const float pdf = matte_weight * pdf_matte + specular_weight * pdf_specular;
            assert(pdf >= 0.0f);

            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            // Define aliases to match the notations in the paper.
            const Vector3f& V = outgoing;
            const Vector3f& L = incoming;
            const Vector3f& N = local_geometry.m_shading_basis.get_normal();
            const float dot_VN = std::abs(dot(V, N));
            const float dot_LN = std::abs(dot(L, N));

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the halfway vector.
            const Vector3f H = normalize(L + V);
            const float dot_HN = std::abs(dot(H, N));
            const float dot_HL = dot(H, L);

            // Compute the specular albedo for the outgoing angle.
            Spectrum specular_albedo_V;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);

            // Compute the matte albedo.
            Spectrum matte_albedo(1.0f);
            matte_albedo -= specular_albedo_V;
            matte_albedo *= values->m_rm;
            matte_albedo *= values->m_rm_multiplier;

            // Compute component weights.
            float matte_weight = ScatteringMode::has_diffuse(modes) ? average_value(matte_albedo) : 0.0f;
            float specular_weight = ScatteringMode::has_glossy(modes) ? 1.0f - matte_weight : 0.0f;
            const float total_weight = matte_weight + specular_weight;
            if (total_weight == 0.0f)
                return 0.0f;
            const float rcp_total_weight = 1.0f / total_weight;
            matte_weight *= rcp_total_weight;
            specular_weight *= rcp_total_weight;

            float pdf_matte = 0.0f, pdf_specular = 0.0f;

            if (ScatteringMode::has_diffuse(modes))
            {
                // Evaluate the PDF of the incoming direction for the matte component.
                pdf_matte = dot_LN * RcpPi<float>();
                assert(pdf_matte >= 0.0f);
            }

            if (ScatteringMode::has_glossy(modes))
            {
                // Evaluate the PDF of the incoming direction for the specular component.
                const float pdf_H = m_mdf->evaluate_pdf(dot_HN);
                pdf_specular = pdf_H / (4.0f * dot_HL);
                assert(pdf_specular >= 0.0f);
            }

            const float pdf = matte_weight * pdf_matte + specular_weight * pdf_specular;
            assert(pdf >= 0.0f);

            return pdf;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum                 m_rm;                           // matte reflectance of the substrate
            float                    m_rm_multiplier;                // matte reflectance multiplier
            Spectrum                 m_rs;                           // specular reflectance at normal incidence
            float                    m_rs_multiplier;                // specular reflectance multiplier
            float                    m_roughness;                    // technically, root-mean-square of the microfacets slopes
        };

        typedef WardMDFAdapter MDFType;

        std::unique_ptr<MDFType>     m_mdf;                          // Microfacet Distribution Function
        Spectrum                     m_a_spec[AlbedoTableSize];      // albedo of the specular component as V varies
        Spectrum                     m_s;                            // normalization constant for the matte component

        // Evaluate the specular component of the BRDF (equation 3).
        template <typename MDF>
        static void evaluate_fr_spec(
            const MDF&               mdf,
            const Spectrum&          rs,
            const float              dot_HL,     // cos_beta in the paper
            const float              dot_HN,
            Spectrum&                fr_spec)
        {
            assert(dot_HL >  0.0f);
            assert(dot_HN >= 0.0f);

            fresnel_reflectance_dielectric_schlick(fr_spec, rs, dot_HL);
            fr_spec *= mdf.evaluate(dot_HN) / (4.0f * dot_HL * dot_HL);
        }

        // Compute the specular albedo function.
        template <typename MDF>
        static void compute_specular_albedo(
            const MDF&          mdf,
            const Spectrum&     rs,
            Spectrum            albedo[])
        {
            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                // Compute an outgoing direction V in the XY plane.
                const float cos_theta = static_cast<float>(i) / (AlbedoTableSize - 1);
                const float sin_theta = std::sqrt(1.0f - cos_theta * cos_theta);
                const Vector3f V(sin_theta, cos_theta, 0.0f);

                // Compute the albedo for this outgoing direction.
                compute_specular_albedo(mdf, rs, V, albedo[i]);
            }
        }

        // Compute the albedo of the specular component for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
        template <typename MDF>
        static void compute_specular_albedo(
            const MDF&          mdf,
            const Spectrum&     rs,
            const Vector3f&     V,
            Spectrum&           albedo)
        {
            // V must lie above or in the surface.
            assert(V.y >= 0.0f);

            albedo.set(0.0f);

            for (size_t i = 0; i < AlbedoSampleCount; ++i)
            {
                // Generate a uniform sample in [0,1)^2.
                static const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, AlbedoSampleCount, i);

                // Sample the microfacet distribution to get an halfway vector H.
                const Vector3f H = mdf.sample(s);
                const float dot_HV = dot(H, V);
                if (dot_HV <= 0.0f)
                    continue;

                // L is the reflection of V around H.
                const Vector3f L = (dot_HV + dot_HV) * H - V;

                // Reject L if it lies in or below the surface.
                if (L.y <= 0.0f)
                    continue;

                // Evaluate the PDF of L.
                const float dot_HN = H.y;
                const float pdf_H = mdf.evaluate_pdf(dot_HN);
                const float pdf_L = pdf_H / (4.0f * dot_HV);
                assert(pdf_L >= 0.0f);
                if (pdf_L == 0.0f)
                    continue;

                // Sanity checks.
                assert(is_normalized(V));
                assert(is_normalized(H));
                assert(is_normalized(L));

                // Evaluate the specular component for this (L, V) pair.
                Spectrum fr_spec;
                fresnel_reflectance_dielectric_schlick(fr_spec, rs, dot_HV);
                fr_spec *= (L.y * mdf.evaluate(dot_HN)) / (4.0f * pdf_L * dot_HV * dot_HV);
                albedo += fr_spec;
            }

            albedo /= static_cast<float>(AlbedoSampleCount);
        }

        // Compute the average specular albedo.
        static void compute_average_specular_albedo(
            const Spectrum      a_spec[],
            Spectrum&           a_spec_avg)
        {
            a_spec_avg.set(0.0f);

            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                const float cos_theta = static_cast<float>(i) / (AlbedoTableSize - 1);
                const float sin_theta = std::sqrt(1.0f - cos_theta * cos_theta);

                Spectrum sample = a_spec[i];
                sample *= cos_theta * sin_theta;

                a_spec_avg += sample;
            }

            a_spec_avg *= HalfPi<float>() / AlbedoTableSize;    // integration over theta
            a_spec_avg *= TwoPi<float>();                       // integration over phi
            a_spec_avg *= RcpPi<float>();                       // average
        }

        // Evaluate the specular albedo function for an arbitrary angle.
        static void evaluate_a_spec(
            const Spectrum      a_spec[],
            const float         cos_theta,
            Spectrum&           result)
        {
            assert(cos_theta >= 0.0f && cos_theta <= 1.0f);

            const float t = (AlbedoTableSize - 1) * cos_theta;
            const size_t i = truncate<size_t>(t);
            const float x = t - i;

            assert(i <= AlbedoTableSize - 1);
            assert(x >= 0.0f && x < 1.0f);

            if (i < AlbedoTableSize - 1)
            {
                // Piecewise linear reconstruction.
                const Spectrum& prev_a = a_spec[i];
                const Spectrum& next_a = a_spec[i + 1];
                result = next_a;
                result -= prev_a;
                result *= x;
                result += prev_a;
            }
            else
            {
                result = a_spec[AlbedoTableSize - 1];
            }
        }

        static void plot_specular_albedo_curves()
        {
            plot_specular_albedo_curve(0.8f, Spectrum(1.0f));
            plot_specular_albedo_curve(0.4f, Spectrum(1.0f));
            plot_specular_albedo_curve(0.03f, Spectrum(1.0f));
            plot_specular_albedo_curve(1.0e-6f, Spectrum(1.0f));
        }

        static void plot_specular_albedo_curve(
            const float         m,
            const Spectrum&     rs)
        {
            generate_specular_albedo_plot_data("Ward", m, WardMDFAdapter(m), rs);
        }

        template <typename MDF>
        static void generate_specular_albedo_plot_data(
            const std::string&  mdf_name,
            const float         m,
            const MDF&          mdf,
            const Spectrum&     rs)
        {
            Spectrum a_spec[AlbedoTableSize];
            compute_specular_albedo(mdf, rs, a_spec);

            std::vector<Vector2f> tabulated_albedos(AlbedoTableSize);

            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                const float cos_angle = static_cast<float>(i) / (AlbedoTableSize - 1);
                const float angle = std::acos(cos_angle);
                const float albedo = average_value(a_spec[i]);
                tabulated_albedos[i] = Vector2f(angle, albedo);
            }

            const size_t PointCount = 256;
            std::vector<Vector2f> reconstructed_albedos(PointCount);

            for (size_t i = 0; i < PointCount; ++i)
            {
                const float cos_angle = static_cast<float>(i) / (PointCount - 1);
                const float angle = std::acos(cos_angle);

                Spectrum albedo_sample;
                evaluate_a_spec(a_spec, cos_angle, albedo_sample);

                const float albedo = average_value(albedo_sample);
                reconstructed_albedos[i] = Vector2f(angle, albedo);
            }

            GnuplotFile plotfile;
            plotfile.set_title("Specular Albedo using " + mdf_name + " Microfacet Distribution Function with m=" + to_string(m));

            plotfile
                .new_plot()
                .set_points(tabulated_albedos)
                .set_title("Tabulated")
                .set_style("points pointtype 13")
                .set_color("black");

            plotfile
                .new_plot()
                .set_points(reconstructed_albedos)
                .set_title("Reconstruction")
                .set_style("lines")
                .set_color("blue");

            std::stringstream filename;
            filename << "kelemen_albedo";
            filename << "_" << lower_case(mdf_name);
            filename << "_" << replace(to_string(m), ".", "_");
            filename << ".gnuplot";

            plotfile.write(filename.str());
        }
    };

    typedef BSDFWrapper<KelemenBRDFImpl> KelemenBRDF;
}


//
// KelemenBRDFFactory class implementation.
//

void KelemenBRDFFactory::release()
{
    delete this;
}

const char* KelemenBRDFFactory::get_model() const
{
    return Model;
}

Dictionary KelemenBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Kelemen BRDF");
}

DictionaryArray KelemenBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "matte_reflectance")
            .insert("label", "Matte Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "matte_reflectance_multiplier")
            .insert("label", "Matte Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance")
            .insert("label", "Specular Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance_multiplier")
            .insert("label", "Specular Reflectance Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "0.5"));

    return metadata;
}

auto_release_ptr<BSDF> KelemenBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new KelemenBRDF(name, params));
}

}   // namespace renderer
