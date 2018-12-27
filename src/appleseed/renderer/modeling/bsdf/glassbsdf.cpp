
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "glassbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/energycompensation.h"
#include "renderer/modeling/bsdf/energycompensationtables.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace bfs = boost::filesystem;

namespace renderer
{

namespace
{
    //
    // Glass BSDF.
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    //   [2] Extending the Disney BRDF to a BSDF with Integrated Subsurface Scattering
    //       http://blog.selfshadow.com/publications/s2015-shading-course/burley/s2015_pbs_disney_bsdf_notes.pdf
    //
    //   [3] Revisiting Physically Based Shading at Imageworks
    //       http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
    //

    float get_dir_albedo(
        const GGXMDF&       mdf,
        const float         eta,
        const float         roughness,
        const float         cos_theta);

    float get_dir_albedo(
        const BeckmannMDF&  mdf,
        const float         eta,
        const float         roughness,
        const float         cos_theta);

    float get_avg_albedo(
        const GGXMDF&       mdf,
        const float         eta,
        const float         roughness);

    float get_avg_albedo(
        const BeckmannMDF&  mdf,
        const float         eta,
        const float         roughness);

    const char* Model = "glass_bsdf";

    class GlassBSDFImpl
      : public BSDF
    {
        typedef GlassBSDFInputValues InputValues;

      public:
        GlassBSDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("surface_transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("surface_transmittance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("reflection_tint", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("refraction_tint", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("roughness", InputFormatFloat, "0.15");
            m_inputs.declare("highlight_falloff", InputFormatFloat, "0.4");
            m_inputs.declare("anisotropy", InputFormatFloat, "0.0");
            m_inputs.declare("ior", InputFormatFloat, "1.5");
            m_inputs.declare("volume_transmittance", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("volume_transmittance_distance", InputFormatFloat, "0.0");
            m_inputs.declare("volume_absorption", InputFormatSpectralReflectance, "0.0");
            m_inputs.declare("volume_density", InputFormatFloat, "0.0");
            m_inputs.declare("volume_scale", InputFormatFloat, "1.0");
            m_inputs.declare("energy_compensation", InputFormatFloat, "0.0");
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

            const OnFrameBeginMessageContext context("bsdf", this);

            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "ggx",
                    make_vector("beckmann", "ggx", "std"),
                    context);

            if (mdf == "ggx")
                m_mdf_type = GGX;
            else if (mdf == "beckmann")
                m_mdf_type = Beckmann;
            else if (mdf == "std")
                m_mdf_type = Std;
            else return false;

            const string volume_parameterization =
                m_params.get_required<string>(
                    "volume_parameterization",
                    "transmittance",
                    make_vector("transmittance", "absorption"),
                    context);

            if (volume_parameterization == "transmittance")
                m_volume_parameterization = TransmittanceParameterization;
            else if (volume_parameterization == "absorption")
                m_volume_parameterization = AbsorptionParameterization;
            else return false;

            return true;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();

            values->m_roughness = max(values->m_roughness, shading_point.get_ray().m_min_roughness);

            if (shading_point.is_entering())
            {
                values->m_precomputed.m_backfacing = false;
                values->m_precomputed.m_outside_ior = shading_point.get_ray().get_current_ior();
            }
            else
            {
                values->m_precomputed.m_backfacing = true;
                values->m_precomputed.m_outside_ior = shading_point.get_ray().get_previous_ior();
            }

            values->m_precomputed.m_reflection_color  = values->m_surface_transmittance;
            values->m_precomputed.m_reflection_color *= values->m_reflection_tint;
            values->m_precomputed.m_reflection_color *= values->m_surface_transmittance_multiplier;

            // [2] Surface absorption, page 5.
            values->m_precomputed.m_refraction_color  = values->m_surface_transmittance;
            values->m_precomputed.m_refraction_color *= values->m_refraction_tint;
            values->m_precomputed.m_refraction_color *= values->m_surface_transmittance_multiplier;
            values->m_precomputed.m_refraction_color  = sqrt(values->m_precomputed.m_refraction_color);

            // Weights used when choosing reflection or refraction.
            values->m_precomputed.m_reflection_weight = max(max_value(values->m_precomputed.m_reflection_color), 0.0f);
            values->m_precomputed.m_refraction_weight = max(max_value(values->m_precomputed.m_refraction_color), 0.0f);
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
                return;

            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(
                          -sample.m_shading_basis.get_normal(),
                           sample.m_shading_basis.get_tangent_u(),
                          -sample.m_shading_basis.get_tangent_v())
                    : sample.m_shading_basis);

            const Vector3f wo = basis.transform_to_local(sample.m_outgoing.get_value());

            const float eta =
                wo.y > 0.0f
                    ? values->m_ior / values->m_precomputed.m_outside_ior
                    : values->m_precomputed.m_outside_ior / values->m_ior;

            if (APPLESEED_UNLIKELY(eta == 1.0f))
                return;

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            sampling_context.split_in_place(3, 1);
            const Vector3f s = sampling_context.next2<Vector3f>();

            Vector3f wi;
            bool is_refraction;
            float probability;

            switch (m_mdf_type)
            {
              case GGX:
                {
                    const GGXMDF mdf;
                    is_refraction = do_sample(
                        mdf,
                        s,
                        adjoint,
                        basis,
                        alpha_x,
                        alpha_y,
                        0.0f, // gamma
                        values->m_precomputed.m_reflection_color,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_color,
                        values->m_precomputed.m_refraction_weight,
                        eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        probability);

                    add_energy_compensation_term(
                        mdf,
                        values,
                        Vector3f(0.0f, 1.0f, 0.0f),
                        wo,
                        wi,
                        sample.m_value.m_glossy);
                }
                break;

              case Beckmann:
                {
                    const BeckmannMDF mdf;
                    is_refraction = do_sample(
                        mdf,
                        s,
                        adjoint,
                        basis,
                        alpha_x,
                        alpha_y,
                        0.0f, // gamma
                        values->m_precomputed.m_reflection_color,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_color,
                        values->m_precomputed.m_refraction_weight,
                        eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        probability);

                    add_energy_compensation_term(
                        mdf,
                        values,
                        Vector3f(0.0f, 1.0f, 0.0f),
                        wo,
                        wi,
                        sample.m_value.m_glossy);
                }
                break;

              case Std:
                {
                    const StdMDF mdf;
                    is_refraction = do_sample(
                        mdf,
                        s,
                        adjoint,
                        basis,
                        alpha_x,
                        alpha_y,
                        highlight_falloff_to_gama(values->m_highlight_falloff),
                        values->m_precomputed.m_reflection_color,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_color,
                        values->m_precomputed.m_refraction_weight,
                        eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        probability);
                }
                break;

              default:
                assert(!"Unexpected MDF type.");
                probability = 0.0f;
                break;
            }

            assert(probability >= 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(ScatteringMode::Glossy, probability);
                sample.m_value.m_beauty = sample.m_value.m_glossy;
                sample.m_incoming = Dual3f(basis.transform_to_parent(wi));
                sample.m_min_roughness = values->m_roughness;

                if (is_refraction)
                    sample.compute_transmitted_differentials(1.0f / eta);
                else sample.compute_reflected_differentials();
            }
        }

        template <typename MDF, typename SpectrumType>
        static bool do_sample(
            const MDF&                  mdf,
            const Vector3f&             s,
            const bool                  adjoint,
            const Basis3f&              basis,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const SpectrumType&         reflection_color,
            const float                 reflection_weight,
            const SpectrumType&         refraction_color,
            const float                 refraction_weight,
            const float                 eta,
            const Vector3f&             wo,
            Vector3f&                   wi,
            SpectrumType&               value,
            float&                      probability)
        {
            // Compute the microfacet normal by sampling the MDF.
            const Vector3f m = mdf.sample(wo, Vector2f(s[0], s[1]), alpha_x, alpha_y, gamma);
            assert(m.y > 0.0f);

            const float rcp_eta = 1.0f / eta;

            const float cos_wom = clamp(dot(wo, m), -1.0f, 1.0f);

            float cos_theta_t;
            const float F = fresnel_reflectance(cos_wom, rcp_eta, cos_theta_t);
            const float r_probability =
                choose_reflection_probability(
                    reflection_weight,
                    refraction_weight,
                    F);

            bool is_refraction;

            // Choose between reflection and refraction.
            if (s[2] < r_probability)
            {
                is_refraction = false;

                // Compute the reflected direction.
                wi = improve_normalization(reflect(wo, m));

                // If incoming and outgoing are on different sides of the surface, this is not a reflection.
                if (wi.y * wo.y <= 0.0f)
                {
                    probability = 0.0f;
                    return true;
                }

                evaluate_reflection(
                    mdf,
                    reflection_color,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    F,
                    value);

                probability =
                    r_probability *
                    reflection_pdf(mdf, wo, m, cos_wom, alpha_x, alpha_y, gamma);
            }
            else
            {
                is_refraction = true;

                // Compute the refracted direction.
                wi =
                    cos_wom > 0.0f
                        ? (rcp_eta * cos_wom - cos_theta_t) * m - rcp_eta * wo
                        : (rcp_eta * cos_wom + cos_theta_t) * m - rcp_eta * wo;
                wi = improve_normalization(wi);

                // If incoming and outgoing are on the same side of the surface, this is not a refraction.
                if (wi.y * wo.y > 0.0f)
                {
                    probability = 0.0f;
                    return false;
                }

                evaluate_refraction(
                    mdf,
                    eta,
                    refraction_color,
                    adjoint,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    1.0f - F,
                    value);

                // Recompute the half vector to have a better
                // match with the result of the pdf method.
                const Vector3f m_pdf = half_refraction_vector(wo, wi, eta);

                probability =
                    (1.0f - r_probability) *
                    refraction_pdf(
                        mdf,
                        wo,
                        wi,
                        m_pdf,
                        alpha_x,
                        alpha_y,
                        gamma,
                        eta);
            }

            assert(probability > 0.0f);

            return is_refraction;
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            float pdf;

            switch (m_mdf_type)
            {
              case GGX:
                {
                    const GGXMDF mdf;
                    pdf = do_evaluate(
                        mdf,
                        values,
                        adjoint,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);
                    add_energy_compensation_term(
                        mdf,
                        values,
                        shading_basis.get_normal(),
                        outgoing,
                        incoming,
                        value.m_glossy);
                }
                break;

              case Beckmann:
                {
                    const BeckmannMDF mdf;
                    pdf = do_evaluate(
                        mdf,
                        values,
                        adjoint,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);
                    add_energy_compensation_term(
                        mdf,
                        values,
                        shading_basis.get_normal(),
                        outgoing,
                        incoming,
                        value.m_glossy);
                }
                break;

              case Std:
                {
                    const StdMDF mdf;
                    pdf = do_evaluate(
                        mdf,
                        values,
                        adjoint,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);
                }
                break;

              default:
                assert(!"Unexpected MDF type.");
                pdf = 0.0f;
                break;
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        template <typename MDF>
        static float do_evaluate(
            const MDF&                  mdf,
            const InputValues*          values,
            const bool                  adjoint,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            DirectShadingComponents&    value)
        {
            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(-shading_basis.get_normal(), shading_basis.get_tangent_u(), -shading_basis.get_tangent_v())
                    : shading_basis);

            const Vector3f wo = basis.transform_to_local(outgoing);

            const float eta =
                wo.y > 0.0f
                    ? values->m_ior / values->m_precomputed.m_outside_ior
                    : values->m_precomputed.m_outside_ior / values->m_ior;

            if (APPLESEED_UNLIKELY(eta == 1.0f))
            {
                value.set(0.0f);
                return 0.0f;
            }

            const float rcp_eta = 1.0f / eta;

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wi = basis.transform_to_local(incoming);
            float pdf;

            if (wi.y * wo.y >= 0.0f)
            {
                // Reflection.
                const Vector3f m = half_reflection_vector(wo, wi);
                const float cos_wom = dot(wo, m);

                const float F = fresnel_reflectance(cos_wom, rcp_eta);

                evaluate_reflection(
                    mdf,
                    values->m_precomputed.m_reflection_color,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    F,
                    value.m_glossy);
                value.m_beauty = value.m_glossy;

                const float r_probability =
                    choose_reflection_probability(
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_weight,
                        F);

                pdf =
                    r_probability *
                    reflection_pdf(mdf, wo, m, cos_wom, alpha_x, alpha_y, gamma);
            }
            else
            {
                // Refraction.

                const Vector3f m = half_refraction_vector(wo, wi, eta);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, 1.0f / eta);

                evaluate_refraction(
                    mdf,
                    eta,
                    values->m_precomputed.m_refraction_color,
                    adjoint,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    1.0f - F,
                    value.m_glossy);
                value.m_beauty = value.m_glossy;

                const float r_probability =
                    choose_reflection_probability(
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_weight,
                        F);

                pdf =
                    (1.0f - r_probability) *
                    refraction_pdf(mdf, wo, wi, m, alpha_x, alpha_y, gamma, eta);
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            float pdf;

            switch (m_mdf_type)
            {
              case GGX:
                {
                    const GGXMDF mdf;
                    pdf = do_evaluate_pdf(
                        mdf,
                        values,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              case Beckmann:
                {
                    const BeckmannMDF mdf;
                    pdf = do_evaluate_pdf(
                        mdf,
                        values,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              case Std:
                {
                    const StdMDF mdf;
                    pdf = do_evaluate_pdf(
                        mdf,
                        values,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              default:
                assert(!"Unexpected MDF type.");
                pdf = 0.0f;
                break;
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        template <typename MDF>
        static float do_evaluate_pdf(
            const MDF&                  mdf,
            const InputValues*          values,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming)
        {
            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(-shading_basis.get_normal(), shading_basis.get_tangent_u(), -shading_basis.get_tangent_v())
                    : shading_basis);

            const Vector3f wo = basis.transform_to_local(outgoing);

            const float eta =
                wo.y > 0.0f
                    ? values->m_ior / values->m_precomputed.m_outside_ior
                    : values->m_precomputed.m_outside_ior / values->m_ior;

            if (APPLESEED_UNLIKELY(eta == 1.0f))
                return 0.0f;

            const float rcp_eta = 1.0f / eta;

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wi = basis.transform_to_local(incoming);
            float pdf;

            if (wi.y * wo.y >= 0.0f)
            {
                // Reflection.
                const Vector3f m = half_reflection_vector(wo, wi);
                const float cos_wom = dot(wo, m);

                const float F = fresnel_reflectance(cos_wom, rcp_eta);

                const float r_probability =
                    choose_reflection_probability(
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_weight,
                        F);

                pdf =
                    r_probability *
                    reflection_pdf(mdf, wo, m, cos_wom, alpha_x, alpha_y, gamma);
            }
            else
            {
                // Refraction.

                const Vector3f m = half_refraction_vector(wo, wi, eta);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, rcp_eta);

                const float r_probability = choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

                pdf =
                    (1.0f - r_probability) *
                    refraction_pdf(mdf, wo, wi, m, alpha_x, alpha_y, gamma, eta);
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        float sample_ior(
            SamplingContext&            sampling_context,
            const void*                 data) const override
        {
            return static_cast<const InputValues*>(data)->m_ior;
        }

        void compute_absorption(
            const void*                 data,
            const float                 distance,
            Spectrum&                   absorption) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (m_volume_parameterization == TransmittanceParameterization)
            {
                if (values->m_volume_transmittance_distance == 0.0f)
                    absorption.set(1.0f);
                else
                {
                    // [2] Volumetric absorption reparameterization, page 5.
                    const float d = distance / values->m_volume_transmittance_distance;
                    for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
                    {
                        const float a = log(max(values->m_volume_transmittance[i], 0.01f));
                        absorption[i] = exp(a * d);
                    }
                }
            }
            else
            {
                const float d = values->m_volume_density * values->m_volume_scale * distance;

                for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
                {
                    //
                    // Reference:
                    //
                    //   Beer-Lambert law:
                    //   https://en.wikipedia.org/wiki/Beer%E2%80%93Lambert_law
                    //

                    const float a = values->m_volume_absorption[i];
                    const float optical_depth = a * d;
                    absorption[i] = exp(-optical_depth);
                }
            }
        }

      private:
        enum MDFType
        {
            GGX = 0,
            Beckmann,
            Std
        };

        MDFType m_mdf_type;

        enum VolumeParameterization
        {
            TransmittanceParameterization,
            AbsorptionParameterization
        };

        VolumeParameterization  m_volume_parameterization;

        static float choose_reflection_probability(
            const float                 reflection_weight,
            const float                 refraction_weight,
            const float                 F)
        {
            const float r_probability = F * reflection_weight;
            const float t_probability = (1.0f - F) * refraction_weight;
            const float sum_probabilities = r_probability + t_probability;
            return sum_probabilities != 0.0f ? r_probability / sum_probabilities : 1.0f;
        }

        static float fresnel_reflectance(
            const float                 cos_theta_i,
            const float                 eta,
            float&                      cos_theta_t)
        {
            const float sin_theta_t2 = (1.0f - square(cos_theta_i)) * square(eta);
            if (sin_theta_t2 > 1.0f)
            {
                cos_theta_t = 0.0f;
                return 1.0f;
            }

            cos_theta_t = min(sqrt(max(1.0f - sin_theta_t2, 0.0f)), 1.0f);

            float F;
            fresnel_reflectance_dielectric(
                F,
                eta,
                abs(cos_theta_i),
                cos_theta_t);

            return F;
        }

        static float fresnel_reflectance(
            const float                 cos_theta_i,
            const float                 eta)
        {
            float cos_theta_t;
            return fresnel_reflectance(cos_theta_i, eta, cos_theta_t);
        }

        static Vector3f half_reflection_vector(
            const Vector3f&             wo,
            const Vector3f&             wi)
        {
            // [1] eq. 13.
            const Vector3f h = normalize(wi + wo);
            return h.y < 0.0f ? -h : h;
        }

        template <typename SpectrumType>
        static void evaluate_reflection(
            const MDF&                  mdf,
            const SpectrumType&         reflection_color,
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const float                 F,
            SpectrumType&               value)
        {
            // [1] eq. 20.
            const float denom = abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                set_to_zero(value);
                return;
            }

            const float D = mdf.D(m, alpha_x, alpha_y, gamma);
            const float G = mdf.G(wi, wo, m, alpha_x, alpha_y, gamma);

            value = reflection_color;
            value *= F * D * G / denom;
        }

        static float reflection_pdf(
            const MDF&                  mdf,
            const Vector3f&             wo,
            const Vector3f&             m,
            const float                 cos_oh,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma)
        {
            // [1] eq. 14.
            if (cos_oh == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * abs(cos_oh));
            return jacobian * mdf.pdf(wo, m, alpha_x, alpha_y, gamma);
        }

        static Vector3f half_refraction_vector(
            const Vector3f&             wo,
            const Vector3f&             wi,
            const float                 eta)
        {
            // [1] eq. 16.
            const Vector3f h = normalize(wo + eta * wi);
            return h.y < 0.0f ? -h : h;
        }

        template <typename SpectrumType>
        static void evaluate_refraction(
            const MDF&                  mdf,
            const float                 eta,
            const SpectrumType&         refraction_color,
            const bool                  adjoint,
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const float                 T,
            SpectrumType&               value)
        {
            if (wo.y == 0.0f || wi.y == 0.0f)
            {
                set_to_zero(value);
                return;
            }

            // [1] eq. 21.
            const float cos_ih = dot(m, wi);
            const float cos_oh = dot(m, wo);
            const float dots = (cos_ih * cos_oh) / (wi.y * wo.y);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (abs(sqrt_denom) < 1.0e-6f)
            {
                set_to_zero(value);
                return;
            }

            const float D = mdf.D(m, alpha_x, alpha_y, gamma);
            const float G = mdf.G(wi, wo, m, alpha_x, alpha_y, gamma);

            float multiplier = abs(dots) * T * D * G / square(sqrt_denom);

            if (!adjoint)
                multiplier *= square(eta);

            value = refraction_color;
            value *= multiplier;
        }

        static float refraction_pdf(
            const MDF&                  mdf,
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const float                 eta)
        {
            // [1] eq. 17.
            const float cos_ih = dot(m, wi);
            const float cos_oh = dot(m, wo);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (abs(sqrt_denom) < 1.0e-6f)
                return 0.0f;

            const float jacobian = abs(cos_ih) * square(eta / sqrt_denom);
            return jacobian * mdf.pdf(wo, m, alpha_x, alpha_y, gamma);
        }

        static void set_to_zero(float& x)
        {
            x = 0.0f;
        }

        static void set_to_zero(Spectrum& x)
        {
            x.set(0.0f);
        }

        static float ec_lobe_ratio(
            const float eta,
            const float favg,
            const float favg_rcp_eta,
            const float eavg,
            const float eavg_rcp_eta,
            const float reflection_weight,
            const float refraction_weight)
        {
            const float a = (1.0f - favg) / (1.0f - eavg_rcp_eta);
            const float b = (1.0f - favg_rcp_eta) * square(eta) / (1.0f - eavg);
            const float ratio = b / (a + b);

            const float r_ratio = reflection_weight * ratio;
            const float t_ratio = refraction_weight * (1.0f - ratio);
            const float sum_ratios = r_ratio + t_ratio;

            if (sum_ratios != 0.0f)
                return r_ratio / sum_ratios;

            return 0.0f;
        }

        template <typename MDF>
        static void add_energy_compensation_term(
            const MDF&                  mdf,
            const InputValues*          values,
            const Vector3f&             normal,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            Spectrum&                   value)
        {
            if (values->m_energy_compensation == 0.0f)
                return;

            const float cos_in = dot(incoming, normal);
            const float cos_on = dot(outgoing, normal);

            const float roughness = values->m_roughness;

            const float eta =
                cos_on > 0.0f
                    ? values->m_ior / values->m_precomputed.m_outside_ior
                    : values->m_precomputed.m_outside_ior / values->m_ior;

            const float rcp_eta = 1.0f / eta;

            const float Eavg = get_avg_albedo(mdf, eta, roughness);
            const float Eavg_rcp_eta = get_avg_albedo(mdf, rcp_eta, roughness);

            // Avoid divisions by zero.
            if (Eavg == 1.0f || Eavg_rcp_eta == 1.0f)
                return;

            const float Favg = average_fresnel_reflectance_dielectric(eta);
            const float Favg_rcp_eta = average_fresnel_reflectance_dielectric(rcp_eta);

            const bool is_entering = !values->m_precomputed.m_backfacing;
            const bool is_reflection = cos_in * cos_on >= 0.0f;

            float fms_multiplier = 0.0f;

            if (is_entering)
            {
                const float ratio =
                    ec_lobe_ratio(
                        eta,
                        Favg,
                        Favg_rcp_eta,
                        Eavg,
                        Eavg_rcp_eta,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_weight);

                const float Eo = get_dir_albedo(
                    mdf,
                    eta,
                    roughness,
                    abs(cos_on));

                if (is_reflection)
                {
                    // ratio(n) * (1 - E(n, uo)) * (1 - E(n, ui))
                    // ------------------------------------------
                    //           Pi * (1 - Eavg(n))

                    const float Ei = get_dir_albedo(
                        mdf,
                        eta,
                        roughness,
                        abs(cos_in));

                    fms_multiplier =
                        ratio * (1.0f - Eo) * (1.0f - Ei) / (Pi<float>() * (1.0f - Eavg));
                }
                else
                {
                    // (1 - ratio(n)) * (1 - E(n, uo)) * (1 - E(1/n, ui))
                    // --------------------------------------------------
                    //             Pi * (1 - Eavg(1/n))

                    const float Ei = get_dir_albedo(
                        mdf,
                        rcp_eta,
                        roughness,
                        abs(cos_in));

                    fms_multiplier =
                        (1.0f - ratio) * (1.0f - Eo) * (1.0f - Ei) / (Pi<float>() * (1.0f - Eavg_rcp_eta));
                }
            }
            else
            {
                const float ratio =
                    ec_lobe_ratio(
                        rcp_eta,
                        Favg_rcp_eta,
                        Favg,
                        Eavg_rcp_eta,
                        Eavg,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_weight);

                const float Eo = get_dir_albedo(
                    mdf,
                    rcp_eta,
                    roughness,
                    abs(cos_on));

                if (is_reflection)
                {
                    // ratio(1/n) * (1 - E(1/n, uo)) * (1 - E(1/n, ui))
                    // ------------------------------------------------
                    //             Pi * (1 - Eavg(n))

                    const float Ei = get_dir_albedo(
                        mdf,
                        rcp_eta,
                        roughness,
                        abs(cos_in));

                    fms_multiplier =
                        ratio * (1.0f - Eo) * (1.0f - Ei) / (Pi<float>() * (1.0f - Eavg));
                }
                else
                {
                    // (1 - ratio(1/n)) * (1 - E(1/n, uo)) * (1 - E(n, ui))
                    // ----------------------------------------------------
                    //               Pi * (1 - Eavg(n))

                    const float Ei = get_dir_albedo(
                        mdf,
                        eta,
                        roughness,
                        abs(cos_in));

                    fms_multiplier =
                        (1.0f - ratio) * (1.0f - Eo) * (1.0f - Ei) / (Pi<float>() * (1.0f - Eavg));
                }
            }

            fms_multiplier = lerp(0.0f, fms_multiplier, values->m_energy_compensation);

            madd(
                value,
                is_reflection
                    ? values->m_precomputed.m_reflection_color
                    : values->m_precomputed.m_refraction_color,
                fms_multiplier);
        }
    };

    typedef BSDFWrapper<GlassBSDFImpl, false> GlassBSDF;

    const float MinEta = 1.01f;
    const float MaxEta = 3.0f;

    class GlassAlbedoTable
      : public AlbedoTable3D
    {
      public:
        GlassAlbedoTable(const float* table, const float min_eta, const float max_eta)
          : AlbedoTable3D(table, min_eta, max_eta)
        {
        }

        template <typename MDF>
        GlassAlbedoTable(const MDF& mdf, const float min_eta, const float max_eta)
          : AlbedoTable3D(min_eta, max_eta)
        {
            for (size_t z = 0; z < TableSize; ++z)
            {
                const float eta = lerp(m_min_eta, m_max_eta, static_cast<float>(z) / (TableSize - 1));

                for (size_t y = 0; y < TableSize; ++y)
                {
                    const float roughness = static_cast<float>(y) / (TableSize - 1);
                    const float alpha = max(square(roughness), 0.001f);

                    for (size_t x = 0; x < TableSize; ++x)
                    {
                        const float cos_theta = static_cast<float>(x) / (TableSize - 1);

                        dir_table(x, y, z) = compute_directional_albedo<MDF>(eta, alpha, cos_theta);
                    }

                    avg_table(y, z) = average_albedo(TableSize, &dir_table(0, y, z));
                }
            }
        }

      private:
        // Compute the albedo for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
        template <typename MDF>
        float compute_directional_albedo(
            const float eta,
            const float alpha,
            const float cos_theta) const
        {
            // Special case.
            if (cos_theta == 0.0f)
                return 1.0f;

            Basis3f shading_basis(
                Vector3f(0.0f, 1.0f, 0.0f),
                Vector3f(1.0f, 0.0f, 0.0f));

            // Build the outgoing vector.
            const float sin_theta = std::sqrt(1.0f - square(cos_theta));
            const Vector3f wo(sin_theta, cos_theta, 0.0f);

            float R = 0.0f;
            const MDF mdf;
            const size_t SampleCount = 512;

            for (size_t i = 0; i < SampleCount; ++i)
            {
                // Generate a uniform sample in [0,1)^3.
                const size_t Bases[] = { 2, 3 };
                const Vector3f s = hammersley_sequence<float, 3>(Bases, SampleCount, i);

                Vector3f wi;
                float value = 0.0f;
                float probability = 0.0f;

                GlassBSDFImpl::do_sample(
                    mdf,
                    s,
                    false,
                    shading_basis,
                    alpha,
                    alpha,
                    1.0f, // gamma
                    1.0f, // reflection_color
                    1.0f, // reflection_weight
                    1.0f, // refraction_color
                    1.0f, // refraction_weight
                    eta,
                    wo,
                    wi,
                    value,
                    probability);
                assert(probability >= 0.0f);

                if (probability < 1.0e-6f)
                    continue;

                R += value * abs(wi.y) / probability;
            }

            return min(R / static_cast<float>(SampleCount), 1.0f);
        }
    };

    struct GlassAlbedoTables
      : public NonCopyable
    {
#ifdef COMPUTE_ALBEDO_TABLES
        GlassAlbedoTables()
          : m_ggx(GGXMDF(), MinEta, MaxEta)
          , m_ggx_rcp_eta(GGXMDF(), 1.0f / MaxEta, 1.0f / MinEta)
          , m_beckmann(BeckmannMDF(), MinEta, MaxEta)
          , m_beckmann_rcp_eta(BeckmannMDF(), 1.0f / MaxEta, 1.0f / MinEta)
        {
        }
#else
        GlassAlbedoTables()
          : m_ggx(g_glass_ggx_albedo_table, MinEta, MaxEta)
          , m_ggx_rcp_eta(g_glass_ggx_rcp_eta_albedo_table, 1.0f / MaxEta, 1.0f / MinEta)
          , m_beckmann(g_glass_beckmann_albedo_table, MinEta, MaxEta)
          , m_beckmann_rcp_eta(g_glass_beckmann_rcp_eta_albedo_table, 1.0f / MaxEta, 1.0f / MinEta)
        {
        }
#endif

        GlassAlbedoTable m_ggx;
        GlassAlbedoTable m_ggx_rcp_eta;
        GlassAlbedoTable m_beckmann;
        GlassAlbedoTable m_beckmann_rcp_eta;
    };

    GlassAlbedoTables g_dir_albedo_tables;

    float get_dir_albedo(
        const GGXMDF&       mdf,
        const float         eta,
        const float         roughness,
        const float         cos_theta)
    {
        if (eta > 1.0f)
        {
            return g_dir_albedo_tables.m_ggx.get_directional_albedo(
                eta,
                roughness,
                cos_theta);
        }
        else
        {
            return g_dir_albedo_tables.m_ggx_rcp_eta.get_directional_albedo(
                eta,
                roughness,
                cos_theta);
        }
    }

    float get_dir_albedo(
        const BeckmannMDF&  mdf,
        const float         eta,
        const float         roughness,
        const float         cos_theta)
    {
        if (eta > 1.0f)
        {
            return g_dir_albedo_tables.m_beckmann.get_directional_albedo(
                eta,
                roughness,
                cos_theta);
        }
        else
        {
            return g_dir_albedo_tables.m_beckmann_rcp_eta.get_directional_albedo(
                eta,
                roughness,
                cos_theta);
        }
    }

    float get_avg_albedo(
        const GGXMDF&       mdf,
        const float         eta,
        const float         roughness)
    {
        if (eta > 1.0f)
        {
            return g_dir_albedo_tables.m_ggx.get_average_albedo(
                eta,
                roughness);
        }
        else
        {
            return g_dir_albedo_tables.m_ggx_rcp_eta.get_average_albedo(
                eta,
                roughness);
        }
    }

    float get_avg_albedo(
        const BeckmannMDF&  mdf,
        const float         eta,
        const float         roughness)
    {
        if (eta > 1.0f)
        {
            return g_dir_albedo_tables.m_beckmann.get_average_albedo(
                eta,
                roughness);
        }
        else
        {
            return g_dir_albedo_tables.m_beckmann_rcp_eta.get_average_albedo(
                eta,
                roughness);
        }
    }
}


//
// GlassBSDFFactory class implementation.
//

void GlassBSDFFactory::release()
{
    delete this;
}

const char* GlassBSDFFactory::get_model() const
{
    return Model;
}

Dictionary GlassBSDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Glass BSDF");
}

DictionaryArray GlassBSDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Beckmann", "beckmann")
                    .insert("GGX", "ggx")
                    .insert("STD", "std"))
            .insert("use", "required")
            .insert("default", "ggx"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_transmittance")
            .insert("label", "Surface Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.85"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_transmittance_multiplier")
            .insert("label", "Surface Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflection_tint")
            .insert("label", "Reflection Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "refraction_tint")
            .insert("label", "Refraction Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.15"));

    metadata.push_back(
        Dictionary()
            .insert("name", "highlight_falloff")
            .insert("label", "Highlight Falloff")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("bounds", "hard")
            .insert("use", "optional")
            .insert("default", "0.4"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_parameterization")
            .insert("label", "Volume Absorption Parameterization")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Transmittance", "transmittance")
                    .insert("Absorption", "absorption"))
            .insert("use", "required")
            .insert("default", "transmittance")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_transmittance")
            .insert("label", "Volume Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("volume_parameterization", "transmittance")));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_transmittance_distance")
            .insert("label", "Volume Transmittance Distance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("default", "0.0")
            .insert("visible_if",
                Dictionary()
                    .insert("volume_parameterization", "transmittance")));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_absorption")
            .insert("label", "Volume Absorption")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("visible_if",
                Dictionary()
                    .insert("volume_parameterization", "absorption")));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_density")
            .insert("label", "Volume Density")
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
            .insert("default", "0.0")
            .insert("visible_if",
                Dictionary()
                    .insert("volume_parameterization", "absorption")));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_scale")
            .insert("label", "Volume Scale")
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
            .insert("default", "1.0")
            .insert("visible_if",
                Dictionary()
                    .insert("volume_parameterization", "absorption")));

    metadata.push_back(
        Dictionary()
            .insert("name", "energy_compensation")
            .insert("label", "Energy Compensation")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<BSDF> GlassBSDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new GlassBSDF(name, params));
}

void write_glass_directional_albedo_tables(const char* directory)
{
    const bfs::path dir(directory);

    const GGXMDF ggx;
    const GlassAlbedoTable ggx_table(ggx, MinEta, MaxEta);
    const GlassAlbedoTable ggx_rcp_eta_table(ggx, 1.0f / MaxEta, 1.0f / MinEta);

    ggx_table.write_table_to_image(
        dir / "glass_ggx_albedo_table.exr");
    ggx_table.write_table_to_cpp_array(
        dir / "glass_ggx_albedo_table.cpp",
        "g_glass_ggx_albedo_table");

    ggx_rcp_eta_table.write_table_to_image(
        dir / "glass_ggx_rcp_eta_albedo_table.exr");
    ggx_rcp_eta_table.write_table_to_cpp_array(
        dir / "glass_ggx_rcp_eta_albedo_table.cpp",
        "g_glass_ggx_rcp_eta_albedo_table");

    const BeckmannMDF beckmann;
    const GlassAlbedoTable beckmann_table(beckmann, MinEta, MaxEta);
    const GlassAlbedoTable beckmann_rcp_eta_table(beckmann, 1.0f / MaxEta, 1.0f / MinEta);

    beckmann_table.write_table_to_image(
        dir / "glass_beckmann_albedo_table.exr");
    beckmann_table.write_table_to_cpp_array(
        dir / "glass_beckmann_albedo_table.cpp",
        "g_glass_beckmann_albedo_table");

    beckmann_rcp_eta_table.write_table_to_image(
        dir / "glass_beckmann_rcp_eta_albedo_table.exr");
    beckmann_rcp_eta_table.write_table_to_cpp_array(
        dir / "glass_beckmann_rcp_eta_albedo_table.cpp",
        "g_glass_beckmann_rcp_eta_albedo_table");
}

}   // namespace renderer
