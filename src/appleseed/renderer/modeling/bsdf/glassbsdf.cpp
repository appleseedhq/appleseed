
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

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
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);

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

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();

            if (shading_point.is_entering())
            {
                values->m_precomputed.m_backfacing = false;
                values->m_precomputed.m_eta = shading_point.get_ray().get_current_ior() / values->m_ior;
            }
            else
            {
                values->m_precomputed.m_backfacing = true;
                values->m_precomputed.m_eta = values->m_ior / shading_point.get_ray().get_previous_ior();
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

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            sampling_context.split_in_place(4, 1);
            const Vector4f s = sampling_context.next2<Vector4f>();

            const Vector3f wo = basis.transform_to_local(sample.m_outgoing.get_value());
            Vector3f wi;

            bool is_refraction;

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
                        0.0f,
                        values->m_precomputed.m_reflection_color,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_color,
                        values->m_precomputed.m_refraction_weight,
                        values->m_precomputed.m_eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        sample.m_probability);
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
                        0.0f,
                        values->m_precomputed.m_reflection_color,
                        values->m_precomputed.m_reflection_weight,
                        values->m_precomputed.m_refraction_color,
                        values->m_precomputed.m_refraction_weight,
                        values->m_precomputed.m_eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        sample.m_probability);
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
                        values->m_precomputed.m_eta,
                        wo,
                        wi,
                        sample.m_value.m_glossy,
                        sample.m_probability);
                }
                break;

              assert_otherwise;
            }

            if (sample.m_probability < 1.0e-9f)
                return;

            sample.m_value.m_beauty = sample.m_value.m_glossy;

            sample.m_mode = ScatteringMode::Glossy;

            sample.m_incoming = Dual3f(basis.transform_to_parent(wi));

            if (is_refraction)
                sample.compute_transmitted_differentials(values->m_precomputed.m_eta);
            else sample.compute_reflected_differentials();
        }

        template <typename MDF, typename SpectrumType>
        static bool do_sample(
            const MDF&                  mdf,
            const Vector4f&             s,
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
            Vector3f m = mdf.sample(wo, Vector3f(s[0], s[1], s[2]), alpha_x, alpha_y, gamma);
            assert(m.y > 0.0f);

            const float cos_wom = clamp(dot(wo, m), -1.0f, 1.0f);
            float cos_theta_t;
            const float F = fresnel_reflectance(cos_wom, eta, cos_theta_t);
            const float r_probability = choose_reflection_probability(
                reflection_weight,
                refraction_weight,
                F);

            bool is_refraction;

            // Choose between reflection and refraction.
            if (s[3] < r_probability)
            {
                is_refraction = false;

                // Compute the reflected direction.
                wi = improve_normalization(reflect(wo, m));

                // If incoming and outgoing are on different sides of the surface, this is not a reflection.
                if (wi.y * wo.y <= 0.0f)
                    return true;

                evaluate_reflection(
                    mdf,
                    reflection_color,
                    wi,
                    wo,
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
                        ? (eta * cos_wom - cos_theta_t) * m - eta * wo
                        : (eta * cos_wom + cos_theta_t) * m - eta * wo;
                wi = improve_normalization(wi);

                // If incoming and outgoing are on the same side of the surface, this is not a refraction.
                if (wi.y * wo.y > 0.0f)
                    return false;

                evaluate_refraction(
                    mdf,
                    eta,
                    refraction_color,
                    adjoint,
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    1.0f - F,
                    value);

                probability =
                    (1.0f - r_probability) *
                    refraction_pdf(mdf, wi, wo, m, alpha_x, alpha_y, gamma, eta);
            }

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

            switch (m_mdf_type)
            {
              case GGX:
                {
                    const GGXMDF mdf;
                    return do_evaluate(
                        mdf,
                        values,
                        adjoint,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);
                }
                break;

              case Beckmann:
                {
                    const BeckmannMDF mdf;
                    return do_evaluate(
                        mdf,
                        values,
                        adjoint,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);
                }
                break;

              case Std:
                {
                    const StdMDF mdf;
                    return do_evaluate(
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
                return 0.0f;
            }
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

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wi = basis.transform_to_local(incoming);
            const Vector3f wo = basis.transform_to_local(outgoing);

            if (wi.y * wo.y >= 0.0f)
            {
                // Reflection.
                const Vector3f m = half_reflection_vector(wi, wo);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, values->m_precomputed.m_eta);

                evaluate_reflection(
                    mdf,
                    values->m_precomputed.m_reflection_color,
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    F,
                    value.m_glossy);
                value.m_beauty = value.m_glossy;

                const float r_probability = choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

                return
                    r_probability * reflection_pdf(mdf, wo, m, cos_wom, alpha_x, alpha_y, gamma);
            }
            else
            {
                // Refraction.
                const Vector3f m = half_refraction_vector(wi, wo, values->m_precomputed.m_eta);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, values->m_precomputed.m_eta);

                evaluate_refraction(
                    mdf,
                    values->m_precomputed.m_eta,
                    values->m_precomputed.m_refraction_color,
                    adjoint,
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma,
                    1.0f - F,
                    value.m_glossy);
                value.m_beauty = value.m_glossy;

                const float r_probability = choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

                return
                    (1.0f - r_probability) *
                    refraction_pdf(mdf, wi, wo, m, alpha_x, alpha_y, gamma, values->m_precomputed.m_eta);
            }
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

            switch (m_mdf_type)
            {
              case GGX:
                {
                    const GGXMDF mdf;
                    return do_evaluate_pdf(
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
                    return do_evaluate_pdf(
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
                    return do_evaluate_pdf(
                        mdf,
                        values,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              default:
                assert(!"Unexpected MDF type.");
                return 0.0f;
            }
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

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wi = basis.transform_to_local(incoming);
            const Vector3f wo = basis.transform_to_local(outgoing);

            if (wi.y * wo.y >= 0.0f)
            {
                // Reflection.
                const Vector3f m = half_reflection_vector(wi, wo);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, values->m_precomputed.m_eta);

                const float r_probability = choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

                return
                    r_probability *
                    reflection_pdf(mdf, wo, m, cos_wom, alpha_x, alpha_y, gamma);
            }
            else
            {
                // Refraction.
                const Vector3f m = half_refraction_vector(wi, wo, values->m_precomputed.m_eta);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, values->m_precomputed.m_eta);

                const float r_probability = choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

                return
                    (1.0f - r_probability) *
                    refraction_pdf(mdf, wi, wo, m, alpha_x, alpha_y, gamma, values->m_precomputed.m_eta);
            }
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

            if (sum_probabilities == 0.0f)
                return 1.0f;

            return r_probability / sum_probabilities;
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
            const Vector3f&             wi,
            const Vector3f&             wo)
        {
            // [1] eq. 13.
            const Vector3f h = normalize(wi + wo);
            return h.y < 0.0f ? -h : h;
        }

        template <typename SpectrumType>
        static void evaluate_reflection(
            const MDF&                  mdf,
            const SpectrumType&         reflection_color,
            const Vector3f&             wi,
            const Vector3f&             wo,
            const Vector3f&             h,
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
                value.set(0.0f);
                return;
            }

            const float D = mdf.D(h, alpha_x, alpha_y, gamma);
            const float G = mdf.G(wi, wo, h, alpha_x, alpha_y, gamma);

            value = reflection_color;
            value *= F * D * G / denom;
        }

        static float reflection_pdf(
            const MDF&                  mdf,
            const Vector3f&             wo,
            const Vector3f&             h,
            const float                 cos_oh,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma)
        {
            // [1] eq. 14.
            if (cos_oh == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * abs(cos_oh));
            return jacobian * mdf.pdf(wo, h, alpha_x, alpha_y, gamma);
        }

        static Vector3f half_refraction_vector(
            const Vector3f&             wi,
            const Vector3f&             wo,
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
            const Vector3f&             wi,
            const Vector3f&             wo,
            const Vector3f&             h,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const float                 T,
            SpectrumType&               value)
        {
            // [1] eq. 21.
            const float cos_ih = dot(h, wi);
            const float cos_oh = dot(h, wo);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (abs(sqrt_denom) < 1.0e-9f)
            {
                value = SpectrumType(0.0f);
                return;
            }

            const float D = mdf.D(h, alpha_x, alpha_y, gamma);
            const float G = mdf.G(wi, wo, h, alpha_x, alpha_y, gamma);

            const float dots = (cos_ih * cos_oh) / (wi.y * wo.y);
            float multiplier = abs(dots) * T * D * G / square(sqrt_denom);

            if (!adjoint)
                multiplier *= square(eta);

            value = refraction_color;
            value *= multiplier;
        }

        static float refraction_pdf(
            const MDF&                  mdf,
            const Vector3f&             wi,
            const Vector3f&             wo,
            const Vector3f&             h,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 gamma,
            const float                 eta)
        {
            // [1] eq. 17.
            const float cos_ih = dot(h, wi);
            const float cos_oh = dot(h, wo);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (abs(sqrt_denom) < 1.0e-9f)
                return 0.0f;

            const float jacobian = abs(cos_ih) * square(eta / sqrt_denom);
            return jacobian * mdf.pdf(wo, h, alpha_x, alpha_y, gamma);
        }
    };

    typedef BSDFWrapper<GlassBSDFImpl, false> GlassBSDF;
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
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.85"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_transmittance_multiplier")
            .insert("label", "Surface Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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

    return metadata;
}

auto_release_ptr<BSDF> GlassBSDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new GlassBSDF(name, params));
}

}   // namespace renderer
