
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
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
namespace bf = boost::filesystem;

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

    float fresnel_reflectance(
        const float         cos_theta_i,
        const float         eta,
        float&              cos_theta_t)
    {
        const float sin_theta_t2 = (1.0f - square(cos_theta_i)) * square(eta);
        if (sin_theta_t2 > 1.0f)
        {
            cos_theta_t = 0.0f;
            return 1.0f;
        }

        cos_theta_t = std::min(std::sqrt(std::max(1.0f - sin_theta_t2, 0.0f)), 1.0f);

        float F;
        fresnel_reflectance_dielectric(
            F,
            eta,
            std::abs(cos_theta_i),
            cos_theta_t);

        return F;
    }

    float fresnel_reflectance(
        const float         cos_theta_i,
        const float         eta)
    {
        float cos_theta_t;
        return fresnel_reflectance(cos_theta_i, eta, cos_theta_t);
    }

    Vector3f refracted_direction(
        const Vector3f&     wo,
        const Vector3f&     m,
        const float         cos_wom,
        const float         cos_theta_t,
        const float         rcp_eta)
    {
        const Vector3f wi =
            cos_wom > 0.0f
                ? (rcp_eta * cos_wom - cos_theta_t) * m - rcp_eta * wo
                : (rcp_eta * cos_wom + cos_theta_t) * m - rcp_eta * wo;
        return improve_normalization(wi);
    }

    float get_dir_albedo(
        const float         eta,
        const float         roughness,
        const float         cos_theta);

    float get_avg_albedo(
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
            m_inputs.declare("surface_transmittance", InputFormat::SpectralReflectance);
            m_inputs.declare("surface_transmittance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("reflection_tint", InputFormat::SpectralReflectance, "1.0");
            m_inputs.declare("refraction_tint", InputFormat::SpectralReflectance, "1.0");
            m_inputs.declare("roughness", InputFormat::Float, "0.15");
            m_inputs.declare("anisotropy", InputFormat::Float, "0.0");
            m_inputs.declare("ior", InputFormat::Float, "1.5");
            m_inputs.declare("volume_transmittance", InputFormat::SpectralReflectance, "1.0");
            m_inputs.declare("volume_transmittance_distance", InputFormat::Float, "0.0");
            m_inputs.declare("volume_absorption", InputFormat::SpectralReflectance, "0.0");
            m_inputs.declare("volume_density", InputFormat::Float, "0.0");
            m_inputs.declare("volume_scale", InputFormat::Float, "1.0");
            m_inputs.declare("energy_compensation", InputFormat::Float, "0.0");
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

            const std::string volume_parameterization =
                m_params.get_required<std::string>(
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

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);

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
            values->m_precomputed.m_reflection_weight = std::max(max_value(values->m_precomputed.m_reflection_color), 0.0f);
            values->m_precomputed.m_refraction_weight = std::max(max_value(values->m_precomputed.m_refraction_color), 0.0f);
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
            if (!ScatteringMode::has_glossy(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(
                          -local_geometry.m_shading_basis.get_normal(),
                           local_geometry.m_shading_basis.get_tangent_u(),
                          -local_geometry.m_shading_basis.get_tangent_v())
                    : local_geometry.m_shading_basis);

            const Vector3f wo = basis.transform_to_local(outgoing.get_value());

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

            // Compute the microfacet normal by sampling the MDF.
            const Vector3f m = GGXMDF::sample(wo, Vector2f(s[0], s[1]), alpha_x, alpha_y);
            assert(m.y > 0.0f);

            // Compute the Fresnel term and the sampling probabilities.
            const float rcp_eta = 1.0f / eta;
            const float cos_wom = clamp(dot(wo, m), -1.0f, 1.0f);

            float cos_theta_t;
            const float F = fresnel_reflectance(cos_wom, rcp_eta, cos_theta_t);
            const float r_probability =
                choose_reflection_probability(
                    values->m_precomputed.m_reflection_weight,
                    values->m_precomputed.m_refraction_weight,
                    F);

            Vector3f wi;
            float probability;
            bool is_refraction;

            // Choose between reflection and refraction.
            if (s[2] < r_probability)
            {
                is_refraction = false;

                // Compute the reflected direction.
                wi = improve_normalization(reflect(wo, m));

                // If incoming and outgoing are on different sides of the surface, this is not a reflection.
                if (wi.y * wo.y <= 0.0f)
                    return;

                evaluate_reflection(
                    values->m_precomputed.m_reflection_color,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    F,
                    sample.m_value.m_glossy);

                probability =
                    r_probability *
                    reflection_pdf(wo, m, cos_wom, alpha_x, alpha_y);
            }
            else
            {
                is_refraction = true;
                wi = refracted_direction(
                    wo,
                    m,
                    cos_wom,
                    cos_theta_t,
                    rcp_eta);

                // If incoming and outgoing are on the same side of the surface, this is not a refraction.
                if (wi.y * wo.y > 0.0f)
                    return;

                evaluate_refraction(
                    eta,
                    values->m_precomputed.m_refraction_color,
                    adjoint,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
                    1.0f - F,
                    sample.m_value.m_glossy);

                probability =
                    (1.0f - r_probability) *
                    refraction_pdf(
                        wo,
                        wi,
                        m,
                        alpha_x,
                        alpha_y,
                        eta);
            }

            assert(probability > 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(ScatteringMode::Glossy, probability);
                sample.m_value.m_beauty = sample.m_value.m_glossy;
                sample.m_incoming = Dual3f(basis.transform_to_parent(wi));
                sample.m_min_roughness = values->m_roughness;

                if (is_refraction)
                {
                    sample.compute_glossy_transmitted_differentials(
                        local_geometry,
                        values->m_roughness,
                        1.0f / eta,
                        local_geometry.m_shading_point->is_entering(),
                        outgoing);
                }
                else
                {
                    sample.compute_glossy_reflected_differentials(
                        local_geometry,
                        values->m_roughness,
                        outgoing);
                }
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
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(
                          -local_geometry.m_shading_basis.get_normal(),
                           local_geometry.m_shading_basis.get_tangent_u(),
                          -local_geometry.m_shading_basis.get_tangent_v())
                    : local_geometry.m_shading_basis);

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

            const Vector3f wi = basis.transform_to_local(incoming);
            float pdf;

            if (wi.y * wo.y >= 0.0f)
            {
                // Reflection.
                const Vector3f m = half_reflection_vector(wo, wi);
                const float cos_wom = dot(wo, m);

                const float F = fresnel_reflectance(cos_wom, rcp_eta);

                evaluate_reflection(
                    values->m_precomputed.m_reflection_color,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
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
                    reflection_pdf(wo, m, cos_wom, alpha_x, alpha_y);
            }
            else
            {
                // Refraction.
                const Vector3f m = half_refraction_vector(wo, wi, eta);
                const float cos_wom = dot(wo, m);
                const float F = fresnel_reflectance(cos_wom, 1.0f / eta);

                evaluate_refraction(
                    eta,
                    values->m_precomputed.m_refraction_color,
                    adjoint,
                    wo,
                    wi,
                    m,
                    alpha_x,
                    alpha_y,
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
                    refraction_pdf(wo, wi, m, alpha_x, alpha_y, eta);
            }

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
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const Basis3f basis(
                values->m_precomputed.m_backfacing
                    ? Basis3f(
                          -local_geometry.m_shading_basis.get_normal(),
                           local_geometry.m_shading_basis.get_tangent_u(),
                          -local_geometry.m_shading_basis.get_tangent_v())
                    : local_geometry.m_shading_basis);

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
                    reflection_pdf(wo, m, cos_wom, alpha_x, alpha_y);
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
                    refraction_pdf(wo, wi, m, alpha_x, alpha_y, eta);
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
                        const float a = std::log(std::max(values->m_volume_transmittance[i], 0.01f));
                        absorption[i] = std::exp(a * d);
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
                    absorption[i] = std::exp(-optical_depth);
                }
            }
        }

      private:
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

        static Vector3f half_reflection_vector(
            const Vector3f&             wo,
            const Vector3f&             wi)
        {
            // [1] eq. 13.
            const Vector3f h = normalize(wi + wo);
            return h.y < 0.0f ? -h : h;
        }

        static void evaluate_reflection(
            const Spectrum&             reflection_color,
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 F,
            Spectrum&                   value)
        {
            // [1] eq. 20.
            const float denom = std::abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            const float D = GGXMDF::D(m, alpha_x, alpha_y);
            const float G = GGXMDF::G(wi, wo, m, alpha_x, alpha_y);

            value = reflection_color;
            value *= F * D * G / denom;
        }

        static float reflection_pdf(
            const Vector3f&             wo,
            const Vector3f&             m,
            const float                 cos_oh,
            const float                 alpha_x,
            const float                 alpha_y)
        {
            // [1] eq. 14.
            if (cos_oh == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * std::abs(cos_oh));
            return jacobian * GGXMDF::pdf(wo, m, alpha_x, alpha_y);
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

        static void evaluate_refraction(
            const float                 eta,
            const Spectrum&             refraction_color,
            const bool                  adjoint,
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 T,
            Spectrum&                   value)
        {
            if (wo.y == 0.0f || wi.y == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            // [1] eq. 21.
            const float cos_ih = dot(m, wi);
            const float cos_oh = dot(m, wo);
            const float dots = (cos_ih * cos_oh) / (wi.y * wo.y);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (std::abs(sqrt_denom) < 1.0e-6f)
            {
                value.set(0.0f);
                return;
            }

            const float D = GGXMDF::D(m, alpha_x, alpha_y);
            const float G = GGXMDF::G(wi, wo, m, alpha_x, alpha_y);

            float multiplier = std::abs(dots) * T * D * G / square(sqrt_denom);

            if (!adjoint)
                multiplier *= square(eta);

            value = refraction_color;
            value *= multiplier;
        }

        static float refraction_pdf(
            const Vector3f&             wo,
            const Vector3f&             wi,
            const Vector3f&             m,
            const float                 alpha_x,
            const float                 alpha_y,
            const float                 eta)
        {
            // [1] eq. 17.
            const float cos_ih = dot(m, wi);
            const float cos_oh = dot(m, wo);

            const float sqrt_denom = cos_oh + eta * cos_ih;
            if (std::abs(sqrt_denom) < 1.0e-6f)
                return 0.0f;

            const float jacobian = std::abs(cos_ih) * square(eta / sqrt_denom);
            return jacobian * GGXMDF::pdf(wo, m, alpha_x, alpha_y);
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

        GlassAlbedoTable(const float min_eta, const float max_eta)
          : AlbedoTable3D(min_eta, max_eta)
        {
            for (size_t z = 0; z < TableSize; ++z)
            {
                const float eta = lerp(m_min_eta, m_max_eta, static_cast<float>(z) / (TableSize - 1));

                for (size_t y = 0; y < TableSize; ++y)
                {
                    const float roughness = static_cast<float>(y) / (TableSize - 1);
                    const float alpha = std::max(square(roughness), 0.001f);

                    for (size_t x = 0; x < TableSize; ++x)
                    {
                        const float cos_theta = static_cast<float>(x) / (TableSize - 1);
                        dir_table(x, y, z) = compute_directional_albedo(eta, alpha, cos_theta);
                    }

                    avg_table(y, z) = average_albedo(TableSize, &dir_table(0, y, z));
                }
            }
        }

      private:
        // Compute the albedo for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
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
            const size_t SampleCount = 512;

            for (size_t i = 0; i < SampleCount; ++i)
            {
                // Generate a uniform sample in [0,1)^2.
                const size_t Bases[] = { 2 };
                const Vector2f s = hammersley_sequence<float, 2>(Bases, SampleCount, i);

                // Compute the microfacet normal by sampling the MDF.
                const Vector3f m = GGXMDF::sample(wo, s, alpha, alpha);
                assert(m.y > 0.0f);

                // Compute the Fresnel term.
                const float rcp_eta = 1.0f / eta;
                const float cos_wom = clamp(dot(wo, m), -1.0f, 1.0f);

                float cos_theta_t;
                const float F = fresnel_reflectance(cos_wom, rcp_eta, cos_theta_t);

                const float rcp_G1 =
                    safe_rcp(GGXMDF::G1(wo, m, alpha, alpha), 1e-8f);

                // Evaluate the reflection lobe.
                {
                    const Vector3f wi = improve_normalization(reflect(wo, m));

                    if (wi.y * wo.y > 0.0f)
                    {
                        const float G = GGXMDF::G(wi, wo, m, alpha, alpha);
                        R += F * G * rcp_G1;
                    }
                }

                // Evaluate the transmission lobe.
                {
                    const Vector3f wi = refracted_direction(
                        wo,
                        m,
                        cos_wom,
                        cos_theta_t,
                        rcp_eta);

                    if (wi.y * wo.y <= 0.0f)
                    {
                        const float G = GGXMDF::G(wi, wo, m, alpha, alpha);
                        R += (1.0f - F) * G * rcp_G1;
                    }
                }
            }

            return std::min(R / static_cast<float>(SampleCount), 1.0f);
        }
    };

    struct GlassAlbedoTables
      : public NonCopyable
    {
#ifdef COMPUTE_ALBEDO_TABLES
        GlassAlbedoTables()
          : m_ggx(GGXMDF(), MinEta, MaxEta)
          , m_ggx_rcp_eta(GGXMDF(), 1.0f / MaxEta, 1.0f / MinEta)
        {
        }
#else
        GlassAlbedoTables()
          : m_ggx(g_glass_ggx_albedo_table, MinEta, MaxEta)
          , m_ggx_rcp_eta(g_glass_ggx_rcp_eta_albedo_table, 1.0f / MaxEta, 1.0f / MinEta)
        {
        }
#endif

        GlassAlbedoTable m_ggx;
        GlassAlbedoTable m_ggx_rcp_eta;
    };

    GlassAlbedoTables g_dir_albedo_tables;

    float get_dir_albedo(
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

    float get_avg_albedo(
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
            .insert("name", "surface_transmittance")
            .insert("label", "Surface Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.99"));

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
    const bf::path dir(directory);

    const GlassAlbedoTable ggx_table(MinEta, MaxEta);
    const GlassAlbedoTable ggx_rcp_eta_table(1.0f / MaxEta, 1.0f / MinEta);

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
}

}   // namespace renderer
