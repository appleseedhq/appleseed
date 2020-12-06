
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Lovro Bosnar, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/utility/shadowterminator.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

namespace renderer
{

//
// MicrofacetBRDFWrapper class corrects the usage of normal maps for Monte Carlo Path Tracing.
//
// Based on:
// [1] Microfacet-based Normal Mapping for Robust Monte Carlo Path Tracing (Schussler et al.).
// https://blogs.unity3d.com/2017/10/02/microfacet-based-normal-mapping-for-robust-monte-carlo-path-tracing/
//
// Light vector: wi, i (paper, mitsuba) - outgoing (appleseed).
// View vector: wo, o (paper, mitsuba) - incoming (appleseed).
//

template <typename BSDFImpl>
class MicrofacetBRDFWrapper : public BSDFImpl
{
  public:
    using LocalGeometry = typename BSDFImpl::LocalGeometry;

    MicrofacetBRDFWrapper() = default;

    MicrofacetBRDFWrapper(
        const char*                         name,
        const ParamArray&                   params);

    void sample(
        SamplingContext&                    sampling_context,
        const void*                         data,                       // input values
        const bool                          adjoint,                    // if true, use the adjoint scattering kernel
        const bool                          cosine_mult,
        const LocalGeometry&                local_geometry,
        const foundation::Dual3f&           outgoing,                   // world space outgoing direction, unit-length
        const int                           modes,                      // allowed scattering modes
        BSDFSample&                         sample) const override;

    float evaluate(
        const void*                         data,                       // input values
        const bool                          adjoint,                    // if true, use the adjoint scattering kernel
        const bool                          cosine_mult,
        const LocalGeometry&                local_geometry,
        const foundation::Vector3f&         outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3f&         incoming,                   // world space incoming direction, unit-length
        const int                           modes,                      // enabled scattering modes
        DirectShadingComponents&            value) const override;      // BSDF value, or BSDF value * |cos(incoming, normal)|

    float evaluate_pdf(
        const void*                         data,                       // input values
        const bool                          adjoint,                    // if true, use the adjoint scattering kernel
        const LocalGeometry&                local_geometry,
        const foundation::Vector3f&         outgoing,                   // world space outgoing direction, unit-length
        const foundation::Vector3f&         incoming,                   // world space incoming direction, unit-length
        const int                           modes) const override;      // enabled scattering modes

  private:
    enum MicrofacetScattering {IPO, IPTO, ITPO};

    static float heaviside(const float a)
    {
        return a < 0.0f ? 0.0f : 1.0f;
    }

    static float abs_dot(
        const foundation::Vector3f& a,
        const foundation::Vector3f& b)
    {
        return std::min(std::abs(foundation::dot(a, b)), 1.0f);
    }

    static float clamped_dot(
        const foundation::Vector3f& a,
        const foundation::Vector3f& b)
    {
        return std::max(foundation::dot(a, b), 0.0f);
    }

    static foundation::Vector3f reflect(
        const foundation::Vector3f& a,
        const foundation::Vector3f& b)
    {
        return foundation::normalize(a - 2.0f * foundation::dot(a, b) * b);
    }

    // Replacement for cosine multiplier
    // from renderer/modeling/bsdf/bsdfwrapper.h.
    static float cosine_multiplier(
        const foundation::Vector3f& incoming,
        const foundation::Vector3f& n,
        const float                 shadow_terminator_freq_mult)
    {
        return shift_cos_in_fast(
            abs_dot(incoming, n), shadow_terminator_freq_mult); // non-adjoint
    }

    static foundation::Vector3f build_tangent(
        const foundation::Vector3f& original_shading_normal,
        const foundation::Vector3f& perturbed_shading_normal)
    {
        const foundation::Basis3f original_shading_basis(original_shading_normal);
        const foundation::Vector3f local_perturbed_shading_normal =
            original_shading_basis.transform_to_local(perturbed_shading_normal);
        const foundation::Vector3f local_tangent =
            foundation::normalize(
                foundation::Vector3f(
                    -local_perturbed_shading_normal.x,
                    0.0f,                                // y is up in local space
                    -local_perturbed_shading_normal.z));
        return original_shading_basis.transform_to_parent(local_tangent);
    }

    static float projected_area(
        const foundation::Vector3f& wi,
        const foundation::Vector3f& wp,
        const foundation::Vector3f& wg,
        const bool                  perturbed_facet)
    {
        const foundation::Vector3f wt = build_tangent(wg, wp);

        const float wi_dot_wp = clamped_dot(wi, wp);
        const float wi_dot_wt = clamped_dot(wi, wt);
        const float wp_dot_wg = clamped_dot(wp, wg);

        return perturbed_facet
            ? wi_dot_wp / wp_dot_wg
            : wi_dot_wt * std::sqrt(1.0f - wp_dot_wg * wp_dot_wg) / wp_dot_wg;
    }

    static float lambda(
        const foundation::Vector3f& wi,
        const foundation::Vector3f& wp,
        const foundation::Vector3f& wg,
        const bool                  perturbed_facet)
    {
        const float ap_wi = projected_area(wi, wp, wg, true);
        const float at_wi = projected_area(wi, wp, wg, false);

        return perturbed_facet ? ap_wi / (ap_wi + at_wi) : at_wi / (ap_wi + at_wi);
    }

    static float g1(
        const foundation::Vector3f& wi,
        const foundation::Vector3f& wp,
        const foundation::Vector3f& wg,
        const bool                  perturbed_facet)
    {
        if (foundation::dot(wi, wg) <= 0.0f)
            return 0.0f;

        const foundation::Vector3f wt = build_tangent(wg, wp);
        const float H = perturbed_facet
            ? heaviside(foundation::dot(wi, wp))
            : heaviside(foundation::dot(wi, wt));

        const float wi_dot_wg = clamped_dot(wi, wg);
        const float ap_wi = projected_area(wi, wp, wg, true);
        const float at_wi = projected_area(wi, wp, wg, false);

        return H * std::min(1.0f, wi_dot_wg / (ap_wi + at_wi));
    }

    static float microfacet_scattering_helper(
        const foundation::Vector3f& incoming,
        const foundation::Vector3f& outgoing,
        const foundation::Vector3f& tangent,
        const foundation::Vector3f& original_shading_normal,
        const foundation::Vector3f& perturbed_shading_normal,
        const MicrofacetScattering  microfacet_scattering)
    {
        switch (microfacet_scattering)
        {
          case MicrofacetScattering::IPO:
            return
                lambda(outgoing, perturbed_shading_normal, original_shading_normal, true) *
                g1(incoming, perturbed_shading_normal, original_shading_normal, true);

          case MicrofacetScattering::IPTO:
            return
                lambda(outgoing, perturbed_shading_normal, original_shading_normal, true) *
                (1.0f - g1(reflect(incoming, tangent), perturbed_shading_normal, original_shading_normal, true)) *
                g1(incoming, perturbed_shading_normal, original_shading_normal, false);

          case MicrofacetScattering::ITPO:
            return
                lambda(outgoing, perturbed_shading_normal, original_shading_normal, false) *
                g1(incoming, perturbed_shading_normal, original_shading_normal, true);

          default:
            return 1.0f;  // no effect for non-existing case
        }
    }
};


//
// MicrofacetBRDFWrapper class implementation.
//

template <typename BSDFImpl>
MicrofacetBRDFWrapper<BSDFImpl>::MicrofacetBRDFWrapper(
    const char*                         name,
    const ParamArray&                   params)
  : BSDFImpl(name, params)
{
}

template <typename BSDFImpl>
void MicrofacetBRDFWrapper<BSDFImpl>::sample(
    SamplingContext&                    sampling_context,
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const LocalGeometry&                local_geometry,
    const foundation::Dual3f&           outgoing,
    const int                           modes,
    BSDFSample&                         sample) const
{
    // World space original shading normal.
    const foundation::Vector3f original_shading_normal(
        local_geometry.m_shading_point->get_original_shading_normal());
    if (foundation::dot(outgoing.get_value(), original_shading_normal) <= 0.0f)
        return;

    const float shadow_terminator_freq_mult =
        local_geometry.m_shading_point->get_object_instance().get_render_data().m_shadow_terminator_freq_mult;

    // World space perturbed shading normal.
    const foundation::Vector3f perturbed_shading_normal(
        local_geometry.m_shading_point->get_shading_normal());

    // Perturbed normal too similar to original. Tangent vector can not be constructed.
    if (foundation::dot(original_shading_normal, perturbed_shading_normal) > 1.0f - 1e-5)
    {
        BSDFImpl::sample(
            sampling_context,
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            modes,
            sample);

        if (cosine_mult)
            sample.m_value *=
                cosine_multiplier(
                    sample.m_incoming.get_value(),
                    perturbed_shading_normal,
                    shadow_terminator_freq_mult);

        return;
    }

    // World space tangent.
    const foundation::Vector3f tangent =
        build_tangent(original_shading_normal, perturbed_shading_normal);

    float final_pdf = 0.0f;

    sampling_context.split_in_place(1, 1);
    const float s1 = sampling_context.next2<float>();

    if (lambda(outgoing.get_value(), perturbed_shading_normal, original_shading_normal, true) > s1)
    {
        // Case: hit perturbed facet.
        BSDFSample perturbed_facet_sample;
        float perturbed_facet_pdf = 0.0f;
        BSDFImpl::sample(
            sampling_context,
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            modes,
            perturbed_facet_sample);

        perturbed_facet_pdf = perturbed_facet_sample.get_probability();

        sampling_context.split_in_place(1, 1);
        const float s2 = sampling_context.next2<float>();

        if (g1(
                perturbed_facet_sample.m_incoming.get_value(),
                perturbed_shading_normal,
                original_shading_normal,
                true) > s2)
        {
            // Sampled incoming direction was reflected on tangent facet (see [1], Fig 10, right).
            // Calculate reflected of incoming direction to find out the direction
            // which intersects perturbed facet and calculate shadowing.
            foundation::Vector3f incoming_reflected =
                reflect(perturbed_facet_sample.m_incoming.get_value(), tangent);

            const float g1_p_value =
                g1(incoming_reflected, perturbed_shading_normal, original_shading_normal, true);

            perturbed_facet_sample.m_value *= g1_p_value;

            perturbed_facet_pdf *= g1_p_value;
        }
        sample.m_incoming = perturbed_facet_sample.m_incoming;
        if (foundation::dot(sample.m_incoming.get_value(), original_shading_normal) <= 0.0f)
            return;

        sample.m_value = perturbed_facet_sample.m_value;
        final_pdf = perturbed_facet_pdf;
    }
    else
    {
        // Case: hit tangent facet.
        // Sampling tangent facet is same as sampling the perturbed facet
        // with reflected outgoing direction because tangent facet is specular.
        // World space outgoing reflected.
        foundation::Vector3f outgoing_reflected = reflect(outgoing.get_value(), tangent);

        BSDFSample tangent_facet_sample;
        float tangent_facet_pdf = 0.0f;
        BSDFImpl::sample(
            sampling_context,
            data,
            adjoint,
            false,
            local_geometry,
            foundation::Dual3f(outgoing_reflected),
            modes,
            tangent_facet_sample);

        const float g1_p_value =
            g1(tangent_facet_sample.m_incoming.get_value(),
                perturbed_shading_normal,
                original_shading_normal,
                true);

        tangent_facet_sample.m_value *= g1_p_value;

        tangent_facet_pdf = tangent_facet_sample.get_probability() * g1_p_value;

        sample.m_incoming = tangent_facet_sample.m_incoming;
        if (foundation::dot(sample.m_incoming.get_value(), original_shading_normal) <= 0.0f)
            return;

        sample.m_value = tangent_facet_sample.m_value;
        final_pdf = tangent_facet_pdf;
    }

    if (cosine_mult)
    {
        sample.m_value *=
            cosine_multiplier(
                sample.m_incoming.get_value(),
                perturbed_shading_normal,
                shadow_terminator_freq_mult);
    }

    if (final_pdf == 0.0f)
        return;

    if (ScatteringMode::has_diffuse(this->get_modes()))
        sample.set_to_scattering(ScatteringMode::Diffuse, final_pdf);
    if (ScatteringMode::has_glossy(this->get_modes()))
        sample.set_to_scattering(ScatteringMode::Glossy, final_pdf);
    if (ScatteringMode::has_volume(this->get_modes()))
        sample.set_to_scattering(ScatteringMode::Volume, final_pdf);
}

template <typename BSDFImpl>
float MicrofacetBRDFWrapper<BSDFImpl>::evaluate(
    const void*                         data,
    const bool                          adjoint,
    const bool                          cosine_mult,
    const LocalGeometry&                local_geometry,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes,
    DirectShadingComponents&            value) const
{
    float final_pdf = 0.0f;

    // World space original shading normal.
    const foundation::Vector3f original_shading_normal(
        local_geometry.m_shading_point->get_original_shading_normal());

    if (foundation::dot(incoming, original_shading_normal) <= 0.0f ||
        foundation::dot(outgoing, original_shading_normal) <= 0.0f)
        return 0.0f;

    // World space perturbed shading normal.
    const foundation::Vector3f perturbed_shading_normal(
        local_geometry.m_shading_point->get_shading_normal());

    const float shadow_terminator_freq_mult =
        local_geometry.m_shading_point->get_object_instance().get_render_data().m_shadow_terminator_freq_mult;

    // Perturbed normal too similar to original. Tangent vector can not be constructed.
    if (foundation::dot(original_shading_normal, perturbed_shading_normal) > 1.0f - 1e-5f)
    {
        if (foundation::dot(incoming, perturbed_shading_normal) < 0.0f)  // cull check for non-adjoint, BSDF::reflective
            return 0.0f;

        const float pdf = BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            incoming,
            modes,
            value);

        if (cosine_mult)
            value *= cosine_multiplier(incoming, perturbed_shading_normal, shadow_terminator_freq_mult);

        return pdf;
    }

    // World space tangent.
    const foundation::Vector3f tangent =
        build_tangent(original_shading_normal, perturbed_shading_normal);

    // Case: i -> p -> o.
    if (foundation::dot(incoming, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        DirectShadingComponents value_ipo;
        float pdf_ipo = BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            incoming,
            modes,
            value_ipo);

        const float ipo_factor =
            microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::IPO);

        value_ipo *= ipo_factor;

        if (cosine_mult)
            value_ipo *= cosine_multiplier(incoming, perturbed_shading_normal, shadow_terminator_freq_mult);

        value += value_ipo;

        final_pdf += pdf_ipo * ipo_factor;
    }

    // World space incoming direction reflected at tangent facet.
    const foundation::Vector3f incoming_reflected = reflect(incoming, tangent);

    // Case: i -> p -> t -> o.
    if (foundation::dot(incoming_reflected, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        DirectShadingComponents value_ipto;
        float pdf_ipto = BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            local_geometry,
            outgoing,
            incoming_reflected,
            modes,
            value_ipto);

        const float ipto_factor =
            microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::IPTO);

        value_ipto *= ipto_factor;

        if (cosine_mult)
            value_ipto *= cosine_multiplier(incoming_reflected, perturbed_shading_normal, shadow_terminator_freq_mult);

        value += value_ipto;

        final_pdf += pdf_ipto * ipto_factor;
    }

    // Case: i -> t -> p -> o.
    if (foundation::dot(incoming, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        DirectShadingComponents value_itpo;
        float pdf_itpo = BSDFImpl::evaluate(
            data,
            adjoint,
            false,
            local_geometry,
            reflect(outgoing, tangent),  // world space outgoing direction reflected at tangent facet
            incoming,
            modes,
            value_itpo);

        const float itpo_factor =
            microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::ITPO);

        value_itpo *= itpo_factor;

        if (cosine_mult)
            value_itpo *= cosine_multiplier(incoming, perturbed_shading_normal, shadow_terminator_freq_mult);

        value += value_itpo;

        final_pdf += pdf_itpo * itpo_factor;
    }

    return final_pdf;
}

template <typename BSDFImpl>
float MicrofacetBRDFWrapper<BSDFImpl>::evaluate_pdf(
    const void*                         data,
    const bool                          adjoint,
    const LocalGeometry&                local_geometry,
    const foundation::Vector3f&         outgoing,
    const foundation::Vector3f&         incoming,
    const int                           modes) const
{
    float final_pdf = 0.0f;

    // World space original shading normal.
    const foundation::Vector3f original_shading_normal(
        local_geometry.m_shading_point->get_original_shading_normal());

    if (foundation::dot(incoming, original_shading_normal) <= 0.0f ||
        foundation::dot(outgoing, original_shading_normal) <= 0.0f)
        return 0.0f;

    // World space perturbed shading normal.
    const foundation::Vector3f perturbed_shading_normal(
        local_geometry.m_shading_point->get_shading_normal());

    // Perturbed normal too similar to original. Tangent vector can not be constructed.
    if (foundation::dot(original_shading_normal, perturbed_shading_normal) > 1.0f - 1e-5f)
    {
        if (foundation::dot(incoming, perturbed_shading_normal) < 0.0f)  // cull check for non-adjoint, BSDF::reflective
            return 0.0f;

        const float pdf = 
            BSDFImpl::evaluate_pdf(
                data,
                adjoint,
                local_geometry,
                outgoing,
                incoming,
                modes);

        return pdf;
    }

    // World space tangent.
    const foundation::Vector3f tangent =
        build_tangent(original_shading_normal, perturbed_shading_normal);

    // Case: i -> p -> o.
    if (foundation::dot(incoming, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        final_pdf +=
            BSDFImpl::evaluate_pdf(
                data,
                adjoint,
                local_geometry,
                outgoing,
                incoming,
                modes)
            * microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::IPO);
    }

    // World space outgoing direction reflected at tangent facet.
    const foundation::Vector3f incoming_reflected = reflect(incoming, tangent);

    // Case: i -> p -> t -> o.
    if (foundation::dot(incoming_reflected, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        final_pdf +=
            BSDFImpl::evaluate_pdf(
                data,
                adjoint,
                local_geometry,
                outgoing,
                incoming_reflected,
                modes)
            * microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::IPTO);
    }

    // Case: i -> t -> p -> o.
    if (foundation::dot(incoming, perturbed_shading_normal) >= 0.0f)  // cull check for non-adjoint, BSDF::reflective
    {
        final_pdf +=
            BSDFImpl::evaluate_pdf(
                data,
                adjoint,
                local_geometry,
                reflect(outgoing, tangent),  // world space incoming direction reflected at tangent facet.
                incoming,
                modes)
            * microfacet_scattering_helper(
                incoming,
                outgoing,
                tangent,
                original_shading_normal,
                perturbed_shading_normal,
                MicrofacetScattering::ITPO);
    }

    return final_pdf;
}

} // namespace renderer
