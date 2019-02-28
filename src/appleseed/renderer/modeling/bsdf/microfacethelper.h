
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cmath>

namespace renderer
{

//
// Map roughness to microfacet distribution function's alpha parameter in a
// perceptually linear fashion. Refactored from the Disney BRDF implementation.
//

inline float microfacet_alpha_from_roughness(const float roughness)
{
    return std::max(0.001f, foundation::square(roughness));
}

inline void microfacet_alpha_from_roughness(
    const float     roughness,
    const float     anisotropy,
    float&          alpha_x,
    float&          alpha_y)
{
    if (anisotropy >= 0.0f)
    {
        const float aspect = std::sqrt(1.0f - anisotropy * 0.9f);
        alpha_x = std::max(0.001f, foundation::square(roughness) / aspect);
        alpha_y = std::max(0.001f, foundation::square(roughness) * aspect);
    }
    else
    {
        const float aspect = std::sqrt(1.0f + anisotropy * 0.9f);
        alpha_x = std::max(0.001f, foundation::square(roughness) * aspect);
        alpha_y = std::max(0.001f, foundation::square(roughness) / aspect);
    }
}


//
// Map highlight falloff to STD microfacet distribution function's gamma parameter
// in a perceptually linear fashion.
//

inline float highlight_falloff_to_gama(const float highlight_falloff)
{
    const float t = highlight_falloff;
    const float t2 = t * t;
    return foundation::mix(1.51f, 40.0f, t2 * t2 * t);
}


//
// Helper class to sample and evaluate microfacet BRDFs.
//

template <bool Flip>
class MicrofacetBRDFHelper
{
  public:
    template <typename MDF, typename FresnelFun>
    static void sample(
        SamplingContext&                sampling_context,
        const MDF&                      mdf,
        const float                     alpha_x,
        const float                     alpha_y,
        const float                     gamma,
        FresnelFun                      f,
        BSDFSample&                     sample)
    {
        const foundation::Vector3f& outgoing = sample.m_outgoing.get_value();
        foundation::Vector3f wo = sample.m_shading_basis.transform_to_local(outgoing);

        if (wo.y == 0.0f)
            return;

        // Flip the outgoing vector to be in the same hemisphere as the shading normal if needed.
        if (Flip)
            wo.y = std::abs(wo.y);

        // Compute the incoming direction by sampling the MDF.
        sampling_context.split_in_place(2, 1);
        const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
        foundation::Vector3f m = mdf.sample(wo, s, alpha_x, alpha_y, gamma);
        foundation::Vector3f wi = foundation::reflect(wo, m);

        // Force the outgoing direction to lie above the geometric surface.
        const foundation::Vector3f ng =
            sample.m_shading_basis.transform_to_local(sample.m_geometric_normal);
        if (BSDF::force_above_surface(wi, ng))
            m = foundation::normalize(wo + wi);

        if (wi.y == 0.0f)
            return;

        const float cos_oh = foundation::dot(wo, m);

        const float probability =
            mdf.pdf(wo, m, alpha_x, alpha_y, gamma) / std::abs(4.0f * cos_oh);
        assert(probability >= 0.0f);

        // Disabled until BSDF are evaluated in local space, because the numerous
        // conversions between local space and world space kill precision.
        //
        // #ifndef NDEBUG
        //         const float ref_probability =
        //             pdf(
        //                 mdf,
        //                 alpha_x,
        //                 alpha_y,
        //                 gamma,
        //                 sample.m_shading_basis,
        //                 outgoing,
        //                 incoming);
        // 
        //         assert(feq(probability, ref_probability, 1.0e-2f));
        // #endif

        // Skip samples with very low probability.
        if (probability > 1.0e-6f)
        {
            sample.set_to_scattering(ScatteringMode::Glossy, probability);

            const float D = mdf.D(m, alpha_x, alpha_y, gamma);
            const float G =
                mdf.G(
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y,
                    gamma);

            const foundation::Vector3f n(0.0f, 1.0f, 0.0f);
            const float cos_on = wo.y;
            const float cos_in = wi.y;

            f(wo, m, n, sample.m_value.m_glossy);
            sample.m_value.m_glossy *= D * G / std::abs(4.0f * cos_on * cos_in);

            const foundation::Vector3f incoming =
                sample.m_shading_basis.transform_to_parent(wi);
            sample.m_incoming = foundation::Dual<foundation::Vector3f>(incoming);

            sample.compute_reflected_differentials();
        }
    }

    template <typename MDF, typename FresnelFun>
    static float evaluate(
        const MDF&                      mdf,
        const float                     alpha_x,
        const float                     alpha_y,
        const float                     gamma,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        FresnelFun                      f,
        Spectrum&                       value)
    {
        foundation::Vector3f wo = shading_basis.transform_to_local(outgoing);
        foundation::Vector3f wi = shading_basis.transform_to_local(incoming);

        if (wo.y == 0.0f || wi.y == 0.0f)
            return 0.0f;

        // Flip the incoming and outgoing vectors to be in the same
        // hemisphere as the shading normal if needed.
        if (Flip)
        {
            wo.y = std::abs(wo.y);
            wi.y = std::abs(wi.y);
        }

        const foundation::Vector3f m = foundation::normalize(wi + wo);

        const float cos_oh = foundation::dot(wo, m);

        if (cos_oh == 0.0f)
            return 0.0f;

        const float D = mdf.D(m, alpha_x, alpha_y, gamma);
        const float G =
            mdf.G(
                wi,
                wo,
                m,
                alpha_x,
                alpha_y,
                gamma);

        const foundation::Vector3f n(0.0f, 1.0f, 0.0f);
        f(wo, m, n, value);

        const float cos_on = wo.y;
        const float cos_in = wi.y;

        value *= D * G / std::abs(4.0f * cos_on * cos_in);

        return mdf.pdf(wo, m, alpha_x, alpha_y, gamma) / std::abs(4.0f * cos_oh);
    }

    template <typename MDF>
    static float pdf(
        const MDF&                      mdf,
        const float                     alpha_x,
        const float                     alpha_y,
        const float                     gamma,
        const foundation::Basis3f&      shading_basis,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming)
    {
        foundation::Vector3f wo = shading_basis.transform_to_local(outgoing);
        foundation::Vector3f wi = shading_basis.transform_to_local(incoming);

        // Flip the incoming and outgoing vectors to be in the same
        // hemisphere as the shading normal if needed.
        if (Flip)
        {
            wo.y = std::abs(wo.y);
            wi.y = std::abs(wi.y);
        }

        const foundation::Vector3f m = foundation::normalize(wi + wo);
        const float cos_oh = foundation::dot(wo, m);

        if (cos_oh == 0.0f)
            return 0.0f;

        return
            mdf.pdf(
                wo,
                m,
                alpha_x,
                alpha_y,
                gamma) / std::abs(4.0f * cos_oh);
    }

    // Simplified version of sample used when computing albedo tables.
    template <typename MDF>
    static float sample(
        const MDF&                      mdf,
        const foundation::Vector2f&     s,
        const float                     alpha,
        const foundation::Vector3f&     wo,
        foundation::Vector3f&           wi,
        float&                          probability)
    {
        foundation::Vector3f m = mdf.sample(wo, s, alpha, alpha, 0.0f);

        const float cos_oh = std::abs(foundation::dot(wo, m));
        const float cos_on = std::abs(wo.y);

        if (cos_on == 0.0f || cos_oh == 0.0f)
        {
            probability = 0.0f;
            return 0.0f;
        }

        const foundation::Vector3f n(0.0f, 1.0f, 0.0f);

        wi = foundation::reflect(wo, m);

        if (BSDF::force_above_surface(wi, n))
            m = foundation::normalize(wo + wi);

        const float cos_in = std::abs(wi.y);

        if (cos_in == 0.0f)
        {
            probability = 0.0f;
            return 0.0f;
        }

        const float gamma = 1.0f;
        const float D = mdf.D(m, alpha, alpha, gamma);
        const float G =
            mdf.G(
                wi,
                wo,
                m,
                alpha,
                alpha,
                gamma);

        probability = mdf.pdf(wo, m, alpha, alpha, gamma) / std::abs(4.0f * cos_oh);
        assert(probability >= 0.0f);

        return D * G / (4.0f * cos_on * cos_in);
    }
};

float get_average_albedo(
    const foundation::GGXMDF&       mdf,
    const float                     roughness);

float get_average_albedo(
    const foundation::BeckmannMDF&  mdf,
    const float                     roughness);

void microfacet_energy_compensation_term(
    const foundation::GGXMDF&       mdf,
    const float                     roughness,
    const float                     cos_in,
    const float                     cos_on,
    float&                          fms,
    float&                          eavg);

void microfacet_energy_compensation_term(
    const foundation::BeckmannMDF&  mdf,
    const float                     roughness,
    const float                     cos_in,
    const float                     cos_on,
    float&                          fms,
    float&                          eavg);


// Write the computed tables to OpenEXR images and C++ arrays.
// Used in Renderer_Modeling_BSDF_EnergyCompensation unit test.
void write_microfacet_directional_albedo_tables(const char* directory);

}   // namespace renderer
