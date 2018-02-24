
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_MICROFACETHELPER_H
#define APPLESEED_RENDERER_MODELING_BSDF_MICROFACETHELPER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
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
        const float                     cos_on,
        BSDFSample&                     sample)
    {
        // Compute the incoming direction by sampling the MDF.
        sampling_context.split_in_place(3, 1);
        const foundation::Vector3f s = sampling_context.next2<foundation::Vector3f>();
        const foundation::Vector3f wo = sample.m_shading_basis.transform_to_local(sample.m_outgoing.get_value());
        foundation::Vector3f m = mdf.sample(wo, s, alpha_x, alpha_y, gamma);
        foundation::Vector3f h = sample.m_shading_basis.transform_to_parent(m);

        const foundation::Vector3f& outgoing = sample.m_outgoing.get_value();
        foundation::Vector3f incoming = foundation::reflect(outgoing, h);

        if (force_above_surface(incoming, sample.m_geometric_normal))
        {
            h = foundation::normalize(outgoing + incoming);
            m = sample.m_shading_basis.transform_to_local(h);
        }

        const foundation::Vector3f& n = sample.m_shading_basis.get_normal();
        const float cos_oh = std::abs(foundation::dot(sample.m_outgoing.get_value(), h));
        const float cos_in = std::abs(foundation::dot(incoming, n));

        const float D = mdf.D(m, alpha_x, alpha_y, gamma);
        const float G =
            mdf.G(
                sample.m_shading_basis.transform_to_local(incoming),
                wo,
                m,
                alpha_x,
                alpha_y,
                gamma);

        f(sample.m_outgoing.get_value(), h, n, sample.m_value.m_glossy);
        sample.m_value.m_glossy *= D * G / (4.0f * cos_on * cos_in);

        sample.m_probability = mdf.pdf(wo, m, alpha_x, alpha_y, gamma) / (4.0f * cos_oh);

        // Skip samples with very low probability.
        if (sample.m_probability > 1e-6f)
        {
            sample.m_mode = ScatteringMode::Glossy;
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
        const float                     cos_in,
        const float                     cos_on,
        Spectrum&                       value)
    {
        const foundation::Vector3f h = foundation::normalize(incoming + outgoing);
        const foundation::Vector3f m = shading_basis.transform_to_local(h);
        const foundation::Vector3f wo = shading_basis.transform_to_local(outgoing);

        const float D = mdf.D(m, alpha_x, alpha_y, gamma);
        const float G =
            mdf.G(
                shading_basis.transform_to_local(incoming),
                wo,
                m,
                alpha_x,
                alpha_y,
                gamma);

        const float cos_oh = foundation::dot(outgoing, h);
        f(outgoing, h, shading_basis.get_normal(), value);
        value *= D * G / (4.0f * cos_on * cos_in);

        return mdf.pdf(wo, m, alpha_x, alpha_y, gamma) / (4.0f * cos_oh);
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
        const foundation::Vector3f h = foundation::normalize(incoming + outgoing);
        const float cos_oh = foundation::dot(outgoing, h);
        return
            mdf.pdf(
                shading_basis.transform_to_local(outgoing),
                shading_basis.transform_to_local(h),
                alpha_x,
                alpha_y,
                gamma) / (4.0f * cos_oh);
    }

    // Simplified version of sample used when computing albedo tables.
    template <typename MDF>
    static float sample(
        const MDF&                      mdf,
        const foundation::Vector3f&     s,
        const float                     alpha,
        const foundation::Vector3f&     wo,
        foundation::Vector3f&           wi,
        float&                          probability)
    {
        foundation::Vector3f h = mdf.sample(wo, s, alpha, alpha, 0.0f);
        const float cos_oh = std::abs(foundation::dot(wo, h));

        if (cos_oh == 0.0f)
        {
            probability = 0.0f;
            return 0.0f;
        }

        const foundation::Vector3f n(0.0f, 1.0f, 0.0f);

        wi = foundation::reflect(wo, h);

        if (force_above_surface(wi, n))
            h = foundation::normalize(wo + wi);

        const float cos_in = std::abs(foundation::dot(wi, n));

        const float gamma = 1.0f;
        const float D = mdf.D(h, alpha, alpha, gamma);
        const float G =
            mdf.G(
                wi,
                wo,
                h,
                alpha,
                alpha,
                gamma);

        probability = mdf.pdf(wo, h, alpha, alpha, gamma) / (4.0f * cos_oh);

        const float cos_on = std::abs(wo.y);
        return D * G / (4.0f * cos_on * cos_in);
    }

  private:
    static bool force_above_surface(
        foundation::Vector3f&           direction,
        const foundation::Vector3f&     normal)
    {
        const float Eps = 1.0e-4f;

        const float cos_theta = foundation::dot(direction, normal);
        const float correction = Eps - cos_theta;

        if (correction > 0.0f)
        {
            direction = foundation::normalize(direction + correction * normal);
            return true;
        }

        return false;
    }
};

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

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_MICROFACETHELPER_H
