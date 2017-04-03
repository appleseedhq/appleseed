
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
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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

inline float microfacet_alpha_from_roughness(const float& roughness)
{
    return std::max(0.001f, foundation::square(roughness));
}

inline void microfacet_alpha_from_roughness(
    const float&   roughness,
    const float&   anisotropy,
    float&         alpha_x,
    float&         alpha_y)
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
    const float t2 = foundation::square(t);
    return foundation::mix(1.51f, 40.0f, foundation::square(t2) * t);
}

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
        const foundation::Vector3f m = mdf.sample(wo, s, alpha_x, alpha_y, gamma);
        const foundation::Vector3f h = sample.m_shading_basis.transform_to_parent(m);
        const foundation::Vector3f incoming = foundation::reflect(sample.m_outgoing.get_value(), h);
        const float cos_oh = foundation::dot(sample.m_outgoing.get_value(), h);

        // No reflection below the shading surface.
        const float cos_in = foundation::dot(incoming, sample.m_shading_basis.get_normal());
        if (cos_in <= 0.0f)
            return;

        const float D = mdf.D(m, alpha_x, alpha_y, gamma);

        const float G =
            mdf.G(
                sample.m_shading_basis.transform_to_local(incoming),
                wo,
                m,
                alpha_x,
                alpha_y,
                gamma);

        f(sample.m_outgoing.get_value(), h, sample.m_shading_basis.get_normal(), sample.m_value);
        sample.m_value *= D * G / (4.0f * cos_on * cos_in);
        sample.m_probability = mdf.pdf(wo, m, alpha_x, alpha_y, gamma) / (4.0f * cos_oh);
        sample.m_mode = ScatteringMode::Glossy;
        sample.m_incoming = foundation::Dual<foundation::Vector3f>(incoming);
        sample.compute_reflected_differentials();
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
        const float D = mdf.D(m, alpha_x, alpha_y, gamma);

        const foundation::Vector3f wo = shading_basis.transform_to_local(outgoing);
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
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_MICROFACETHELPER_H
