
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
#include "foundation/math/dual.h"
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
    return std::max(0.001f, roughness * roughness);
}

inline void microfacet_alpha_from_roughness(
    const float     roughness,
    const float     anisotropy,
    float&          alpha_x,
    float&          alpha_y)
{
    const float square_roughness = roughness * roughness;
    if (anisotropy >= 0.0f)
    {
        const float aspect = std::sqrt(1.0f - anisotropy * 0.9f);
        alpha_x = std::max(0.001f, square_roughness / aspect);
        alpha_y = std::max(0.001f, square_roughness * aspect);
    }
    else
    {
        const float aspect = std::sqrt(1.0f + anisotropy * 0.9f);
        alpha_x = std::max(0.001f, square_roughness * aspect);
        alpha_y = std::max(0.001f, square_roughness / aspect);
    }
}


//
// Helper class to sample and evaluate microfacet BRDFs.
//

template <typename MDF>
class MicrofacetBRDFHelper
{
  public:
    template <typename FresnelFun>
    static void sample(
        SamplingContext&                sampling_context,
        const float                     roughness,
        const float                     alpha_x,
        const float                     alpha_y,
        FresnelFun                      f,
        const BSDF::LocalGeometry&      local_geometry,
        const foundation::Dual3f&       outgoing,
        BSDFSample&                     sample)
    {
        foundation::Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing.get_value());

        if (wo.y == 0.0f)
            return;

        // Compute the incoming direction by sampling the MDF.
        sampling_context.split_in_place(2, 1);
        const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
        foundation::Vector3f m = MDF::sample(wo, s, alpha_x, alpha_y);
        foundation::Vector3f wi = foundation::reflect(wo, m);

        // Force the outgoing direction to lie above the geometric surface.
        const foundation::Vector3f ng =
            local_geometry.m_shading_basis.transform_to_local(local_geometry.m_geometric_normal);
        if (BSDF::force_above_surface(wi, ng))
            m = foundation::normalize(wo + wi);

        if (wi.y == 0.0f)
            return;

        const float cos_oh = foundation::dot(wo, m);

        const float probability =
            MDF::pdf(wo, m, alpha_x, alpha_y) / std::abs(4.0f * cos_oh);
        assert(probability >= 0.0f);

        // Disabled until BSDF are evaluated in local space, because the numerous
        // conversions between local space and world space kill precision.
        //
        // #ifndef NDEBUG
        //         const float ref_probability =
        //             pdf(
        //                 alpha_x,
        //                 alpha_y,
        //                 local_geometry.m_shading_basis,
        //                 outgoing,
        //                 incoming);
        //
        //         assert(feq(probability, ref_probability, 1.0e-2f));
        // #endif

        // Skip samples with very low probability.
        if (probability > 1.0e-6f)
        {
            sample.set_to_scattering(ScatteringMode::Glossy, probability);

            const float D = MDF::D(m, alpha_x, alpha_y);
            const float G =
                MDF::G(
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y);

            const foundation::Vector3f n(0.0f, 1.0f, 0.0f);
            const float cos_on = wo.y;
            const float cos_in = wi.y;

            f(wo, m, n, sample.m_value.m_glossy);
            sample.m_value.m_glossy *= D * G / std::abs(4.0f * cos_on * cos_in);

            const foundation::Vector3f incoming =
                local_geometry.m_shading_basis.transform_to_parent(wi);
            sample.m_incoming = foundation::Dual<foundation::Vector3f>(incoming);

            sample.compute_glossy_reflected_differentials(local_geometry, roughness, outgoing);
        }
    }

    template <typename FresnelFun>
    static float evaluate(
        const float                     alpha_x,
        const float                     alpha_y,
        FresnelFun                      f,
        const BSDF::LocalGeometry&      local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming,
        Spectrum&                       value)
    {
        foundation::Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
        foundation::Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);

        if (wo.y == 0.0f || wi.y == 0.0f)
            return 0.0f;

        const foundation::Vector3f m = foundation::normalize(wi + wo);

        const float cos_oh = foundation::dot(wo, m);

        if (cos_oh == 0.0f)
            return 0.0f;

        const float D = MDF::D(m, alpha_x, alpha_y);
        const float G =
            MDF::G(
                wi,
                wo,
                m,
                alpha_x,
                alpha_y);

        const foundation::Vector3f n(0.0f, 1.0f, 0.0f);
        f(wo, m, n, value);

        const float cos_on = wo.y;
        const float cos_in = wi.y;

        value *= D * G / std::abs(4.0f * cos_on * cos_in);

        return MDF::pdf(wo, m, alpha_x, alpha_y) / std::abs(4.0f * cos_oh);
    }

    static float pdf(
        const float                     alpha_x,
        const float                     alpha_y,
        const BSDF::LocalGeometry&      local_geometry,
        const foundation::Vector3f&     outgoing,
        const foundation::Vector3f&     incoming)
    {
        foundation::Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
        foundation::Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);

        const foundation::Vector3f m = foundation::normalize(wi + wo);
        const float cos_oh = foundation::dot(wo, m);

        if (cos_oh == 0.0f)
            return 0.0f;

        return
            MDF::pdf(
                wo,
                m,
                alpha_x,
                alpha_y) / std::abs(4.0f * cos_oh);
    }
};

float get_directional_albedo(const float cos_theta, const float roughness);
float get_average_albedo(const float roughness);

// Write the computed tables to OpenEXR images and C++ arrays.
// Used in Renderer_Modeling_BSDF_EnergyCompensation unit tests.
void write_microfacet_directional_albedo_tables(const char* directory);

}   // namespace renderer
