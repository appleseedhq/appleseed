
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/fresnel.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cmath>

namespace renderer
{

//
// Map roughness to microfacet distribution function's alpha parameter in a
// perceptually linear fashion.
// Refactored from the Disney BRDF implementation.
//

template <typename T>
inline T microfacet_alpha_from_roughness(const T& roughness)
{
    return std::max(T(0.001), foundation::square(roughness));
}

template <typename T>
inline void microfacet_alpha_from_roughness(
    const T&   roughness,
    const T&   anisotropic,
    T&         alpha_x,
    T&         alpha_y)
{
    if (anisotropic >= 0.0)
    {
        const T aspect = std::sqrt(T(1.0) - anisotropic * T(0.9));
        alpha_x = std::max(T(0.001), foundation::square(roughness) / aspect);
        alpha_y = std::max(T(0.001), foundation::square(roughness) * aspect);
    }
    else
    {
        const T aspect = std::sqrt(T(1.0) + anisotropic * T(0.9));
        alpha_x = std::max(T(0.001), foundation::square(roughness) * aspect);
        alpha_y = std::max(T(0.001), foundation::square(roughness) / aspect);
    }
}


template <typename T>
class FresnelDielectricFun
{
  public:
    FresnelDielectricFun(
        const Spectrum& reflectance,
        const T         reflectance_multiplier,
        const T         eta)
      : m_reflectance(reflectance)
      , m_reflectance_multiplier(reflectance_multiplier)
      , m_eta(eta)
    {
    }

    void operator()(
        const foundation::Vector<T,3>&  o,
        const foundation::Vector<T,3>&  h,
        const foundation::Vector<T,3>&  n,
        Spectrum&                       value) const
    {
        value = m_reflectance;
        double f;
        foundation::fresnel_reflectance_dielectric(f, m_eta, foundation::dot(o, h));
        value *= static_cast<float>(f * m_reflectance_multiplier);
    }

  private:
    const Spectrum& m_reflectance;
    const T         m_reflectance_multiplier;
    const T         m_eta;
};


template <typename T>
class MicrofacetBRDFHelper
{
  public:
    typedef foundation::Vector<T, 3> VectorType;
    typedef foundation::Basis3<T> BasisType;

    template <typename MDF, typename FresnelFun>
    static void sample(
        SamplingContext&    sampling_context,
        const MDF&          mdf,
        const T             alpha_x,
        const T             alpha_y,
        FresnelFun          f,
        const T             cos_on,
        BSDFSample&         sample)
    {
        // gcc needs the qualifier, otherwise
        // it complains about missing operator() for BSDFSample.
        MicrofacetBRDFHelper<T>::sample(
            sampling_context,
            mdf,
            alpha_x,
            alpha_y,
            alpha_x,
            alpha_y,
            f,
            cos_on,
            sample);
    }

    template <typename MDF, typename FresnelFun>
    static T evaluate(
        const MDF&          mdf,
        const T             alpha_x,
        const T             alpha_y,
        const BasisType&    shading_basis,
        const VectorType&   outgoing,
        const VectorType&   incoming,
        FresnelFun          f,
        const T             cos_in,
        const T             cos_on,
        Spectrum&           value)
    {
        return evaluate(
            mdf,
            alpha_x,
            alpha_y,
            alpha_x,
            alpha_y,
            shading_basis,
            outgoing,
            incoming,
            f,
            cos_in,
            cos_on,
            value);
    }

    template <typename MDF>
    static T pdf(
        const MDF&          mdf,
        const T             alpha_x,
        const T             alpha_y,
        const BasisType&    shading_basis,
        const VectorType&   outgoing,
        const VectorType&   incoming)
    {
        const VectorType h = foundation::normalize(incoming + outgoing);
        const T cos_oh = foundation::dot(outgoing, h);
        return
            mdf.pdf(
                shading_basis.transform_to_local(outgoing),
                shading_basis.transform_to_local(h),
                alpha_x,
                alpha_y) / (T(4.0) * cos_oh);
    }

    //
    // Decoupled distribution and shadowing alpha parameters.
    // They are used in the Disney BRDF implementation.
    //

    template <typename MDF, typename FresnelFun>
    static void sample(
        SamplingContext&    sampling_context,
        const MDF&          mdf,
        const T             alpha_x,
        const T             alpha_y,
        const T             g_alpha_x,
        const T             g_alpha_y,
        FresnelFun          f,
        const T             cos_on,
        BSDFSample&         sample)
    {
        // Compute the incoming direction by sampling the MDF.
        sampling_context.split_in_place(3, 1);
        const VectorType s = sampling_context.next_vector2<3>();
        const VectorType wo = sample.get_shading_basis().transform_to_local(sample.m_outgoing.get_value());
        const VectorType m = mdf.sample(wo, s, alpha_x, alpha_y);
        const VectorType h = sample.get_shading_basis().transform_to_parent(m);
        const VectorType incoming = foundation::reflect(sample.m_outgoing.get_value(), h);
        const T cos_oh = foundation::dot(sample.m_outgoing.get_value(), h);

        // No reflection below the shading surface.
        const VectorType& n = sample.get_shading_normal();
        const T cos_in = foundation::dot(incoming, n);
        if (cos_in < T(0.0))
            return;

        const T D = mdf.D(m, alpha_x, alpha_y);

        const T G =
            mdf.G(
                sample.get_shading_basis().transform_to_local(incoming),
                wo,
                m,
                g_alpha_x,
                g_alpha_y);

        f(sample.m_outgoing.get_value(), h, sample.get_shading_normal(), sample.m_value);
        sample.m_value *= static_cast<float>(D * G / (T(4.0) * cos_on * cos_in));
        sample.m_probability = mdf.pdf(wo, m, alpha_x, alpha_y) / (T(4.0) * cos_oh);
        sample.m_mode = ScatteringMode::Glossy;
        sample.m_incoming = foundation::Dual<VectorType>(incoming);
        sample.compute_reflected_differentials();
    }

    template <typename MDF, typename FresnelFun>
    static T evaluate(
        const MDF&          mdf,
        const T             alpha_x,
        const T             alpha_y,
        const T             g_alpha_x,
        const T             g_alpha_y,
        const BasisType&    shading_basis,
        const VectorType&   outgoing,
        const VectorType&   incoming,
        FresnelFun          f,
        const T             cos_in,
        const T             cos_on,
        Spectrum&           value)
    {
        const VectorType h = foundation::normalize(incoming + outgoing);
        const VectorType m = shading_basis.transform_to_local(h);
        const T D = mdf.D(m, alpha_x, alpha_y);

        const VectorType wo = shading_basis.transform_to_local(outgoing);
        const T G =
            mdf.G(
                shading_basis.transform_to_local(incoming),
                wo,
                m,
                g_alpha_x,
                g_alpha_y);

        const T cos_oh = foundation::dot(outgoing, h);
        f(outgoing, h, shading_basis.get_normal(), value);
        value *= static_cast<float>(D * G / (T(4.0) * cos_on * cos_in));
        return mdf.pdf(wo, m, alpha_x, alpha_y) / (T(4.0) * cos_oh);
    }
};

#endif

}   // namespace renderer
