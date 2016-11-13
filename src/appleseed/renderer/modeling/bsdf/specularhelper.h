
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_BSDF_SPECULARHELPER_H
#define APPLESEED_RENDERER_MODELING_BSDF_SPECULARHELPER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

namespace renderer
{

class SpecularBRDFHelper
{
  public:
    template <typename FresnelFun>
    static void sample(
        FresnelFun          f,
        BSDFSample&         sample)
    {
        const foundation::Vector3f& n = sample.m_shading_basis.get_normal();
        const foundation::Vector3f& outgoing = sample.m_outgoing.get_value();

        // Compute the incoming direction.
        const foundation::Vector3f incoming(
            BSDF::force_above_surface(
                foundation::reflect(outgoing, n),
                sample.m_geometric_normal));

        // No reflection below the shading surface.
        const float cos_in = dot(incoming, n);
        if (cos_in < 0.0f)
            return;

        f(outgoing, n, n, sample.m_value);
        sample.m_value *= (1.0f / cos_in);

        // The probability density of the sampled direction is the Dirac delta.
        sample.m_probability = BSDF::DiracDelta;

        // Set the scattering mode.
        sample.m_mode = ScatteringMode::Specular;

        sample.m_incoming = foundation::Dual3f(incoming);
        sample.compute_reflected_differentials();
    }
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_SPECULARHELPER_H
