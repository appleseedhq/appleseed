
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"

namespace renderer
{

class SpecularBRDFHelper
{
  public:
    template <typename FresnelFun>
    static void sample(
        FresnelFun                  f,
        const BSDF::LocalGeometry&  local_geometry,
        const foundation::Dual3f&   outgoing,
        BSDFSample&                 sample)
    {
        const foundation::Vector3f& n = local_geometry.m_shading_basis.get_normal();

        // Compute the incoming direction.
        foundation::Vector3f incoming = foundation::reflect(outgoing.get_value(), n);
        BSDF::force_above_surface(incoming, local_geometry.m_geometric_normal);

        // No reflection below the shading surface.
        const float cos_in = dot(incoming, n);
        if (cos_in <= 0.0f)
            return;

        sample.set_to_scattering(ScatteringMode::Specular, BSDF::DiracDelta);

        f(outgoing.get_value(), n, n, sample.m_value.m_glossy);
        sample.m_value.m_glossy /= cos_in;

        sample.m_incoming = foundation::Dual3f(incoming);
        sample.compute_specular_reflected_differentials(local_geometry, outgoing);
    }
};

}   // namespace renderer
