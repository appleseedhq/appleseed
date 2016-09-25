
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "bumpmappingmodifier.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"

using namespace foundation;

namespace renderer
{

BumpMappingModifier::BumpMappingModifier(
    const Source*       map,
    const float         offset,
    const float         amplitude)
  : m_map(map)
  , m_amplitude(amplitude)
{
    const TextureSource* texture_map = dynamic_cast<const TextureSource*>(map);

    if (texture_map)
    {
        Texture& texture = texture_map->get_texture_instance().get_texture();
        const CanvasProperties& props = texture.properties();

        m_du = offset / props.m_canvas_width;
        m_dv = offset / props.m_canvas_height;
    }
    else
    {
        m_du = m_dv = offset;
    }

    m_rcp_du = 1.0f / m_du;
    m_rcp_dv = 1.0f / m_dv;
}

Basis3d BumpMappingModifier::modify(
    TextureCache&       texture_cache,
    const Vector2f&     uv,
    const Basis3d&      basis) const
{
    // Evaluate the displacement function at (u, v).
    double val;
    m_map->evaluate(texture_cache, uv, val);

    // Evaluate the displacement function at (u + delta_u, v).
    double val_du;
    m_map->evaluate(texture_cache, Vector2f(uv[0] + m_du, uv[1]), val_du);

    // Evaluate the displacement function at (u, v + delta_v).
    double val_dv;
    m_map->evaluate(texture_cache, Vector2f(uv[0], uv[1] + m_dv), val_dv);

    // Compute the partial derivatives of the displacement function d(u, v).
    const double ddispdu = m_amplitude * (val_du - val) * m_rcp_du;
    const double ddispdv = m_amplitude * (val_dv - val) * m_rcp_dv;

    // Compute the partial derivatives of the displaced surface p(u, v) + d(u, v) * n.
    const Vector3d perturbed_dpdu = basis.get_tangent_u() + ddispdu * basis.get_normal();
    const Vector3d perturbed_dpdv = basis.get_tangent_v() + ddispdv * basis.get_normal();

    // Compute the perturbed normal.
    Vector3d perturbed_n = normalize(cross(perturbed_dpdu, perturbed_dpdv));

    // Make sure the perturbed normal lies in the same hemisphere as the original one.
    if (dot(perturbed_n, basis.get_normal()) < 0.0)
        perturbed_n = -perturbed_n;

    // Construct an orthonormal basis around the perturbed normal.
    return Basis3d(perturbed_n, perturbed_dpdu);
}

}   // namespace renderer
