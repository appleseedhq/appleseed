
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;

namespace renderer
{

//#define USE_SCREEN_SPACE_UV_DERIVATIVES

BumpMappingModifier::BumpMappingModifier(
    const Source*           map,
    const float             offset,
    const float             amplitude)
  : m_map(map)
  , m_amplitude(static_cast<double>(amplitude))
{
#ifndef USE_SCREEN_SPACE_UV_DERIVATIVES
    const TextureSource* texture_map = dynamic_cast<const TextureSource*>(map);

    if (texture_map != nullptr)
    {
        Texture& texture = texture_map->get_texture_instance().get_texture();
        const CanvasProperties& props = texture.properties();

        m_delta_u = offset / props.m_canvas_width;
        m_delta_v = offset / props.m_canvas_height;
    }
    else
    {
        m_delta_u = m_delta_v = offset;
    }

    m_rcp_delta_u = 1.0 / static_cast<double>(m_delta_u);
    m_rcp_delta_v = 1.0 / static_cast<double>(m_delta_v);
#endif
}

Basis3d BumpMappingModifier::modify(
    TextureCache&           texture_cache,
    const Basis3d&          basis,
    const ShadingPoint&     shading_point) const
{
    const size_t UVSet = 0;
    const Vector2f& uv = shading_point.get_uv(UVSet);
    const Vector3d& dpdu = shading_point.get_dpdu(UVSet);
    const Vector3d& dpdv = shading_point.get_dpdv(UVSet);
    const Vector3d& n = basis.get_normal();
    const Vector3d& dndu = shading_point.get_dndu(UVSet);
    const Vector3d& dndv = shading_point.get_dndv(UVSet);

#ifdef USE_SCREEN_SPACE_UV_DERIVATIVES
    const Vector2f& duvdx = shading_point.get_duvdx(UVSet);
    const Vector2f& duvdy = shading_point.get_duvdy(UVSet);
    const float delta_u = 0.5f * (abs(duvdx[0]) + abs(duvdy[0]));
    const float delta_v = 0.5f * (abs(duvdx[1]) + abs(duvdy[1]));
#else
    const float delta_u = m_delta_u;
    const float delta_v = m_delta_v;
#endif

    // Evaluate the height function at (u, v), (u + delta_u, v) and (u, v + delta_v).
    const double h =  evaluate_height(texture_cache, uv[0],           uv[1]);
    const double hu = evaluate_height(texture_cache, uv[0] + delta_u, uv[1]);
    const double hv = evaluate_height(texture_cache, uv[0],           uv[1] + delta_v);

    // Compute the partial derivatives of the height function at (u, v).
#ifdef USE_SCREEN_SPACE_UV_DERIVATIVES
    const double dhdu = (hu - h) / delta_u;
    const double dhdv = (hv - h) / delta_v;
#else
    const double dhdu = (hu - h) * m_rcp_delta_u;
    const double dhdv = (hv - h) * m_rcp_delta_v;
#endif

    // Compute the partial derivatives of the displaced surface p'(u, v) = p(u, v) + amplitude * h(u, v) * n.
    const Vector3d displaced_dpdu = dpdu + m_amplitude * (dhdu * n + h * dndu);
    const Vector3d displaced_dpdv = dpdv + m_amplitude * (dhdv * n + h * dndv);

    // Compute the perturbed normal.
    Vector3d perturbed_n = normalize(cross(displaced_dpdu, displaced_dpdv));

    // Make sure the perturbed normal lies in the same hemisphere as the original one.
    if (dot(perturbed_n, n) < 0.0)
        perturbed_n = -perturbed_n;

    // Construct an orthonormal basis around the perturbed normal.
    return Basis3d(perturbed_n, displaced_dpdu);
}

double BumpMappingModifier::evaluate_height(
    TextureCache&           texture_cache,
    const float             u,
    const float             v) const
{
    float h;
    m_map->evaluate(texture_cache, Vector2f(u, v), h);
    return static_cast<double>(h);
}

}   // namespace renderer
