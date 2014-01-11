
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
    const double        offset,
    const double        amplitude)
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

    m_rcp_du = 1.0 / m_du;
    m_rcp_dv = 1.0 / m_dv;
}

Vector3d BumpMappingModifier::evaluate(
    TextureCache&       texture_cache,
    const Vector3d&     n,
    const Vector2d&     uv,
    const Vector3d&     dpdu,
    const Vector3d&     dpdv) const
{
    // Lookup the bump map.
    // todo: we don't have ray differentials so our offsets
    // in the texture are not yet based on du/dx, du/dy etc.
    double disp, disp_du, disp_dv;
    m_map->evaluate(texture_cache, uv, disp);
    m_map->evaluate(texture_cache, uv + Vector2d(m_du, 0.0), disp_du);
    m_map->evaluate(texture_cache, uv + Vector2d(0.0, m_dv), disp_dv);

    const double ddispdu = m_amplitude * (disp_du - disp) * m_rcp_du;
    const double ddispdv = m_amplitude * (disp_dv - disp) * m_rcp_dv;

    const Vector3d modified_dpdu = dpdu + ddispdu * n;
    const Vector3d modified_dpdv = dpdv + ddispdv * n;
    const Vector3d modified_n = normalize(cross(modified_dpdu, modified_dpdv));

    return dot(n, modified_n) > 0.0 ? modified_n : -modified_n;
}

}   // namespace renderer
