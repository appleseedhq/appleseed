
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "normalmappingmodifier.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"

using namespace foundation;

namespace renderer
{

NormalMappingModifier::NormalMappingModifier(
    const Source*       map,
    const UpVector      up_vector)
  : m_map(map)
  , m_y(up_vector == UpVectorY ? 1 : 2)
{
}

Basis3d NormalMappingModifier::modify(
    TextureCache&       texture_cache,
    const Vector2d&     uv,
    const Basis3d&      basis) const
{
    // Lookup the normal map.
    Color3f normal_rgb;
    m_map->evaluate(texture_cache, uv, normal_rgb);

    // Reconstruct the normal from the texel value.
    const Vector3d normal(
        static_cast<double>(normal_rgb[0]) * 2.0 - 1.0,
        static_cast<double>(normal_rgb[m_y]) * 2.0 - 1.0,
        static_cast<double>(normal_rgb[3 - m_y]) * 2.0 - 1.0);

    // Construct an orthonormal basis around that new normal.
    return Basis3d(
        normalize(basis.transform_to_parent(normal)),
        basis.get_tangent_u());
}

}   // namespace renderer
