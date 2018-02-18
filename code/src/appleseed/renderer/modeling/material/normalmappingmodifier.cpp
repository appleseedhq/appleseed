
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
#include "normalmappingmodifier.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/vector.h"

using namespace foundation;

namespace renderer
{

NormalMappingModifier::NormalMappingModifier(
    const Source*           map,
    const UpVector          up_vector)
  : m_map(map)
  , m_y(up_vector == UpVectorY ? 1 : 2)
{
}

Basis3d NormalMappingModifier::modify(
    TextureCache&           texture_cache,
    const Basis3d&          basis,
    const ShadingPoint&     shading_point) const
{
    const size_t UVSet = 0;
    const Vector2f& uv = shading_point.get_uv(UVSet);

    // Lookup the normal map.
    Color3f normal_rgb;
    m_map->evaluate(texture_cache, SourceInputs(uv), normal_rgb);

    // Reconstruct the normal from the texel value.
    const double x = static_cast<double>(normal_rgb[0]);
    const double y = static_cast<double>(normal_rgb[m_y]);
    const double z = static_cast<double>(normal_rgb[3 - m_y]);
    const Vector3d normal(
        x * 2.0 - 1.0,
        y * 2.0 - 1.0,
        z * 2.0 - 1.0);

    // Construct an orthonormal basis around the new normal.
    return Basis3d(
        normalize(basis.transform_to_parent(normal)),
        basis.get_tangent_u());
}

}   // namespace renderer
