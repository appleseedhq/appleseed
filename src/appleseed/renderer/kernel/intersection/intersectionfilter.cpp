
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "intersectionfilter.h"

// appleseed.renderer headers.
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

IntersectionFilter::IntersectionFilter(
    const Scene&            scene,
    const Assembly&         assembly,
    const size_t            object_instance_index,
    TextureCache&           texture_cache)
{
    // Retrieve the object instance.
    const ObjectInstance* object_instance =
        assembly.object_instances().get_by_index(object_instance_index);
    assert(object_instance);

    copy_alpha_mask(scene, assembly, object_instance, texture_cache);
    copy_uv_coordinates(assembly, object_instance);
}

double IntersectionFilter::get_transparent_pixel_ratio() const
{
    return m_transparent_texel_ratio;
}

void IntersectionFilter::copy_alpha_mask(
    const Scene&            scene,
    const Assembly&         assembly,
    const ObjectInstance*   object_instance,
    TextureCache&           texture_cache)
{
    // Retrieve the first front material of this object instance.
    assert(!object_instance->get_front_materials().empty());
    const Material* material = object_instance->get_front_materials()[0];

    // Retrieve the alpha map of this material.
    const Source* alpha_map = material->get_alpha_map();
    assert(alpha_map);

    // Compute the dimensions of the alpha mask.
    if (dynamic_cast<const TextureSource*>(alpha_map))
    {
        const CanvasProperties& texture_props =
            static_cast<const TextureSource*>(alpha_map)->get_texture_instance().get_texture()->properties();
        m_alpha_mask_width = texture_props.m_canvas_width;
        m_alpha_mask_height = texture_props.m_canvas_height;
    }
    else
    {
        m_alpha_mask_width = 1;
        m_alpha_mask_height = 1;
    }

    const double rcp_alpha_mask_width = 1.0 / m_alpha_mask_width;
    const double rcp_alpha_mask_height = 1.0 / m_alpha_mask_height;
    const size_t texel_count = m_alpha_mask_width * m_alpha_mask_height;
    size_t transparent_texel_count = 0;

    // Allocate the alpha mask.
    m_alpha_mask.resize(texel_count);

    // Compute the alpha mask.
    for (size_t y = 0; y < m_alpha_mask_height; ++y)
    {
        for (size_t x = 0; x < m_alpha_mask_width; ++x)
        {
            // Evaluate the alpha map at the center of the texel.
            const Vector2d uv(
                (x + 0.5) * rcp_alpha_mask_width,
                1.0 - (y + 0.5) * rcp_alpha_mask_height);
            Alpha alpha;
            alpha_map->evaluate(texture_cache, uv, alpha);

            // Mark this texel as opaque or transparent in the alpha mask.
            const uint8 opaque = alpha[0] > 0.0f ? ~0 : 0;
            m_alpha_mask[y * m_alpha_mask_width + x] = opaque;

            // Keep track of the number of opaque texels.
            transparent_texel_count += (~opaque) & 1;
        }
    }

    // Compute the ratio of transparent texels to the total number of texels.
    m_transparent_texel_ratio = static_cast<double>(transparent_texel_count) / texel_count;

    // Precompute some values used during lookup.
    m_max_x = static_cast<float>(m_alpha_mask_width) - 1.0f;
    m_max_y = static_cast<float>(m_alpha_mask_height) - 1.0f;
}

void IntersectionFilter::copy_uv_coordinates(
    const Assembly&         assembly,
    const ObjectInstance*   object_instance)
{
    // Retrieve the object.
    Object& object = object_instance->get_object();

    // Retrieve the region kit of the object.
    Access<RegionKit> region_kit(&object.get_region_kit());

    // Iterate over the regions of this object.
    for (const_each<RegionKit> region_it = *region_kit; region_it; ++region_it)
    {
        // Retrieve the region.
        const IRegion* region = *region_it;

        // Retrieve the tessellation of the region.
        Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

        // Iterate over the triangles of this region.
        for (const_each<StaticTriangleTess::PrimitiveArray> tri_it = tess->m_primitives; tri_it; ++tri_it)
        {
            // Fetch the triangle.
            const Triangle& triangle = *tri_it;

            // Copy and rescale UV coordinates.
            if (triangle.has_vertex_attributes() && tess->get_uv_vertex_count() > 0)
            {
                const Vector2f uv0(tess->get_uv_vertex(triangle.m_a0));
                const Vector2f uv1(tess->get_uv_vertex(triangle.m_a1));
                const Vector2f uv2(tess->get_uv_vertex(triangle.m_a2));

                m_uv.push_back(Vector2f(uv0[0] * m_alpha_mask_width, (1.0f - uv0[1]) * m_alpha_mask_height));
                m_uv.push_back(Vector2f(uv1[0] * m_alpha_mask_width, (1.0f - uv1[1]) * m_alpha_mask_height));
                m_uv.push_back(Vector2f(uv2[0] * m_alpha_mask_width, (1.0f - uv2[1]) * m_alpha_mask_height));
            }
            else
            {
                m_uv.push_back(Vector2f(0.0f));
                m_uv.push_back(Vector2f(0.0f));
                m_uv.push_back(Vector2f(0.0f));
            }
        }
    }
}

}   // namespace renderer
