
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"

using namespace foundation;

namespace renderer
{

IntersectionFilter::IntersectionFilter(
    const ObjectInstance&   object_instance,
    TextureCache&           texture_cache)
{
    // Create one alpha mask per material in the object instance.
    const MaterialArray& materials = object_instance.get_front_materials();
    m_alpha_masks.resize(materials.size(), 0);
    for (size_t i = 0; i < materials.size(); ++i)
    {
        const Material* material = materials[i];
        if (material == 0)
            continue;

        // Use the uncached version of get_alpha_map() since at this point
        // entity binding hasn't been performed yet when intersection filters
        // are updated on existing triangle trees.
        const Source* alpha_map = material->get_uncached_alpha_map();
        if (alpha_map == 0)
            continue;

        m_alpha_masks[i] = create_alpha_mask(alpha_map, texture_cache);
    }

    // Make a local copy of the object's UV coordinates.
    copy_uv_coordinates(object_instance);
}

IntersectionFilter::~IntersectionFilter()
{
    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
        delete m_alpha_masks[i];
}

bool IntersectionFilter::keep() const
{
    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
    {
        if (m_alpha_masks[i])
        {
            // If at least one alpha mask has some transparency, keep the intersection filter.
            if (m_alpha_masks[i]->m_transparency >= 5.0 / 100)
                return true;
        }
    }

    return false;
}

size_t IntersectionFilter::get_memory_size() const
{
    size_t size = sizeof(*this);

    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
    {
        if (m_alpha_masks[i])
        {
            size += sizeof(*m_alpha_masks[i]);
            size += m_alpha_masks[i]->m_bits.capacity() * sizeof(uint8);
        }
    }

    size += m_uv.capacity() * sizeof(Vector2f);

    return size;
}

IntersectionFilter::Bitmap* IntersectionFilter::create_alpha_mask(
    const Source*           alpha_map,
    TextureCache&           texture_cache)
{
    Bitmap* alpha_mask = new Bitmap();

    // Compute the dimensions of the alpha mask.
    if (dynamic_cast<const TextureSource*>(alpha_map))
    {
        const CanvasProperties& texture_props =
            static_cast<const TextureSource*>(alpha_map)->get_texture_instance().get_texture().properties();
        alpha_mask->m_width = texture_props.m_canvas_width;
        alpha_mask->m_height = texture_props.m_canvas_height;
    }
    else
    {
        alpha_mask->m_width = 1;
        alpha_mask->m_height = 1;
    }

    // Precompute some values used during lookup.
    alpha_mask->m_max_x = static_cast<float>(alpha_mask->m_width) - 1.0f;
    alpha_mask->m_max_y = static_cast<float>(alpha_mask->m_height) - 1.0f;

    const double rcp_width = 1.0 / alpha_mask->m_width;
    const double rcp_height = 1.0 / alpha_mask->m_height;

    const size_t texel_count = alpha_mask->m_width * alpha_mask->m_height;
    size_t transparent_texel_count = 0;

    // Allocate the alpha mask.
    alpha_mask->m_bits.resize(texel_count);

    // Compute the alpha mask.
    for (size_t y = 0; y < alpha_mask->m_height; ++y)
    {
        for (size_t x = 0; x < alpha_mask->m_width; ++x)
        {
            // Evaluate the alpha map at the center of the texel.
            const Vector2d uv(
                (x + 0.5) * rcp_width,
                1.0 - (y + 0.5) * rcp_height);
            Alpha alpha;
            alpha_map->evaluate(texture_cache, uv, alpha);

            // Mark this texel as opaque or transparent in the alpha mask.
            const uint8 opaque = alpha[0] > 0.0f ? ~0 : 0;
            alpha_mask->m_bits[y * alpha_mask->m_width + x] = opaque;

            // Keep track of the number of opaque texels.
            transparent_texel_count += (~opaque) & 1;
        }
    }

    // Compute the ratio of transparent texels to the total number of texels.
    alpha_mask->m_transparency = static_cast<double>(transparent_texel_count) / texel_count;

    return alpha_mask;
}

void IntersectionFilter::copy_uv_coordinates(const ObjectInstance& object_instance)
{
    // Retrieve the object.
    Object& object = object_instance.get_object();

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

                m_uv.push_back(Vector2f(uv0[0], 1.0f - uv0[1]));
                m_uv.push_back(Vector2f(uv1[0], 1.0f - uv1[1]));
                m_uv.push_back(Vector2f(uv2[0], 1.0f - uv2[1]));
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
