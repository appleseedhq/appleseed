
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
using namespace std;

namespace renderer
{

namespace
{
    size_t get_triangle_count(Object& object)
    {
        size_t triangle_count = 0;

        Access<RegionKit> region_kit(&object.get_region_kit());

        for (const_each<RegionKit> i = *region_kit; i; ++i)
        {
            const IRegion* region = *i;
            Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

            triangle_count += tess->m_primitives.size();
        }

        return triangle_count;
    }

    void copy_uv_coordinates(const StaticTriangleTess& tess, vector<Vector2f>& uv)
    {
        for (const_each<StaticTriangleTess::PrimitiveArray> i = tess.m_primitives; i; ++i)
        {
            if (i->has_vertex_attributes() && tess.get_tex_coords_count() > 0)
            {
                const Vector2f uv0(tess.get_tex_coords(i->m_a0));
                const Vector2f uv1(tess.get_tex_coords(i->m_a1));
                const Vector2f uv2(tess.get_tex_coords(i->m_a2));

                uv.push_back(Vector2f(uv0[0], 1.0f - uv0[1]));
                uv.push_back(Vector2f(uv1[0], 1.0f - uv1[1]));
                uv.push_back(Vector2f(uv2[0], 1.0f - uv2[1]));
            }
            else
            {
                uv.push_back(Vector2f(0.0f));
                uv.push_back(Vector2f(0.0f));
                uv.push_back(Vector2f(0.0f));
            }
        }
    }

    void copy_uv_coordinates(Object& object, vector<Vector2f>& uv)
    {
        Access<RegionKit> region_kit(&object.get_region_kit());

        for (const_each<RegionKit> i = *region_kit; i; ++i)
        {
            const IRegion* region = *i;
            Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

            copy_uv_coordinates(*tess, uv);
        }
    }
}

IntersectionFilter::IntersectionFilter(
    Object&                 object,
    const MaterialArray&    materials,
    TextureCache&           texture_cache)
{
    // Initialize the material -> alpha mask mapping.
    m_alpha_map_signatures.assign(materials.size(), 0);
    m_alpha_masks.assign(materials.size(), 0);

    // Create alpha masks.
    update(materials, texture_cache);

    if (has_alpha_masks())
    {
        // Make a local copy of the object's UV coordinates.
        m_uv.reserve(get_triangle_count(object) * 3);
        copy_uv_coordinates(object, m_uv);
    }
}

IntersectionFilter::~IntersectionFilter()
{
    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
        delete m_alpha_masks[i];
}

namespace
{
    template <typename T>
    void delete_and_clear(T*& ptr)
    {
        delete ptr;
        ptr = 0;
    }
}

void IntersectionFilter::update(
    const MaterialArray&    materials,
    TextureCache&           texture_cache)
{
    assert(m_alpha_map_signatures.size() == materials.size());
    assert(m_alpha_masks.size() == materials.size());

    for (size_t i = 0; i < materials.size(); ++i)
    {
        // Retrieve the material.
        const Material* material = materials[i];
        if (material == 0)
        {
            delete_and_clear(m_alpha_masks[i]);
            continue;
        }

        // Intersection filters would prevent shading fully transparent shading points,
        // so don't create one if shading fully transparent shading points is enabled.
        if (material->shade_alpha_cutouts())
        {
            delete_and_clear(m_alpha_masks[i]);
            continue;
        }

        // Use the uncached version of get_alpha_map() since at this point
        // on_frame_begin() hasn't been called on the materials, when
        // intersection filters are updated on existing triangle trees
        // prior to rendering.
        const Source* alpha_map = material->get_uncached_alpha_map();
        if (alpha_map == 0)
        {
            delete_and_clear(m_alpha_masks[i]);
            continue;
        }

        // Don't do anything if there is already an alpha mask and it is up-to-date.
        const uint64 alpha_map_sig = alpha_map->compute_signature();
        if (m_alpha_masks[i] != 0 && alpha_map_sig == m_alpha_map_signatures[i])
            continue;

        // Build the alpha mask.
        double transparency;
        auto_ptr<AlphaMask> alpha_mask(
            create_alpha_mask(
                alpha_map,
                texture_cache,
                transparency));

        // Discard the alpha mask if it's mostly opaque.
        if (transparency < 5.0 / 100)
        {
            delete_and_clear(m_alpha_masks[i]);
            continue;
        }

        // Store the alpha mask.
        delete m_alpha_masks[i];
        m_alpha_masks[i] = alpha_mask.release();
        m_alpha_map_signatures[i] = alpha_map_sig;
    }
}

bool IntersectionFilter::has_alpha_masks() const
{
    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
    {
        if (m_alpha_masks[i])
            return true;
    }

    return false;
}

size_t IntersectionFilter::get_masks_memory_size() const
{
    size_t size = 0;

    for (size_t i = 0; i < m_alpha_masks.size(); ++i)
    {
        if (m_alpha_masks[i])
            size += m_alpha_masks[i]->get_memory_size();
    }

    return size;
}

size_t IntersectionFilter::get_uv_memory_size() const
{
    return m_uv.capacity() * sizeof(Vector2f);
}

IntersectionFilter::AlphaMask* IntersectionFilter::create_alpha_mask(
    const Source*           alpha_map,
    TextureCache&           texture_cache,
    double&                 transparency)
{
    assert(alpha_map);

    // Compute the dimensions of the alpha mask.
    size_t width, height;
    if (dynamic_cast<const TextureSource*>(alpha_map))
    {
        const CanvasProperties& texture_props =
            static_cast<const TextureSource*>(alpha_map)->get_texture_instance().get_texture().properties();
        width = texture_props.m_canvas_width;
        height = texture_props.m_canvas_height;
    }
    else
    {
        width = 1;
        height = 1;
    }

    // Create and initialize the alpha mask.
    AlphaMask* alpha_mask = new AlphaMask(width, height);

    const double rcp_width = 1.0 / width;
    const double rcp_height = 1.0 / height;
    size_t transparent_texel_count = 0;

    // Compute the alpha mask.
    for (size_t y = 0; y < height; ++y)
    {
        for (size_t x = 0; x < width; ++x)
        {
            // Evaluate the alpha map at the center of the texel.
            const Vector2d uv(
                (x + 0.5) * rcp_width,
                1.0 - (y + 0.5) * rcp_height);
            Alpha alpha;
            alpha_map->evaluate(texture_cache, uv, alpha);

            // Mark this texel as opaque or transparent in the alpha mask.
            const bool opaque = alpha[0] > 0.0f;
            alpha_mask->set_opaque(x, y, opaque);

            // Keep track of the number of transparent texels.
            transparent_texel_count += opaque ? 0 : 1;
        }
    }

    // Compute the ratio of transparent texels to the total number of texels.
    transparency = static_cast<double>(transparent_texel_count) / (width * height);

    return alpha_mask;
}

}   // namespace renderer
