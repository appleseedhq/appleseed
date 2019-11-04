
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <memory>

using namespace foundation;

namespace renderer
{

namespace
{
    size_t get_triangle_count(Object& object)
    {
        const MeshObject& mesh = static_cast<const MeshObject&>(object);
        const StaticTriangleTess& tess = mesh.get_static_triangle_tess();
        return tess.m_primitives.size();
    }

    void copy_uv_coordinates(const StaticTriangleTess& tess, std::vector<Vector2f>& uv)
    {
        for (const_each<StaticTriangleTess::PrimitiveArray> i = tess.m_primitives; i; ++i)
        {
            if (i->has_vertex_attributes() && tess.get_tex_coords_count() > 0)
            {
                const Vector2f uv0(tess.get_tex_coords(i->m_a0));
                const Vector2f uv1(tess.get_tex_coords(i->m_a1));
                const Vector2f uv2(tess.get_tex_coords(i->m_a2));

                uv.emplace_back(uv0[0], 1.0f - uv0[1]);
                uv.emplace_back(uv1[0], 1.0f - uv1[1]);
                uv.emplace_back(uv2[0], 1.0f - uv2[1]);
            }
            else
            {
                uv.emplace_back(0.0f);
                uv.emplace_back(0.0f);
                uv.emplace_back(0.0f);
            }
        }
    }

    void copy_uv_coordinates(Object& object, std::vector<Vector2f>& uv)
    {
        const MeshObject& mesh = static_cast<const MeshObject&>(object);
        const StaticTriangleTess& tess = mesh.get_static_triangle_tess();
        copy_uv_coordinates(tess, uv);
    }
}

IntersectionFilter::IntersectionFilter(
    Object&                 object,
    const MaterialArray&    materials,
    TextureCache&           texture_cache)
  : m_obj_alpha_mask(nullptr)
  , m_obj_alpha_map_signature(0)
{
    // Initialize the material -> alpha mask mapping.
    m_material_alpha_map_signatures.assign(materials.size(), 0);
    m_material_alpha_masks.assign(materials.size(), nullptr);

    // Create alpha masks.
    update(object, materials, texture_cache);

    if (has_alpha_masks())
    {
        // Make a local copy of the object's UV coordinates.
        m_uv.reserve(get_triangle_count(object) * 3);
        copy_uv_coordinates(object, m_uv);
    }
}

IntersectionFilter::~IntersectionFilter()
{
    delete m_obj_alpha_mask;

    for (size_t i = 0; i < m_material_alpha_masks.size(); ++i)
        delete m_material_alpha_masks[i];
}

namespace
{
    template <typename T>
    void delete_and_clear(T*& ptr)
    {
        delete ptr;
        ptr = nullptr;
    }
}

template <typename EntityType>
void IntersectionFilter::do_update(
    const EntityType&               entity,
    TextureCache&                   texture_cache,
    IntersectionFilter::AlphaMask*& mask,
    std::uint64_t&                  signature)
{
    // Use the uncached version of get_alpha_map() since at this point
    // on_frame_begin() hasn't been called on the materials, when
    // intersection filters are updated on existing triangle trees
    // prior to rendering.
    const Source* alpha_map = entity.get_uncached_alpha_map();

    if (alpha_map == nullptr)
    {
        delete_and_clear(mask);
        return;
    }

    // Don't do anything if there is already an alpha mask and it is up-to-date.
    const std::uint64_t alpha_map_sig = alpha_map->compute_signature();
    if (mask != nullptr && alpha_map_sig == signature)
        return;

    // Build the alpha mask.
    double transparency;
    std::unique_ptr<AlphaMask> alpha_mask(
        create_alpha_mask(
            alpha_map,
            texture_cache,
            transparency));

    // Discard the alpha mask if it's mostly opaque.
    if (transparency < 5.0 / 100)
    {
        delete_and_clear(mask);
        return;
    }

    // Store the alpha mask.
    delete mask;
    mask = alpha_mask.release();
    signature = alpha_map_sig;
}

void IntersectionFilter::update(
    const Object&           object,
    const MaterialArray&    materials,
    TextureCache&           texture_cache)
{
    assert(m_material_alpha_map_signatures.size() == materials.size());
    assert(m_material_alpha_masks.size() == materials.size());

    do_update(object, texture_cache, m_obj_alpha_mask, m_obj_alpha_map_signature);

    for (size_t i = 0; i < materials.size(); ++i)
    {
        if (const Material* material = materials[i])
        {
            do_update(
                *material,
                texture_cache,
                m_material_alpha_masks[i],
                m_material_alpha_map_signatures[i]);
        }
        else
            delete_and_clear(m_material_alpha_masks[i]);
    }
}

bool IntersectionFilter::has_alpha_masks() const
{
    if (m_obj_alpha_mask)
        return true;

    for (size_t i = 0; i < m_material_alpha_masks.size(); ++i)
    {
        if (m_material_alpha_masks[i])
            return true;
    }

    return false;
}

size_t IntersectionFilter::get_masks_memory_size() const
{
    size_t size = 0;

    if (m_obj_alpha_mask)
        size += m_obj_alpha_mask->get_memory_size();

    for (size_t i = 0; i < m_material_alpha_masks.size(); ++i)
    {
        if (m_material_alpha_masks[i])
            size += m_material_alpha_masks[i]->get_memory_size();
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

    // Create and initialize the alpha mask.
    const Source::Hints hints = alpha_map->get_hints();
    AlphaMask* alpha_mask = new AlphaMask(hints.m_width, hints.m_height);

    const float rcp_width = 1.0f / hints.m_width;
    const float rcp_height = 1.0f / hints.m_height;
    size_t transparent_texel_count = 0;

    // Compute the alpha mask.
    for (size_t y = 0; y < hints.m_height; ++y)
    {
        for (size_t x = 0; x < hints.m_width; ++x)
        {
            // Evaluate the alpha map at the center of the texel.
            const Vector2f uv(
                (x + 0.5f) * rcp_width,
                1.0f - (y + 0.5f) * rcp_height);
            Alpha alpha;
            alpha_map->evaluate(texture_cache, SourceInputs(uv), alpha);

            // Mark this texel as opaque or transparent in the alpha mask.
            const bool opaque = alpha[0] > 0.0f;
            alpha_mask->set_opaque(x, y, opaque);

            // Keep track of the number of transparent texels.
            transparent_texel_count += opaque ? 0 : 1;
        }
    }

    // Compute the ratio of transparent texels to the total number of texels.
    transparency = static_cast<double>(transparent_texel_count) / (hints.m_width * hints.m_height);

    return alpha_mask;
}

}   // namespace renderer
