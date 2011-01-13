
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "shadingpoint.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/utility/attributeset.h"
#include "foundation/utility/casts.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShadingPoint class implementation.
//

void ShadingPoint::fetch_source_geometry() const
{
    assert(m_region_kit_cache);
    assert(m_tess_cache);
    assert(m_scene);
    assert(hit());
    assert(!(m_members & HasSourceGeometry));

    // Retrieve the assembly instance.
    m_assembly_instance = m_scene->assembly_instances().get(m_asm_instance_uid);
    assert(m_assembly_instance);

    // Retrieve the assembly.
    m_assembly = &m_assembly_instance->get_assembly();

    // Retrieve the object instance.
    m_object_instance = m_assembly->object_instances().get(m_object_instance_index);
    assert(m_object_instance);

    // Retrieve the object.
    m_object = m_assembly->objects().get(m_object_instance->get_object_index());
    assert(m_object);

    // Retrieve the region kit of the object.
    const RegionKit& region_kit =
        *m_region_kit_cache->access(
            m_object->get_uid(), m_object->get_region_kit());

    // Retrieve the region.
    const IRegion* region = region_kit[m_region_index];

    // Retrieve the tessellation of the region.
    const StaticTriangleTess& tess =
        *m_tess_cache->access(
            region->get_uid(), region->get_static_triangle_tess());

    // Retrieve the triangle.
    const Triangle& triangle = tess.m_primitives[m_triangle_index];

    // Copy the index of the triangle attribute.
    m_triangle_pa = triangle.m_pa;

    // Copy the texture coordinates from UV set #0.
    AttributeSet::ChannelID uv0_channel_id;
    if (triangle.m_a0 != Triangle::None &&
        triangle.m_a1 != Triangle::None &&
        triangle.m_a2 != Triangle::None &&
        (uv0_channel_id = tess.m_vertex_attributes.find_channel("uv0")) != AttributeSet::InvalidChannelID)
    {
        tess.m_vertex_attributes.get_attribute(
            uv0_channel_id,
            triangle.m_a0,
            &m_v0_uv);
        tess.m_vertex_attributes.get_attribute(
            uv0_channel_id,
            triangle.m_a1,
            &m_v1_uv);
        tess.m_vertex_attributes.get_attribute(
            uv0_channel_id,
            triangle.m_a2,
            &m_v2_uv);
    }
    else
    {
        // UV set #0 doesn't exist, or this triangle doesn't have vertex attributes.
        m_v0_uv =
        m_v1_uv =
        m_v2_uv = GVector2(0.0);
    }

    // Copy the object instance space triangle vertices.
    assert(triangle.m_v0 != Triangle::None);
    assert(triangle.m_v1 != Triangle::None);
    assert(triangle.m_v2 != Triangle::None);
    m_v0 = tess.m_vertices[triangle.m_v0];
    m_v1 = tess.m_vertices[triangle.m_v1];
    m_v2 = tess.m_vertices[triangle.m_v2];

    // Copy the object instance space triangle vertex normals.
    assert(triangle.m_n0 != Triangle::None);
    assert(triangle.m_n1 != Triangle::None);
    assert(triangle.m_n2 != Triangle::None);
    m_n0 = tess.m_vertex_normals[triangle.m_n0];
    m_n1 = tess.m_vertex_normals[triangle.m_n1];
    m_n2 = tess.m_vertex_normals[triangle.m_n2];
}

namespace
{
    // Robustly offset a given point along a given vector.
    inline Vector3d offset(const Vector3d& p, Vector3d n)
    {
        const double Threshold = 1.0e-25;
        const int EpsLut[2] = { 8, -8 };

        // Check whether any of the components of p is close to zero, in absolute value.
        const bool small_x = std::abs(p.x) < Threshold;
        const bool small_y = std::abs(p.y) < Threshold;
        const bool small_z = std::abs(p.z) < Threshold;

        // If it is the case, we need n to be normalized.
        if (small_x || small_y || small_z)
            n = normalize(n);

        Vector3d res;

        // Offset X component.
        if (small_x)
            res.x = p.x + n.x * Threshold;
        else
        {
            uint64 ix = binary_cast<uint64>(p.x);
            ix += EpsLut[(ix ^ binary_cast<uint64>(n.x)) >> 63];
            res.x = binary_cast<double>(ix);
        }

        // Offset Y component.
        if (small_y)
            res.y = p.y + n.y * Threshold;
        else
        {
            uint64 iy = binary_cast<uint64>(p.y);
            iy += EpsLut[(iy ^ binary_cast<uint64>(n.y)) >> 63];
            res.y = binary_cast<double>(iy);
        }

        // Offset Z component.
        if (small_z)
            res.z = p.z + n.z * Threshold;
        else
        {
            uint64 iz = binary_cast<uint64>(p.z);
            iz += EpsLut[(iz ^ binary_cast<uint64>(n.z)) >> 63];
            res.z = binary_cast<double>(iz);
        }

        return res;
    }
}

void ShadingPoint::refine_and_offset() const
{
    assert(hit());
    assert(!(m_members & ShadingPoint::HasRefinedPoints));

    // Cache the source geometry.
    cache_source_geometry();

    // Transform the ray to assembly instance space.
    ShadingRay::RayType local_ray =
        m_assembly_instance->get_transform().transform_to_local(m_ray);

    // Compute the refined intersection point.
    local_ray.m_org += local_ray.m_tmax * local_ray.m_dir;
    const size_t RefinementSteps = 2;
    for (size_t i = 0; i < RefinementSteps; ++i)
    {
        const double t = m_triangle_support_plane.intersect(local_ray);
        local_ray.m_org += t * local_ray.m_dir;
    }

    // Retrieve the object instance space to assembly instance space transform.
    const Transformd& transform = m_object_instance->get_transform();

    // Compute the assembly instance space geometric normal to the hit triangle.
    m_asm_geo_normal = Vector3d(cross(m_v1 - m_v0, m_v2 - m_v0));
    m_asm_geo_normal = transform.transform_normal_to_parent(m_asm_geo_normal);
    m_asm_geo_normal = faceforward(m_asm_geo_normal, local_ray.m_dir);

    // Compute the offset points.
    m_front_point = offset(local_ray.m_org, m_asm_geo_normal);
    m_back_point = offset(local_ray.m_org, -m_asm_geo_normal);

    // The refined intersection points are now available.
    m_members |= ShadingPoint::HasRefinedPoints;
}

}   // namespace renderer
