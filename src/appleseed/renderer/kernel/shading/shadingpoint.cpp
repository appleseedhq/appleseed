
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
#include "shadingpoint.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/utility/attributeset.h"

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
    m_assembly_instance = m_scene->assembly_instances().get_by_uid(m_asm_instance_uid);
    assert(m_assembly_instance);

    // Retrieve the assembly.
    m_assembly = &m_assembly_instance->get_assembly();

    // Retrieve the object instance.
    m_object_instance = m_assembly->object_instances().get_by_index(m_object_instance_index);
    assert(m_object_instance);

    // Retrieve the object.
    m_object = &m_object_instance->get_object();

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
    const size_t motion_segment_count = tess.get_motion_segment_count();

    // Retrieve the triangle.
    const Triangle& triangle = tess.m_primitives[m_triangle_index];

    // Copy the index of the triangle attribute.
    m_triangle_pa = triangle.m_pa;

    // Copy the texture coordinates from UV set #0.
    if (triangle.has_vertex_attributes() && tess.get_uv_vertex_count() > 0)
    {
        m_v0_uv = tess.get_uv_vertex(triangle.m_a0);
        m_v1_uv = tess.get_uv_vertex(triangle.m_a1);
        m_v2_uv = tess.get_uv_vertex(triangle.m_a2);
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

    if (motion_segment_count > 0)
    {
        // Fetch triangle vertices from the previous pose.
        const size_t prev_index = truncate<size_t>(m_ray.m_time * motion_segment_count);
        GVector3 prev_v0, prev_v1, prev_v2;
        if (prev_index == 0)
        {
            prev_v0 = tess.m_vertices[triangle.m_v0];
            prev_v1 = tess.m_vertices[triangle.m_v1];
            prev_v2 = tess.m_vertices[triangle.m_v2];
        }
        else
        {
            prev_v0 = tess.get_vertex_pose(triangle.m_v0, prev_index - 1);
            prev_v1 = tess.get_vertex_pose(triangle.m_v1, prev_index - 1);
            prev_v2 = tess.get_vertex_pose(triangle.m_v2, prev_index - 1);
        }

        // Fetch triangle vertices from the next pose.
        const GVector3 next_v0 = tess.get_vertex_pose(triangle.m_v0, prev_index);
        const GVector3 next_v1 = tess.get_vertex_pose(triangle.m_v1, prev_index);
        const GVector3 next_v2 = tess.get_vertex_pose(triangle.m_v2, prev_index);

        // Interpolate triangle vertices.
        const GScalar k = static_cast<GScalar>(m_ray.m_time * motion_segment_count - prev_index);
        m_v0 = (GScalar(1.0) - k) * prev_v0 + k * next_v0;
        m_v1 = (GScalar(1.0) - k) * prev_v1 + k * next_v1;
        m_v2 = (GScalar(1.0) - k) * prev_v2 + k * next_v2;
    }
    else
    {
        m_v0 = tess.m_vertices[triangle.m_v0];
        m_v1 = tess.m_vertices[triangle.m_v1];
        m_v2 = tess.m_vertices[triangle.m_v2];
    }

    // Copy the object instance space triangle vertex normals.
    assert(triangle.m_n0 != Triangle::None);
    assert(triangle.m_n1 != Triangle::None);
    assert(triangle.m_n2 != Triangle::None);
    m_n0 = tess.m_vertex_normals[triangle.m_n0];
    m_n1 = tess.m_vertex_normals[triangle.m_n1];
    m_n2 = tess.m_vertex_normals[triangle.m_n2];
}

void ShadingPoint::refine_and_offset() const
{
    assert(hit());
    assert(!(m_members & ShadingPoint::HasRefinedPoints));

    // Cache the source geometry.
    cache_source_geometry();

    // Compute the location of the intersection point in assembly instance space.
    ShadingRay::RayType local_ray =
        m_assembly_instance->get_transform().transform_to_local(m_ray);
    local_ray.m_org += local_ray.m_tmax * local_ray.m_dir;

    // Refine the location of the intersection point.
    local_ray.m_org =
        Intersector::refine(
            m_triangle_support_plane,
            local_ray.m_org,
            local_ray.m_dir);

    // Compute the geometric normal to the hit triangle in assembly instance space.
    // Note that it doesn't need to be normalized at this point.
    m_asm_geo_normal = Vector3d(cross(m_v1 - m_v0, m_v2 - m_v0));
    m_asm_geo_normal = m_object_instance->get_transform().transform_normal_to_parent(m_asm_geo_normal);
    m_asm_geo_normal = faceforward(m_asm_geo_normal, local_ray.m_dir);

    // Compute the offset points in assembly instance space.
#ifdef RENDERER_ADAPTIVE_OFFSET
    Intersector::adaptive_offset(
        m_triangle_support_plane,
        local_ray.m_org,
        m_asm_geo_normal,
        m_front_point,
        m_back_point);
#else
    Intersector::offset(
        local_ray.m_org,
        m_asm_geo_normal,
        m_front_point,
        m_back_point);
#endif

    // The refined intersection points are now available.
    m_members |= ShadingPoint::HasRefinedPoints;
}

}   // namespace renderer
