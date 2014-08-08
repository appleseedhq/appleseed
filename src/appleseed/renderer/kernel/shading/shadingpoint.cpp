
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
#include "shadingpoint.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/utility/attributeset.h"
#include "foundation/utility/otherwise.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShadingPoint class implementation.
//

void ShadingPoint::fetch_source_geometry() const
{
    assert(hit());
    assert(!(m_members & HasSourceGeometry));

    // Retrieve the assembly.
    m_assembly = &m_assembly_instance->get_assembly();

    // Retrieve the object instance.
    m_object_instance = m_assembly->object_instances().get_by_index(m_object_instance_index);
    assert(m_object_instance);

    // Retrieve the object.
    m_object = &m_object_instance->get_object();

    // Fetch primitive-specific geometry.
    if (m_primitive_type == PrimitiveTriangle)
        fetch_triangle_source_geometry();
    else
    {
        assert(m_primitive_type == PrimitiveCurve);
        fetch_curve_source_geometry();
    }
}

void ShadingPoint::fetch_triangle_source_geometry() const
{
    // Retrieve the region kit of the object.
    assert(m_region_kit_cache);
    const RegionKit& region_kit =
        *m_region_kit_cache->access(
            m_object->get_uid(), m_object->get_region_kit());

    // Retrieve the region.
    const IRegion* region = region_kit[m_region_index];

    // Retrieve the tessellation of the region.
    assert(m_tess_cache);
    const StaticTriangleTess& tess =
        *m_tess_cache->access(
            region->get_uid(), region->get_static_triangle_tess());
    const size_t motion_segment_count = tess.get_motion_segment_count();

    // Retrieve the triangle.
    const Triangle& triangle = tess.m_primitives[m_primitive_index];

    // Copy the index of the triangle attribute.
    m_primitive_pa = triangle.m_pa;

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
    assert(is_normalized(m_n0));
    assert(is_normalized(m_n1));
    assert(is_normalized(m_n2));
}

void ShadingPoint::fetch_curve_source_geometry() const
{
    // Set primitive attribute to default value of 0.
    // todo: fix.
    m_primitive_pa = 0;
}

void ShadingPoint::refine_and_offset() const
{
    assert(hit());
    assert(!(m_members & ShadingPoint::HasRefinedPoints));

    // Cache the source geometry.
    cache_source_geometry();

    // Compute the location of the intersection point in assembly instance space.
    ShadingRay::RayType local_ray = m_assembly_instance_transform.to_local(m_ray);
    local_ray.m_org += local_ray.m_tmax * local_ray.m_dir;

    if (m_primitive_type == PrimitiveTriangle)
    {
        // Refine the location of the intersection point.
        local_ray.m_org =
            Intersector::refine(
                m_triangle_support_plane,
                local_ray.m_org,
                local_ray.m_dir);

        // Compute the geometric normal to the hit triangle in assembly instance space.
        // Note that it doesn't need to be normalized at this point.
        m_asm_geo_normal = Vector3d(cross(m_v1 - m_v0, m_v2 - m_v0));
        m_asm_geo_normal = m_object_instance->get_transform().normal_to_parent(m_asm_geo_normal);
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
    }
    else
    {
        assert(m_primitive_type == PrimitiveCurve);

        m_asm_geo_normal = normalize(-local_ray.m_dir);

        const double Eps = 1.0e-6;
        m_front_point = local_ray.m_org + Eps * m_asm_geo_normal;
        m_back_point = local_ray.m_org - Eps * m_asm_geo_normal;
    }

    // The refined intersection points are now available.
    m_members |= ShadingPoint::HasRefinedPoints;
}

Vector3d ShadingPoint::get_biased_point(const Vector3d& direction) const
{
    assert(hit());

    if (!(m_members & HasBiasedPoint))
    {
        const Vector3d point = m_ray.point_at(m_ray.m_tmax);

        switch (m_object_instance->get_ray_bias_method())
        {
          case ObjectInstance::RayBiasMethodNone:
            {
                m_biased_point = point;
                m_members |= HasBiasedPoint;
                return m_biased_point;
            }

          case ObjectInstance::RayBiasMethodNormal:
            {
                const Vector3d& n = get_geometric_normal();
                const double bias = m_object_instance->get_ray_bias_distance();
                return dot(direction, n) > 0.0 ? point + bias * n : point - bias * n;
            }

          case ObjectInstance::RayBiasMethodIncomingDirection:
            {
                const double bias = m_object_instance->get_ray_bias_distance();
                m_biased_point = point + bias * normalize(m_ray.m_dir);
                m_members |= HasBiasedPoint;
                return m_biased_point;
            }

          case ObjectInstance::RayBiasMethodOutgoingDirection:
            {
                const double bias = m_object_instance->get_ray_bias_distance();
                return point + bias * normalize(direction);
            }

          assert_otherwise;
        }
    }

    return m_biased_point;
}

#ifdef WITH_OSL

bool ShadingPoint::OSLObjectTransformInfo::is_animated() const
{
    return !m_assembly_instance_transform->empty();
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_transform() const
{
    assert(!is_animated());

    const Transformd& assembly_xform = m_assembly_instance_transform->get_earliest_transform();
    const Transformd::MatrixType m(
        m_object_instance_transform->get_local_to_parent() * assembly_xform.get_local_to_parent());

    return Matrix4f(transpose(m));
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_transform(float t) const
{
    const Transformd assembly_xform = m_assembly_instance_transform->evaluate(t);
    const Transformd::MatrixType m(
        m_object_instance_transform->get_local_to_parent() * assembly_xform.get_local_to_parent());

    return Matrix4f(transpose(m));
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_inverse_transform() const
{
    assert(!is_animated());

    const Transformd& assembly_xform = m_assembly_instance_transform->get_earliest_transform();
    const Transformd::MatrixType m(
        m_object_instance_transform->get_parent_to_local() * assembly_xform.get_parent_to_local());

    return Matrix4f(transpose(m));
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_inverse_transform(float t) const
{
    const Transformd assembly_xform = m_assembly_instance_transform->evaluate(t);
    const Transformd::MatrixType m(
        m_object_instance_transform->get_parent_to_local() * assembly_xform.get_parent_to_local());

    return Matrix4f(transpose(m));
}

OSL::ShaderGlobals& ShadingPoint::get_osl_shader_globals() const
{
    assert(hit());

    if (!(m_members & HasOSLShaderGlobals))
    {
        const ShadingRay& ray(get_ray());

        m_shader_globals.P = Vector3f(get_point());
        m_shader_globals.dPdx = OSL::Vec3(0, 0, 0);
        m_shader_globals.dPdy = OSL::Vec3(0, 0, 0);
        m_shader_globals.dPdz = OSL::Vec3(0, 0, 0);

        m_shader_globals.I = Vector3f(normalize(ray.m_dir));
        m_shader_globals.dIdx = OSL::Vec3(0, 0, 0);
        m_shader_globals.dIdy = OSL::Vec3(0, 0, 0);

        m_shader_globals.N = Vector3f(get_shading_normal());
        m_shader_globals.Ng = Vector3f(get_geometric_normal());

        m_shader_globals.u = get_uv(0).x;
        m_shader_globals.dudx = 0;
        m_shader_globals.dudy = 0;

        m_shader_globals.v = get_uv(0).y;
        m_shader_globals.dvdx = 0;
        m_shader_globals.dvdy = 0;

        m_shader_globals.dPdu = Vector3f(get_dpdu(0));
        m_shader_globals.dPdv = Vector3f(get_dpdv(0));

        m_shader_globals.time = ray.m_time;
        m_shader_globals.dtime = 0;
        m_shader_globals.dPdtime = OSL::Vec3(0, 0, 0);

        m_shader_globals.Ps = OSL::Vec3(0, 0, 0);
        m_shader_globals.dPsdx = OSL::Vec3(0, 0, 0);
        m_shader_globals.dPsdy = OSL::Vec3(0, 0, 0);

        m_shader_globals.renderstate = 
            const_cast<void*>(reinterpret_cast<const void*>(this));

        memset(reinterpret_cast<void*>(&m_osl_trace_data), 0, sizeof(OSLTraceData));
        m_shader_globals.tracedata = reinterpret_cast<void*>(&m_osl_trace_data);                

        m_shader_globals.objdata = 0;

        m_obj_transform_info.m_assembly_instance_transform =
            &get_assembly_instance().cumulated_transform_sequence();
        m_obj_transform_info.m_object_instance_transform =
            &get_object_instance().get_transform();

        m_shader_globals.object2common = reinterpret_cast<OSL::TransformationPtr>(&m_obj_transform_info);

        m_shader_globals.shader2common = 0;
        m_shader_globals.surfacearea = 0;

        m_shader_globals.raytype = static_cast<int>(ray.m_type);

        m_shader_globals.flipHandedness = 0;
        m_shader_globals.backfacing = get_side() == ObjectInstance::FrontSide ? 0 : 1;

        m_shader_globals.context = 0;
        m_shader_globals.Ci = 0;

        m_members |= HasOSLShaderGlobals;
    }
    else
    {
        // Update always the raytype, as it might have changed from the previous run.
        m_shader_globals.raytype = static_cast<int>(get_ray().m_type);
    }

    return m_shader_globals;
}

#endif

}   // namespace renderer
