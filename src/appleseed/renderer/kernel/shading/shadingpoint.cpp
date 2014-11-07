
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
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/math/scalar.h"
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
        assert(m_primitive_type == PrimitiveCurve1 || m_primitive_type == PrimitiveCurve3);
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
    const bool triangle_has_vertex_attributes = triangle.has_vertex_attributes();

    // Copy the index of the triangle attribute.
    m_primitive_pa = triangle.m_pa;

    // Copy the texture coordinates from UV set #0.
    if (triangle_has_vertex_attributes && tess.get_tex_coords_count() > 0)
    {
        m_v0_uv = tess.get_tex_coords(triangle.m_a0);
        m_v1_uv = tess.get_tex_coords(triangle.m_a1);
        m_v2_uv = tess.get_tex_coords(triangle.m_a2);
    }
    else
    {
        // UV set #0 doesn't exist, or this triangle doesn't have vertex attributes.
        m_v0_uv = GVector2(0.0);
        m_v1_uv = GVector2(0.0);
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
        m_v0 = lerp(prev_v0, next_v0, k);
        m_v1 = lerp(prev_v1, next_v1, k);
        m_v2 = lerp(prev_v2, next_v2, k);
    }
    else
    {
        m_v0 = tess.m_vertices[triangle.m_v0];
        m_v1 = tess.m_vertices[triangle.m_v1];
        m_v2 = tess.m_vertices[triangle.m_v2];
    }

    // Copy the object instance space vertex normals.
    assert(triangle.m_n0 != Triangle::None);
    assert(triangle.m_n1 != Triangle::None);
    assert(triangle.m_n2 != Triangle::None);
    m_n0 = tess.m_vertex_normals[triangle.m_n0];
    m_n1 = tess.m_vertex_normals[triangle.m_n1];
    m_n2 = tess.m_vertex_normals[triangle.m_n2];
    assert(is_normalized(m_n0));
    assert(is_normalized(m_n1));
    assert(is_normalized(m_n2));

    // Copy the object instance space vertex tangents.
    if (triangle_has_vertex_attributes && tess.get_vertex_tangent_count() > 0)
    {
        m_members |= HasTriangleVertexTangents;
        m_t0 = tess.get_vertex_tangent(triangle.m_v0);
        m_t1 = tess.get_vertex_tangent(triangle.m_v1);
        m_t2 = tess.get_vertex_tangent(triangle.m_v2);
        assert(is_normalized(m_t0));
        assert(is_normalized(m_t1));
        assert(is_normalized(m_t2));
    }
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
        assert(m_primitive_type == PrimitiveCurve1 || m_primitive_type == PrimitiveCurve3);

        m_asm_geo_normal = normalize(-local_ray.m_dir);

        // todo: this does not look correct, considering the flat ribbon nature of curves.
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
        const Vector3d& point = get_point();

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

void ShadingPoint::compute_partial_derivatives() const
{
    cache_source_geometry();

    if (m_primitive_type == PrimitiveTriangle)
    {
        //
        // Reference:
        //
        //   Physically Based Rendering, first edition, pp. 128-129
        //

        const double du0 = static_cast<double>(m_v0_uv[0] - m_v2_uv[0]);
        const double dv0 = static_cast<double>(m_v0_uv[1] - m_v2_uv[1]);
        const double du1 = static_cast<double>(m_v1_uv[0] - m_v2_uv[0]);
        const double dv1 = static_cast<double>(m_v1_uv[1] - m_v2_uv[1]);

        const double det = du0 * dv1 - dv0 * du1;

        if (det == 0.0)
        {
            const Basis3d basis(get_original_shading_normal());

            m_dpdu = basis.get_tangent_u();
            m_dpdv = basis.get_tangent_v();
        }
        else
        {
            const Vector3d& v2 = get_vertex(2);
            const Vector3d dp0 = get_vertex(0) - v2;
            const Vector3d dp1 = get_vertex(1) - v2;
            const double rcp_det = 1.0 / det;

            m_dpdu = (dv1 * dp0 - dv0 * dp1) * rcp_det;
            m_dpdv = (du0 * dp1 - du1 * dp0) * rcp_det;
        }
    }
    else if (m_primitive_type == PrimitiveCurve1)
    {
        assert(m_primitive_type == PrimitiveCurve1);

        const CurveObject* curves = static_cast<const CurveObject*>(m_object);
        const CurveType1& curve = curves->get_curve1(m_primitive_index);

        const GScalar v = static_cast<GScalar>(m_bary[1]);
        const GVector3 tangent = curve.evaluate_tangent(v);
        const Vector3d& sn = get_original_shading_normal();

        m_dpdu = normalize(Vector3d(tangent));
        m_dpdv = normalize(cross(sn, m_dpdu));
    }
    else
    {
        assert(m_primitive_type == PrimitiveCurve3);

        const CurveObject* curves = static_cast<const CurveObject*>(m_object);
        const CurveType3& curve = curves->get_curve3(m_primitive_index);

        const GScalar v = static_cast<GScalar>(m_bary[1]);
        const GVector3 tangent = curve.evaluate_tangent(v);
        const Vector3d& sn = get_original_shading_normal();

        m_dpdu = normalize(Vector3d(tangent));
        m_dpdv = normalize(cross(sn, m_dpdu));
    }
}

void ShadingPoint::compute_geometric_normal() const
{
    if (m_primitive_type == PrimitiveTriangle)
    {
        if (m_members & HasWorldSpaceTriangleVertices)
        {
            // We already have the world space vertices of the hit triangle.
            // Use them to compute the geometric normal directly in world space.
            m_geometric_normal = cross(m_v1_w - m_v0_w, m_v2_w - m_v0_w);
        }
        else
        {
            cache_source_geometry();

            // Compute the object instance space geometric normal.
            const Vector3d v0(m_v0);
            const Vector3d v1(m_v1);
            const Vector3d v2(m_v2);
            m_geometric_normal = cross(v1 - v0, v2 - v0);

            // Transform the geometric normal to world space.
            m_geometric_normal =
                m_assembly_instance_transform.normal_to_parent(
                    m_object_instance->get_transform().normal_to_parent(m_geometric_normal));
        }

        // Normalize the geometric normal.
        m_geometric_normal = normalize(m_geometric_normal);

        // Place the geometric normal in the same hemisphere as the original shading normal.
        if (dot(m_geometric_normal, get_original_shading_normal()) < 0.0)
            m_geometric_normal = -m_geometric_normal;

        // Remember which side of the geometric surface we hit.
        m_side =
            dot(m_ray.m_dir, m_geometric_normal) > 0.0
                ? ObjectInstance::BackSide
                : ObjectInstance::FrontSide;

        // Finally make the geometric normal face the direction of the incoming ray.
        if (m_side == ObjectInstance::BackSide)
            m_geometric_normal = -m_geometric_normal;
    }
    else
    {
        assert(m_primitive_type == PrimitiveCurve1 || m_primitive_type == PrimitiveCurve3);

        // We assume flat ribbons facing incoming rays.
        m_geometric_normal = -normalize(m_ray.m_dir);
        m_side = ObjectInstance::FrontSide;
    }
}

void ShadingPoint::compute_shading_normal() const
{
    // Start with the original shading normal.
    m_shading_normal = get_original_shading_normal();

    if (m_primitive_type == PrimitiveTriangle)
    {
        // Apply the normal modifier if the material has one.
        const Material* material = get_material();
        if (material)
        {
            const INormalModifier* modifier = material->get_normal_modifier();
            if (modifier)
            {
                m_shading_normal =
                    modifier->evaluate(
                        *m_texture_cache,
                        m_shading_normal,
                        get_uv(0),
                        get_dpdu(0),
                        get_dpdv(0));
            }
        }

        // Place the shading normal in the same hemisphere as the geometric normal.
        if (m_side == ObjectInstance::BackSide)
            m_shading_normal = -m_shading_normal;
    }
}

void ShadingPoint::compute_original_shading_normal() const
{
    if (m_primitive_type == PrimitiveTriangle)
    {
        cache_source_geometry();

        // Compute the object instance space shading normal.
        m_original_shading_normal =
              Vector3d(m_n0) * (1.0 - m_bary[0] - m_bary[1])
            + Vector3d(m_n1) * m_bary[0]
            + Vector3d(m_n2) * m_bary[1];

        // Transform the shading normal to world space.
        m_original_shading_normal =
            m_assembly_instance_transform.normal_to_parent(
                m_object_instance->get_transform().normal_to_parent(m_original_shading_normal));

        // Normalize the shading normal.
        m_original_shading_normal = normalize(m_original_shading_normal);
    }
    else
    {
        assert(m_primitive_type == PrimitiveCurve1 || m_primitive_type == PrimitiveCurve3);

        // We assume flat ribbons facing incoming rays.
        m_original_shading_normal = -normalize(m_ray.m_dir);
    }
}

void ShadingPoint::compute_shading_basis() const
{
    //
    // Reference:
    //
    //   Physically Based Rendering, first edition, pp. 133
    //

    // Retrieve or compute the first tangent vector.
    const Vector3d tangent =
        (m_members & HasTriangleVertexTangents) != 0
            ? m_assembly_instance_transform.vector_to_parent(
                  m_object_instance->get_transform().vector_to_parent(
                        Vector3d(m_t0) * (1.0 - m_bary[0] - m_bary[1])
                      + Vector3d(m_t1) * m_bary[0]
                      + Vector3d(m_t2) * m_bary[1]))
            : get_dpdu(0);

    // Compute the final tangent vectors.
    const Vector3d& sn = get_shading_normal();
    const Vector3d t = normalize(cross(tangent, sn));
    const Vector3d s = normalize(cross(sn, t));

    // Construct an orthonormal basis.
    m_shading_basis.build(sn, s, t);
}

void ShadingPoint::compute_world_space_triangle_vertices() const
{
    cache_source_geometry();

    // Transform vertices to assembly space.
    const Transformd& obj_instance_transform = m_object_instance->get_transform();
    m_v0_w = obj_instance_transform.point_to_parent(Vector3d(m_v0));
    m_v1_w = obj_instance_transform.point_to_parent(Vector3d(m_v1));
    m_v2_w = obj_instance_transform.point_to_parent(Vector3d(m_v2));

    // Transform vertices to world space.
    m_v0_w = m_assembly_instance_transform.point_to_parent(m_v0_w);
    m_v1_w = m_assembly_instance_transform.point_to_parent(m_v1_w);
    m_v2_w = m_assembly_instance_transform.point_to_parent(m_v2_w);
}

void ShadingPoint::compute_point_velocity() const
{
    m_point_velocity = Vector3d(0.0);

    if (m_primitive_type == PrimitiveTriangle)
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

        // Copy the object instance space triangle vertices.
        assert(triangle.m_v0 != Triangle::None);
        assert(triangle.m_v1 != Triangle::None);
        assert(triangle.m_v2 != Triangle::None);
        if (motion_segment_count > 0)
        {
            // Fetch triangle vertices from the last pose.
            const GVector3 last_v0 = tess.get_vertex_pose(triangle.m_v0, motion_segment_count - 1);
            const GVector3 last_v1 = tess.get_vertex_pose(triangle.m_v1, motion_segment_count - 1);
            const GVector3 last_v2 = tess.get_vertex_pose(triangle.m_v2, motion_segment_count - 1);

            // Compute velocities per vertex.
            const GVector3 vel_v0 = last_v0 - tess.m_vertices[triangle.m_v0];
            const GVector3 vel_v1 = last_v1 - tess.m_vertices[triangle.m_v1];
            const GVector3 vel_v2 = last_v2 - tess.m_vertices[triangle.m_v2];

            const float v = static_cast<float>(m_bary[0]);
            const float w = static_cast<float>(m_bary[1]);
            const float u = 1.0f - v - w;

            // Compute point velocity.
            const GVector3 velocity =
                vel_v0 * u
              + vel_v1 * v
              + vel_v2 * w;

            m_point_velocity = Vector3d(velocity);

            // Transform to assembly space.
            const Transformd& obj_instance_transform = m_object_instance->get_transform();
            m_point_velocity = obj_instance_transform.vector_to_parent(m_point_velocity);

            // Transform to world space.
            m_point_velocity = m_assembly_instance_transform.vector_to_parent(m_point_velocity);
        }
    }
}

#ifdef APPLESEED_WITH_OSL

//
// ShadingPoint::OSLObjectTransformInfo class implementation.
//

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

void ShadingPoint::initialize_osl_shader_globals(
    const ShaderGroup&          sg,
    const ShadingRay::TypeType  ray_type,
    OSL::RendererServices*      renderer,
    const float                 surface_area) const
{
    assert(hit());
    assert(renderer);

    if (!(m_members & HasOSLShaderGlobals))
    {
        const ShadingRay& ray(get_ray());

        m_shader_globals.P = Vector3f(get_point());
        m_shader_globals.dPdx = OSL::Vec3(0.0f, 0.0f, 0.0f);
        m_shader_globals.dPdy = OSL::Vec3(0.0f, 0.0f, 0.0f);
        m_shader_globals.dPdz = OSL::Vec3(0.0f, 0.0f, 0.0f);

        m_shader_globals.I = Vector3f(normalize(ray.m_dir));
        m_shader_globals.dIdx = OSL::Vec3(0.0f, 0.0f, 0.0f);
        m_shader_globals.dIdy = OSL::Vec3(0.0f, 0.0f, 0.0f);

        m_shader_globals.N = Vector3f(get_shading_normal());
        m_shader_globals.Ng = Vector3f(get_geometric_normal());

        const Vector2d& uv = get_uv(0);

        m_shader_globals.u = static_cast<float>(uv[0]);
        m_shader_globals.dudx = 0.0f;
        m_shader_globals.dudy = 0.0f;

        m_shader_globals.v = static_cast<float>(uv[1]);
        m_shader_globals.dvdx = 0.0f;
        m_shader_globals.dvdy = 0.0f;

        m_shader_globals.dPdu = Vector3f(get_dpdu(0));
        m_shader_globals.dPdv = Vector3f(get_dpdv(0));

        m_shader_globals.time = static_cast<float>(ray.m_time);
        m_shader_globals.dtime = static_cast<float>(get_dtime());

        m_shader_globals.dPdtime =
            sg.uses_dPdtime()
                ? Vector3f(get_point_velocity())
                : Vector3f(0.0f);

        m_shader_globals.Ps = OSL::Vec3(0.0f, 0.0f, 0.0f);
        m_shader_globals.dPsdx = OSL::Vec3(0.0f, 0.0f, 0.0f);
        m_shader_globals.dPsdy = OSL::Vec3(0.0f, 0.0f, 0.0f);

        m_shader_globals.renderer = renderer;
        m_shader_globals.renderstate =
            const_cast<void*>(reinterpret_cast<const void*>(this));

        memset(reinterpret_cast<void*>(&m_osl_trace_data), 0, sizeof(OSLTraceData));
        m_shader_globals.tracedata = reinterpret_cast<void*>(&m_osl_trace_data);

        m_shader_globals.objdata = 0;

        m_obj_transform_info.m_assembly_instance_transform =
            m_assembly_instance_transform_seq;
        m_obj_transform_info.m_object_instance_transform =
            &m_object_instance->get_transform();

        m_shader_globals.object2common = reinterpret_cast<OSL::TransformationPtr>(&m_obj_transform_info);

        m_shader_globals.shader2common = 0;

        m_shader_globals.flipHandedness = 0;
        m_shader_globals.backfacing = get_side() == ObjectInstance::FrontSide ? 0 : 1;

        m_members |= HasOSLShaderGlobals;
    }

    // Always update the raytype and surface area.
    m_shader_globals.raytype = static_cast<int>(ray_type);
    m_shader_globals.surfacearea = surface_area;

    // These are set by OSL when the shader is executed.
    m_shader_globals.context = 0;
    m_shader_globals.Ci = 0;
}

OSL::ShaderGlobals& ShadingPoint::get_osl_shader_globals() const
{
    assert(hit());
    assert(m_members & HasOSLShaderGlobals);

    return m_shader_globals;
}

#endif

}   // namespace renderer
