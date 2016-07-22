
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/scene.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif

// appleseed.foundation headers.
#include "foundation/math/intersection/rayplane.h"
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
        assert(is_curve_primitive());
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

    // Compute motion interpolation parameters.
    const size_t motion_segment_count = tess.get_motion_segment_count();
    const double base_time = m_ray.m_time.m_normalized * motion_segment_count;
    const size_t base_index = truncate<size_t>(base_time);
    const GScalar frac = static_cast<GScalar>(base_time - base_index);
    const GScalar one_minus_frac = GScalar(1.0) - frac;

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
        // Make sure ShadingPoint::get_uv() simply returns the barycentric coordinates.
        m_v0_uv = GVector2(0.0, 0.0);
        m_v1_uv = GVector2(1.0, 0.0);
        m_v2_uv = GVector2(0.0, 1.0);
    }

    // Copy or compute triangle vertices (in object instance space).
    assert(triangle.m_v0 != Triangle::None);
    assert(triangle.m_v1 != Triangle::None);
    assert(triangle.m_v2 != Triangle::None);
    if (motion_segment_count > 0)
    {
        // Fetch vertices from previous pose.
        if (base_index == 0)
        {
            m_v0 = tess.m_vertices[triangle.m_v0];
            m_v1 = tess.m_vertices[triangle.m_v1];
            m_v2 = tess.m_vertices[triangle.m_v2];
        }
        else
        {
            m_v0 = tess.get_vertex_pose(triangle.m_v0, base_index - 1);
            m_v1 = tess.get_vertex_pose(triangle.m_v1, base_index - 1);
            m_v2 = tess.get_vertex_pose(triangle.m_v2, base_index - 1);
        }

        // Interpolate with vertices from next pose.
        m_v0 *= one_minus_frac;
        m_v1 *= one_minus_frac;
        m_v2 *= one_minus_frac;
        m_v0 += tess.get_vertex_pose(triangle.m_v0, base_index) * frac;
        m_v1 += tess.get_vertex_pose(triangle.m_v1, base_index) * frac;
        m_v2 += tess.get_vertex_pose(triangle.m_v2, base_index) * frac;
    }
    else
    {
        m_v0 = tess.m_vertices[triangle.m_v0];
        m_v1 = tess.m_vertices[triangle.m_v1];
        m_v2 = tess.m_vertices[triangle.m_v2];
    }

    // Copy or compute triangle vertex normals (in object instance space).
    if (triangle.m_n0 != Triangle::None &&
        triangle.m_n1 != Triangle::None &&
        triangle.m_n2 != Triangle::None)
    {
        if (motion_segment_count > 0)
        {
            // Fetch vertex normals from previous pose.
            if (base_index == 0)
            {
                m_n0 = tess.m_vertex_normals[triangle.m_n0];
                m_n1 = tess.m_vertex_normals[triangle.m_n1];
                m_n2 = tess.m_vertex_normals[triangle.m_n2];
            }
            else
            {
                m_n0 = tess.get_vertex_normal_pose(triangle.m_n0, base_index - 1);
                m_n1 = tess.get_vertex_normal_pose(triangle.m_n1, base_index - 1);
                m_n2 = tess.get_vertex_normal_pose(triangle.m_n2, base_index - 1);
            }

            // Interpolate with vertex normals from next pose.
            // Assume small motion, stick to linear interpolation.
            m_n0 *= one_minus_frac;
            m_n1 *= one_minus_frac;
            m_n2 *= one_minus_frac;
            m_n0 += tess.get_vertex_normal_pose(triangle.m_n0, base_index) * frac;
            m_n1 += tess.get_vertex_normal_pose(triangle.m_n1, base_index) * frac;
            m_n2 += tess.get_vertex_normal_pose(triangle.m_n2, base_index) * frac;

            // Renormalize interpolated normals.
            m_n0 = normalize(m_n0);
            m_n1 = normalize(m_n1);
            m_n2 = normalize(m_n2);
        }
        else
        {
            m_n0 = tess.m_vertex_normals[triangle.m_n0];
            m_n1 = tess.m_vertex_normals[triangle.m_n1];
            m_n2 = tess.m_vertex_normals[triangle.m_n2];
        }

        assert(is_normalized(m_n0));
        assert(is_normalized(m_n1));
        assert(is_normalized(m_n2));

        m_members |= HasTriangleVertexNormals;
    }

    // Copy vertex tangents (in object instance space).
    if (triangle_has_vertex_attributes && tess.get_vertex_tangent_count() > 0)
    {
        if (motion_segment_count > 0)
        {
            // Fetch vertex tangents from previous pose.
            if (base_index == 0)
            {
                m_t0 = tess.get_vertex_tangent(triangle.m_v0);
                m_t1 = tess.get_vertex_tangent(triangle.m_v1);
                m_t2 = tess.get_vertex_tangent(triangle.m_v2);
            }
            else
            {
                m_t0 = tess.get_vertex_tangent_pose(triangle.m_v0, base_index - 1);
                m_t1 = tess.get_vertex_tangent_pose(triangle.m_v1, base_index - 1);
                m_t2 = tess.get_vertex_tangent_pose(triangle.m_v2, base_index - 1);
            }

            // Interpolate with vertex tangents from next pose.
            // Assume small motion, stick to linear interpolation.
            m_t0 *= one_minus_frac;
            m_t1 *= one_minus_frac;
            m_t2 *= one_minus_frac;
            m_t0 += tess.get_vertex_tangent_pose(triangle.m_v0, base_index) * frac;
            m_t1 += tess.get_vertex_tangent_pose(triangle.m_v1, base_index) * frac;
            m_t2 += tess.get_vertex_tangent_pose(triangle.m_v2, base_index) * frac;

            // Renormalize interpolated tangents.
            m_t0 = normalize(m_t0);
            m_t1 = normalize(m_t1);
            m_t2 = normalize(m_t2);
        }
        else
        {
            m_t0 = tess.get_vertex_tangent(triangle.m_v0);
            m_t1 = tess.get_vertex_tangent(triangle.m_v1);
            m_t2 = tess.get_vertex_tangent(triangle.m_v2);
        }

        assert(is_normalized(m_t0));
        assert(is_normalized(m_t1));
        assert(is_normalized(m_t2));

        m_members |= HasTriangleVertexTangents;
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
        assert(is_curve_primitive());

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
        cache_source_geometry();

        switch (m_object_instance->get_ray_bias_method())
        {
          case ObjectInstance::RayBiasMethodNone:
            {
                m_biased_point = get_point();
                m_members |= HasBiasedPoint;
                return m_biased_point;
            }

          case ObjectInstance::RayBiasMethodNormal:
            {
                const Vector3d& p = get_point();
                const Vector3d& n = get_geometric_normal();
                const double bias = m_object_instance->get_ray_bias_distance();
                return dot(direction, n) > 0.0 ? p + bias * n : p - bias * n;
            }

          case ObjectInstance::RayBiasMethodIncomingDirection:
            {
                const Vector3d& p = get_point();
                const double bias = m_object_instance->get_ray_bias_distance();
                m_biased_point = p + bias * m_ray.m_dir;
                m_members |= HasBiasedPoint;
                return m_biased_point;
            }

          case ObjectInstance::RayBiasMethodOutgoingDirection:
            {
                const Vector3d& p = get_point();
                const double bias = m_object_instance->get_ray_bias_distance();
                return p + bias * normalize(direction);
            }

          assert_otherwise;
        }
    }

    return m_biased_point;
}

void ShadingPoint::compute_world_space_partial_derivatives() const
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
            m_dndu = m_dndv = Vector3d(0.0);
        }
        else
        {
            const Vector3d& v2 = get_vertex(2);
            const Vector3d dp0 = get_vertex(0) - v2;
            const Vector3d dp1 = get_vertex(1) - v2;

            const double rcp_det = 1.0 / det;

            m_dpdu = (dv1 * dp0 - dv0 * dp1) * rcp_det;
            m_dpdv = (du0 * dp1 - du1 * dp0) * rcp_det;

            // Make sure dPdv x dPdu points in the same direction as
            // the original shading normal.
            if (dot(get_original_shading_normal(), cross(m_dpdv, m_dpdu)) < 0.0)
                m_dpdu = -m_dpdu;

            const Vector3d dn0(m_n0 - m_n2);
            const Vector3d dn1(m_n1 - m_n2);

            m_dndu = (dv1 * dn0 - dv0 * dn1) * rcp_det;
            m_dndv = (du0 * dn1 - du1 * dn0) * rcp_det;

            // Transform the normal derivatives to world space.
            const Transformd& obj_instance_transform =
                m_object_instance->get_transform();

            m_dndu =
                m_assembly_instance_transform.normal_to_parent(
                    obj_instance_transform.normal_to_parent(m_dndu));

            m_dndv =
                m_assembly_instance_transform.normal_to_parent(
                    obj_instance_transform.normal_to_parent(m_dndv));
        }
    }
    else
    {
        assert(is_curve_primitive());

        const GScalar v = static_cast<GScalar>(m_bary[1]);

        const CurveObject* curves = static_cast<const CurveObject*>(m_object);
        const GVector3 tangent =
            m_primitive_type == PrimitiveCurve1
                ? curves->get_curve1(m_primitive_index).evaluate_tangent(v)
                : curves->get_curve3(m_primitive_index).evaluate_tangent(v);

        const Vector3d& sn = get_original_shading_normal();

        m_dpdu = normalize(Vector3d(tangent));
        m_dpdv = normalize(cross(sn, m_dpdu));
        m_dndu = m_dndv = Vector3d(0.0);
    }
}

void ShadingPoint::compute_screen_space_partial_derivatives() const
{
    const ShadingRay& ray = get_ray();

    if (ray.m_has_differentials)
    {
        const Vector3d& p = get_point();
        const Vector3d& n = get_shading_normal();

        const double tx = intersect(ray.m_rx, p, n);
        const Vector3d px = ray.m_rx.point_at(tx);
        m_dpdx = px - p;

        const double ty = intersect(ray.m_ry, p, n);
        const Vector3d py = ray.m_ry.point_at(ty);
        m_dpdy = py - p;

        // Make sure dPdy x dPdx points in the same direction as
        // the original shading normal.
        if (dot(get_original_shading_normal(), cross(m_dpdy, m_dpdx)) < 0.0)
            m_dpdx = -m_dpdx;

        // Select the two smallest axes.
        const size_t max_index = max_abs_index(n);
        const size_t axes[2] = { max_index == 0, 2 - (max_index >> 1) };

        const Vector3d& dpdu = get_dpdu(0);
        const Vector3d& dpdv = get_dpdv(0);

        const Vector2d a0(dpdu[axes[0]], dpdu[axes[1]]);
        const Vector2d a1(dpdv[axes[0]], dpdv[axes[1]]);

        const double d = det(a0, a1);

        if (d == 0.0)
        {
            m_duvdx = Vector2d(0.0);
            m_duvdy = Vector2d(0.0);
            return;
        }

        const double rcp_d = 1.0 / d;

        const Vector2d bx(
            px[axes[0]] - p[axes[0]],
            px[axes[1]] - p[axes[1]]);

        m_duvdx[0] = (a1[1] * bx[0] - a0[1] * bx[1]) * rcp_d;
        m_duvdx[1] = (a0[0] * bx[1] - a1[0] * bx[0]) * rcp_d;

        const Vector2d by(
            py[axes[0]] - p[axes[0]],
            py[axes[1]] - p[axes[1]]);

        m_duvdy[0] = (a1[1] * by[0] - a0[1] * by[1]) * rcp_d;
        m_duvdy[1] = (a0[0] * by[1] - a1[0] * by[0]) * rcp_d;
    }
    else
    {
        m_dpdx = Vector3d(0.0);
        m_dpdy = Vector3d(0.0);
        m_duvdx = Vector2d(0.0);
        m_duvdy = Vector2d(0.0);
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

        if (m_members & HasTriangleVertexNormals)
        {
            // We have per-vertex normals, so we can compute the original shading normal:
            // place the geometric normal in the same hemisphere as the original shading normal.
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
            // In the absence of per-vertex normals, we have no way to know if we are
            // hitting the front face or the back face of the surface. Assume we are
            // always hitting the front face...
            m_side = ObjectInstance::FrontSide;

            // ...and if the geometric normal is not facing the ray, flip it.
            if (dot(m_ray.m_dir, m_geometric_normal) > 0.0)
                m_geometric_normal = -m_geometric_normal;
        }
    }
    else
    {
        assert(is_curve_primitive());

        // We assume flat ribbons facing incoming rays.
        m_geometric_normal = -m_ray.m_dir;
        m_side = ObjectInstance::FrontSide;
    }
}

void ShadingPoint::compute_original_shading_normal() const
{
    if (m_primitive_type == PrimitiveTriangle)
    {
        cache_source_geometry();

        if (m_members & HasTriangleVertexNormals)
        {
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
            // Use the geometric normal if per-vertex normals are absent.
            m_original_shading_normal = get_geometric_normal();
        }
    }
    else
    {
        assert(is_curve_primitive());

        // We assume flat ribbons facing incoming rays.
        m_original_shading_normal = -m_ray.m_dir;
    }
}

void ShadingPoint::compute_shading_basis() const
{
    // Compute the unperturbed shading normal.
    Vector3d sn = get_original_shading_normal();

    // Place the unperturbed shading normal in the same hemisphere as the geometric normal.
    if (get_side() == ObjectInstance::BackSide)
        sn = -sn;

    // Retrieve or compute the first tangent direction (non-normalized).
    // Reference: Physically Based Rendering, first edition, pp. 133
    const Vector3d tangent =
        (m_members & HasTriangleVertexTangents) != 0
            ? m_assembly_instance_transform.vector_to_parent(
                  m_object_instance->get_transform().vector_to_parent(
                        Vector3d(m_t0) * (1.0 - m_bary[0] - m_bary[1])
                      + Vector3d(m_t1) * m_bary[0]
                      + Vector3d(m_t2) * m_bary[1]))
            : get_dpdu(0);

    // Construct an orthonormal basis.
    const Vector3d t = normalize(cross(tangent, sn));
    const Vector3d s = normalize(cross(sn, t));
    m_shading_basis.build(sn, s, t);

    // Apply the basis modifier if the material has one.
    if (m_primitive_type == PrimitiveTriangle)
    {
        const Material* material = get_material();
        if (material)
        {
            const Material::RenderData& material_data = material->get_render_data();
            if (material_data.m_basis_modifier)
            {
                m_shading_basis =
                    material_data.m_basis_modifier->modify(
                        *m_texture_cache,
                        get_uv(0),
                        m_shading_basis);
            }
        }
    }
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

void ShadingPoint::compute_world_space_point_velocity() const
{
    Vector3d p0 = get_point();
    Vector3d p1 = p0;

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
            // Fetch triangle vertices from the first pose.
            const GVector3& first_v0 = tess.m_vertices[triangle.m_v0];
            const GVector3& first_v1 = tess.m_vertices[triangle.m_v1];
            const GVector3& first_v2 = tess.m_vertices[triangle.m_v2];

            // Fetch triangle vertices from the last pose.
            const GVector3 last_v0 = tess.get_vertex_pose(triangle.m_v0, motion_segment_count - 1);
            const GVector3 last_v1 = tess.get_vertex_pose(triangle.m_v1, motion_segment_count - 1);
            const GVector3 last_v2 = tess.get_vertex_pose(triangle.m_v2, motion_segment_count - 1);

            // Compute barycentric coordinates.
            const float v = static_cast<float>(m_bary[0]);
            const float w = static_cast<float>(m_bary[1]);
            const float u = 1.0f - v - w;

            // Compute positions at shutter open and close times.
            p0 = Vector3d(first_v0 * u + first_v1 * v + first_v2 * w);
            p1 = Vector3d( last_v0 * u +  last_v1 * v +  last_v2 * w);
        }
    }

    // Transform positions to assembly space.
    const Transformd& obj_instance_transform = m_object_instance->get_transform();
    p0 = obj_instance_transform.point_to_parent(p0);
    p1 = obj_instance_transform.point_to_parent(p1);

    // Transform positions to world space.
    if (m_assembly_instance_transform_seq->size() > 1)
    {
        const Camera* camera = m_scene->get_camera();
        Transformd scratch;

        const Transformd& assembly_instance_transform0 =
            m_assembly_instance_transform_seq->evaluate(
                camera->get_shutter_open_time(),
                scratch);
        p0 = assembly_instance_transform0.point_to_parent(p0);

        const Transformd& assembly_instance_transform1 =
            m_assembly_instance_transform_seq->evaluate(
                camera->get_shutter_close_time(),
                scratch);
        p1 = assembly_instance_transform1.point_to_parent(p1);
    }
    else
    {
        p0 = m_assembly_instance_transform.point_to_parent(p0);
        p1 = m_assembly_instance_transform.point_to_parent(p1);
    }

    m_point_velocity = p1 - p0;
}

void ShadingPoint::compute_alpha() const
{
    m_alpha.set(1.0f);

    if (m_primitive_type == PrimitiveTriangle)
    {
        if (const Source* alpha_map = get_object().get_alpha_map())
        {
            Alpha a;
            alpha_map->evaluate(*m_texture_cache, get_uv(0), a);
            m_alpha *= a;
        }

        if (const Material* material = get_material())
        {
            const Material::RenderData& material_data = material->get_render_data();
            if (material_data.m_alpha_map)
            {
                Alpha a;
                material_data.m_alpha_map->evaluate(*m_texture_cache, get_uv(0), a);
                m_alpha *= a;
            }
        }
    }
    else
    {
        assert(is_curve_primitive());

        // TODO: interpolate per vertex alpha for curves here...
    }
}

#ifdef APPLESEED_WITH_OSL

void ShadingPoint::initialize_osl_shader_globals(
    const ShaderGroup&          sg,
    const VisibilityFlags::Type ray_flags,
    OSL::RendererServices*      renderer) const
{
    assert(hit());
    assert(renderer);

    if (!(m_members & HasOSLShaderGlobals))
    {
        const ShadingRay& ray = get_ray();
        assert(is_normalized(ray.m_dir));

        // Surface position and incident ray direction.
        m_shader_globals.P = Vector3f(get_point());
        m_shader_globals.I = Vector3f(ray.m_dir);

        // Handedness flip flag.
        // Currently it is unused, but we might want to enable it in the future.
        m_shader_globals.flipHandedness =
            m_assembly_instance_transform_seq->swaps_handedness(m_assembly_instance_transform) !=
            get_object_instance().transform_swaps_handedness() ? 1 : 0;

        // Surface position and incident ray direction differentials.
        if (ray.m_has_differentials)
        {
            m_shader_globals.dPdx =
                m_shader_globals.flipHandedness
                    ?  Vector3f(get_dpdx())
                    : -Vector3f(get_dpdx());
            m_shader_globals.dPdy = Vector3f(get_dpdy());
            m_shader_globals.dPdz = Vector3f(0.0);
            m_shader_globals.dIdx = Vector3f(ray.m_rx.m_dir);
            m_shader_globals.dIdy = Vector3f(ray.m_ry.m_dir);
        }
        else
        {
            m_shader_globals.dPdx = Vector3f(0.0f);
            m_shader_globals.dPdy = Vector3f(0.0f);
            m_shader_globals.dPdz = Vector3f(0.0);
            m_shader_globals.dIdx = Vector3f(0.0f);
            m_shader_globals.dIdy = Vector3f(0.0f);
        }

        // Shading and geometric normals and backfacing flag.
        m_shader_globals.N = Vector3f(get_original_shading_normal());
        m_shader_globals.Ng = Vector3f(get_geometric_normal());

        m_shader_globals.backfacing =
            get_side() == ObjectInstance::FrontSide ? 0 : 1;

        // Surface parameters and their differentials.
        const Vector2d& uv = get_uv(0);
        m_shader_globals.u = static_cast<float>(uv[0]);
        m_shader_globals.v = static_cast<float>(uv[1]);
        if (ray.m_has_differentials)
        {
            const Vector2d& duvdx = get_duvdx(0);
            const Vector2d& duvdy = get_duvdy(0);
            m_shader_globals.dudx = static_cast<float>(duvdx[0]);
            m_shader_globals.dudy = static_cast<float>(duvdy[0]);
            m_shader_globals.dvdx = static_cast<float>(duvdx[1]);
            m_shader_globals.dvdy = static_cast<float>(duvdy[1]);
        }
        else
        {
            m_shader_globals.dudx = 0.0f;
            m_shader_globals.dudy = 0.0f;
            m_shader_globals.dvdx = 0.0f;
            m_shader_globals.dvdy = 0.0f;
        }

        // Surface tangents.
        m_shader_globals.dPdu =
            m_shader_globals.flipHandedness
                ?  Vector3f(get_dpdu(0))
                : -Vector3f(get_dpdu(0));

        m_shader_globals.dPdv = Vector3f(get_dpdv(0));

        // Time and its derivative.
        m_shader_globals.time = static_cast<float>(ray.m_time.m_absolute);
        m_shader_globals.dtime = static_cast<float>(m_scene->get_camera()->get_shutter_open_time_interval());

        // Velocity vector.
        m_shader_globals.dPdtime =
            sg.uses_dPdtime()
                ? Vector3f(get_world_space_point_velocity())
                : Vector3f(0.0f);

        // Point being illuminated and its differentials.
        m_shader_globals.Ps = Vector3f(0.0f);
        m_shader_globals.dPsdx = Vector3f(0.0f);
        m_shader_globals.dPsdy = Vector3f(0.0f);

        // Opaque state pointers.
        m_shader_globals.renderstate = const_cast<ShadingPoint*>(this);
        memset(&m_osl_trace_data, 0, sizeof(OSLTraceData));
        m_shader_globals.tracedata = &m_osl_trace_data;
        m_shader_globals.objdata = 0;

        // Pointer to the RendererServices object.
        m_shader_globals.renderer = renderer;

        // Transformations.
        m_obj_transform_info.m_assembly_instance_transform = m_assembly_instance_transform_seq;
        m_obj_transform_info.m_object_instance_transform = &m_object_instance->get_transform();
        m_shader_globals.object2common = reinterpret_cast<OSL::TransformationPtr>(&m_obj_transform_info);
        m_shader_globals.shader2common = 0;

        m_members |= HasOSLShaderGlobals;
    }

    // Always update the ray type flags.
    m_shader_globals.raytype = static_cast<int>(ray_flags);

    // Always update the surface area of emissive objects.
    m_shader_globals.surfacearea =
        ray_flags == VisibilityFlags::LightRay && sg.has_emission()
            ? static_cast<float>(sg.get_surface_area(&get_assembly_instance(), &get_object_instance()))
            : 0.0f;

    // Output closure.
    m_shader_globals.Ci = 0;
}


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
        m_object_instance_transform->get_local_to_parent() *
        assembly_xform.get_local_to_parent());

    return Matrix4f(m);
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_transform(const float t) const
{
    const Transformd assembly_xform = m_assembly_instance_transform->evaluate(t);
    const Transformd::MatrixType m(
        m_object_instance_transform->get_local_to_parent() *
        assembly_xform.get_local_to_parent());

    return Matrix4f(m);
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_inverse_transform() const
{
    assert(!is_animated());

    const Transformd& assembly_xform = m_assembly_instance_transform->get_earliest_transform();
    const Transformd::MatrixType m(
        m_object_instance_transform->get_parent_to_local() *
        assembly_xform.get_parent_to_local());

    return Matrix4f(m);
}

OSL::Matrix44 ShadingPoint::OSLObjectTransformInfo::get_inverse_transform(const float t) const
{
    const Transformd assembly_xform = m_assembly_instance_transform->evaluate(t);
    const Transformd::MatrixType m(
        m_object_instance_transform->get_parent_to_local() *
        assembly_xform.get_parent_to_local());

    return Matrix4f(m);
}

#endif

}   // namespace renderer
