
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
#include "shadingpoint.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/refining.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/material/ibasismodifier.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/proceduralobject.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/utility/triangle.h"

// appleseed.foundation headers.
#include "foundation/math/intersection/rayplane.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/attributeset.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <utility>

using namespace foundation;

namespace renderer
{

//
// ShadingPoint class implementation.
//

void ShadingPoint::flip_side()
{
    assert(hit_surface());

#if 1

    //
    // Move the ray's origin to the other side of the intersection point,
    // and inverse the ray's direction. Pretending the ray came from the
    // other side of the surface is enough to flip the side on which lies
    // the shading point.
    //
    // The two downsides of this approach are:
    //
    //   1. The ray that led to this shading point is modified, and that
    //      could have consequences if not taken into account.
    //
    //   2. All precomputed values are lost and will need to be recomputed
    //      if they are needed.
    //

    const double t = 2.0 * m_ray.m_tmax;

    m_ray.m_org = m_ray.point_at(t);
    m_ray.m_rx_org = m_ray.m_rx_org + t * m_ray.m_rx_dir;
    m_ray.m_ry_org = m_ray.m_ry_org + t * m_ray.m_ry_dir;

    m_ray.m_dir = -m_ray.m_dir;
    m_ray.m_rx_dir = -m_ray.m_rx_dir;
    m_ray.m_ry_dir = -m_ray.m_ry_dir;

    m_members = 0;

#else

    //
    // Flip all the fields related to the side on which the shading point
    // lies. While this should work (the code below is not well tested),
    // the major downside of this approach is that this flips precomputed
    // values, not the primary intersection results. This means that the
    // flip is lost if the shading point is copied, or if for any other
    // reason, m_members is set to 0.
    //

    // Force the side to be computed.
    get_side();

    // Flip the side.
    if (m_side == ObjectInstance::FrontSide)
        m_side = ObjectInstance::BackSide;
    else m_side = ObjectInstance::FrontSide;

    // Flip the geometric normal.
    assert(m_members & HasGeometricNormal);
    m_geometric_normal = -m_geometric_normal;

    // Flip the original shading normal.
    if (m_members & HasOriginalShadingNormal)
        m_original_shading_normal = -m_original_shading_normal;

    // Flip the modified shading normal.
    if (m_members & HasShadingBasis)
    {
        // todo: add a more efficient flip() method to foundation::Basis.
        m_shading_basis = Basis3d(
            -m_shading_basis.get_normal(),
            -m_shading_basis.get_tangent_u(),
             m_shading_basis.get_tangent_v());
    }

    // Swap materials.
    if (m_members & HasMaterials)
        std::swap(m_material, m_opposite_material);

    // Update OSL shader globals.
    if (m_members & HasOSLShaderGlobals)
    {
        m_shader_globals.N = -m_shader_globals.N;
        m_shader_globals.Ng = -m_shader_globals.Ng;
        m_shader_globals.backfacing = 1 - m_shader_globals.backfacing;
    }

#endif
}

void ShadingPoint::fetch_source_geometry() const
{
    assert(hit_surface());
    assert(!(m_members & HasSourceGeometry));

    // Retrieve the assembly.
    m_assembly = &m_assembly_instance->get_assembly();

    // Retrieve the object instance.
    m_object_instance = m_assembly->object_instances().get_by_index(m_object_instance_index);
    assert(m_object_instance);

    // Retrieve the object.
    m_object = &m_object_instance->get_object();

    // Fetch primitive-specific geometry.
    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
        fetch_triangle_source_geometry();
        break;

      case PrimitiveProceduralSurface:
        // Nothing to do.
        break;

      case PrimitiveCurve1:
      case PrimitiveCurve3:
        fetch_curve_source_geometry();
        break;

      assert_otherwise;
    }
}

void ShadingPoint::fetch_triangle_source_geometry() const
{
    // Retrieve the tessellation of the object.
    const MeshObject& mesh = static_cast<const MeshObject&>(*m_object);
    const StaticTriangleTess& tess = mesh.get_static_triangle_tess();

    // Compute motion interpolation parameters.
    const size_t motion_segment_count = tess.get_motion_segment_count();
    const double base_time = m_ray.m_time.m_normalized * motion_segment_count;
    const size_t base_index = truncate<size_t>(base_time);
    const GScalar frac = static_cast<GScalar>(base_time - base_index);
    const GScalar one_minus_frac = GScalar(1.0) - frac;

    // Retrieve the triangle.
    const Triangle& triangle = tess.m_primitives[m_primitive_index];
    assert(triangle.m_v0 != Triangle::None);
    assert(triangle.m_v1 != Triangle::None);
    assert(triangle.m_v2 != Triangle::None);

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
    // Set primitive attribute to default value 0.
    // todo: fix.
    m_primitive_pa = 0;
}

void ShadingPoint::refine_and_offset() const
{
    assert(hit_surface());
    assert(!(m_members & ShadingPoint::HasRefinedPoints));

    cache_source_geometry();

    // Compute the location of the intersection point in assembly instance space.
    ShadingRay::RayType refine_space_ray = m_assembly_instance_transform.to_local(m_ray);

    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
        {
            // Offset the ray origin to the hit point.
            refine_space_ray.m_org += refine_space_ray.m_tmax * refine_space_ray.m_dir;

            const auto intersection_handling = [this](const Vector3d& p, const Vector3d& n)
            {
                return m_triangle_support_plane.intersect(p, n);
            };

            // Refine the location of the intersection point.
            refine_space_ray.m_org =
                refine(
                    refine_space_ray.m_org,
                    refine_space_ray.m_dir,
                    intersection_handling);

            // Compute the geometric normal to the hit triangle in assembly instance space (non unit-length).
            m_refine_space_geo_normal = Vector3d(compute_triangle_normal(m_v0, m_v1, m_v2));
            m_refine_space_geo_normal = m_object_instance->get_transform().normal_to_parent(m_refine_space_geo_normal);
            m_refine_space_geo_normal = faceforward(m_refine_space_geo_normal, refine_space_ray.m_dir);

            // Compute the offset points in assembly instance space.
#ifdef RENDERER_ADAPTIVE_OFFSET
            adaptive_offset(
                refine_space_ray.m_org,
                m_refine_space_geo_normal,
                m_refine_space_front_point,
                m_refine_space_back_point,
                intersection_handling);
#else
            fixed_offset(
                local_ray.m_org,
                m_refine_space_geo_normal,
                m_refine_space_front_point,
                m_refine_space_back_point);
#endif
        }
        break;

      case PrimitiveProceduralSurface:
          {
#ifdef RENDERER_ADAPTIVE_OFFSET
              // Compute the location of the intersection point in object space.
              refine_space_ray = m_object_instance->get_transform().to_local(refine_space_ray);

              // Offset the ray origin to the hit point.
              refine_space_ray.m_org += refine_space_ray.m_tmax * refine_space_ray.m_dir;

              // Compute the offset points and geometric normal in object space.
              const ProceduralObject& object = static_cast<const ProceduralObject&>(get_object());
              object.refine_and_offset(
                  refine_space_ray,
                  m_refine_space_front_point,
                  m_refine_space_back_point,
                  m_refine_space_geo_normal);
#else
              assert(false);
#endif
          }
        break;

      case PrimitiveCurve1:
      case PrimitiveCurve3:
        {
            // Offset the ray origin to the hit point.
            refine_space_ray.m_org += refine_space_ray.m_tmax * refine_space_ray.m_dir;

            assert(is_curve_primitive());

            const Vector3d x = get_dpdu(0);
            const Vector3d& sn_curve = -refine_space_ray.m_dir;
            const Vector3d z = cross(sn_curve, x);
            m_refine_space_geo_normal = cross(z, x);

            // todo: this does not look correct, considering the flat ribbon nature of curves.
            const double Eps = 1.0e-6;
            m_refine_space_front_point = refine_space_ray.m_org + Eps * m_refine_space_geo_normal;
            m_refine_space_back_point = refine_space_ray.m_org - Eps * m_refine_space_geo_normal;
        }
        break;

      assert_otherwise;
    }

    // Check that refined values are not NaN.
    assert(m_refine_space_back_point == m_refine_space_back_point);
    assert(m_refine_space_front_point == m_refine_space_front_point);
    assert(m_refine_space_geo_normal == m_refine_space_geo_normal);

    // The refined intersection points are now available.
    m_members |= ShadingPoint::HasRefinedPoints;
}

void ShadingPoint::compute_world_space_partial_derivatives() const
{
    cache_source_geometry();

    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
        {
            //
            // Reference:
            //
            //   Physically Based Rendering, first edition, pp. 128-129
            //

            const Vector3d& n = get_original_shading_normal();

            const double du0 = static_cast<double>(m_v0_uv[0] - m_v2_uv[0]);
            const double dv0 = static_cast<double>(m_v0_uv[1] - m_v2_uv[1]);
            const double du1 = static_cast<double>(m_v1_uv[0] - m_v2_uv[0]);
            const double dv1 = static_cast<double>(m_v1_uv[1] - m_v2_uv[1]);
            const double det = dv1 * du0 - dv0 * du1;

            if (det != 0.0)
            {
                const double rcp_det = 1.0 / det;

                const Vector3d& v2 = get_vertex(2);
                const Vector3d dp0 = get_vertex(0) - v2;
                const Vector3d dp1 = get_vertex(1) - v2;

                m_dpdu = (dv1 * dp0 - dv0 * dp1) * rcp_det;
                m_dpdv = (du0 * dp1 - du1 * dp0) * rcp_det;

                //
                // Subtract the component of dPdu (resp. dPdv) that is not orthogonal to the shading normal.
                // Assuming that the geometric and shading normals don't differ excessively, this leads to a
                // very negligible shortening of dPdu (resp. dPdv).
                //
                // A length-preserving but more costly approach is as follow:
                //
                //   m_dpdu = normalize(cross(n, cross(m_dpdu, n))) * norm(m_dpdu);
                //   m_dpdv = normalize(cross(n, cross(m_dpdv, n))) * norm(m_dpdv);
                //

                m_dpdu -= dot(m_dpdu, n) * n;
                m_dpdv -= dot(m_dpdv, n) * n;

                if (m_members & HasTriangleVertexNormals)
                {
                    const Vector3d dn0(m_n0 - m_n2);
                    const Vector3d dn1(m_n1 - m_n2);

                    m_dndu = (dv1 * dn0 - dv0 * dn1) * rcp_det;
                    m_dndv = (du0 * dn1 - du1 * dn0) * rcp_det;

                    // Transform the normal derivatives to world space.
                    const Transformd& obj_instance_transform = m_object_instance->get_transform();
                    m_dndu =
                        m_assembly_instance_transform.normal_to_parent(
                            obj_instance_transform.normal_to_parent(m_dndu));
                    m_dndv =
                        m_assembly_instance_transform.normal_to_parent(
                            obj_instance_transform.normal_to_parent(m_dndv));
                }
                else
                {
                    m_dndu = m_dndv = Vector3d(0.0);
                }
            }
            else
            {
                const Basis3d basis(n);
                m_dpdu = basis.get_tangent_u();
                m_dpdv = basis.get_tangent_v();
                m_dndu = m_dndv = Vector3d(0.0);
            }
        }
        break;

      case PrimitiveProceduralSurface:
        {
            // todo: fix.
            const Basis3d basis(m_original_shading_normal);
            m_dpdu = basis.get_tangent_u();
            m_dpdv = basis.get_tangent_v();
            m_dndu = m_dndv = Vector3d(0.0);
        }
        break;

      case PrimitiveCurve1:
      case PrimitiveCurve3:
        {
            assert(is_curve_primitive());

            const GScalar v = m_bary[1];

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
        break;

      assert_otherwise;
    }
}

void ShadingPoint::compute_screen_space_partial_derivatives() const
{
    //
    // Reference:
    //
    //   Physically Based Rendering, second edition, pp. 506-509
    //

    if (!m_ray.m_has_differentials)
    {
        m_dpdx = Vector3d(0.0);
        m_dpdy = Vector3d(0.0);
        m_duvdx = Vector2f(0.0f);
        m_duvdy = Vector2f(0.0f);
        return;
    }

    const Vector3d& p = get_point();
    const Vector3d& n = get_original_shading_normal();

    double tx, ty;
    if (!intersect_ray_plane(m_ray.m_rx_org, m_ray.m_rx_dir, p, n, tx) ||
        !intersect_ray_plane(m_ray.m_ry_org, m_ray.m_ry_dir, p, n, ty))
    {
        m_dpdx = Vector3d(0.0);
        m_dpdy = Vector3d(0.0);
        m_duvdx = Vector2f(0.0f);
        m_duvdy = Vector2f(0.0f);
        return;
    }

    m_dpdx = m_ray.m_rx_org + tx * m_ray.m_rx_dir - p;
    m_dpdy = m_ray.m_ry_org + ty * m_ray.m_ry_dir - p;

    // Select the two axes along which the normal has the smallest components.
    static const size_t Axes[3][2] = { {1, 2}, {0, 2}, {0, 1} };
    const size_t max_index = max_abs_index(n);
    const size_t axis0 = Axes[max_index][0];
    const size_t axis1 = Axes[max_index][1];

    const Vector3d& dpdu = get_dpdu(0);
    const Vector3d& dpdv = get_dpdv(0);

    const Vector2f plane_dpdu(
        static_cast<float>(dpdu[axis0]),
        static_cast<float>(dpdu[axis1]));
    const Vector2f plane_dpdv(
        static_cast<float>(dpdv[axis0]),
        static_cast<float>(dpdv[axis1]));

    const float d = det(plane_dpdu, plane_dpdv);

    if (d == 0.0f)
    {
        m_duvdx = Vector2f(0.0f);
        m_duvdy = Vector2f(0.0f);
        return;
    }

    const Vector2f plane_dpdx(
        static_cast<float>(m_dpdx[axis0]),
        static_cast<float>(m_dpdx[axis1]));
    const Vector2f plane_dpdy(
        static_cast<float>(m_dpdy[axis0]),
        static_cast<float>(m_dpdy[axis1]));

    const float rcp_d = 1.0f / d;

    m_duvdx[0] = det(plane_dpdx, plane_dpdv) * rcp_d;   // dudx
    m_duvdx[1] = det(plane_dpdu, plane_dpdx) * rcp_d;   // dvdx
    m_duvdy[0] = det(plane_dpdy, plane_dpdv) * rcp_d;   // dudy
    m_duvdy[1] = det(plane_dpdu, plane_dpdy) * rcp_d;   // dvdy
}

void ShadingPoint::compute_normals() const
{
    cache_source_geometry();

    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
        compute_triangle_normals();
        break;

      case PrimitiveProceduralSurface:
        // Nothing to do.
        break;

      case PrimitiveCurve1:
      case PrimitiveCurve3:
        compute_curve_normals();
        break;

      assert_otherwise;
    }

    //
    // Determine which side of the geometric surface we hit, and flip the normals.
    //
    // In order to shade front and back sides indifferently, we need the geometric
    // normal to be facing the incoming ray and the shading normals (original and
    // modified) to be in the same hemisphere as the geometric normal.
    //

    // If we have per-vertex normals, the shading normal decides which side is front.
    // Since we must compute which side we hit by checking the geometric normal,
    // place the geometric normal in the same hemisphere as the shading normal.
    if ((m_members & HasTriangleVertexNormals) &&
        dot(m_geometric_normal, m_original_shading_normal) < 0.0)
        m_geometric_normal = -m_geometric_normal;

    // Store which side of the geometric surface we hit.
    const bool back = dot(m_ray.m_dir, m_geometric_normal) > 0.0;
    m_side = back ^ m_object_instance->must_flip_normals()
        ? ObjectInstance::BackSide
        : ObjectInstance::FrontSide;

    // Make the geometric normal face the direction of the incoming ray.
    if (back)
        m_geometric_normal = -m_geometric_normal;

    // Place the unperturbed shading normal in the same hemisphere as the geometric normal.
    if (dot(m_original_shading_normal, m_geometric_normal) < 0.0)
        m_original_shading_normal = -m_original_shading_normal;
}

void ShadingPoint::compute_triangle_normals() const
{
    cache_source_geometry();

    //
    // Compute the geometric normal to the triangle.
    //

    if (m_members & HasWorldSpaceTriangleVertices)
    {
        // We already have the world space vertices of the hit triangle.
        // Use them to compute the geometric normal directly in world space.
        m_geometric_normal = compute_triangle_normal(m_v0_w, m_v1_w, m_v2_w);
    }
    else
    {
        // Compute the object instance space geometric normal.
        const Vector3d v0(m_v0);
        const Vector3d v1(m_v1);
        const Vector3d v2(m_v2);
        m_geometric_normal = compute_triangle_normal(v0, v1, v2);

        // Transform the geometric normal to world space.
        m_geometric_normal =
            m_assembly_instance_transform.normal_to_parent(
                m_object_instance->get_transform().normal_to_parent(m_geometric_normal));
    }

    m_geometric_normal = normalize(m_geometric_normal);

    //
    // Compute the unperturbed shading normal.
    //

    if (m_members & HasTriangleVertexNormals)
    {
        // Compute the object instance space shading normal.
        m_original_shading_normal =
              Vector3d(m_n0) * static_cast<double>(1.0 - m_bary[0] - m_bary[1])
            + Vector3d(m_n1) * static_cast<double>(m_bary[0])
            + Vector3d(m_n2) * static_cast<double>(m_bary[1]);

        // Transform the shading normal to world space.
        m_original_shading_normal =
            m_assembly_instance_transform.normal_to_parent(
                m_object_instance->get_transform().normal_to_parent(m_original_shading_normal));

        m_original_shading_normal = normalize(m_original_shading_normal);
    }
    else
    {
        // Use the geometric normal if per-vertex normals are absent.
        m_original_shading_normal = m_geometric_normal;
    }
}

void ShadingPoint::compute_curve_normals() const
{
    // We assume flat ribbons facing incoming rays.
    m_geometric_normal = m_original_shading_normal = normalize(-m_ray.m_dir);
}

void ShadingPoint::compute_shading_basis() const
{
    const Material* material = get_material();

    // Compute the first tangent.
    Vector3d tangent;
    const Transformd& object_instance_transform = m_object_instance->get_transform();
    if ((m_members & HasTriangleVertexTangents) != 0)
    {
        // Explicit per-vertex tangents.
        tangent =
            m_assembly_instance_transform.vector_to_parent(
                object_instance_transform.vector_to_parent(
                      Vector3d(m_t0) * static_cast<double>(1.0f - m_bary[0] - m_bary[1])
                    + Vector3d(m_t1) * static_cast<double>(m_bary[0])
                    + Vector3d(m_t2) * static_cast<double>(m_bary[1])));
    }
    else if (material == nullptr ||
             material->get_render_data().m_default_tangent_mode == Material::RenderData::DefaultTangentMode::Radial)
    {
        // Radial default tangent mode.
        tangent =
            get_point() -
            m_assembly_instance_transform.point_to_parent(
                object_instance_transform.get_parent_origin());
    }
    else
    {
        // UV/X+/Y+/Z+ default tangent modes.
        switch (material->get_render_data().m_default_tangent_mode)
        {
          case Material::RenderData::DefaultTangentMode::UV:
            tangent = normalize(get_dpdu(0));
            break;

          case Material::RenderData::DefaultTangentMode::LocalX:
            tangent = m_assembly_instance_transform.vector_to_parent(object_instance_transform.get_parent_x());
            break;

          case Material::RenderData::DefaultTangentMode::LocalY:
            tangent = m_assembly_instance_transform.vector_to_parent(object_instance_transform.get_parent_y());
            break;

          case Material::RenderData::DefaultTangentMode::LocalZ:
            tangent = m_assembly_instance_transform.vector_to_parent(object_instance_transform.get_parent_z());
            break;
        }
    }

    if (m_primitive_type == PrimitiveCurve3)
    {
        // Contruct shading basis for hair BSDF.
        // todo: add flag to differentiate curves from hair.
        const Vector3d x = normalize(get_dpdu(0));
        const Vector3d sn_curve = get_original_shading_normal();
        const Vector3d z = normalize(cross(sn_curve, x));
        const Vector3d y = cross(z, x);
        m_shading_basis.build(y, x, z);
    }
    else
    {
        // Construct an orthonormal basis.
        const Vector3d& sn = get_original_shading_normal();
        const Vector3d bitangent = cross(tangent, sn);
        const double norm_bitangent = norm(bitangent);
        if (norm_bitangent >= 1.0e-6)
        {
            const Vector3d t = bitangent / norm_bitangent;
            const Vector3d s = cross(sn, t);
            m_shading_basis.build(sn, s, t);
        }
        else
        {
            // Fall back to arbitrary tangents if the tangent and the shading normal are colinear.
            m_shading_basis.build(sn);
        }
    }

    // Apply the basis modifier if the material has one.
    if (material != nullptr)
    {
        const Material::RenderData& material_data = material->get_render_data();
        if (material_data.m_basis_modifier)
        {
            m_shading_basis =
                material_data.m_basis_modifier->modify(
                    *m_texture_cache,
                    m_shading_basis,
                    *this);
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
        // Retrieve the tessellation of the mesh.
        const MeshObject& mesh = static_cast<const MeshObject&>(*m_object);
        const StaticTriangleTess& tess = mesh.get_static_triangle_tess();

        // Retrieve the triangle.
        const Triangle& triangle = tess.m_primitives[m_primitive_index];
        assert(triangle.m_v0 != Triangle::None);
        assert(triangle.m_v1 != Triangle::None);
        assert(triangle.m_v2 != Triangle::None);

        const size_t motion_segment_count = tess.get_motion_segment_count();
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
            const float v = m_bary[0];
            const float w = m_bary[1];
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
        const Camera* camera = m_scene->get_render_data().m_active_camera;
        Transformd scratch;

        const Transformd& assembly_instance_transform0 =
            m_assembly_instance_transform_seq->evaluate(
                camera->get_shutter_open_begin_time(),
                scratch);
        p0 = assembly_instance_transform0.point_to_parent(p0);

        const Transformd& assembly_instance_transform1 =
            m_assembly_instance_transform_seq->evaluate(
                camera->get_shutter_close_end_time(),
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

    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
      case PrimitiveProceduralSurface:
        {
            if (const Source* alpha_map = get_object().get_render_data().m_alpha_map)
            {
                Alpha a;
                alpha_map->evaluate(*m_texture_cache, SourceInputs(get_uv(0)), a);
                m_alpha *= a;
            }

            if (const Material* material = get_material())
            {
                const Material::RenderData& material_data = material->get_render_data();
                if (material_data.m_alpha_map)
                {
                    Alpha a;
                    material_data.m_alpha_map->evaluate(*m_texture_cache, SourceInputs(get_uv(0)), a);
                    m_alpha *= a;
                }
            }
        }
        break;

      case PrimitiveCurve1:
        {
            assert(is_curve_primitive());
            const GScalar v = m_bary[1];
            const CurveObject *curves = static_cast<const CurveObject *>(&get_object());
            m_alpha *= Alpha(curves->get_curve1(m_primitive_index).evaluate_opacity(v));
        }
        break;

      case PrimitiveCurve3:
        {
            assert(is_curve_primitive());
            const GScalar v = m_bary[1];
            const CurveObject *curves = static_cast<const CurveObject *>(&get_object());
            m_alpha *= Alpha(curves->get_curve3(m_primitive_index).evaluate_opacity(v));
        }
        break;

      assert_otherwise;
    }
}

void ShadingPoint::compute_per_vertex_color() const
{
    switch (m_primitive_type)
    {
      case PrimitiveTriangle:
      case PrimitiveProceduralSurface:
        m_color.set(1.0f);
        break;

      case PrimitiveCurve1:
        {
            const GScalar v = m_bary[1];
            const CurveObject* curves = static_cast<const CurveObject*>(&get_object());
            m_color = Color3f(curves->get_curve1(m_primitive_index).evaluate_color(v));
        }
        break;

      case PrimitiveCurve3:
        {
            const GScalar v = m_bary[1];
            const CurveObject* curves = static_cast<const CurveObject*>(&get_object());
            m_color = Color3f(curves->get_curve3(m_primitive_index).evaluate_color(v));
        }
        break;

      assert_otherwise;
    }
}

void ShadingPoint::initialize_osl_shader_globals(
    const ShaderGroup&          sg,
    const VisibilityFlags::Type ray_flags,
    OSL::RendererServices*      renderer) const
{
    assert(hit_surface());
    assert(renderer);

    if (!(m_members & HasOSLShaderGlobals))
    {
        assert(is_normalized(m_ray.m_dir));

        // Surface position and incident ray direction.
        m_shader_globals.P = Vector3f(get_point());
        m_shader_globals.I = Vector3f(m_ray.m_dir);

        m_shader_globals.flipHandedness =
            m_assembly_instance_transform_seq->swaps_handedness(m_assembly_instance_transform) !=
            get_object_instance().get_render_data().m_transform_swaps_handedness ? 1 : 0;

        // Surface position and incident ray direction differentials.
        if (m_ray.m_has_differentials)
        {
            m_shader_globals.dPdx = Vector3f(get_dpdx());
            m_shader_globals.dPdy = Vector3f(get_dpdy());
            m_shader_globals.dPdz = Vector3f(0.0);
            m_shader_globals.dIdx = Vector3f(m_ray.m_rx_dir - m_ray.m_dir);
            m_shader_globals.dIdy = Vector3f(m_ray.m_ry_dir - m_ray.m_dir);
        }
        else
        {
            m_shader_globals.dPdx = Vector3f(0.0f);
            m_shader_globals.dPdy = Vector3f(0.0f);
            m_shader_globals.dPdz = Vector3f(0.0f);
            m_shader_globals.dIdx = Vector3f(0.0f);
            m_shader_globals.dIdy = Vector3f(0.0f);
        }

        // Shading and geometric normals and backfacing flag.
        m_shader_globals.N = Vector3f(get_shading_normal());
        m_shader_globals.Ng = Vector3f(get_geometric_normal());
        m_shader_globals.backfacing = get_side() == ObjectInstance::FrontSide ? 0 : 1;

        // Surface parameters and their differentials.
        const Vector2f& uv = get_uv(0);
        m_shader_globals.u = uv[0];
        m_shader_globals.v = uv[1];
        if (m_ray.m_has_differentials)
        {
            const Vector2f& duvdx = get_duvdx(0);
            const Vector2f& duvdy = get_duvdy(0);
            m_shader_globals.dudx = duvdx[0];
            m_shader_globals.dudy = duvdy[0];
            m_shader_globals.dvdx = duvdx[1];
            m_shader_globals.dvdy = duvdy[1];
        }
        else
        {
            m_shader_globals.dudx = 0.0f;
            m_shader_globals.dudy = 0.0f;
            m_shader_globals.dvdx = 0.0f;
            m_shader_globals.dvdy = 0.0f;
        }

        // Surface tangents.
        m_shader_globals.dPdu = Vector3f(get_dpdu(0));
        m_shader_globals.dPdv = Vector3f(get_dpdv(0));

        // Time and its derivative.
        m_shader_globals.time = m_ray.m_time.m_absolute;
        m_shader_globals.dtime = m_scene->get_render_data().m_active_camera->get_shutter_time_interval();

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
        m_shader_globals.objdata = nullptr;

        // Pointer to the RendererServices object.
        m_shader_globals.renderer = renderer;

        // Transformations.
        m_obj_transform_info.m_assembly_instance_transform = m_assembly_instance_transform_seq;
        m_obj_transform_info.m_object_instance_transform = &m_object_instance->get_transform();
        m_shader_globals.object2common = reinterpret_cast<OSL::TransformationPtr>(&m_obj_transform_info);
        m_shader_globals.shader2common = nullptr;

        m_members |= HasOSLShaderGlobals;
    }

    // Always update the ray type flags.
    m_shader_globals.raytype = static_cast<int>(ray_flags);

    // Always update the surface area of emissive objects.
    m_shader_globals.surfacearea =
        ray_flags == VisibilityFlags::LightRay && sg.has_emission()
            ? sg.get_surface_area(&get_assembly_instance(), &get_object_instance())
            : 0.0f;

    // Output closure.
    m_shader_globals.Ci = nullptr;
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

}   // namespace renderer

namespace foundation
{

template <>
class PoisonImpl<OSL::Vec3>
{
  public:
    static void do_poison(OSL::Vec3& v)
    {
        always_poison(v.x);
        always_poison(v.y);
        always_poison(v.z);
    }
};

void PoisonImpl<renderer::ShadingPoint>::do_poison(renderer::ShadingPoint& point)
{
    always_poison(point.m_texture_cache);
    always_poison(point.m_scene);
    always_poison(point.m_ray);

    always_poison(point.m_primitive_type);
    always_poison(point.m_bary);
    always_poison(point.m_assembly_instance);
    always_poison(point.m_assembly_instance_transform);
    always_poison(point.m_assembly_instance_transform_seq);
    always_poison(point.m_object_instance_index);
    always_poison(point.m_primitive_index);
    always_poison(point.m_triangle_support_plane);

    always_poison(point.m_members);

    always_poison(point.m_assembly);
    always_poison(point.m_object_instance);
    always_poison(point.m_object);
    always_poison(point.m_primitive_pa);
    always_poison(point.m_v0_uv);
    always_poison(point.m_v1_uv);
    always_poison(point.m_v2_uv);
    always_poison(point.m_v0);
    always_poison(point.m_v1);
    always_poison(point.m_v2);
    always_poison(point.m_n0);
    always_poison(point.m_n1);
    always_poison(point.m_n2);
    always_poison(point.m_t0);
    always_poison(point.m_t1);
    always_poison(point.m_t2);

    always_poison(point.m_uv);
    always_poison(point.m_duvdx);
    always_poison(point.m_duvdy);
    always_poison(point.m_point);
    always_poison(point.m_dpdu);
    always_poison(point.m_dpdv);
    always_poison(point.m_dndu);
    always_poison(point.m_dndv);
    always_poison(point.m_dpdx);
    always_poison(point.m_dpdy);
    always_poison(point.m_geometric_normal);
    always_poison(point.m_original_shading_normal);
    always_poison(point.m_shading_basis);
    always_poison(point.m_side);
    always_poison(point.m_v0_w);
    always_poison(point.m_v1_w);
    always_poison(point.m_v2_w);
    always_poison(point.m_point_velocity);
    always_poison(point.m_material);
    always_poison(point.m_opposite_material);
    always_poison(point.m_alpha);
    always_poison(point.m_color);

    always_poison(point.m_refine_space_geo_normal);
    always_poison(point.m_refine_space_front_point);
    always_poison(point.m_refine_space_back_point);

    always_poison(point.m_obj_transform_info.m_assembly_instance_transform);
    always_poison(point.m_obj_transform_info.m_object_instance_transform);

    always_poison(point.m_osl_trace_data.m_traced);
    always_poison(point.m_osl_trace_data.m_hit);
    always_poison(point.m_osl_trace_data.m_hit_distance);
    always_poison(point.m_osl_trace_data.m_P);
    always_poison(point.m_osl_trace_data.m_N);
    always_poison(point.m_osl_trace_data.m_Ng);
    always_poison(point.m_osl_trace_data.m_u);
    always_poison(point.m_osl_trace_data.m_v);

    always_poison(point.m_shader_globals.P);
    always_poison(point.m_shader_globals.dPdx);
    always_poison(point.m_shader_globals.dPdy);
    always_poison(point.m_shader_globals.dPdz);
    always_poison(point.m_shader_globals.I);
    always_poison(point.m_shader_globals.dIdx);
    always_poison(point.m_shader_globals.dIdy);
    always_poison(point.m_shader_globals.N);
    always_poison(point.m_shader_globals.Ng);
    always_poison(point.m_shader_globals.u);
    always_poison(point.m_shader_globals.dudx);
    always_poison(point.m_shader_globals.dudy);
    always_poison(point.m_shader_globals.v);
    always_poison(point.m_shader_globals.dvdx);
    always_poison(point.m_shader_globals.dvdy);
    always_poison(point.m_shader_globals.dPdu);
    always_poison(point.m_shader_globals.dPdv);
    always_poison(point.m_shader_globals.time);
    always_poison(point.m_shader_globals.dtime);
    always_poison(point.m_shader_globals.dPdtime);
    always_poison(point.m_shader_globals.Ps);
    always_poison(point.m_shader_globals.dPsdx);
    always_poison(point.m_shader_globals.dPsdy);
    always_poison(point.m_shader_globals.renderstate);
    always_poison(point.m_shader_globals.tracedata);
    always_poison(point.m_shader_globals.objdata);
    always_poison(point.m_shader_globals.context);
    always_poison(point.m_shader_globals.renderer);
    always_poison(point.m_shader_globals.object2common);
    always_poison(point.m_shader_globals.shader2common);
    always_poison(point.m_shader_globals.Ci);
    always_poison(point.m_shader_globals.surfacearea);
    always_poison(point.m_shader_globals.raytype);
    always_poison(point.m_shader_globals.flipHandedness);
    always_poison(point.m_shader_globals.backfacing);

    always_poison(point.m_surface_shader_diffuse);
    always_poison(point.m_surface_shader_glossy);
    always_poison(point.m_surface_shader_emission);
}

}   // namespace foundation
