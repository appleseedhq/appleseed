
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "meshobjectoperations.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/triangle.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

void compute_smooth_vertex_normals_base_pose(MeshObject& object)
{
    assert(object.get_vertex_normal_count() == 0);

    const size_t vertex_count = object.get_vertex_count();
    const size_t triangle_count = object.get_triangle_count();

    vector<GVector3> normals(vertex_count, GVector3(0.0));

    for (size_t i = 0; i < triangle_count; ++i)
    {
        Triangle& triangle = object.get_triangle(i);
        triangle.m_n0 = triangle.m_v0;
        triangle.m_n1 = triangle.m_v1;
        triangle.m_n2 = triangle.m_v2;

        const GVector3& v0 = object.get_vertex(triangle.m_v0);
        const GVector3& v1 = object.get_vertex(triangle.m_v1);
        const GVector3& v2 = object.get_vertex(triangle.m_v2);
        const GVector3 normal = normalize(compute_triangle_normal(v0, v1, v2));

        normals[triangle.m_v0] += normal;
        normals[triangle.m_v1] += normal;
        normals[triangle.m_v2] += normal;
    }

    object.reserve_vertex_normals(vertex_count);

    for (size_t i = 0; i < vertex_count; ++i)
        object.push_vertex_normal(safe_normalize(normals[i]));
}

void compute_smooth_vertex_normals_pose(MeshObject& object, const size_t motion_segment_index)
{
    const size_t vertex_count = object.get_vertex_count();
    const size_t triangle_count = object.get_triangle_count();

    vector<GVector3> normals(vertex_count, GVector3(0.0));

    for (size_t i = 0; i < triangle_count; ++i)
    {
        const Triangle& triangle = object.get_triangle(i);

        const GVector3& v0 = object.get_vertex_pose(triangle.m_v0, motion_segment_index);
        const GVector3& v1 = object.get_vertex_pose(triangle.m_v1, motion_segment_index);
        const GVector3& v2 = object.get_vertex_pose(triangle.m_v2, motion_segment_index);
        const GVector3 normal = normalize(compute_triangle_normal(v0, v1, v2));

        normals[triangle.m_v0] += normal;
        normals[triangle.m_v1] += normal;
        normals[triangle.m_v2] += normal;
    }

    for (size_t i = 0; i < vertex_count; ++i)
        object.set_vertex_normal_pose(i, motion_segment_index, safe_normalize(normals[i]));
}

void compute_smooth_vertex_normals(MeshObject& object)
{
    compute_smooth_vertex_normals_base_pose(object);

    for (size_t i = 0; i < object.get_motion_segment_count(); ++i)
        compute_smooth_vertex_normals_pose(object, i);
}

void compute_smooth_vertex_tangents_base_pose(MeshObject& object)
{
    assert(object.get_vertex_tangent_count() == 0);
    assert(object.get_tex_coords_count() > 0);

    const size_t vertex_count = object.get_vertex_count();
    const size_t triangle_count = object.get_triangle_count();

    vector<GVector3> tangents(vertex_count, GVector3(0.0));

    for (size_t i = 0; i < triangle_count; ++i)
    {
        const Triangle& triangle = object.get_triangle(i);

        if (!triangle.has_vertex_attributes())
            continue;

        const GVector2 v0_uv = object.get_tex_coords(triangle.m_a0);
        const GVector2 v1_uv = object.get_tex_coords(triangle.m_a1);
        const GVector2 v2_uv = object.get_tex_coords(triangle.m_a2);

        //
        // Reference:
        //
        //   Physically Based Rendering, first edition, pp. 128-129
        //

        const GScalar du0 = v0_uv[0] - v2_uv[0];
        const GScalar dv0 = v0_uv[1] - v2_uv[1];
        const GScalar du1 = v1_uv[0] - v2_uv[0];
        const GScalar dv1 = v1_uv[1] - v2_uv[1];
        const GScalar det = du0 * dv1 - dv0 * du1;

        if (det == GScalar(0.0))
            continue;

        const GVector3& v2 = object.get_vertex(triangle.m_v2);
        const GVector3 dp0 = object.get_vertex(triangle.m_v0) - v2;
        const GVector3 dp1 = object.get_vertex(triangle.m_v1) - v2;
        const GVector3 tangent = normalize(dv1 * dp0 - dv0 * dp1);

        tangents[triangle.m_v0] += tangent;
        tangents[triangle.m_v1] += tangent;
        tangents[triangle.m_v2] += tangent;
    }

    object.reserve_vertex_tangents(vertex_count);

    for (size_t i = 0; i < vertex_count; ++i)
        object.push_vertex_tangent(safe_normalize(tangents[i]));
}

void compute_smooth_vertex_tangents_pose(MeshObject& object, const size_t motion_segment_index)
{
    assert(object.get_tex_coords_count() > 0);

    const size_t vertex_count = object.get_vertex_count();
    const size_t triangle_count = object.get_triangle_count();

    vector<GVector3> tangents(vertex_count, GVector3(0.0));

    for (size_t i = 0; i < triangle_count; ++i)
    {
        const Triangle& triangle = object.get_triangle(i);

        if (!triangle.has_vertex_attributes())
            continue;

        const GVector2 v0_uv = object.get_tex_coords(triangle.m_a0);
        const GVector2 v1_uv = object.get_tex_coords(triangle.m_a1);
        const GVector2 v2_uv = object.get_tex_coords(triangle.m_a2);

        //
        // Reference:
        //
        //   Physically Based Rendering, first edition, pp. 128-129
        //

        const GScalar du0 = v0_uv[0] - v2_uv[0];
        const GScalar dv0 = v0_uv[1] - v2_uv[1];
        const GScalar du1 = v1_uv[0] - v2_uv[0];
        const GScalar dv1 = v1_uv[1] - v2_uv[1];
        const GScalar det = du0 * dv1 - dv0 * du1;

        if (det == GScalar(0.0))
            continue;

        const GVector3& v2 = object.get_vertex_pose(triangle.m_v2, motion_segment_index);
        const GVector3 dp0 = object.get_vertex_pose(triangle.m_v0, motion_segment_index) - v2;
        const GVector3 dp1 = object.get_vertex_pose(triangle.m_v1, motion_segment_index) - v2;
        const GVector3 tangent = normalize(dv1 * dp0 - dv0 * dp1);

        tangents[triangle.m_v0] += tangent;
        tangents[triangle.m_v1] += tangent;
        tangents[triangle.m_v2] += tangent;
    }

    for (size_t i = 0; i < vertex_count; ++i)
        object.set_vertex_tangent_pose(i, motion_segment_index, safe_normalize(tangents[i]));
}

void compute_smooth_vertex_tangents(MeshObject& object)
{
    compute_smooth_vertex_tangents_base_pose(object);

    for (size_t i = 0; i < object.get_motion_segment_count(); ++i)
        compute_smooth_vertex_tangents_pose(object, i);
}

}   // namespace renderer
