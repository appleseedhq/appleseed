
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
#include "triangleitemhandler.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/trianglevertexinfo.h"

// appleseed.foundation headers.
#include "foundation/math/intersection/aabbtriangle.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <cassert>
#include <limits>

using namespace foundation;

namespace renderer
{

TriangleItemHandler::TriangleItemHandler(
    const std::vector<TriangleVertexInfo>&   triangle_vertex_infos,
    const std::vector<GVector3>&             triangle_vertices,
    const std::vector<AABB3d>&               triangle_bboxes)
  : m_triangle_vertex_infos(triangle_vertex_infos)
  , m_triangle_vertices(triangle_vertices)
  , m_triangle_bboxes(triangle_bboxes)
{
}

double TriangleItemHandler::get_bbox_grow_eps() const
{
    return 2.0e-9;
}

AABB3d TriangleItemHandler::clip(
    const size_t                        item_index,
    const size_t                        dimension,
    const double                        slab_min,
    const double                        slab_max) const
{
    const TriangleVertexInfo& vertex_info = m_triangle_vertex_infos[item_index];

    if (vertex_info.m_motion_segment_count > 0)
    {
        AABB3d triangle_bbox = m_triangle_bboxes[item_index];

        if (triangle_bbox.min[dimension] < slab_min)
            triangle_bbox.min[dimension] = slab_min;

        if (triangle_bbox.max[dimension] > slab_max)
            triangle_bbox.max[dimension] = slab_max;

        return triangle_bbox;
    }

#ifdef APPLESEED_USE_SSE

    APPLESEED_SIMD4_ALIGN const Vector3d v0(m_triangle_vertices[vertex_info.m_vertex_index + 0]);
    APPLESEED_SIMD4_ALIGN const Vector3d v1(m_triangle_vertices[vertex_info.m_vertex_index + 1]);
    APPLESEED_SIMD4_ALIGN const Vector3d v2(m_triangle_vertices[vertex_info.m_vertex_index + 2]);

    const double v0d = v0[dimension];
    const double v1d = v1[dimension];
    const double v2d = v2[dimension];

    const int v0_ge_min = v0d >= slab_min ? 1 : 0;
    const int v0_le_max = v0d <= slab_max ? 1 : 0;
    const int v1_ge_min = v1d >= slab_min ? 1 : 0;
    const int v1_le_max = v1d <= slab_max ? 1 : 0;
    const int v2_ge_min = v2d >= slab_min ? 1 : 0;
    const int v2_le_max = v2d <= slab_max ? 1 : 0;

    __m128d bbox_min_xy = _mm_set1_pd(+std::numeric_limits<double>::max());
    __m128d bbox_min_zz = _mm_set1_pd(+std::numeric_limits<double>::max());
    __m128d bbox_max_xy = _mm_set1_pd(-std::numeric_limits<double>::max());
    __m128d bbox_max_zz = _mm_set1_pd(-std::numeric_limits<double>::max());

    const __m128d v0_xy = _mm_load_pd(&v0.x);
    const __m128d v0_zz = _mm_set1_pd(v0.z);
    const __m128d v1_xy = _mm_load_pd(&v1.x);
    const __m128d v1_zz = _mm_set1_pd(v1.z);
    const __m128d v2_xy = _mm_load_pd(&v2.x);
    const __m128d v2_zz = _mm_set1_pd(v2.z);

    if (v0_ge_min & v0_le_max)
    {
        bbox_min_xy = _mm_min_pd(bbox_min_xy, v0_xy);
        bbox_max_xy = _mm_max_pd(bbox_max_xy, v0_xy);
        bbox_min_zz = _mm_min_pd(bbox_min_zz, v0_zz);
        bbox_max_zz = _mm_max_pd(bbox_max_zz, v0_zz);
    }

    if (v1_ge_min & v1_le_max)
    {
        bbox_min_xy = _mm_min_pd(bbox_min_xy, v1_xy);
        bbox_max_xy = _mm_max_pd(bbox_max_xy, v1_xy);
        bbox_min_zz = _mm_min_pd(bbox_min_zz, v1_zz);
        bbox_max_zz = _mm_max_pd(bbox_max_zz, v1_zz);
    }

    if (v2_ge_min & v2_le_max)
    {
        bbox_min_xy = _mm_min_pd(bbox_min_xy, v2_xy);
        bbox_max_xy = _mm_max_pd(bbox_max_xy, v2_xy);
        bbox_min_zz = _mm_min_pd(bbox_min_zz, v2_zz);
        bbox_max_zz = _mm_max_pd(bbox_max_zz, v2_zz);
    }

    const int v0v1_cross_min = v0_ge_min ^ v1_ge_min;
    const int v0v1_cross_max = v0_le_max ^ v1_le_max;
    const int v1v2_cross_min = v1_ge_min ^ v2_ge_min;
    const int v1v2_cross_max = v1_le_max ^ v2_le_max;
    const int v2v0_cross_min = v2_ge_min ^ v0_ge_min;
    const int v2v0_cross_max = v2_le_max ^ v0_le_max;

    if (v0v1_cross_min | v0v1_cross_max)
    {
        const double rcp_v0v1 = 1.0 / (v1[dimension] - v0[dimension]);

        if (v0v1_cross_min)
        {
            const double t = (slab_min - v0[dimension]) * rcp_v0v1;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v0_xy, mt1), _mm_mul_pd(v1_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v0_zz, mt1), _mm_mul_pd(v1_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }

        if (v0v1_cross_max)
        {
            const double t = (slab_max - v0[dimension]) * rcp_v0v1;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v0_xy, mt1), _mm_mul_pd(v1_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v0_zz, mt1), _mm_mul_pd(v1_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }
    }

    if (v1v2_cross_min | v1v2_cross_max)
    {
        const double rcp_v1v2 = 1.0 / (v2[dimension] - v1[dimension]);

        if (v1v2_cross_min)
        {
            const double t = (slab_min - v1[dimension]) * rcp_v1v2;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v1_xy, mt1), _mm_mul_pd(v2_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v1_zz, mt1), _mm_mul_pd(v2_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }

        if (v1v2_cross_max)
        {
            const double t = (slab_max - v1[dimension]) * rcp_v1v2;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v1_xy, mt1), _mm_mul_pd(v2_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v1_zz, mt1), _mm_mul_pd(v2_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }
    }

    if (v2v0_cross_min | v2v0_cross_max)
    {
        const double rcp_v2v0 = 1.0 / (v0[dimension] - v2[dimension]);

        if (v2v0_cross_min)
        {
            const double t = (slab_min - v2[dimension]) * rcp_v2v0;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v2_xy, mt1), _mm_mul_pd(v0_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v2_zz, mt1), _mm_mul_pd(v0_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }

        if (v2v0_cross_max)
        {
            const double t = (slab_max - v2[dimension]) * rcp_v2v0;
            assert(t >= 0.0 && t <= 1.0);

            const __m128d mt = _mm_set1_pd(t);
            const __m128d mt1 = _mm_set1_pd(1.0 - t);
            const __m128d p_xy = _mm_add_pd(_mm_mul_pd(v2_xy, mt1), _mm_mul_pd(v0_xy, mt));
            const __m128d p_zz = _mm_add_pd(_mm_mul_pd(v2_zz, mt1), _mm_mul_pd(v0_zz, mt));

            bbox_min_xy = _mm_min_pd(bbox_min_xy, p_xy);
            bbox_max_xy = _mm_max_pd(bbox_max_xy, p_xy);
            bbox_min_zz = _mm_min_pd(bbox_min_zz, p_zz);
            bbox_max_zz = _mm_max_pd(bbox_max_zz, p_zz);
        }
    }

    APPLESEED_SIMD4_ALIGN AABB3d bbox;

    _mm_store_pd(&bbox.min.x, bbox_min_xy);
    _mm_store_sd(&bbox.min.z, bbox_min_zz);
    _mm_storeu_pd(&bbox.max.x, bbox_max_xy);
    _mm_store_sd(&bbox.max.z, bbox_max_zz);

    if (bbox.min[dimension] < slab_min)
        bbox.min[dimension] = slab_min;

    if (bbox.max[dimension] > slab_max)
        bbox.max[dimension] = slab_max;

#else

    const Vector3d v0(m_triangle_vertices[vertex_info.m_vertex_index + 0]);
    const Vector3d v1(m_triangle_vertices[vertex_info.m_vertex_index + 1]);
    const Vector3d v2(m_triangle_vertices[vertex_info.m_vertex_index + 2]);

    const int v0_ge_min = v0[dimension] >= slab_min ? 1 : 0;
    const int v0_le_max = v0[dimension] <= slab_max ? 1 : 0;
    const int v1_ge_min = v1[dimension] >= slab_min ? 1 : 0;
    const int v1_le_max = v1[dimension] <= slab_max ? 1 : 0;
    const int v2_ge_min = v2[dimension] >= slab_min ? 1 : 0;
    const int v2_le_max = v2[dimension] <= slab_max ? 1 : 0;

    AABB3d bbox;
    bbox.invalidate();

    if (v0_ge_min & v0_le_max)
        bbox.insert(v0);

    if (v1_ge_min & v1_le_max)
        bbox.insert(v1);

    if (v2_ge_min & v2_le_max)
        bbox.insert(v2);

    if (v0_ge_min != v1_ge_min)
        bbox.insert(segment_plane_intersection(v0, v1, dimension, slab_min));

    if (v0_le_max != v1_le_max)
        bbox.insert(segment_plane_intersection(v0, v1, dimension, slab_max));

    if (v1_ge_min != v2_ge_min)
        bbox.insert(segment_plane_intersection(v1, v2, dimension, slab_min));

    if (v1_le_max != v2_le_max)
        bbox.insert(segment_plane_intersection(v1, v2, dimension, slab_max));

    if (v2_ge_min != v0_ge_min)
        bbox.insert(segment_plane_intersection(v2, v0, dimension, slab_min));

    if (v2_le_max != v0_le_max)
        bbox.insert(segment_plane_intersection(v2, v0, dimension, slab_max));

#endif

    return bbox;
}

bool TriangleItemHandler::intersect(
    const size_t                        item_index,
    const AABB3d&                       bbox) const
{
    const TriangleVertexInfo& vertex_info = m_triangle_vertex_infos[item_index];

    return
        vertex_info.m_motion_segment_count > 0
            ? AABB3d::overlap(bbox, m_triangle_bboxes[item_index])
            : foundation::intersect(
                    bbox,
                    Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 0]),
                    Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 1]),
                    Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 2]));
}

Vector3d TriangleItemHandler::segment_plane_intersection(
    const Vector3d&                     a,
    const Vector3d&                     b,
    const size_t                        d,
    const double                        x)
{
    const double ab = b[d] - a[d];

    if (ab == 0.0)
        return a;

    const double t = (x - a[d]) / ab;

    assert(t >= 0.0 && t <= 1.0);

    Vector3d result;
    result = a * (1.0 - t) + b * t;
    result[d] = x;

    return result;
}

}   // namespace renderer
