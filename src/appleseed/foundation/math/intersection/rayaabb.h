
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/minmax.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// 3D ray-AABB intersection and clipping functions.
//
// References:
//
//   An Efficient and Robust Ray-Box Intersection Algorithm
//   Amy Williams, Steve Barrus, R. Keith Morley, Peter Shirley
//   jgt vol. 10 number 1, pp. 49-54, 2005.
//   http://www.cs.utah.edu/~awilliam/box/box.pdf
//
//   Scalar ray/AABB intersection code on Flipcode, by Thierry Berger-Perrin:
//   http://www.flipcode.com/archives/SSE_RayBox_Intersection_Test.shtml
//
//   Packet ray/AABB intersection code in Radius, by Thierry Berger-Perrin:
//   http://cvs.gna.org/cvsweb/radius/src/rt_render_packet.cc?rev=1.3;cvsroot=radius#l382
//
//   Robust BVH Ray Traversal
//   Thiago Ize, Solid Angle
//   http://jcgt.org/published/0002/02/02/paper.pdf
//
// todo: experiment with intersection predicates based on Plucker coordinates
// (reference: http://jgt.akpeters.com/papers/MahovskyWyvill04/).
//

// Test the intersection between a ray and a bounding box.
template <typename T>
bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox);

// Test the intersection between a ray and a bounding box.
// If the ray and the bounding box intersect, the distance
// to the closest intersection is returned in 'tmin'.
// Otherwise, 'tmin' is left unchanged.
template <typename T>
bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin);

// Test the intersection between a ray and a bounding box.
// If the ray and the bounding box intersect, the distance
// to both intersections is returned in 'tmin' and 'tmax'.
// Otherwise, 'tmin' and 'tmax' are left unchanged.
template <typename T>
bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin,
    T&                      tmax);

// Test the intersection between a ray and a bounding box.
// If the ray and the bounding box intersect, the distance
// to the closest intersection is returned in 'tmin' and
// the normal is returned in `normal`. Otherwise, 'tmin'
// and `normal` are left unchanged.
template <typename T>
bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin,
    Vector<T, 3>&           normal);

// Clip a ray to its intersection with a bounding box.
// If the ray and the bounding box intersect, the ray is
// clipped against the bounding box and true is returned.
// Otherwise, the ray is left unmodified and false is returned.
template <typename T>
bool clip(
    Ray<T, 3>&              ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox);


//
// 3D ray-AABB intersection and clipping functions implementation.
//

template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox)
{
    const T xl1 = ray_info.m_rcp_dir.x * (bbox[1 - ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl1 = ray_info.m_rcp_dir.y * (bbox[1 - ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl1 = ray_info.m_rcp_dir.z * (bbox[1 - ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T xl2 = ray_info.m_rcp_dir.x * (bbox[    ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox[    ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox[    ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T tmin = ssemax(zl1, ssemax(yl1, ssemax(xl1, ray.m_tmin)));
    const T tmax = ssemin(zl2, ssemin(yl2, ssemin(xl2, ray.m_tmax)));

    return !(tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax);
}

#ifdef APPLESEED_USE_SSE

template <>
inline bool intersect<float>(
    const Ray3f&            ray,
    const RayInfo3f&        ray_info,
    const AABB3f&           bbox)
{
    const __m128 org_x = _mm_set1_ps(ray.m_org.x);
    const __m128 org_y = _mm_set1_ps(ray.m_org.y);
    const __m128 org_z = _mm_set1_ps(ray.m_org.z);

    const __m128 rcp_dir_x = _mm_set1_ps(ray_info.m_rcp_dir.x);
    const __m128 rcp_dir_y = _mm_set1_ps(ray_info.m_rcp_dir.y);
    const __m128 rcp_dir_z = _mm_set1_ps(ray_info.m_rcp_dir.z);

    const __m128 xl1 = _mm_mul_ps(rcp_dir_x, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.x].x), org_x));
    const __m128 yl1 = _mm_mul_ps(rcp_dir_y, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.y].y), org_y));
    const __m128 zl1 = _mm_mul_ps(rcp_dir_z, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.z].z), org_z));

    const __m128 xl2 = _mm_mul_ps(rcp_dir_x, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.x].x), org_x));
    const __m128 yl2 = _mm_mul_ps(rcp_dir_y, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.y].y), org_y));
    const __m128 zl2 = _mm_mul_ps(rcp_dir_z, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.z].z), org_z));

    const __m128 ray_tmin = _mm_set1_ps(ray.m_tmin);
    const __m128 ray_tmax = _mm_set1_ps(ray.m_tmax);

    const __m128 tmin = _mm_max_ps(zl1, _mm_max_ps(yl1, _mm_max_ps(xl1, ray_tmin)));
    const __m128 tmax = _mm_min_ps(zl2, _mm_min_ps(yl2, _mm_min_ps(xl2, ray_tmax)));

    return
        _mm_movemask_ps(
            _mm_or_ps(
                _mm_cmpgt_ps(tmin, tmax),
                _mm_or_ps(
                    _mm_cmplt_ps(tmax, ray_tmin),
                    _mm_cmpge_ps(tmin, ray_tmax)))) == 0;
}

template <>
inline bool intersect<double>(
    const Ray3d&            ray,
    const RayInfo3d&        ray_info,
    const AABB3d&           bbox)
{
    const __m128d org_x = _mm_set1_pd(ray.m_org.x);
    const __m128d org_y = _mm_set1_pd(ray.m_org.y);
    const __m128d org_z = _mm_set1_pd(ray.m_org.z);

    const __m128d rcp_dir_x = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d rcp_dir_y = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d rcp_dir_z = _mm_set1_pd(ray_info.m_rcp_dir.z);

    const __m128d xl1 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.x].x), org_x));
    const __m128d yl1 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.y].y), org_y));
    const __m128d zl1 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.z].z), org_z));

    const __m128d xl2 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.x].x), org_x));
    const __m128d yl2 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.y].y), org_y));
    const __m128d zl2 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.z].z), org_z));

    const __m128d ray_tmin = _mm_set1_pd(ray.m_tmin);
    const __m128d ray_tmax = _mm_set1_pd(ray.m_tmax);

    const __m128d tmin = _mm_max_pd(zl1, _mm_max_pd(yl1, _mm_max_pd(xl1, ray_tmin)));
    const __m128d tmax = _mm_min_pd(zl2, _mm_min_pd(yl2, _mm_min_pd(xl2, ray_tmax)));

    return
        _mm_movemask_pd(
            _mm_or_pd(
                _mm_cmpgt_pd(tmin, tmax),
                _mm_or_pd(
                    _mm_cmplt_pd(tmax, _mm_set1_pd(ray.m_tmin)),
                    _mm_cmpge_pd(tmin, _mm_set1_pd(ray.m_tmax))))) == 0;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin_out)
{
    const T xl1 = ray_info.m_rcp_dir.x * (bbox[1 - ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl1 = ray_info.m_rcp_dir.y * (bbox[1 - ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl1 = ray_info.m_rcp_dir.z * (bbox[1 - ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T xl2 = ray_info.m_rcp_dir.x * (bbox[    ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox[    ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox[    ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T tmin = ssemax(zl1, ssemax(yl1, ssemax(xl1, ray.m_tmin)));
    const T tmax = ssemin(zl2, ssemin(yl2, ssemin(xl2, ray.m_tmax)));

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    tmin_out = ssemax(ray.m_tmin, tmin);

    return true;
}

template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin_out,
    T&                      tmax_out)
{
    const T xl1 = ray_info.m_rcp_dir.x * (bbox[1 - ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl1 = ray_info.m_rcp_dir.y * (bbox[1 - ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl1 = ray_info.m_rcp_dir.z * (bbox[1 - ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T xl2 = ray_info.m_rcp_dir.x * (bbox[    ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox[    ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox[    ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T tmin = ssemax(zl1, ssemax(yl1, ssemax(xl1, ray.m_tmin)));
    const T tmax = ssemin(zl2, ssemin(yl2, ssemin(xl2, ray.m_tmax)));

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    tmin_out = ssemax(ray.m_tmin, tmin);
    tmax_out = ssemin(ray.m_tmax, tmax);

    return true;
}

template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin_out,
    Vector<T, 3>&           normal_out)
{
    const T xl1 = ray_info.m_rcp_dir.x * (bbox[1 - ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl1 = ray_info.m_rcp_dir.y * (bbox[1 - ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl1 = ray_info.m_rcp_dir.z * (bbox[1 - ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T xl2 = ray_info.m_rcp_dir.x * (bbox[    ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox[    ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox[    ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T tmin = ssemax(zl1, ssemax(yl1, ssemax(xl1, ray.m_tmin)));
    const T tmax = ssemin(zl2, ssemin(yl2, ssemin(xl2, ray.m_tmax)));

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    tmin_out = ssemax(ray.m_tmin, tmin);

    const size_t i = max_index(xl1, yl1, zl1);
    normal_out[0] = normal_out[1] = normal_out[2] = T(0.0);
    normal_out[i] = ray_info.m_sgn_dir[i] > 0 ? T(-1.0) : T(1.0);

    return true;
}

template <typename T>
inline bool clip(
    Ray<T, 3>&              ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox)
{
    const T xl1 = ray_info.m_rcp_dir.x * (bbox[1 - ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl1 = ray_info.m_rcp_dir.y * (bbox[1 - ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl1 = ray_info.m_rcp_dir.z * (bbox[1 - ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T xl2 = ray_info.m_rcp_dir.x * (bbox[    ray_info.m_sgn_dir.x].x - ray.m_org.x);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox[    ray_info.m_sgn_dir.y].y - ray.m_org.y);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox[    ray_info.m_sgn_dir.z].z - ray.m_org.z);

    const T tmin = ssemax(zl1, ssemax(yl1, ssemax(xl1, ray.m_tmin)));
    const T tmax = ssemin(zl2, ssemin(yl2, ssemin(xl2, ray.m_tmax)));

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    ray.m_tmin = ssemax(ray.m_tmin, tmin);
    ray.m_tmax = ssemin(ray.m_tmax, tmax);

    return true;
}

#ifdef APPLESEED_USE_SSE

template <>
inline bool clip<float>(
    Ray3f&                  ray,
    const RayInfo3f&        ray_info,
    const AABB3f&           bbox)
{
    const __m128 org_x = _mm_set1_ps(ray.m_org.x);
    const __m128 org_y = _mm_set1_ps(ray.m_org.y);
    const __m128 org_z = _mm_set1_ps(ray.m_org.z);

    const __m128 rcp_dir_x = _mm_set1_ps(ray_info.m_rcp_dir.x);
    const __m128 rcp_dir_y = _mm_set1_ps(ray_info.m_rcp_dir.y);
    const __m128 rcp_dir_z = _mm_set1_ps(ray_info.m_rcp_dir.z);

    const __m128 xl1 = _mm_mul_ps(rcp_dir_x, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.x].x), org_x));
    const __m128 yl1 = _mm_mul_ps(rcp_dir_y, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.y].y), org_y));
    const __m128 zl1 = _mm_mul_ps(rcp_dir_z, _mm_sub_ps(_mm_set1_ps(bbox[1 - ray_info.m_sgn_dir.z].z), org_z));

    const __m128 xl2 = _mm_mul_ps(rcp_dir_x, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.x].x), org_x));
    const __m128 yl2 = _mm_mul_ps(rcp_dir_y, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.y].y), org_y));
    const __m128 zl2 = _mm_mul_ps(rcp_dir_z, _mm_sub_ps(_mm_set1_ps(bbox[    ray_info.m_sgn_dir.z].z), org_z));

    const __m128 ray_tmin = _mm_set1_ps(ray.m_tmin);
    const __m128 ray_tmax = _mm_set1_ps(ray.m_tmax);

    const __m128 tmin = _mm_max_ps(zl1, _mm_max_ps(yl1, _mm_max_ps(xl1, ray_tmin)));
    const __m128 tmax = _mm_min_ps(zl2, _mm_min_ps(yl2, _mm_min_ps(xl2, ray_tmax)));

    const bool hit =
        _mm_movemask_ps(
            _mm_or_ps(
                _mm_cmpgt_ps(tmin, tmax),
                _mm_or_ps(
                    _mm_cmplt_ps(tmax, ray_tmin),
                    _mm_cmpge_ps(tmin, ray_tmax)))) == 0;

    if (hit)
    {
        _mm_store_ss(&ray.m_tmin, _mm_max_ps(ray_tmin, tmin));
        _mm_store_ss(&ray.m_tmax, _mm_min_ps(ray_tmax, tmax));
    }

    return hit;
}

template <>
inline bool clip<double>(
    Ray3d&                  ray,
    const RayInfo3d&        ray_info,
    const AABB3d&           bbox)
{
    const __m128d org_x = _mm_set1_pd(ray.m_org.x);
    const __m128d org_y = _mm_set1_pd(ray.m_org.y);
    const __m128d org_z = _mm_set1_pd(ray.m_org.z);

    const __m128d rcp_dir_x = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d rcp_dir_y = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d rcp_dir_z = _mm_set1_pd(ray_info.m_rcp_dir.z);

    const __m128d xl1 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.x].x), org_x));
    const __m128d yl1 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.y].y), org_y));
    const __m128d zl1 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_set1_pd(bbox[1 - ray_info.m_sgn_dir.z].z), org_z));

    const __m128d xl2 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.x].x), org_x));
    const __m128d yl2 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.y].y), org_y));
    const __m128d zl2 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_set1_pd(bbox[    ray_info.m_sgn_dir.z].z), org_z));

    const __m128d ray_tmin = _mm_set1_pd(ray.m_tmin);
    const __m128d ray_tmax = _mm_set1_pd(ray.m_tmax);

    const __m128d tmin = _mm_max_pd(zl1, _mm_max_pd(yl1, _mm_max_pd(xl1, ray_tmin)));
    const __m128d tmax = _mm_min_pd(zl2, _mm_min_pd(yl2, _mm_min_pd(xl2, ray_tmax)));

    const bool hit =
        _mm_movemask_pd(
            _mm_or_pd(
                _mm_cmpgt_pd(tmin, tmax),
                _mm_or_pd(
                    _mm_cmplt_pd(tmax, ray_tmin),
                    _mm_cmpge_pd(tmin, ray_tmax)))) == 0;

    if (hit)
    {
        _mm_store_sd(&ray.m_tmin, _mm_max_pd(ray_tmin, tmin));
        _mm_store_sd(&ray.m_tmax, _mm_min_pd(ray_tmax, tmax));
    }

    return hit;
}

#endif  // APPLESEED_USE_SSE

}   // namespace foundation
