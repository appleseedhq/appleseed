
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

#ifndef APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYAABB_H
#define APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYAABB_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/fp.h"
#include "foundation/math/minmax.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

// todo: experiment with intersection predicates based on Plucker coordinates
// (reference: http://jgt.akpeters.com/papers/MahovskyWyvill04/).


//
// 3D ray-AABB intersection and clipping.
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

#ifdef APPLESEED_FOUNDATION_USE_SSE

// Test the intersection between a ray and a bounding box.
template <>
inline bool intersect<float>(
    const Ray3f&            ray,
    const RayInfo3f&        ray_info,
    const AABB3f&           bbox)
{
    const sse4f pos_inf = set1ps(FP<float>::pos_inf());
    const sse4f neg_inf = set1ps(FP<float>::neg_inf());

    const sse4f org_x = set1ps(ray.m_org.x);
    const sse4f rcp_dir_x = set1ps(ray_info.m_rcp_dir.x);
    const sse4f xl1 = mulps(rcp_dir_x, subps(set1ps(bbox.min.x), org_x));
    const sse4f xl2 = mulps(rcp_dir_x, subps(set1ps(bbox.max.x), org_x));

    sse4f tmax = maxps(minps(xl1, pos_inf), minps(xl2, pos_inf));
    sse4f tmin = minps(maxps(xl1, neg_inf), maxps(xl2, neg_inf));

    const sse4f org_y = set1ps(ray.m_org.y);
    const sse4f rcp_dir_y = set1ps(ray_info.m_rcp_dir.y);
    const sse4f yl1 = mulps(rcp_dir_y, subps(set1ps(bbox.min.y), org_y));
    const sse4f yl2 = mulps(rcp_dir_y, subps(set1ps(bbox.max.y), org_y));

    tmax = minps(maxps(minps(yl1, pos_inf), minps(yl2, pos_inf)), tmax);
    tmin = maxps(minps(maxps(yl1, neg_inf), maxps(yl2, neg_inf)), tmin);

    const sse4f org_z = set1ps(ray.m_org.z);
    const sse4f rcp_dir_z = set1ps(ray_info.m_rcp_dir.z);
    const sse4f zl1 = mulps(rcp_dir_z, subps(set1ps(bbox.min.z), org_z));
    const sse4f zl2 = mulps(rcp_dir_z, subps(set1ps(bbox.max.z), org_z));

    tmax = minps(maxps(minps(zl1, pos_inf), minps(zl2, pos_inf)), tmax);
    tmin = maxps(minps(maxps(zl1, neg_inf), maxps(zl2, neg_inf)), tmin);

    return
        movemaskps(
            orps(
                cmpgtps(tmin, tmax),
                orps(
                    cmpltps(tmax, set1ps(ray.m_tmin)),
                    cmpgeps(tmin, set1ps(ray.m_tmax))))) == 0;
}

// Test the intersection between a ray and a bounding box.
template <>
inline bool intersect<double>(
    const Ray3d&            ray,
    const RayInfo3d&        ray_info,
    const AABB3d&           bbox)
{
    const sse2d pos_inf = set1pd(FP<double>::pos_inf());
    const sse2d neg_inf = set1pd(FP<double>::neg_inf());

    const sse2d org_x = set1pd(ray.m_org.x);
    const sse2d rcp_dir_x = set1pd(ray_info.m_rcp_dir.x);
    const sse2d xl1 = mulpd(rcp_dir_x, subpd(set1pd(bbox.min.x), org_x));
    const sse2d xl2 = mulpd(rcp_dir_x, subpd(set1pd(bbox.max.x), org_x));

    sse2d tmax = maxpd(minpd(xl1, pos_inf), minpd(xl2, pos_inf));
    sse2d tmin = minpd(maxpd(xl1, neg_inf), maxpd(xl2, neg_inf));

    const sse2d org_y = set1pd(ray.m_org.y);
    const sse2d rcp_dir_y = set1pd(ray_info.m_rcp_dir.y);
    const sse2d yl1 = mulpd(rcp_dir_y, subpd(set1pd(bbox.min.y), org_y));
    const sse2d yl2 = mulpd(rcp_dir_y, subpd(set1pd(bbox.max.y), org_y));

    tmax = minpd(maxpd(minpd(yl1, pos_inf), minpd(yl2, pos_inf)), tmax);
    tmin = maxpd(minpd(maxpd(yl1, neg_inf), maxpd(yl2, neg_inf)), tmin);

    const sse2d org_z = set1pd(ray.m_org.z);
    const sse2d rcp_dir_z = set1pd(ray_info.m_rcp_dir.z);
    const sse2d zl1 = mulpd(rcp_dir_z, subpd(set1pd(bbox.min.z), org_z));
    const sse2d zl2 = mulpd(rcp_dir_z, subpd(set1pd(bbox.max.z), org_z));

    tmax = minpd(maxpd(minpd(zl1, pos_inf), minpd(zl2, pos_inf)), tmax);
    tmin = maxpd(minpd(maxpd(zl1, neg_inf), maxpd(zl2, neg_inf)), tmin);

    return
        movemaskpd(
            orpd(
                cmpgtpd(tmin, tmax),
                orpd(
                    cmpltpd(tmax, set1pd(ray.m_tmin)),
                    cmpgepd(tmin, set1pd(ray.m_tmax))))) == 0;
}

#else

// Test the intersection between a ray and a bounding box.
template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox)
{
    const T pos_inf = FP<T>::pos_inf();
    const T neg_inf = FP<T>::neg_inf();

    const T xl1 = ray_info.m_rcp_dir.x * (bbox.min.x - ray.m_org.x);
    const T xl2 = ray_info.m_rcp_dir.x * (bbox.max.x - ray.m_org.x);

    T tmax = ssemax(ssemin(xl1, pos_inf), ssemin(xl2, pos_inf));
    T tmin = ssemin(ssemax(xl1, neg_inf), ssemax(xl2, neg_inf));

    const T yl1 = ray_info.m_rcp_dir.y * (bbox.min.y - ray.m_org.y);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox.max.y - ray.m_org.y);

    tmax = ssemin(ssemax(ssemin(yl1, pos_inf), ssemin(yl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(yl1, neg_inf), ssemax(yl2, neg_inf)), tmin);

    const T zl1 = ray_info.m_rcp_dir.z * (bbox.min.z - ray.m_org.z);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox.max.z - ray.m_org.z);

    tmax = ssemin(ssemax(ssemin(zl1, pos_inf), ssemin(zl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(zl1, neg_inf), ssemax(zl2, neg_inf)), tmin);

    return !(tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax);
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

// Test the intersection between a ray and a bounding box.
template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin_out)
{
    const T pos_inf = FP<T>::pos_inf();
    const T neg_inf = FP<T>::neg_inf();

    const T xl1 = ray_info.m_rcp_dir.x * (bbox.min.x - ray.m_org.x);
    const T xl2 = ray_info.m_rcp_dir.x * (bbox.max.x - ray.m_org.x);

    T tmax = ssemax(ssemin(xl1, pos_inf), ssemin(xl2, pos_inf));
    T tmin = ssemin(ssemax(xl1, neg_inf), ssemax(xl2, neg_inf));

    const T yl1 = ray_info.m_rcp_dir.y * (bbox.min.y - ray.m_org.y);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox.max.y - ray.m_org.y);

    tmax = ssemin(ssemax(ssemin(yl1, pos_inf), ssemin(yl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(yl1, neg_inf), ssemax(yl2, neg_inf)), tmin);

    const T zl1 = ray_info.m_rcp_dir.z * (bbox.min.z - ray.m_org.z);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox.max.z - ray.m_org.z);

    tmax = ssemin(ssemax(ssemin(zl1, pos_inf), ssemin(zl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(zl1, neg_inf), ssemax(zl2, neg_inf)), tmin);

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    tmin_out = ssemax(ray.m_tmin, tmin);

    return true;
}

// Test the intersection between a ray and a bounding box.
template <typename T>
inline bool intersect(
    const Ray<T, 3>&        ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox,
    T&                      tmin_out,
    T&                      tmax_out)
{
    const T pos_inf = FP<T>::pos_inf();
    const T neg_inf = FP<T>::neg_inf();

    const T xl1 = ray_info.m_rcp_dir.x * (bbox.min.x - ray.m_org.x);
    const T xl2 = ray_info.m_rcp_dir.x * (bbox.max.x - ray.m_org.x);

    T tmax = ssemax(ssemin(xl1, pos_inf), ssemin(xl2, pos_inf));
    T tmin = ssemin(ssemax(xl1, neg_inf), ssemax(xl2, neg_inf));

    const T yl1 = ray_info.m_rcp_dir.y * (bbox.min.y - ray.m_org.y);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox.max.y - ray.m_org.y);

    tmax = ssemin(ssemax(ssemin(yl1, pos_inf), ssemin(yl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(yl1, neg_inf), ssemax(yl2, neg_inf)), tmin);

    const T zl1 = ray_info.m_rcp_dir.z * (bbox.min.z - ray.m_org.z);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox.max.z - ray.m_org.z);

    tmax = ssemin(ssemax(ssemin(zl1, pos_inf), ssemin(zl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(zl1, neg_inf), ssemax(zl2, neg_inf)), tmin);

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    tmin_out = ssemax(ray.m_tmin, tmin);
    tmax_out = ssemin(ray.m_tmax, tmax);

    return true;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

// Clip a ray to its intersection with a bounding box.
template <>
inline bool clip<float>(
    Ray3f&                  ray,
    const RayInfo3f&        ray_info,
    const AABB3f&           bbox)
{
    const sse4f pos_inf = set1ps(FP<float>::pos_inf());
    const sse4f neg_inf = set1ps(FP<float>::neg_inf());

    const sse4f org_x = set1ps(ray.m_org.x);
    const sse4f rcp_dir_x = set1ps(ray_info.m_rcp_dir.x);
    const sse4f xl1 = mulps(rcp_dir_x, subps(set1ps(bbox.min.x), org_x));
    const sse4f xl2 = mulps(rcp_dir_x, subps(set1ps(bbox.max.x), org_x));

    sse4f tmax = maxps(minps(xl1, pos_inf), minps(xl2, pos_inf));
    sse4f tmin = minps(maxps(xl1, neg_inf), maxps(xl2, neg_inf));

    const sse4f org_y = set1ps(ray.m_org.y);
    const sse4f rcp_dir_y = set1ps(ray_info.m_rcp_dir.y);
    const sse4f yl1 = mulps(rcp_dir_y, subps(set1ps(bbox.min.y), org_y));
    const sse4f yl2 = mulps(rcp_dir_y, subps(set1ps(bbox.max.y), org_y));

    tmax = minps(maxps(minps(yl1, pos_inf), minps(yl2, pos_inf)), tmax);
    tmin = maxps(minps(maxps(yl1, neg_inf), maxps(yl2, neg_inf)), tmin);

    const sse4f org_z = set1ps(ray.m_org.z);
    const sse4f rcp_dir_z = set1ps(ray_info.m_rcp_dir.z);
    const sse4f zl1 = mulps(rcp_dir_z, subps(set1ps(bbox.min.z), org_z));
    const sse4f zl2 = mulps(rcp_dir_z, subps(set1ps(bbox.max.z), org_z));

    tmax = minps(maxps(minps(zl1, pos_inf), minps(zl2, pos_inf)), tmax);
    tmin = maxps(minps(maxps(zl1, neg_inf), maxps(zl2, neg_inf)), tmin);

    const sse4f ray_tmin = set1ps(ray.m_tmin);
    const sse4f ray_tmax = set1ps(ray.m_tmax);

    const bool hit =
        movemaskps(
            orps(
                cmpgtps(tmin, tmax),
                orps(
                    cmpltps(tmax, ray_tmin),
                    cmpgeps(tmin, ray_tmax)))) == 0;

    if (hit)
    {
        storess(&ray.m_tmin, maxps(ray_tmin, tmin));
        storess(&ray.m_tmax, minps(ray_tmax, tmax));
    }

    return hit;
}

// Clip a ray to its intersection with a bounding box.
template <>
inline bool clip<double>(
    Ray3d&                  ray,
    const RayInfo3d&        ray_info,
    const AABB3d&           bbox)
{
    const sse2d pos_inf = set1pd(FP<double>::pos_inf());
    const sse2d neg_inf = set1pd(FP<double>::neg_inf());

    const sse2d org_x = set1pd(ray.m_org.x);
    const sse2d rcp_dir_x = set1pd(ray_info.m_rcp_dir.x);
    const sse2d xl1 = mulpd(rcp_dir_x, subpd(set1pd(bbox.min.x), org_x));
    const sse2d xl2 = mulpd(rcp_dir_x, subpd(set1pd(bbox.max.x), org_x));

    sse2d tmax = maxpd(minpd(xl1, pos_inf), minpd(xl2, pos_inf));
    sse2d tmin = minpd(maxpd(xl1, neg_inf), maxpd(xl2, neg_inf));

    const sse2d org_y = set1pd(ray.m_org.y);
    const sse2d rcp_dir_y = set1pd(ray_info.m_rcp_dir.y);
    const sse2d yl1 = mulpd(rcp_dir_y, subpd(set1pd(bbox.min.y), org_y));
    const sse2d yl2 = mulpd(rcp_dir_y, subpd(set1pd(bbox.max.y), org_y));

    tmax = minpd(maxpd(minpd(yl1, pos_inf), minpd(yl2, pos_inf)), tmax);
    tmin = maxpd(minpd(maxpd(yl1, neg_inf), maxpd(yl2, neg_inf)), tmin);

    const sse2d org_z = set1pd(ray.m_org.z);
    const sse2d rcp_dir_z = set1pd(ray_info.m_rcp_dir.z);
    const sse2d zl1 = mulpd(rcp_dir_z, subpd(set1pd(bbox.min.z), org_z));
    const sse2d zl2 = mulpd(rcp_dir_z, subpd(set1pd(bbox.max.z), org_z));

    tmax = minpd(maxpd(minpd(zl1, pos_inf), minpd(zl2, pos_inf)), tmax);
    tmin = maxpd(minpd(maxpd(zl1, neg_inf), maxpd(zl2, neg_inf)), tmin);

    const sse2d ray_tmin = set1pd(ray.m_tmin);
    const sse2d ray_tmax = set1pd(ray.m_tmax);

    const bool hit =
        movemaskpd(
            orpd(
                cmpgtpd(tmin, tmax),
                orpd(
                    cmpltpd(tmax, ray_tmin),
                    cmpgepd(tmin, ray_tmax)))) == 0;

    if (hit)
    {
        storesd(&ray.m_tmin, maxpd(ray_tmin, tmin));
        storesd(&ray.m_tmax, minpd(ray_tmax, tmax));
    }

    return hit;
}

#else

// Clip a ray to its intersection with a bounding box.
template <typename T>
inline bool clip(
    Ray<T, 3>&              ray,
    const RayInfo<T, 3>&    ray_info,
    const AABB<T, 3>&       bbox)
{
    const T pos_inf = FP<T>::pos_inf();
    const T neg_inf = FP<T>::neg_inf();

    const T xl1 = ray_info.m_rcp_dir.x * (bbox.min.x - ray.m_org.x);
    const T xl2 = ray_info.m_rcp_dir.x * (bbox.max.x - ray.m_org.x);

    T tmax = ssemax(ssemin(xl1, pos_inf), ssemin(xl2, pos_inf));
    T tmin = ssemin(ssemax(xl1, neg_inf), ssemax(xl2, neg_inf));

    const T yl1 = ray_info.m_rcp_dir.y * (bbox.min.y - ray.m_org.y);
    const T yl2 = ray_info.m_rcp_dir.y * (bbox.max.y - ray.m_org.y);

    tmax = ssemin(ssemax(ssemin(yl1, pos_inf), ssemin(yl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(yl1, neg_inf), ssemax(yl2, neg_inf)), tmin);

    const T zl1 = ray_info.m_rcp_dir.z * (bbox.min.z - ray.m_org.z);
    const T zl2 = ray_info.m_rcp_dir.z * (bbox.max.z - ray.m_org.z);

    tmax = ssemin(ssemax(ssemin(zl1, pos_inf), ssemin(zl2, pos_inf)), tmax);
    tmin = ssemax(ssemin(ssemax(zl1, neg_inf), ssemax(zl2, neg_inf)), tmin);

    if (tmin > tmax || tmax < ray.m_tmin || tmin >= ray.m_tmax)
        return false;

    ray.m_tmin = ssemax(ray.m_tmin, tmin);
    ray.m_tmax = ssemin(ray.m_tmax, tmax);

    return true;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_INTERSECTION_RAYAABB_H
