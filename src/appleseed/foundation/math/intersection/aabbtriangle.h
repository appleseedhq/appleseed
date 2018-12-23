
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
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// 3D AABB-triangle intersection function.
//

// Return true if a given bounding box and a given triangle intersect.
template <typename T>
bool intersect(
    const AABB<T, 3>&       bbox,
    const Vector<T, 3>&     v0,
    const Vector<T, 3>&     v1,
    const Vector<T, 3>&     v2);


//
// 3D AABB-triangle intersection function implementation.
//

namespace aabbtriangle_impl
{

    // Return a bitmask indicating the location of a vertex with respect
    // to each face of a bounding box. Each of the 6 bits is set to 1 if
    // the vertex lies on the interior side of the corresponding face
    // (or on the face itself, since the boundary of the bounding box is
    // considered to belong to the bounding box), or to 0 otherwise.
    template <typename T>
    inline uint8 classify_vertex(
        const AABB<T, 3>&   bbox,
        const Vector<T, 3>& v)
    {
        uint8 mask = 0;
        if (v[0] >= bbox.min[0]) mask |= 1UL << 0;
        if (v[1] >= bbox.min[1]) mask |= 1UL << 1;
        if (v[2] >= bbox.min[2]) mask |= 1UL << 2;
        if (v[0] <= bbox.max[0]) mask |= 1UL << 3;
        if (v[1] <= bbox.max[1]) mask |= 1UL << 4;
        if (v[2] <= bbox.max[2]) mask |= 1UL << 5;
        return mask;
    }

    // Return true if a given segment [s0, s1] and a given face of a
    // given bounding box intersect, or false otherwise.
    template <typename T, size_t MinMax, size_t Dim, size_t Bit>
    inline bool check_edge_face(
        const AABB<T, 3>&   bbox,
        const Vector<T, 3>& s0,
        const Vector<T, 3>& s1,
        const uint8         mask)
    {
        // If the segment [s0, s1] does not straddle the plane of the
        // bounding box face along the specified dimension, it cannot
        // intersect the face.
        if (!(mask & (1UL << Bit)))
            return false;

        const T plane = bbox[MinMax][Dim];
        const T x0 = s0[Dim];
        const T x1 = s1[Dim];

        // Make sure the segment effectively straddle the face plane.
        assert(x0 != x1);
        assert(
            (x0 <= plane && x1 >= plane) ||
            (x1 <= plane && x0 >= plane));

        // Compute the intersection between between the segment and
        // the plane of the bounding box face.
        const T t = (plane - x0) / (x1 - x0);
        assert(t >= T(0.0));
        assert(t <= T(1.0));

/*      A division-free alternative:

        T abs_dx;
        T t;

        if (x0 < x1)
        {
            abs_dx = x1 - x0;
            t = plane - x0;
        }
        else
        {
            abs_dx = x0 - x1;
            t = x0 - plane;
        }

        const T y = t * (s1[1] - s0[1]);
        if (y < (bbox.min[1] - s0[1]) * abs_dx ||
            y > (bbox.max[1] - s0[1]) * abs_dx)
            return false;

        const T z = t * (s1[2] - s0[2]);
        if (z < (bbox.min[2] - s0[2]) * abs_dx ||
            z > (bbox.max[2] - s0[2]) * abs_dx)
            return false;
*/

        // If the intersection point is outside of the bounds of the
        // bounding box face, the segment cannot intersect the face.
        switch (Dim)
        {
          case 0:
            {
                const T y = s0[1] + t * (s1[1] - s0[1]);
                if (y < bbox.min[1] || y > bbox.max[1])
                    return false;
                const T z = s0[2] + t * (s1[2] - s0[2]);
                if (z < bbox.min[2] || z > bbox.max[2])
                    return false;
                break;
            }
          case 1:
            {
                const T x = s0[0] + t * (s1[0] - s0[0]);
                if (x < bbox.min[0] || x > bbox.max[0])
                    return false;
                const T z = s0[2] + t * (s1[2] - s0[2]);
                if (z < bbox.min[2] || z > bbox.max[2])
                    return false;
                break;
            }
          case 2:
            {
                const T x = s0[0] + t * (s1[0] - s0[0]);
                if (x < bbox.min[0] || x > bbox.max[0])
                    return false;
                const T y = s0[1] + t * (s1[1] - s0[1]);
                if (y < bbox.min[1] || y > bbox.max[1])
                    return false;
                break;
            }
        }

        // The segment and the bounding box face intersect.
        return true;
    }

    // Find the two bounding box corners that are the farthest
    // (on opposite sides) from a given plane.
    template <typename T, size_t Corner>
    inline void find_extrema_corners(
        const AABB<T, 3>&   bbox,
        const Vector<T, 3>& n,
        T&                  dmin,
        T&                  dmax,
        Vector<T, 3>&       pmin,
        Vector<T, 3>&       pmax)
    {
        // Compute the coordinates of the bounding box corner.
        Vector<T, 3> p;
        switch (Corner)
        {
          case 0: p = Vector<T, 3>(bbox.min[0], bbox.min[1], bbox.min[2]); break;
          case 1: p = Vector<T, 3>(bbox.min[0], bbox.min[1], bbox.max[2]); break;
          case 2: p = Vector<T, 3>(bbox.min[0], bbox.max[1], bbox.min[2]); break;
          case 3: p = Vector<T, 3>(bbox.min[0], bbox.max[1], bbox.max[2]); break;
          case 4: p = Vector<T, 3>(bbox.max[0], bbox.min[1], bbox.min[2]); break;
          case 5: p = Vector<T, 3>(bbox.max[0], bbox.min[1], bbox.max[2]); break;
          case 6: p = Vector<T, 3>(bbox.max[0], bbox.max[1], bbox.min[2]); break;
          case 7: p = Vector<T, 3>(bbox.max[0], bbox.max[1], bbox.max[2]); break;
        }

        // Keep track of the extrema corners.
        const T d = dot(p, n);
        if (d < dmin)
        {
            dmin = d;
            pmin = p;
        }
        if (d > dmax)
        {
            dmax = d;
            pmax = p;
        }
    }

    // Return true if a given segment [s0, s1] and a given triangle
    // intersect, or false otherwise.
    template <typename T>
    inline bool check_segment_triangle(
        const Vector<T, 3>& s0,
        const Vector<T, 3>& s1,
        const Vector<T, 3>& v0,
        const Vector<T, 3>& e0,
        const Vector<T, 3>& e1)
    {
        // Find direction of segment support line.
        const Vector<T, 3> dir = s1 - s0;

        // Begin calculating determinant - also used to calculate u parameter.
        const Vector<T, 3> pvec = cross(dir, e1);

        // If determinant is near zero, segment lies in plane of triangle.
        const T det = dot(e0, pvec);
        if (det == T(0.0))
            return false;

        // Calculate distance from v0 to s0.
        const Vector<T, 3> tvec = s0 - v0;

        if (det > 0.0)
        {
            // Calculate u parameter and test bounds.
            const T u = dot(tvec, pvec);
            if (u < T(0.0) || u > det)
                return false;

            // Prepare to test v parameter.
            const Vector<T, 3> qvec = cross(tvec, e0);

            // Calculate v parameter and test bounds.
            const T v = dot(dir, qvec);
            if (v < T(0.0) || u + v > det)
                return false;

            // Calculate t parameter and test bounds.
            const T t = dot(e1, qvec);
            if (t < T(0.0) || t > det)
                return false;
        }
        else
        {
            // Calculate u parameter and test bounds.
            const T u = dot(tvec, pvec);
            if (u > T(0.0) || u < det)
                return false;

            // Prepare to test v parameter.
            const Vector<T, 3> qvec = cross(tvec, e0);

            // Calculate v parameter and test bounds.
            const T v = dot(dir, qvec);
            if (v > T(0.0) || u + v < det)
                return false;

            // Calculate t parameter and test bounds.
            const T t = dot(e1, qvec);
            if (t > T(0.0) || t < det)
                return false;
        }

        // Segment intersects triangle.
        return true;
    }

}   // namespace aabbtriangle_impl

template <typename T>
bool intersect(
    const AABB<T, 3>&       bbox,
    const Vector<T, 3>&     v0,
    const Vector<T, 3>&     v1,
    const Vector<T, 3>&     v2)
{
    assert(bbox.is_valid());
//  assert(bbox.rank() == 3);

    // If any of the three vertices is inside the bounding box, then
    // the triangle and the bounding box intersect.
    const uint8 mask0 = aabbtriangle_impl::classify_vertex(bbox, v0);
    if (mask0 == 0x3F)
        return true;
    const uint8 mask1 = aabbtriangle_impl::classify_vertex(bbox, v1);
    if (mask1 == 0x3F)
        return true;
    const uint8 mask2 = aabbtriangle_impl::classify_vertex(bbox, v2);
    if (mask2 == 0x3F)
        return true;

    // If all three vertices are on the exterior side of the same face,
    // then the triangle and the bounding box are disjoint.
    if ((mask0 | mask1 | mask2) != 0x3F)
        return false;

    // If any edge of the triangle intersects any face of the bounding box,
    // then the triangle and the bounding box intersect.
    const uint8 mask01 = mask0 ^ mask1;
    if (aabbtriangle_impl::check_edge_face<T, 0, 0, 0>(bbox, v0, v1, mask01) ||
        aabbtriangle_impl::check_edge_face<T, 0, 1, 1>(bbox, v0, v1, mask01) ||
        aabbtriangle_impl::check_edge_face<T, 0, 2, 2>(bbox, v0, v1, mask01) ||
        aabbtriangle_impl::check_edge_face<T, 1, 0, 3>(bbox, v0, v1, mask01) ||
        aabbtriangle_impl::check_edge_face<T, 1, 1, 4>(bbox, v0, v1, mask01) ||
        aabbtriangle_impl::check_edge_face<T, 1, 2, 5>(bbox, v0, v1, mask01))
        return true;
    const uint8 mask02 = mask0 ^ mask2;
    if (aabbtriangle_impl::check_edge_face<T, 0, 0, 0>(bbox, v0, v2, mask02) ||
        aabbtriangle_impl::check_edge_face<T, 0, 1, 1>(bbox, v0, v2, mask02) ||
        aabbtriangle_impl::check_edge_face<T, 0, 2, 2>(bbox, v0, v2, mask02) ||
        aabbtriangle_impl::check_edge_face<T, 1, 0, 3>(bbox, v0, v2, mask02) ||
        aabbtriangle_impl::check_edge_face<T, 1, 1, 4>(bbox, v0, v2, mask02) ||
        aabbtriangle_impl::check_edge_face<T, 1, 2, 5>(bbox, v0, v2, mask02))
        return true;
    const uint8 mask12 = mask1 ^ mask2;
    if (aabbtriangle_impl::check_edge_face<T, 0, 0, 0>(bbox, v1, v2, mask12) ||
        aabbtriangle_impl::check_edge_face<T, 0, 1, 1>(bbox, v1, v2, mask12) ||
        aabbtriangle_impl::check_edge_face<T, 0, 2, 2>(bbox, v1, v2, mask12) ||
        aabbtriangle_impl::check_edge_face<T, 1, 0, 3>(bbox, v1, v2, mask12) ||
        aabbtriangle_impl::check_edge_face<T, 1, 1, 4>(bbox, v1, v2, mask12) ||
        aabbtriangle_impl::check_edge_face<T, 1, 2, 5>(bbox, v1, v2, mask12))
        return true;

    // Compute the normal to the support plane of the triangle.
    const Vector<T, 3> e0 = v1 - v0;
    const Vector<T, 3> e1 = v2 - v0;
    const Vector<T, 3> n = cross(e0, e1);
    assert(
           n[0] != T(0.0)
        || n[1] != T(0.0)
        || n[2] != T(0.0));

    // Find the bounding box corners the farthest from the support plane
    // of the triangle, on each side of the plane.
    Vector<T, 3> pmin(+std::numeric_limits<T>::max());
    Vector<T, 3> pmax(-std::numeric_limits<T>::max());
    T dmin = +std::numeric_limits<T>::max();
    T dmax = -std::numeric_limits<T>::max();
    aabbtriangle_impl::find_extrema_corners<T, 0>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 1>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 2>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 3>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 4>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 5>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 6>(bbox, n, dmin, dmax, pmin, pmax);
    aabbtriangle_impl::find_extrema_corners<T, 7>(bbox, n, dmin, dmax, pmin, pmax);

    // The bounding box and the triangle intersect if and only if
    // the segment and the triangle intersect.
    return aabbtriangle_impl::check_segment_triangle(pmin, pmax, v0, e0, e1);
}

}   // namespace foundation
