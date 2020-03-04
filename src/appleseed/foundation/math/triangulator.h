
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
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"
#include "foundation/utility/vpythonfile.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <vector>

DECLARE_TEST_CASE(Foundation_Math_Triangulator, ComputePolygonOrientation_GivenLowestLeftmostTriangleIsValid_ReturnsCorrectOrientation);
DECLARE_TEST_CASE(Foundation_Math_Triangulator, ComputePolygonOrientation_GivenLowestLeftmostTriangleIsDegenerate_ReturnsCorrectOrientation);

#undef FOUNDATION_TRIANGULATOR_DEBUG

namespace foundation
{

//
// A simple triangulator for approximately planar 3D polygons.
// Implements the O(N^2) Ear Clipping algorithm.
//
// Reference:
//
//   http://www.geometrictools.com/Documentation/TriangulationByEarClipping.pdf
//
// todo: make polygon normal computation and projection more robust
// (see http://webcvs.freedesktop.org/mesa/Mesa-oldtree/si-glu/libtess/alg-outline?revision=1.2&view=markup).
//
// todo: use a faster code path when the input polygon is a quad?
//

template <typename T>
class Triangulator
{
  public:
    // Value and vector types.
    typedef T ValueType;
    typedef Vector<T, 2> Vector2Type;
    typedef Vector<T, 3> Vector3Type;

    // Polygon types.
    typedef std::vector<Vector2Type> Polygon2;
    typedef std::vector<Vector3Type> Polygon3;

    // Index array.
    typedef std::vector<size_t> IndexArray;

    enum Options
    {
        Default                 = 0,        // none of the flags below
        KeepDegenerateTriangles = 1UL << 0  // insert degenerate triangles into triangulation
    };

    // Constructor.
    explicit Triangulator(const int options = Default);

    // Triangulate a polygon.
    // Returns true if triangulation was successful, false otherwise.
    bool triangulate(
        const Polygon3&     polygon,
        IndexArray&         triangles);

  private:
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Triangulator, ComputePolygonOrientation_GivenLowestLeftmostTriangleIsValid_ReturnsCorrectOrientation);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Math_Triangulator, ComputePolygonOrientation_GivenLowestLeftmostTriangleIsDegenerate_ReturnsCorrectOrientation);

    enum Orientation
    {
        Degenerate,                         // degenerate polygon, no clear orientation
        CCW,                                // counterclockwise orientation
        CW                                  // clockwise orientation
    };

    struct Link
    {
        size_t  m_prev;
        size_t  m_next;
    };

    const int               m_options;
    std::vector<Link>       m_links;
    Polygon2                m_polygon2;

    // Compute the approximate normal of a 3D, nearly planar polygon.
    // The returned normal is not unit-length. It may also be a null
    // vector, if the polygon is degenerate.
    static Vector3Type compute_polygon_normal(const Polygon3& polygon);

    // Project an approximately planar 3D polygon onto a plane. The
    // resulting 2D polygon is stored in m_polygon2. Return true if
    // the projection succeeded, false otherwise.
    static bool project_polygon(
        const Polygon3&     polygon3,
        Polygon2&           polygon2);

    // Compute the orientation of a 2D triangle.
    static Orientation compute_triangle_orientation(
        const Vector2Type&  v0,
        const Vector2Type&  v1,
        const Vector2Type&  v2);

    // Compute the orientation of a 2D polygon.
    static Orientation compute_polygon_orientation(
        const Polygon2&     polygon);

    // Check whether a given point p is inside a given triangle (v0, v1, v2).
    // The vertices of the triangle must be given in counterclockwise order.
    static bool point_in_triangle(
        const Vector2Type&  p,
        const Vector2Type&  v0,
        const Vector2Type&  v1,
        const Vector2Type&  v2);

    // Check whether a given convex triangle is a valid ear.
    bool is_ear(
        const size_t        prev,
        const size_t        curr,
        const size_t        next) const;

    void debug(
        const size_t        remaining_vertices,
        const size_t        failed_iterations,
        const size_t        prev,
        const size_t        curr,
        const size_t        next) const;
};


//
// Triangulator class implementation.
//

template <typename T>
Triangulator<T>::Triangulator(const int options)
  : m_options(options)
{
}

template <typename T>
bool Triangulator<T>::triangulate(
    const Polygon3&         polygon,
    IndexArray&             triangles)
{
    assert(polygon.size() >= 3);

    // Project the input 3D polygon onto a plane.
    clear_keep_memory(m_polygon2);
    if (!project_polygon(polygon, m_polygon2))
        return false;

    // Create the initial linked list of polygon vertices.
    const size_t n = m_polygon2.size();
    if (m_links.size() < n)
        m_links.resize(n);
    switch (compute_polygon_orientation(m_polygon2))
    {
      // The input polygon is degenerate: don't triangulate.
      case Degenerate:
        return false;

      // The input polygon is oriented counterclockwise:
      // insert the vertices in this order.
      case CCW:
        m_links[0].m_prev = n - 1;
        m_links[0].m_next = 1;
        m_links[n - 1].m_prev = n - 2;
        m_links[n - 1].m_next = 0;
        for (size_t i = 1; i < n - 1; ++i)
        {
            m_links[i].m_prev = i - 1;
            m_links[i].m_next = i + 1;
        }
        break;

      // Polygon is oriented clockwise:
      // insert the vertices in reverse order.
      case CW:
        m_links[0].m_prev = 1;
        m_links[0].m_next = n - 1;
        m_links[n - 1].m_prev = 0;
        m_links[n - 1].m_next = n - 2;
        for (size_t i = 1; i < n - 1; ++i)
        {
            m_links[i].m_prev = i + 1;
            m_links[i].m_next = i - 1;
        }
        break;
    }

    // Reserve memory for the triangles.
    triangles.reserve(3 * (n - 2));

    // Ear clipping loop.
    // todo: the quality of the triangulation could be improved by choosing each
    // time, amongst the candidate ears, the ear whose shape is the less sliver-like.
    size_t remaining_vertices = n;
    size_t failed_iterations = 0;
    size_t curr = 0;
    while (remaining_vertices > 2)
    {
        // Find the neighboring vertices.
        const size_t prev = m_links[curr].m_prev;
        const size_t next = m_links[curr].m_next;

#ifdef FOUNDATION_TRIANGULATOR_DEBUG
        debug(remaining_vertices, failed_iterations, prev, curr, next);
#endif

        // Fetch the vertices of the triangle (prev, curr, next).
        const Vector2Type& v0 = m_polygon2[prev];
        const Vector2Type& v1 = m_polygon2[curr];
        const Vector2Type& v2 = m_polygon2[next];

        // Compute the determinant of the two edges.
        const Vector2Type e0 = v1 - v0;
        const Vector2Type e1 = v2 - v0;
        const ValueType det = e0[0] * e1[1] - e1[0] * e0[1];

        if (det == ValueType(0.0))
        {
            // Degenerate triangle.
            if (m_options & KeepDegenerateTriangles)
            {
                triangles.push_back(prev);
                triangles.push_back(curr);
                triangles.push_back(next);
            }
        }
        else if (det > ValueType(0.0) && is_ear(prev, curr, next))
        {
            // Ear.
            triangles.push_back(prev);
            triangles.push_back(curr);
            triangles.push_back(next);
        }
        else
        {
            // Not an ear: update the failed iteration counter and detect infinite loop.
            if (++failed_iterations >= remaining_vertices)
                return false;

            // Consider the next vertex.
            curr = next;
            continue;
        }

        // Remove the current vertex from the list.
        m_links[prev].m_next = next;
        m_links[next].m_prev = prev;
        --remaining_vertices;

        // Continue with the vertex curr + 2 to avoid creating high valence triangles.
        curr = m_links[next].m_next;
        failed_iterations = 0;
    }

    // Triangulation succeeded.
    return true;
}

template <typename T>
Vector<T, 3> Triangulator<T>::compute_polygon_normal(const Polygon3& polygon)
{
    //
    // Reference:
    //
    //   Fast Polygon Area and Newell Normal Computation
    //   Daniel Sunday, journal of graphics tools, 7(2):9-13, 2002.
    //   http://jgt.akpeters.com/papers/Sunday02/
    //

    assert(polygon.size() >= 3);

    // Compute the Newell normal.
    Vector3Type normal(ValueType(0.0));
    const size_t n = polygon.size();
    for (size_t i = 1, j = 2, k = 0; k < n - 2; ++i, ++j, ++k)
    {
        normal[0] += polygon[i][1] * (polygon[j][2] - polygon[k][2]);
        normal[1] += polygon[i][2] * (polygon[j][0] - polygon[k][0]);
        normal[2] += polygon[i][0] * (polygon[j][1] - polygon[k][1]);
    }

    // The last two iterations of the loop above.
    normal[0] += polygon[n - 1][1] * (polygon[0][2] - polygon[n - 2][2]);
    normal[1] += polygon[n - 1][2] * (polygon[0][0] - polygon[n - 2][0]);
    normal[2] += polygon[n - 1][0] * (polygon[0][1] - polygon[n - 2][1]);
    normal[0] += polygon[0][1] * (polygon[1][2] - polygon[n - 1][2]);
    normal[1] += polygon[0][2] * (polygon[1][0] - polygon[n - 1][0]);
    normal[2] += polygon[0][0] * (polygon[1][1] - polygon[n - 1][1]);

    // Return the computed normal.
    return normal;
}

template <typename T>
bool Triangulator<T>::project_polygon(
    const Polygon3&         polygon3,
    Polygon2&               polygon2)
{
    assert(polygon3.size() >= 3);

    // Compute the normal to the polygon.
    Vector3Type normal = compute_polygon_normal(polygon3);
    const ValueType normal_length = norm(normal);

    // Normalize the polygon normal, or return false if the normal is a null vector.
    if (normal_length > ValueType(0.0))
        normal /= normal_length;
    else return false;

    // Build an orthonormal basis around the normal.
    const Basis3<T> basis(normal);
    const Vector3Type u = basis.get_tangent_u();
    const Vector3Type v = basis.get_tangent_v();

    // Project the polygon onto the plane.
    const size_t n = polygon3.size();
    polygon2.reserve(n);
    for (size_t i = 0; i < n; ++i)
        polygon2.push_back(Vector2Type(dot(polygon3[i], u), dot(polygon3[i], v)));

    // Projection succeeded.
    return true;
}

template <typename T>
inline typename Triangulator<T>::Orientation
Triangulator<T>::compute_triangle_orientation(
    const Vector2Type&      v0,
    const Vector2Type&      v1,
    const Vector2Type&      v2)
{
    const Vector2Type e0 = v1 - v0;
    const Vector2Type e1 = v2 - v0;
    const ValueType det = e0[0] * e1[1] - e1[0] * e0[1];

    if (det == ValueType(0.0))
        return Degenerate;
    else return det > ValueType(0.0) ? CCW : CW;
}

template <typename T>
typename Triangulator<T>::Orientation
Triangulator<T>::compute_polygon_orientation(const Polygon2& polygon)
{
    const size_t n = polygon.size();

    assert(n >= 3);

    // Find the lowest, leftmost vertex of the polygon.
    size_t corner_index = 0;
    for (size_t i = 1; i < n; ++i)
    {
        const Vector2Type& v = polygon[i];
        const Vector2Type& corner_vertex = polygon[corner_index];

        if ((v[1] <  corner_vertex[1]) ||
            (v[1] == corner_vertex[1] && v[0] < corner_vertex[0]))
        {
            // Found a better corner vertex.
            corner_index = i;
        }
    }

    for (size_t i = 0; i < n; ++i)
    {
        // Find the two neighbors of the corner vertex.
        const size_t prev_index = corner_index > 0 ? corner_index - 1 : n - 1;
        const size_t next_index = corner_index < n - 1 ? corner_index + 1 : 0;

        // Compute the orientation of this triangle.
        const Orientation orientation =
            compute_triangle_orientation(
                polygon[prev_index],
                polygon[corner_index],
                polygon[next_index]);

        // The first non-denegerate triangle determines the orientation of the polygon.
        if (orientation != Degenerate)
            return orientation;

        // Consider the next vertex as the corner vertex.
        corner_index = corner_index < n - 1 ? corner_index + 1 : 0;
    }

    // All triangles are degenerate thus the polygon is degenerate.
    return Degenerate;
}

template <typename T>
inline bool Triangulator<T>::point_in_triangle(
    const Vector2Type&      p,
    const Vector2Type&      v0,
    const Vector2Type&      v1,
    const Vector2Type&      v2)
{
    assert(compute_triangle_orientation(v0, v1, v2) == CCW);

    const Vector2Type u  = p - v0;
    const Vector2Type e0 = v1 - v0;
    const ValueType det0 = u[0] * e0[1] - e0[0] * u[1];

    if (det0 > ValueType(0.0))
        return false;

    const Vector2Type v = p - v1;
    const Vector2Type e1 = v2 - v1;
    const ValueType det1 = v[0] * e1[1] - e1[0] * v[1];

    if (det1 > ValueType(0.0))
        return false;

    const Vector2Type w = p - v2;
    const Vector2Type e2 = v0 - v2;
    const ValueType det2 = w[0] * e2[1] - e2[0] * w[1];

    if (det2 > ValueType(0.0))
        return false;

    return true;
}

template <typename T>
bool Triangulator<T>::is_ear(
    const size_t            prev,
    const size_t            curr,
    const size_t            next) const
{
    // Fetch the vertices of the triangle (prev, curr, next).
    const Vector2Type& v0 = m_polygon2[prev];
    const Vector2Type& v1 = m_polygon2[curr];
    const Vector2Type& v2 = m_polygon2[next];

    // Test for earity: if any vertex except prev and next is inside the closure
    // of the triangle (prev, curr, next) then this is not an ear. In practice,
    // it is equivalent and more efficient to check only the reflex vertices.
    // todo: check only the reflex vertices.
    for (size_t i = m_links[next].m_next; i != prev; i = m_links[i].m_next)
    {
        if (point_in_triangle(m_polygon2[i], v0, v1, v2))
            return false;
    }

    return true;
}

template <typename T>
void Triangulator<T>::debug(
    const size_t            remaining_vertices,
    const size_t            failed_iterations,
    const size_t            prev,
    const size_t            curr,
    const size_t            next) const
{
    // Open a VPython file.
    char filename[100];
    std::sprintf(filename, "poly-remaining=" FMT_SIZE_T "-attempt=" FMT_SIZE_T ".py", remaining_vertices, failed_iterations + 1);
    VPythonFile file(filename);

    // Draw the remaining polygon.
    std::vector<Vector3d> poly;
    for (size_t i = 0, c = curr; i < remaining_vertices + 1; ++i, c = m_links[c].m_next)
        poly.push_back(Vector3d(m_polygon2[c].x, m_polygon2[c].y, 0.0));
    file.draw_polyline(poly.size(), &poly[0], "color.white", 0.0025);

    // Fetch the vertices of the candidate ear.
    const Vector2Type& v0 = m_polygon2[prev];
    const Vector2Type& v1 = m_polygon2[curr];
    const Vector2Type& v2 = m_polygon2[next];

    // Compute the orientation of the candidate ear.
    const Vector2Type e0 = v1 - v0;
    const Vector2Type e1 = v2 - v0;
    const ValueType det = e0[0] * e1[1] - e1[0] * e0[1];

    // Determine if this triangle is effectively an ear.
    const bool ear = det > ValueType(0.0) && is_ear(prev, curr, next);

    // Draw the candidate ear.
    poly.clear();
    poly.push_back(Vector3d(v0.x, v0.y, 0.0));
    poly.push_back(Vector3d(v1.x, v1.y, 0.0));
    poly.push_back(Vector3d(v2.x, v2.y, 0.0));
    poly.push_back(Vector3d(v0.x, v0.y, 0.0));
    file.draw_polyline(poly.size(), &poly[0], ear ? "color.green" : "color.red", 0.0025);

    // Highlight the tip of the candidate ear.
    file.draw_point(Vector3d(v1.x, v1.y, 0.0), "color.yellow", 10);
}

}   // namespace foundation
