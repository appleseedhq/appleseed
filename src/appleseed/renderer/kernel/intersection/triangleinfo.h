
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLEINFO_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLEINFO_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// The TriangleInfo class uniquely identifies a triangle within an assembly.
//

class TriangleInfo
{
  public:
    // Constructors.
    TriangleInfo();                                     // leave all fields uninitialized
    TriangleInfo(
        const size_t        object_instance_index,
        const size_t        region_index,
        const size_t        triangle_index,
        const GVector3&     v0,                         // triangle vertices in assembly space
        const GVector3&     v1,
        const GVector3&     v2);

    // Return the index of the object instance within the assembly.
    size_t get_object_instance_index() const;

    // Return the index of the region within the region kit of the object.
    size_t get_region_index() const;

    // Return the index of the triangle within the region.
    size_t get_triangle_index() const;

    // Return the i'th vertex of the triangle in assembly space.
    const GVector3& get_vertex(const size_t index) const;

  private:
    size_t                  m_object_instance_index;
    size_t                  m_region_index;
    size_t                  m_triangle_index;
    GVector3                m_vertices[3];              // triangle vertices in assembly space
};


//
// TriangleInfo class implementation.
//

inline TriangleInfo::TriangleInfo()
{
}

inline TriangleInfo::TriangleInfo(
    const size_t            object_instance_index,
    const size_t            region_index,
    const size_t            triangle_index,
    const GVector3&         v0,
    const GVector3&         v1,
    const GVector3&         v2)
  : m_object_instance_index(object_instance_index)
  , m_region_index(region_index)
  , m_triangle_index(triangle_index)
{
    m_vertices[0] = v0;
    m_vertices[1] = v1;
    m_vertices[2] = v2;
}

inline size_t TriangleInfo::get_object_instance_index() const
{
    return m_object_instance_index;
}

inline size_t TriangleInfo::get_region_index() const
{
    return m_region_index;
}

inline size_t TriangleInfo::get_triangle_index() const
{
    return m_triangle_index;
}

inline const GVector3& TriangleInfo::get_vertex(const size_t index) const
{
    assert(index < 3);
    return m_vertices[index];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLEINFO_H
