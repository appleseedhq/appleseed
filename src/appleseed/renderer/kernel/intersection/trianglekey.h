
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

// Standard headers.
#include <cstddef>
#include <cstdint>

namespace renderer
{

//
// The TriangleKey class uniquely identifies a triangle within an assembly.
//

class TriangleKey
{
  public:
    // Constructors.
    TriangleKey();      // leave all fields uninitialized
    TriangleKey(
        const size_t    object_instance_index,
        const size_t    triangle_index,
        const size_t    triangle_pa);

    // Return the index of the object instance within the assembly.
    size_t get_object_instance_index() const;

    // Return the index of the triangle within the region.
    size_t get_triangle_index() const;

    // Return the primitive attribute index of the triangle.
    size_t get_triangle_pa() const;

  private:
    std::uint32_t       m_object_instance_index;
    std::uint16_t       m_triangle_pa;       // todo: there is a 2-byte hole after this member
    std::uint32_t       m_triangle_index;
};


//
// TriangleKey class implementation.
//

inline TriangleKey::TriangleKey()
{
}

inline TriangleKey::TriangleKey(
    const size_t        object_instance_index,
    const size_t        triangle_index,
    const size_t        triangle_pa)
  : m_object_instance_index(static_cast<std::uint32_t>(object_instance_index))
  , m_triangle_pa(static_cast<std::uint16_t>(triangle_pa))
  , m_triangle_index(static_cast<std::uint32_t>(triangle_index))
{
}

inline size_t TriangleKey::get_object_instance_index() const
{
    return static_cast<size_t>(m_object_instance_index);
}

inline size_t TriangleKey::get_triangle_index() const
{
    return static_cast<size_t>(m_triangle_index);
}

inline size_t TriangleKey::get_triangle_pa() const
{
    return static_cast<size_t>(m_triangle_pa);
}

}   // namespace renderer
