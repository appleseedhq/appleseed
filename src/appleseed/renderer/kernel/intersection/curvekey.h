
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
// The CurveKey class uniquely identifies a Bezier curve within an assembly.
//

class CurveKey
{
  public:
    // Constructors.
    CurveKey();         // leave all fields uninitialized
    CurveKey(
        const size_t    object_instance_index,
        const size_t    curve_index_object,
        const size_t    curve_index_tree,
        const size_t    curve_pa,
        const size_t    curve_degree);

    // Return the index of the object instance within the assembly.
    size_t get_object_instance_index() const;

    // Return the index of the curve within the object.
    size_t get_curve_index_object() const;

    // Return the index of the curve within the tree.
    void set_curve_index_tree(const size_t curve_index_tree);
    size_t get_curve_index_tree() const;

    // Return the primitive attribute index of the curve.
    size_t get_curve_pa() const;

    // Return the curve type
    size_t get_curve_degree() const;

  private:
    std::uint32_t       m_object_instance_index;
    std::uint32_t       m_curve_index_object;
    std::uint32_t       m_curve_index_tree;
    std::uint16_t       m_curve_pa;
    std::uint16_t       m_curve_degree;
};


//
// CurveKey class implementation.
//

inline CurveKey::CurveKey()
{
}

inline CurveKey::CurveKey(
    const size_t        object_instance_index,
    const size_t        curve_index_object,
    const size_t        curve_index_tree,
    const size_t        curve_pa,
    const size_t        curve_degree)
  : m_object_instance_index(static_cast<std::uint32_t>(object_instance_index))
  , m_curve_index_object(static_cast<std::uint32_t>(curve_index_object))
  , m_curve_index_tree(static_cast<std::uint32_t>(curve_index_tree))
  , m_curve_pa(static_cast<std::uint16_t>(curve_pa))
  , m_curve_degree(static_cast<std::uint16_t>(curve_degree))
{
}

inline size_t CurveKey::get_object_instance_index() const
{
    return static_cast<size_t>(m_object_instance_index);
}

inline size_t CurveKey::get_curve_index_object() const
{
    return static_cast<size_t>(m_curve_index_object);
}

inline void CurveKey::set_curve_index_tree(const size_t curve_index_tree)
{
    m_curve_index_tree = static_cast<std::uint32_t>(curve_index_tree);
}

inline size_t CurveKey::get_curve_index_tree() const
{
    return static_cast<size_t>(m_curve_index_tree);
}

inline size_t CurveKey::get_curve_pa() const
{
    return static_cast<size_t>(m_curve_pa);
}

inline size_t CurveKey::get_curve_degree() const
{
    return static_cast<size_t>(m_curve_degree);
}

}   // namespace renderer
