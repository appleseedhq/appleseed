
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
// The Triangle class defines a triangle as a set of indices into feature arrays.
// It doesn't *identify* a triangle; that's what the renderer::TriangleKey class
// is for.
//

class Triangle
{
  public:
    // todo: indices could be stored as 16-bit integers if the number of
    // vertices / vertex normals / vertex attributes per region would be
    // limited to 65535 (one value is used to indicate an absent feature).

    // Special index value used to indicate that a feature is not present.
    static const std::uint32_t None = ~std::uint32_t(0);

    // Public members.
    std::uint32_t  m_v0, m_v1, m_v2;    // vertex indices
    std::uint32_t  m_n0, m_n1, m_n2;    // vertex normal indices
    std::uint32_t  m_a0, m_a1, m_a2;    // vertex attribute indices
    std::uint32_t  m_pa;                // primitive attribute index

    // Constructors.
    Triangle();                         // leave all fields uninitialized
    Triangle(
        const size_t v0,
        const size_t v1,
        const size_t v2);
    Triangle(
        const size_t v0,
        const size_t v1,
        const size_t v2,
        const size_t pa);
    Triangle(
        const size_t v0,
        const size_t v1,
        const size_t v2,
        const size_t n0,
        const size_t n1,
        const size_t n2,
        const size_t pa);
    Triangle(
        const size_t v0,
        const size_t v1,
        const size_t v2,
        const size_t n0,
        const size_t n1,
        const size_t n2,
        const size_t a0,
        const size_t a1,
        const size_t a2,
        const size_t pa);

    // Return true if all three vertices of this triangle have vertex attributes.
    bool has_vertex_attributes() const;
};


//
// Triangle class implementation.
//

inline Triangle::Triangle()
{
}

inline Triangle::Triangle(
    const size_t    v0,
    const size_t    v1,
    const size_t    v2)
  : m_v0(static_cast<std::uint32_t>(v0))
  , m_v1(static_cast<std::uint32_t>(v1))
  , m_v2(static_cast<std::uint32_t>(v2))
  , m_n0(None)
  , m_n1(None)
  , m_n2(None)
  , m_a0(None)
  , m_a1(None)
  , m_a2(None)
  , m_pa(None)
{
}

inline Triangle::Triangle(
    const size_t    v0,
    const size_t    v1,
    const size_t    v2,
    const size_t    pa)
  : m_v0(static_cast<std::uint32_t>(v0))
  , m_v1(static_cast<std::uint32_t>(v1))
  , m_v2(static_cast<std::uint32_t>(v2))
  , m_n0(None)
  , m_n1(None)
  , m_n2(None)
  , m_a0(None)
  , m_a1(None)
  , m_a2(None)
  , m_pa(static_cast<std::uint32_t>(pa))
{
}

inline Triangle::Triangle(
    const size_t    v0,
    const size_t    v1,
    const size_t    v2,
    const size_t    n0,
    const size_t    n1,
    const size_t    n2,
    const size_t    pa)
  : m_v0(static_cast<std::uint32_t>(v0))
  , m_v1(static_cast<std::uint32_t>(v1))
  , m_v2(static_cast<std::uint32_t>(v2))
  , m_n0(static_cast<std::uint32_t>(n0))
  , m_n1(static_cast<std::uint32_t>(n1))
  , m_n2(static_cast<std::uint32_t>(n2))
  , m_a0(None)
  , m_a1(None)
  , m_a2(None)
  , m_pa(static_cast<std::uint32_t>(pa))
{
}

inline Triangle::Triangle(
    const size_t    v0,
    const size_t    v1,
    const size_t    v2,
    const size_t    n0,
    const size_t    n1,
    const size_t    n2,
    const size_t    a0,
    const size_t    a1,
    const size_t    a2,
    const size_t    pa)
  : m_v0(static_cast<std::uint32_t>(v0))
  , m_v1(static_cast<std::uint32_t>(v1))
  , m_v2(static_cast<std::uint32_t>(v2))
  , m_n0(static_cast<std::uint32_t>(n0))
  , m_n1(static_cast<std::uint32_t>(n1))
  , m_n2(static_cast<std::uint32_t>(n2))
  , m_a0(static_cast<std::uint32_t>(a0))
  , m_a1(static_cast<std::uint32_t>(a1))
  , m_a2(static_cast<std::uint32_t>(a2))
  , m_pa(static_cast<std::uint32_t>(pa))
{
}

inline bool Triangle::has_vertex_attributes() const
{
    return m_a0 != None && m_a1 != None && m_a2 != None;
}

}   // namespace renderer
