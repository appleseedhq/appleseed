
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation { class Array; }

namespace foundation
{

AABB3f compute_bounding_box(const Array& vertices);
AABB3f compute_bounding_box(const Array& vertices, const AABB3f& initial_bbox);

struct FaceSidesInfo
{
    FaceSidesInfo()
      : m_face_count(0)
      , m_triangle_count(0)
      , m_quad_count(0)
      , m_ngon_count(0)
      , m_invalid_count(0)
      , m_max_face_sides(0)
    {
    }

    size_t m_face_count;
    size_t m_triangle_count;
    size_t m_quad_count;
    size_t m_ngon_count;
    size_t m_invalid_count;
    size_t m_max_face_sides;
};

FaceSidesInfo get_face_sides_info(const Array& verts_per_face);

}       // namespace foundation
