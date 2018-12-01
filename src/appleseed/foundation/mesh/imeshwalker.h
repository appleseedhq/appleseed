
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// Mesh walker interface.
//

class APPLESEED_DLLSYMBOL IMeshWalker
  : public NonCopyable
{
  public:
    // Special index value indicating that a feature is absent.
    enum { None = ~0 };

    // Destructor.
    virtual ~IMeshWalker() {}

    // Return the name of the mesh.
    virtual const char* get_name() const = 0;

    // Return vertices.
    virtual size_t get_vertex_count() const = 0;
    virtual Vector3d get_vertex(const size_t i) const = 0;

    // Return vertex normals.
    virtual size_t get_vertex_normal_count() const = 0;
    virtual Vector3d get_vertex_normal(const size_t i) const = 0;

    // Return texture coordinates.
    virtual size_t get_tex_coords_count() const = 0;
    virtual Vector2d get_tex_coords(const size_t i) const = 0;

    // Return material slots.
    virtual size_t get_material_slot_count() const = 0;
    virtual const char* get_material_slot(const size_t i) const = 0;

    // Return the number of faces.
    virtual size_t get_face_count() const = 0;

    // Return the number of vertices in a given face.
    virtual size_t get_face_vertex_count(const size_t face_index) const = 0;

    // Return data for a given vertex of a given face.
    virtual size_t get_face_vertex(const size_t face_index, const size_t vertex_index) const = 0;
    virtual size_t get_face_vertex_normal(const size_t face_index, const size_t vertex_index) const = 0;
    virtual size_t get_face_tex_coords(const size_t face_index, const size_t vertex_index) const = 0;

    // Return the material assigned to a given face.
    virtual size_t get_face_material(const size_t face_index) const = 0;
};

}   // namespace foundation
