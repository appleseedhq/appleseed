
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
// Mesh builder interface.
//

class APPLESEED_DLLSYMBOL IMeshBuilder
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~IMeshBuilder() {}

    // Begin the definition of a mesh.
    virtual void begin_mesh(const char* name) = 0;

    // Append a vertex to the mesh.
    // Return the index of the vertex within the mesh.
    virtual size_t push_vertex(const Vector3d& v) = 0;

    // Append a vertex normal to the mesh. The normal is NOT necessarily unit-length.
    // Return the index of the normal within the mesh.
    virtual size_t push_vertex_normal(const Vector3d& v) = 0;

    // Append a texture coordinate to the mesh.
    // Return the index of the vector within the mesh.
    virtual size_t push_tex_coords(const Vector2d& v) = 0;

    // Append a material slot to the mesh.
    virtual size_t push_material_slot(const char* name) = 0;

    // Begin the definition of a face.
    virtual void begin_face(const size_t vertex_count) = 0;

    // Assign vertices to the face.
    virtual void set_face_vertices(const size_t vertices[]) = 0;

    // Assign vertex normals to the face.
    virtual void set_face_vertex_normals(const size_t vertex_normals[]) = 0;

    // Assign texture coordinates to the face.
    virtual void set_face_vertex_tex_coords(const size_t tex_coords[]) = 0;

    // Assign a material to the face.
    virtual void set_face_material(const size_t material) = 0;

    // End the definition of the face.
    virtual void end_face() = 0;

    // End the definition of the mesh.
    virtual void end_mesh() = 0;
};

}   // namespace foundation
