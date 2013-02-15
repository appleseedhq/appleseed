
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_MESH_IMESHWALKER_H
#define APPLESEED_FOUNDATION_MESH_IMESHWALKER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace foundation
{

//
// Mesh walker interface.
//

class IMeshWalker
  : public NonCopyable
{
  public:
    struct Face
    {
        // Special index value indicating that a feature is not present.
        static const size_t None = ~0;

        size_t  m_v0, m_v1, m_v2;
        size_t  m_n0, m_n1, m_n2;
        size_t  m_t0, m_t1, m_t2;
        size_t  m_material;
    };

    // Destructor.
    virtual ~IMeshWalker() {}

    // Return the name of the mesh.
    virtual std::string get_name() const = 0;

    // Return the number of vertices in the mesh.
    virtual size_t get_vertex_count() const = 0;

    // Return a given vertex from the mesh.
    virtual Vector3d get_vertex(const size_t i) const = 0;

    // Return the number of vertex normals in the mesh.
    virtual size_t get_vertex_normal_count() const = 0;

    // Return a given vertex normal from the mesh.
    virtual Vector3d get_vertex_normal(const size_t i) const = 0;

    // Return the number of texture coordinates in the mesh.
    virtual size_t get_tex_coords_count() const = 0;

    // Return a given texture coordinate from the mesh.
    virtual Vector2d get_tex_coords(const size_t i) const = 0;

    // Return the number of faces in the mesh.
    virtual size_t get_face_count() const = 0;

    // Return a given face from the mesh.
    virtual Face get_face(const size_t i) const = 0;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MESH_IMESHWALKER_H
