
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
#include "foundation/math/vector.h"
#include "foundation/meshio/imeshfilewriter.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <string>

// Forward declarations.
namespace foundation    { class IMeshWalker; }

namespace foundation
{

//
// Wavefront OBJ mesh file writer.
//
// Reference:
//
//   http://people.scs.fsu.edu/~burkardt/txt/obj_format.txt
//

class OBJMeshFileWriter
  : public IMeshFileWriter
{
  public:
    // Constructor.
    explicit OBJMeshFileWriter(const std::string& filename);

    // Destructor, closes the file.
    ~OBJMeshFileWriter() override;

    // Write a mesh.
    void write(const IMeshWalker& walker) override;

    // Close the file.
    void close();

  private:
    const std::string   m_filename;
    std::FILE*          m_file;
    size_t              m_base_vertex_index;
    size_t              m_base_vertex_normal_index;
    size_t              m_base_tex_coords_index;

    void write_vertices(const IMeshWalker& walker) const;
    void write_vertex_normals(const IMeshWalker& walker) const;
    void write_texture_coordinates(const IMeshWalker& walker) const;
    void write_faces(const IMeshWalker& walker) const;
    void write_faces_no_vn_no_vt(const IMeshWalker& walker) const;
    void write_faces_vn_no_vt(const IMeshWalker& walker) const;
    void write_faces_no_vn_vt(const IMeshWalker& walker) const;
    void write_faces_vn_vt(const IMeshWalker& walker) const;
    void write_vector(const char* prefix, const Vector2d& v) const;
    void write_vector(const char* prefix, const Vector3d& v) const;
};

}   // namespace foundation
