
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
#include "foundation/meshio/imeshfilereader.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Forward declarations.
namespace foundation    { class BufferedFile; }
namespace foundation    { class IMeshBuilder; }
namespace foundation    { class ReaderAdapter; }

namespace foundation
{

//
// Read for a simple binary mesh file format.
//

class BinaryMeshFileReader
  : public IMeshFileReader
{
  public:
    // Constructor.
    explicit BinaryMeshFileReader(const std::string& filename);

    // Read a mesh.
    void read(IMeshBuilder& builder) override;

  private:
    const std::string       m_filename;
    std::vector<size_t>     m_vertices;
    std::vector<size_t>     m_vertex_normals;
    std::vector<size_t>     m_tex_coords;

    static void read_and_check_signature(BufferedFile& file);

    static std::string read_string(ReaderAdapter& reader);

    template <typename T> void read_meshes(ReaderAdapter& reader, IMeshBuilder& builder);
    template <typename T> void read_vertices(ReaderAdapter& reader, IMeshBuilder& builder);
    template <typename T> void read_vertex_normals(ReaderAdapter& reader, IMeshBuilder& builder);
    template <typename T> void read_texture_coordinates(ReaderAdapter& reader, IMeshBuilder& builder);
    void read_material_slots(ReaderAdapter& reader, IMeshBuilder& builder);
    void read_faces(ReaderAdapter& reader, IMeshBuilder& builder);
    void read_face(ReaderAdapter& reader, IMeshBuilder& builder);
};

}   // namespace foundation
