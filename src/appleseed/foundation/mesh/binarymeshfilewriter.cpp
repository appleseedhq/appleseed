
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

// Interface header.
#include "binarymeshfilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/imeshwalker.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstring>

using namespace std;

namespace foundation
{

//
// BinaryMeshFileWriter class implementation.
//

BinaryMeshFileWriter::BinaryMeshFileWriter(const string& filename)
  : m_filename(filename)
  , m_file(0)
{
}

BinaryMeshFileWriter::~BinaryMeshFileWriter()
{
    close();
}

void BinaryMeshFileWriter::write(const IMeshWalker& walker)
{
    if (m_file == 0)
    {
        // Open the file for writing.
        m_file = fopen(m_filename.c_str(), "wb");
        if (m_file == 0)
            throw ExceptionIOError();

        // Write the file signature.
        static const char Signature[10] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'M', 'E', 'S', 'H' };
        fwrite(Signature, sizeof(Signature), 1, m_file);

        // Write version information.
        static const uint16 Version = 1;
        fwrite(&Version, sizeof(Version), 1, m_file);
    }

    // Write the mesh data.
    write_string(walker.get_name());
    write_vertices(walker);
    write_vertex_normals(walker);
    write_texture_coordinates(walker);
    write_material_slots(walker);
    write_faces(walker);
}

void BinaryMeshFileWriter::close()
{
    if (m_file)
    {
        fclose(m_file);
        m_file = 0;
    }
}

void BinaryMeshFileWriter::write_string(const char* s) const
{
    const uint16 length = static_cast<uint16>(strlen(s));
    fwrite(&length, sizeof(length), 1, m_file);
    fwrite(s, length, 1, m_file);
}

void BinaryMeshFileWriter::write_vertices(const IMeshWalker& walker) const
{
    const uint32 count = static_cast<uint32>(walker.get_vertex_count());
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint32 i = 0; i < count; ++i)
    {
        const Vector3d v = walker.get_vertex(i);
        fwrite(&v, sizeof(v), 1, m_file);
    }
}

void BinaryMeshFileWriter::write_vertex_normals(const IMeshWalker& walker) const
{
    const uint32 count = static_cast<uint32>(walker.get_vertex_normal_count());
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint32 i = 0; i < count; ++i)
    {
        const Vector3d v = walker.get_vertex_normal(i);
        fwrite(&v, sizeof(v), 1, m_file);
    }
}

void BinaryMeshFileWriter::write_texture_coordinates(const IMeshWalker& walker) const
{
    const uint32 count = static_cast<uint32>(walker.get_tex_coords_count());
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint32 i = 0; i < count; ++i)
    {
        const Vector2d v = walker.get_tex_coords(i);
        fwrite(&v, sizeof(v), 1, m_file);
    }
}

void BinaryMeshFileWriter::write_material_slots(const IMeshWalker& walker) const
{
    const uint16 count = static_cast<uint16>(walker.get_material_slot_count());
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint16 i = 0; i < count; ++i)
        write_string(walker.get_material_slot(i));
}

void BinaryMeshFileWriter::write_faces(const IMeshWalker& walker) const
{
    const uint32 count = static_cast<uint32>(walker.get_face_count());
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint32 i = 0; i < count; ++i)
        write_face(walker, i);
}

void BinaryMeshFileWriter::write_face(const IMeshWalker& walker, const size_t face_index) const
{
    const uint16 count = static_cast<uint16>(walker.get_face_vertex_count(face_index));
    fwrite(&count, sizeof(count), 1, m_file);

    for (uint16 i = 0; i < count; ++i)
    {
        const uint32 face_vertex = static_cast<uint32>(walker.get_face_vertex(face_index, i));
        fwrite(&face_vertex, sizeof(face_vertex), 1, m_file);

        const uint32 face_vertex_normal = static_cast<uint32>(walker.get_face_vertex_normal(face_index, i));
        fwrite(&face_vertex_normal, sizeof(face_vertex_normal), 1, m_file);

        const uint32 face_tex_coords = static_cast<uint32>(walker.get_face_tex_coords(face_index, i));
        fwrite(&face_tex_coords, sizeof(face_tex_coords), 1, m_file);
    }

    const uint16 face_material = static_cast<uint16>(walker.get_face_material(face_index));
    fwrite(&face_material, sizeof(face_material), 1, m_file);
}

}   // namespace foundation
