
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

// Interface header.
#include "binarymeshfilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/meshio/imeshwalker.h"

// Standard headers.
#include <cstdint>
#include <cstring>

namespace foundation
{

//
// BinaryMeshFileWriter class implementation.
//

namespace
{
    // Version of the BinaryMesh file format being written by this code.
    const std::uint16_t Version = 4;
}

BinaryMeshFileWriter::BinaryMeshFileWriter(const std::string& filename)
  : m_filename(filename)
  , m_writer(m_file, 256 * 1024)
{
}

void BinaryMeshFileWriter::write(const IMeshWalker& walker)
{
    if (!m_file.is_open())
    {
        m_file.open(
            m_filename.c_str(),
            BufferedFile::BinaryType,
            BufferedFile::WriteMode);

        if (!m_file.is_open())
            throw ExceptionIOError();

        write_signature();
        write_version();
    }

    write_mesh(walker);
}

void BinaryMeshFileWriter::write_signature()
{
    static const char Signature[10] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'M', 'E', 'S', 'H' };
    checked_write(m_file, Signature, sizeof(Signature));
}

void BinaryMeshFileWriter::write_version()
{
    checked_write(m_file, Version);
}

void BinaryMeshFileWriter::write_string(const char* s)
{
    const std::uint16_t length = static_cast<std::uint16_t>(strlen(s));

    checked_write(m_writer, length);
    checked_write(m_writer, s, length);
}

void BinaryMeshFileWriter::write_mesh(const IMeshWalker& walker)
{
    write_string(walker.get_name());
    write_vertices(walker);
    write_vertex_normals(walker);
    write_texture_coordinates(walker);
    write_material_slots(walker);
    write_faces(walker);
}

void BinaryMeshFileWriter::write_vertices(const IMeshWalker& walker)
{
    const std::uint32_t count = static_cast<std::uint32_t>(walker.get_vertex_count());
    checked_write(m_writer, count);

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, Vector3f(walker.get_vertex(i)));
}

void BinaryMeshFileWriter::write_vertex_normals(const IMeshWalker& walker)
{
    const std::uint32_t count = static_cast<std::uint32_t>(walker.get_vertex_normal_count());
    checked_write(m_writer, count);

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, Vector3f(walker.get_vertex_normal(i)));
}

void BinaryMeshFileWriter::write_texture_coordinates(const IMeshWalker& walker)
{
    const std::uint32_t count = static_cast<std::uint32_t>(walker.get_tex_coords_count());
    checked_write(m_writer, count);

    for (std::uint32_t i = 0; i < count; ++i)
        checked_write(m_writer, Vector2f(walker.get_tex_coords(i)));
}

void BinaryMeshFileWriter::write_material_slots(const IMeshWalker& walker)
{
    const std::uint16_t count = static_cast<std::uint16_t>(walker.get_material_slot_count());
    checked_write(m_writer, count);

    for (std::uint16_t i = 0; i < count; ++i)
        write_string(walker.get_material_slot(i));
}

void BinaryMeshFileWriter::write_faces(const IMeshWalker& walker)
{
    const std::uint32_t count = static_cast<std::uint32_t>(walker.get_face_count());
    checked_write(m_writer, count);

    for (std::uint32_t i = 0; i < count; ++i)
        write_face(walker, i);
}

void BinaryMeshFileWriter::write_face(const IMeshWalker& walker, const size_t face_index)
{
    const std::uint16_t count = static_cast<std::uint16_t>(walker.get_face_vertex_count(face_index));
    checked_write(m_writer, count);

    for (std::uint16_t i = 0; i < count; ++i)
    {
        checked_write(m_writer, static_cast<std::uint32_t>(walker.get_face_vertex(face_index, i)));
        checked_write(m_writer, static_cast<std::uint32_t>(walker.get_face_vertex_normal(face_index, i)));
        checked_write(m_writer, static_cast<std::uint32_t>(walker.get_face_tex_coords(face_index, i)));
    }

    checked_write(m_writer, static_cast<std::uint16_t>(walker.get_face_material(face_index)));
}

}   // namespace foundation
