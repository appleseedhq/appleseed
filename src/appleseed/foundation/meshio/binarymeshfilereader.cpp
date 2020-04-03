
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
#include "binarymeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/meshio/imeshbuilder.h"
#include "foundation/utility/bufferedfile.h"

// Standard headers.
#include <cstdint>
#include <cstring>
#include <memory>

namespace foundation
{

//
// BinaryMeshFileReader class implementation.
//

BinaryMeshFileReader::BinaryMeshFileReader(const std::string& filename)
  : m_filename(filename)
{
}

void BinaryMeshFileReader::read(IMeshBuilder& builder)
{
    BufferedFile file(
        m_filename.c_str(),
        BufferedFile::BinaryType,
        BufferedFile::ReadMode);

    if (!file.is_open())
        throw ExceptionIOError();

    read_and_check_signature(file);

    std::uint16_t version;
    checked_read(file, version);

    switch (version)
    {
      // Uncompressed, double-precision geometry.
      case 1:
        {
            PassthroughReaderAdapter reader(file);
            read_meshes<double>(reader, builder);
        }
        break;

      // LZO-compressed, double-precision geometry.
      case 2:
        throw ExceptionIOError(
            "binarymesh format version 2 is no longer supported; "
            "please use the convertmeshfile tool that ships with appleseed 1.1.0 alpha-21 or earlier");

      // LZ4-compressed, double-precision geometry.
      case 3:
        {
            LZ4CompressedReaderAdapter reader(file);
            read_meshes<double>(reader, builder);
        }
        break;

      // LZ4-compressed, single-precision geometry.
      case 4:
        {
            LZ4CompressedReaderAdapter reader(file);
            read_meshes<float>(reader, builder);
        }
        break;

      // Unknown format.
      default:
        throw ExceptionIOError("unknown binarymesh format version");
    }
}

void BinaryMeshFileReader::read_and_check_signature(BufferedFile& file)
{
    static const char ExpectedSig[10] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'M', 'E', 'S', 'H' };

    char signature[sizeof(ExpectedSig)];
    checked_read(file, signature, sizeof(signature));

    if (memcmp(signature, ExpectedSig, sizeof(ExpectedSig)))
        throw ExceptionIOError("invalid binarymesh format signature");
}

std::string BinaryMeshFileReader::read_string(ReaderAdapter& reader)
{
    std::uint16_t length;
    checked_read(reader, length);

    std::string s;
    s.resize(length);
    checked_read(reader, &s[0], length);

    return s;
}

template <typename T>
void BinaryMeshFileReader::read_meshes(ReaderAdapter& reader, IMeshBuilder& builder)
{
    try
    {
        while (true)
        {
            // Read the name of the next mesh.
            std::string mesh_name;
            try
            {
                mesh_name = read_string(reader);
            }
            catch (const ExceptionEOF&)
            {
                // Expected EOF.
                break;
            }

            builder.begin_mesh(mesh_name.c_str());
            read_vertices<T>(reader, builder);
            read_vertex_normals<T>(reader, builder);
            read_texture_coordinates<T>(reader, builder);
            read_material_slots(reader, builder);
            read_faces(reader, builder);

            builder.end_mesh();
        }
    }
    catch (const ExceptionEOF&)
    {
        // Unexpected EOF.
        throw ExceptionIOError();
    }
}

template <typename T>
void BinaryMeshFileReader::read_vertices(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint32_t count;
    checked_read(reader, count);

    for (std::uint32_t i = 0; i < count; ++i)
    {
        Vector<T, 3> v;
        checked_read(reader, v);
        builder.push_vertex(Vector3d(v));
    }
}

template <typename T>
void BinaryMeshFileReader::read_vertex_normals(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint32_t count;
    checked_read(reader, count);

    for (std::uint32_t i = 0; i < count; ++i)
    {
        Vector<T, 3> v;
        checked_read(reader, v);
        builder.push_vertex_normal(Vector3d(v));
    }
}

template <typename T>
void BinaryMeshFileReader::read_texture_coordinates(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint32_t count;
    checked_read(reader, count);

    for (std::uint32_t i = 0; i < count; ++i)
    {
        Vector<T, 2> v;
        checked_read(reader, v);
        builder.push_tex_coords(Vector2d(v));
    }
}

void BinaryMeshFileReader::read_material_slots(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint16_t count;
    checked_read(reader, count);

    for (std::uint16_t i = 0; i < count; ++i)
    {
        const std::string material_slot = read_string(reader);
        builder.push_material_slot(material_slot.c_str());
    }
}

void BinaryMeshFileReader::read_faces(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint32_t count;
    checked_read(reader, count);

    for (std::uint32_t i = 0; i < count; ++i)
        read_face(reader, builder);
}

void BinaryMeshFileReader::read_face(ReaderAdapter& reader, IMeshBuilder& builder)
{
    std::uint16_t count;
    checked_read(reader, count);

    ensure_minimum_size(m_vertices, count);
    ensure_minimum_size(m_vertex_normals, count);
    ensure_minimum_size(m_tex_coords, count);

    for (std::uint16_t i = 0; i < count; ++i)
    {
        std::uint32_t face_vertex;
        checked_read(reader, face_vertex);
        m_vertices[i] = face_vertex;

        std::uint32_t face_vertex_normal;
        checked_read(reader, face_vertex_normal);
        m_vertex_normals[i] = face_vertex_normal;

        std::uint32_t face_tex_coords;
        checked_read(reader, face_tex_coords);
        m_tex_coords[i] = face_tex_coords;
    }

    std::uint16_t material;
    checked_read(reader, material);

    builder.begin_face(count);
    builder.set_face_vertices(&m_vertices[0]);
    builder.set_face_vertex_normals(&m_vertex_normals[0]);
    builder.set_face_vertex_tex_coords(&m_tex_coords[0]);
    builder.set_face_material(material);
    builder.end_face();
}

}   // namespace foundation
