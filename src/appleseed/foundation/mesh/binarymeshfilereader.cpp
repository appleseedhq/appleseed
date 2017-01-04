
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/imeshbuilder.h"
#include "foundation/platform/types.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cstring>
#include <memory>

using namespace std;

namespace foundation
{

//
// BinaryMeshFileReader class implementation.
//

namespace
{
    struct ExceptionEOF : public Exception {};

    template <typename File>
    inline void checked_read(File& file, void* outbuf, const size_t size)
    {
        if (size == 0)
            return;

        const size_t bytes_read = file.read(outbuf, size);

        if (bytes_read == 0)
            throw ExceptionEOF();

        if (bytes_read < size)
            throw ExceptionIOError();
    }

    template <typename File, typename T>
    inline void checked_read(File& file, T& object)
    {
        checked_read(file, &object, sizeof(T));
    }
}

BinaryMeshFileReader::BinaryMeshFileReader(const string& filename)
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

    uint16 version;
    checked_read(file, version);

    auto_ptr<ReaderAdapter> reader;

    switch (version)
    {
      // Uncompressed.
      case 1:
        reader.reset(new PassthroughReaderAdapter(file));
        break;

      // LZO-compressed.
      case 2:
        throw ExceptionIOError(
            "binarymesh format version 2 is no longer supported; "
            "please use the convertmeshfile tool that ships with appleseed 1.1.0 alpha-21 or earlier");

      // LZ4-compressed.
      case 3:
        reader.reset(new LZ4CompressedReaderAdapter(file));
        break;

      // Unknown format.
      default:
        throw ExceptionIOError("unknown binarymesh format version");
    }

    read_meshes(*reader.get(), builder);
}

void BinaryMeshFileReader::read_and_check_signature(BufferedFile& file)
{
    static const char ExpectedSig[10] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'M', 'E', 'S', 'H' };

    char signature[sizeof(ExpectedSig)];
    checked_read(file, signature, sizeof(signature));

    if (memcmp(signature, ExpectedSig, sizeof(ExpectedSig)))
        throw ExceptionIOError("invalid binarymesh format signature");
}

string BinaryMeshFileReader::read_string(ReaderAdapter& reader)
{
    uint16 length;
    checked_read(reader, length);

    string s;
    s.resize(length);
    checked_read(reader, &s[0], length);

    return s;
}

void BinaryMeshFileReader::read_meshes(ReaderAdapter& reader, IMeshBuilder& builder)
{
    try
    {
        while (true)
        {
            // Read the name of the next mesh.
            string mesh_name;
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

            read_vertices(reader, builder);
            read_vertex_normals(reader, builder);
            read_texture_coordinates(reader, builder);
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

void BinaryMeshFileReader::read_vertices(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint32 count;
    checked_read(reader, count);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector3d v;
        checked_read(reader, v);
        builder.push_vertex(v);
    }
}

void BinaryMeshFileReader::read_vertex_normals(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint32 count;
    checked_read(reader, count);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector3d v;
        checked_read(reader, v);
        builder.push_vertex_normal(v);
    }
}

void BinaryMeshFileReader::read_texture_coordinates(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint32 count;
    checked_read(reader, count);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector2d v;
        checked_read(reader, v);
        builder.push_tex_coords(v);
    }
}

void BinaryMeshFileReader::read_material_slots(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint16 count;
    checked_read(reader, count);

    for (uint16 i = 0; i < count; ++i)
    {
        const string material_slot = read_string(reader);
        builder.push_material_slot(material_slot.c_str());
    }
}

void BinaryMeshFileReader::read_faces(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint32 count;
    checked_read(reader, count);

    for (uint32 i = 0; i < count; ++i)
        read_face(reader, builder);
}

void BinaryMeshFileReader::read_face(ReaderAdapter& reader, IMeshBuilder& builder)
{
    uint16 count;
    checked_read(reader, count);

    ensure_minimum_size(m_vertices, count);
    ensure_minimum_size(m_vertex_normals, count);
    ensure_minimum_size(m_tex_coords, count);

    for (uint16 i = 0; i < count; ++i)
    {
        uint32 face_vertex;
        checked_read(reader, face_vertex);
        m_vertices[i] = face_vertex;

        uint32 face_vertex_normal;
        checked_read(reader, face_vertex_normal);
        m_vertex_normals[i] = face_vertex_normal;

        uint32 face_tex_coords;
        checked_read(reader, face_tex_coords);
        m_tex_coords[i] = face_tex_coords;
    }

    uint16 material;
    checked_read(reader, material);

    builder.begin_face(count);
    builder.set_face_vertices(&m_vertices[0]);
    builder.set_face_vertex_normals(&m_vertex_normals[0]);
    builder.set_face_vertex_tex_coords(&m_tex_coords[0]);
    builder.set_face_material(material);
    builder.end_face();
}

}   // namespace foundation
