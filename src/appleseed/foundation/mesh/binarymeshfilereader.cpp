
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
#include "binarymeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/imeshbuilder.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cstring>

using namespace std;

namespace foundation
{

//
// BinaryMeshFileReader class implementation.
//

BinaryMeshFileReader::BinaryMeshFileReader(const string& filename)
  : m_filename(filename)
{
}

void BinaryMeshFileReader::read(IMeshBuilder& builder)
{
    // Open the file.
    FILE* file = fopen(m_filename.c_str(), "rb");
    if (file == 0)
        throw ExceptionIOError();

    // Read and check the file signature.
    static const char ExpectedSig[10] = { 'B', 'I', 'N', 'A', 'R', 'Y', 'M', 'E', 'S', 'H' };
    char signature[sizeof(ExpectedSig)];
    fread(signature, sizeof(signature), 1, file);
    if (memcmp(signature, ExpectedSig, sizeof(ExpectedSig)))
        throw ExceptionIOError();   // todo: throw better-qualified exception

    // Read version information.
    const uint16 ExpectedVersion = 1;
    uint16 version;
    fread(&version, sizeof(version), 1, file);
    if (version != ExpectedVersion)
        throw ExceptionIOError();   // todo: throw better-qualified exception

    // Read meshes.
    while (read_mesh(builder, file)) ;

    // Close the file.
    fclose(file);
}

string BinaryMeshFileReader::read_string(FILE* file)
{
    string s;
    uint16 length;

    if (fread(&length, sizeof(length), 1, file) > 0)
    {
        s.resize(length);
        fread(&s[0], length, 1, file);
    }

    return s;
}

bool BinaryMeshFileReader::read_mesh(IMeshBuilder& builder, FILE* file)
{
    const string mesh_name = read_string(file);

    if (feof(file))
        return false;

    // Read the mesh data.
    builder.begin_mesh(mesh_name.c_str());
    read_vertices(builder, file);
    read_vertex_normals(builder, file);
    read_texture_coordinates(builder, file);
    read_material_slots(builder, file);
    read_faces(builder, file);
    builder.end_mesh();

    return true;
}

void BinaryMeshFileReader::read_vertices(IMeshBuilder& builder, FILE* file)
{
    uint32 count;
    fread(&count, sizeof(count), 1, file);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector3d v;
        fread(&v, sizeof(v), 1, file);
        builder.push_vertex(v);
    }
}

void BinaryMeshFileReader::read_vertex_normals(IMeshBuilder& builder, FILE* file)
{
    uint32 count;
    fread(&count, sizeof(count), 1, file);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector3d v;
        fread(&v, sizeof(v), 1, file);
        builder.push_vertex_normal(v);
    }
}

void BinaryMeshFileReader::read_texture_coordinates(IMeshBuilder& builder, FILE* file)
{
    uint32 count;
    fread(&count, sizeof(count), 1, file);

    for (uint32 i = 0; i < count; ++i)
    {
        Vector2d v;
        fread(&v, sizeof(v), 1, file);
        builder.push_tex_coords(v);
    }
}

void BinaryMeshFileReader::read_material_slots(IMeshBuilder& builder, FILE* file)
{
    uint16 count;
    fread(&count, sizeof(count), 1, file);

    for (uint16 i = 0; i < count; ++i)
    {
        const string material_slot = read_string(file);
        builder.push_material_slot(material_slot.c_str());
    }
}

void BinaryMeshFileReader::read_faces(IMeshBuilder& builder, FILE* file)
{
    uint32 count;
    fread(&count, sizeof(count), 1, file);

    for (uint32 i = 0; i < count; ++i)
        read_face(builder, file);
}

void BinaryMeshFileReader::read_face(IMeshBuilder& builder, FILE* file)
{
    uint16 count;
    fread(&count, sizeof(count), 1, file);

    ensure_minimum_size(m_vertices, count);
    ensure_minimum_size(m_vertex_normals, count);
    ensure_minimum_size(m_tex_coords, count);

    for (uint16 i = 0; i < count; ++i)
    {
        uint32 face_vertex;
        fread(&face_vertex, sizeof(face_vertex), 1, file);
        m_vertices[i] = face_vertex;

        uint32 face_vertex_normal;
        fread(&face_vertex_normal, sizeof(face_vertex_normal), 1, file);
        m_vertex_normals[i] = face_vertex_normal;

        uint32 face_tex_coords;
        fread(&face_tex_coords, sizeof(face_tex_coords), 1, file);
        m_tex_coords[i] = face_tex_coords;
    }

    uint16 material;
    fread(&material, sizeof(material), 1, file);

    builder.begin_face(count);
    builder.set_face_vertices(&m_vertices[0]);
    builder.set_face_vertex_normals(&m_vertex_normals[0]);
    builder.set_face_vertex_tex_coords(&m_tex_coords[0]);
    builder.set_face_material(material);
    builder.end_face();
}

}   // namespace foundation
