
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "mesh.h"

// appleseed.foundation headers.
#include "foundation/array/arrayview.h"

// Standard headers.
#include <algorithm>

namespace foundation
{

Mesh::Mesh()
  : m_verts_per_face(Array(UInt32Type))
  , m_vert_indices(Array(UInt32Type))
  , m_P(KeyFramedArray(Vector3fType))
  , m_uv_indices(Array(UInt32Type))
  , m_uv(Array(Vector2fType))
  , m_normal_indices(Array(UInt32Type))
  , m_N(KeyFramedArray(CompressedUnitVectorType))
  , m_tangent_indices(Array(UInt32Type))
  , m_T(KeyFramedArray(CompressedUnitVectorType))
  , m_material_indices(Array(UInt32Type))
  , m_geometry_version_id(0)
  , m_topology_version_id(0)
  , m_scheme(NoSubdivision)
{
}

VersionID Mesh::get_geometry_version_id() const
{
    return m_geometry_version_id;
}

void Mesh::bump_geometry_version_id()
{
    ++m_geometry_version_id;
}

VersionID Mesh::get_topology_version_id() const
{
    return m_topology_version_id;
}

void Mesh::bump_topology_version_id()
{
    ++m_topology_version_id;
}

Mesh::SubdivisionScheme Mesh::get_subdivision_scheme() const
{
    return m_scheme;
}

void Mesh::set_subdivision_scheme(const SubdivisionScheme scheme)
{
    m_scheme = scheme;
}

size_t Mesh::get_face_count() const
{
    return m_verts_per_face.read().size();
}

const Mesh::ArrayType& Mesh::get_verts_per_face() const
{
    return m_verts_per_face;
}

Mesh::ArrayType& Mesh::get_verts_per_face()
{
    return m_verts_per_face;
}

const Mesh::ArrayType& Mesh::get_vertex_indices() const
{
    return m_vert_indices;
}

Mesh::ArrayType& Mesh::get_vertex_indices()
{
    return m_vert_indices;
}

const Mesh::KeyFramedArrayType& Mesh::get_vertices() const
{
    return m_P;
}

Mesh::KeyFramedArrayType& Mesh::get_vertices()
{
    return m_P;
}

size_t Mesh::get_vertex_count() const
{
    return m_P.read().get_key(0).size();
}

bool Mesh::has_uvs() const
{
    return !m_uv_indices.read().empty();
}

const Mesh::ArrayType& Mesh::get_uv_indices() const
{
    return m_uv_indices;
}

Mesh::ArrayType& Mesh::get_uv_indices()
{
    return m_uv_indices;
}

const Mesh::ArrayType& Mesh::get_uvs() const
{
    return m_uv;
}

Mesh::ArrayType& Mesh::get_uvs()
{
    return m_uv;
}

bool Mesh::has_normals() const
{
    return !m_normal_indices.read().empty();
}

const Mesh::ArrayType& Mesh::get_normal_indices() const
{
    return m_normal_indices;
}

Mesh::ArrayType& Mesh::get_normal_indices()
{
    return m_normal_indices;
}

const Mesh::KeyFramedArrayType& Mesh::get_normals() const
{
    return m_N;
}

Mesh::KeyFramedArrayType& Mesh::get_normals()
{
    return m_N;
}

size_t Mesh::get_normal_count() const
{
    return m_N.read().get_key(0).size();
}

bool Mesh::has_tangents() const
{
    return !m_tangent_indices.read().empty();
}

const Mesh::ArrayType& Mesh::get_tangent_indices() const
{
    return m_tangent_indices;
}

Mesh::ArrayType& Mesh::get_tangent_indices()
{
    return m_tangent_indices;
}

const Mesh::KeyFramedArrayType& Mesh::get_tangents() const
{
    return m_T;
}

Mesh::KeyFramedArrayType& Mesh::get_tangents()
{
    return m_T;
}

bool Mesh::has_per_face_materials() const
{
    return !m_material_indices.read().empty();
}

const Mesh::ArrayType& Mesh::get_material_indices() const
{
    return m_material_indices;
}

Mesh::ArrayType& Mesh::get_material_indices()
{
    return m_material_indices;
}

void Mesh::shrink_arrays()
{
    m_verts_per_face.write().shrink_to_fit();
    m_vert_indices.write().shrink_to_fit();
    m_P.write().shrink_to_fit();

    m_uv_indices.write().shrink_to_fit();
    m_uv.write().shrink_to_fit();

    m_normal_indices.write().shrink_to_fit();
    m_N.write().shrink_to_fit();

    m_tangent_indices.write().shrink_to_fit();
    m_T.write().shrink_to_fit();

    m_material_indices.write().shrink_to_fit();
}

size_t Mesh::get_key_count() const
{
    return m_P.read().get_key_count();
}

void Mesh::remove_all_animation()
{
    m_P.write().set_key_count(1);

    if (has_normals())
        m_N.write().set_key_count(1);

    if (has_tangents())
        m_T.write().set_key_count(1);
}

AABB3f Mesh::compute_bounding_box() const
{
    AABB3f bbox;
    for (size_t i = 0, e = m_P.read().get_key_count(); i < e; ++i)
    {
        const ArrayView<Vector3f> vertices(m_P.read().get_key(i));
        for (const Vector3f& v : vertices)
            bbox.insert(v);
    }

    return bbox;
}

Mesh::FaceSidesInfo Mesh::get_face_sides_info() const
{
    FaceSidesInfo stats;

    const ArrayView<uint32> verts_per_face(m_verts_per_face.read());
    for (const uint32 n : verts_per_face)
    {
        stats.m_face_count++;

             if (n <  3) stats.m_invalid_count++;
        else if (n == 3) stats.m_triangle_count++;
        else if (n == 4) stats.m_quad_count++;
        else             stats.m_ngon_count++;

        stats.m_max_face_sides = max(stats.m_max_face_sides, static_cast<size_t>(n));
    }

    return stats;
}

}       // namespace foundation
