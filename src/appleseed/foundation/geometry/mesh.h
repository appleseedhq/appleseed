
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

#pragma once

// appleseed.foundation headers.
#include "foundation/array/array.h"
#include "foundation/array/keyframedarray.h"
#include "foundation/math/aabb.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/copyonwrite.h"
#include "foundation/utility/version.h"

namespace foundation
{

class Mesh
{
  public:
    typedef CopyOnWrite<Array>          ArrayType;
    typedef CopyOnWrite<KeyFramedArray> KeyFramedArrayType;

    enum SubdivisionScheme
    {
        NoSubdivision,
        BilinearSubdivision,
        CatmullClarkSubdivision
    };

    // Constructor.
    Mesh();

    // Versioning.
    VersionID get_geometry_version_id() const;
    void bump_geometry_version_id();

    VersionID get_topology_version_id() const;
    void bump_topology_version_id();

    // Subdivision scheme.
    SubdivisionScheme get_subdivision_scheme() const;
    void set_subdivision_scheme(const SubdivisionScheme scheme);

    // Vertices per face.
    size_t get_face_count() const;

    const ArrayType& get_verts_per_face() const;
    ArrayType& get_verts_per_face();

    // Vertices.
    const ArrayType& get_vertex_indices() const;
    ArrayType& get_vertex_indices();

    const KeyFramedArrayType& get_vertices() const;
    KeyFramedArrayType& get_vertices();

    size_t get_vertex_count() const;

    // UVs
    bool has_uvs() const;

    const ArrayType& get_uv_indices() const;
    ArrayType& get_uv_indices();

    const ArrayType& get_uvs() const;
    ArrayType& get_uvs();

    // Normals.
    bool has_normals() const;

    const ArrayType& get_normal_indices() const;
    ArrayType& get_normal_indices();

    const KeyFramedArrayType& get_normals() const;
    KeyFramedArrayType& get_normals();

    size_t get_normal_count() const;

    // Tangents.
    bool has_tangents() const;

    const ArrayType& get_tangent_indices() const;
    ArrayType& get_tangent_indices();

    const KeyFramedArrayType& get_tangents() const;
    KeyFramedArrayType& get_tangents();

    // Material indices.
    bool has_per_face_materials() const;

    const ArrayType& get_material_indices() const;
    ArrayType& get_material_indices();

    // Remove excess capacity from all arrays.
    void shrink_arrays();

    // Return the number of keyframes of this mesh.
    size_t get_key_count() const;

    // Remove all animation.
    void remove_all_animation();

    // Compute the bounding box of the mesh.
    AABB3f compute_bounding_box() const;

    struct FaceSidesInfo
    {
        FaceSidesInfo()
          : m_face_count(0)
          , m_triangle_count(0)
          , m_quad_count(0)
          , m_ngon_count(0)
          , m_invalid_count(0)
          , m_max_face_sides(0)
        {
        }

        size_t m_face_count;
        size_t m_triangle_count;
        size_t m_quad_count;
        size_t m_ngon_count;
        size_t m_invalid_count;
        size_t m_max_face_sides;
    };

    FaceSidesInfo get_face_sides_info() const;

  private:
    ArrayType           m_verts_per_face;
    ArrayType           m_vert_indices;
    KeyFramedArrayType  m_P;
    ArrayType           m_uv_indices;
    ArrayType           m_uv;
    ArrayType           m_normal_indices;
    KeyFramedArrayType  m_N;
    ArrayType           m_tangent_indices;
    KeyFramedArrayType  m_T;
    ArrayType           m_material_indices;
    VersionID           m_geometry_version_id;
    VersionID           m_topology_version_id;
    SubdivisionScheme   m_scheme;
};

}       // namespace foundation
