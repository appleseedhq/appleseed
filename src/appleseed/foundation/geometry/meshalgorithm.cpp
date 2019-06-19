
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
#include "meshalgorithm.h"

// appleseed.foundation headers.
#include "foundation/array/algorithm.h"
#include "foundation/array/array.h"
#include "foundation/array/arrayref.h"
#include "foundation/array/arrayview.h"
#include "foundation/geometry/mesh.h"
#include "foundation/math/triangulator.h"

// Standard headers.
#include <cassert>
#include <numeric>
#include <vector>

using namespace std;

namespace foundation
{

void unshare_normals(Mesh& mesh)
{
    if (!mesh.has_normals())
        return;

    assert(mesh.get_normals().read().get_key_count() == 1);

    const ArrayView<uint32> nindx(mesh.get_normal_indices().read());
    Array new_normals(
        copy_indexed(
            mesh.get_normals().read().get_key(0),
            nindx.begin(),
            nindx.end()));

    Array new_normal_indices(UInt32Type, new_normals.size());
    ArrayRef<uint32> new_nindx(new_normal_indices);
    std::iota(new_nindx.begin(), new_nindx.end(), 0);

    mesh.get_normals().write().get_key(0) = new_normals;
    mesh.get_normal_indices().write() = new_normal_indices;
}

void compute_smooth_normals(Mesh& mesh)
{
    if (!mesh.has_normals())
        return;

    // Allocate new normals and set them to zero.
    const size_t vertex_count = mesh.get_vertices().read().get_key(0).size();
    const size_t key_count = mesh.get_vertices().read().get_key_count();
    KeyFramedArray accumulated_normals(Vector3fType, vertex_count);
    accumulated_normals.set_key_count(key_count);

    for (size_t k = 0; k < key_count; ++k)
    {
        ArrayRef<Vector3f> N(accumulated_normals.get_key(k));
        for (size_t i = 0, ie = N.size(); i < ie; ++i)
            N[i] = Vector3f(0.0f);
    }

    // Compute face normals and add them to each vertex.
    const ArrayView<uint32> verts_per_face(mesh.get_verts_per_face().read());
    const ArrayView<uint32> vindx(mesh.get_vertex_indices().read());

    for (size_t k = 0; k < key_count; ++k)
    {
        const uint32* v = vindx.begin();
        const ArrayView<Vector3f> P(mesh.get_vertices().read().get_key(k));
        ArrayRef<Vector3f> N(accumulated_normals.get_key(k));

        for (size_t i = 0, e = verts_per_face.size(); i < e; ++i)
        {
            const uint32 sides = verts_per_face[i];

            if (sides == 3)
            {
                const uint32 i0 = *v++;
                const uint32 i1 = *v++;
                const uint32 i2 = *v++;
                const Vector3f& v0 = P[i0];
                const Vector3f& v1 = P[i1];
                const Vector3f& v2 = P[i2];
                const Vector3f n = normalize(cross(v1 - v0, v2 - v0));
                N[i0] += n;
                N[i1] += n;
                N[i2] += n;
            }
            else
            {
                // todo: implement me...
            }
        }
    }

    // Normalize and compress the normals.
    KeyFramedArray normals(CompressedUnitVectorType, vertex_count);
    normals.set_key_count(key_count);

    for (size_t k = 0; k < key_count; ++k)
    {
        const ArrayView<Vector3f> Nsum(accumulated_normals.get_key(k));
        ArrayRef<CompressedUnitVector> N(normals.get_key(k));

        for (size_t i = 0; i < vertex_count; ++i)
            N[i] = CompressedUnitVector(normalize(Nsum[i]));
    }

    // Update the mesh.
    mesh.get_normal_indices() = mesh.get_vertex_indices();
    mesh.get_normals() = Mesh::KeyFramedArrayType(move(normals));
}

void compute_smooth_tangents(Mesh& mesh)
{
    // todo: MeshObject2 implement me!!
}

Mesh triangulate(const Mesh& mesh, const bool keep_quads)
{
    Mesh m(mesh);

    const ArrayView<uint32> verts_per_face(mesh.get_verts_per_face().read());
    const ArrayView<uint32> vert_indices(mesh.get_vertex_indices().read());
    const ArrayView<Vector3f> P(mesh.get_vertices().read().get_key(0));

    const uint32* material_indices =
        mesh.has_per_face_materials()
            ? reinterpret_cast<const uint32*>(mesh.get_material_indices().read().data())
            : nullptr;

    Array new_verts_per_face_array(UInt32Type);
    ArrayRef<uint32> new_verts_per_face(new_verts_per_face_array);

    Array new_material_indices_array(UInt32Type);
    ArrayRef<uint32> new_material_indices(new_material_indices_array);

    std::vector<uint32> index_map;
    uint32 face_index = 0;
    uint32 vertex_index = 0;

    Triangulator<float> triangulator(Triangulator<float>::KeepDegenerateTriangles);
    std::vector<Vector3f> ngon_vertices;
    std::vector<uint32> ngon_indices;
    std::vector<size_t> triangulated_indices;

    for (const uint32 nverts : verts_per_face)
    {
        if (nverts == 3)
        {
            new_verts_per_face.push_back(3);
            index_map.push_back(vertex_index++);
            index_map.push_back(vertex_index++);
            index_map.push_back(vertex_index++);

            if (material_indices)
                new_material_indices.push_back(material_indices[face_index]);
        }
        else if (nverts == 4)
        {
            if (keep_quads)
            {
                new_verts_per_face.push_back(4);
                index_map.push_back(vertex_index++);
                index_map.push_back(vertex_index++);
                index_map.push_back(vertex_index++);
                index_map.push_back(vertex_index++);

                if (material_indices)
                    new_material_indices.push_back(material_indices[face_index]);
            }
            else
            {
                // todo: check this...
                const uint32 i0 = vertex_index++;
                const uint32 i1 = vertex_index++;
                const uint32 i2 = vertex_index++;
                const uint32 i3 = vertex_index++;

                new_verts_per_face.push_back(3);
                index_map.push_back(i0);
                index_map.push_back(i1);
                index_map.push_back(i2);

                new_verts_per_face.push_back(3);
                index_map.push_back(i2);
                index_map.push_back(i3);
                index_map.push_back(i0);

                if (material_indices)
                {
                    new_material_indices.push_back(material_indices[face_index]);
                    new_material_indices.push_back(material_indices[face_index]);
                }
            }
        }
        else
        {
            ngon_vertices.clear();
            ngon_indices.clear();
            triangulated_indices.clear();

            for (uint32 i = 0; i < nverts; ++i)
            {
                const uint32 vi = vert_indices[vertex_index];
                ngon_vertices.push_back(P[vi]);
                ngon_indices.push_back(vertex_index++);
            }

            triangulator.triangulate(ngon_vertices, triangulated_indices);
            for (size_t i = 0, e = triangulated_indices.size(); i < e; i += 3)
            {
                new_verts_per_face.push_back(3);
                index_map.push_back(ngon_indices[triangulated_indices[i + 0]]);
                index_map.push_back(ngon_indices[triangulated_indices[i + 1]]);
                index_map.push_back(ngon_indices[triangulated_indices[i + 2]]);

                if (material_indices)
                    new_material_indices.push_back(material_indices[face_index]);
            }
        }

        ++face_index;
    }

    // Set the new verts per face array.
    m.get_verts_per_face().write() = new_verts_per_face_array;

    // Update the index arrays.
    m.get_vertex_indices().write() =
        copy_indexed(mesh.get_vertex_indices().read(),index_map.begin(), index_map.end());

    if (mesh.has_uvs())
    {
        m.get_uv_indices().write() =
            copy_indexed(mesh.get_uv_indices().read(),index_map.begin(), index_map.end());
    }

    if (mesh.has_normals())
    {
        m.get_normal_indices().write() =
            copy_indexed(mesh.get_normal_indices().read(),index_map.begin(), index_map.end());
    }

    if (mesh.has_tangents())
    {
        m.get_tangent_indices().write() =
            copy_indexed(mesh.get_tangent_indices().read(),index_map.begin(), index_map.end());
    }

    if (mesh.has_per_face_materials())
        m.get_material_indices().write() = move(new_material_indices_array);

    return m;
}

}       // namespace foundation
