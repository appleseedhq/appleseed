
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Jonathan Dent, The appleseedhq Organization
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

// appleseed.python headers.
#include "bindentitycontainers.h"

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/murmurhash.h"

// Standard headers.
#include <iostream>
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;
using namespace std;

// Work around a regression in Visual Studio 2015 Update 3.
#if defined(_MSC_VER) && _MSC_VER == 1900
namespace boost
{
}
#endif

// Assign typedef
typedef unsigned int u_int;

// Blender data structures
struct MeshFace
{
    u_int v[4];
    short mat_nr;
    char edcode, flag;
};

struct MeshVert
{
    float co[3];
    short no[3];
    char flag, bweight;
};

struct MeshTexFace
{
    float uv[4][2];
    void *tpage;
    char flag, transp;
    short mode, tile, unwrap;
};


// The following function takes a series of pointers to Blender mesh data and modifies the appleseed MeshObject entity
//      blender_object = the appleseed MeshObject created earlier in the export process
//      vertices_length = the numbe rof vertices in the mesh
//      vertex_pointer = a string pointer to the first element of the vertex array
//      Same template applies to mesh faces

void convert_mesh(
    MeshObject*                 blender_mesh,
    const size_t                vertices_length,
    const size_t                vertex_pointer,
    const size_t                triangle_length,
    const size_t                triangle_pointer,
    const size_t                uv_layer_pointer,
    const bool                  export_normals,
    const bool                  export_uvs)
{
    // Convert size_t numbers to actual pointers
    MeshVert* vertices = reinterpret_cast<MeshVert *>(vertex_pointer);
    MeshFace* triangles = reinterpret_cast<MeshFace *>(triangle_pointer);
    MeshTexFace* uv_face = reinterpret_cast<MeshTexFace *>(uv_layer_pointer);

    blender_mesh->reserve_vertices(vertices_length);
    blender_mesh->reserve_triangles(triangle_length);

    if (export_normals == true)
    {
        blender_mesh->reserve_vertex_normals(vertices_length);
    }

    if (export_uvs == true)
    {
        blender_mesh->reserve_tex_coords(vertices_length);
    }

    // Push vertices
    for (size_t vertex_index = 0; vertex_index < vertices_length; ++vertex_index)
    {
        MeshVert &vert = vertices[vertex_index];
        blender_mesh->push_vertex(
            GVector3(
                vert.co[0],
                vert.co[1],
                vert.co[2]));

        // Push normals
        if (export_normals == true)
        {
            blender_mesh->push_vertex_normal(
                GVector3(
                    vert.no[0],
                    vert.no[1],
                    vert.no[2]));
        }
    }

    int uv_index = 0;

    // Push triangles
    for (size_t tri_index = 0; tri_index < triangle_length; ++tri_index)
    {
        MeshFace &face = triangles[tri_index];
        blender_mesh->push_triangle(
            Triangle(
                face.v[0],
                face.v[1],
                face.v[2],
                face.mat_nr));

        if (export_normals == true || export_uvs == true)
        {
            Triangle& tri = blender_mesh->get_triangle(tri_index);

            // Tie vertex normals to mesh faces
            if (export_normals == true)
            {
                tri.m_n0 = face.v[0];
                tri.m_n1 = face.v[1];
                tri.m_n2 = face.v[2];
            }

            // Tie uv coordinates to mesh faces
            if (export_uvs == true)
            {
                MeshTexFace &tex_face = uv_face[tri_index];

                blender_mesh->push_tex_coords(
                    GVector2(
                        tex_face.uv[0][0],
                        tex_face.uv[0][1]));
                tri.m_a0 = uv_index++;

                blender_mesh->push_tex_coords(
                    GVector2(
                        tex_face.uv[1][0],
                        tex_face.uv[1][1]));
                tri.m_a1 = uv_index++;

                blender_mesh->push_tex_coords(
                    GVector2(
                        tex_face.uv[2][0],
                        tex_face.uv[2][1]));
                tri.m_a2 = uv_index++;
            }
        }
    }
}

void convert_vertex_pose(
    MeshObject*                 blender_mesh,
    const size_t                pose,
    const size_t                vertices_length,
    const size_t                vertex_pointer,
    const size_t                triangle_length,
    const size_t                triangle_pointer,
    const bool                  export_normals)
{
    // Convert size_t numbers to actual pointers
    MeshVert* vertices = reinterpret_cast<MeshVert *>(vertex_pointer);
    MeshFace* triangles = reinterpret_cast<MeshFace *>(triangle_pointer);

    // Push vertices
    for (size_t vertex_index = 0; vertex_index < vertices_length; ++vertex_index)
    {
        MeshVert &vert = vertices[vertex_index];
        blender_mesh->set_vertex_pose(
            vertex_index,
            pose,
            GVector3(
                vert.co[0],
                vert.co[1],
                vert.co[2]));

        // Push normals
        if (export_normals == true)
        {
            blender_mesh->set_vertex_normal_pose(
                vertex_index,
                pose,
                GVector3(
                    vert.no[0],
                    vert.no[1],
                    vert.no[2]));
        }
    }
}

void bind_blender_mesh_converter()
{
    bpy::def("convert_mesh", &convert_mesh);
    bpy::def("convert_vertex_pose", &convert_vertex_pose);
}
