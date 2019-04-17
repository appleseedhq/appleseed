
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

// appleseed.renderer headers.
#include "renderer/api/object.h"

// appleseed.foundation headers.
#include "foundation/platform/python.h"

// Standard headers.
#include <cstdint>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    // Blender data structures.
    // https://developer.blender.org/diffusion/B/browse/master/source/blender/makesdna/DNA_meshdata_types.h
    struct MVert
    {
        float co[3];
        short no[3];
        char flag, bweight;
    };

    struct MLoop
    {
        // Vertex index.
        unsigned int v;
        unsigned int e;
    };

    struct MLoopTri
    {
        // Vertex id's.
        unsigned int tri[3];
        unsigned int poly;
    };

    struct MLoopUV
    {
        float uv[2];
        int flag;
    };

    struct MPoly
    {
        int loopstart;
        int totloop;
        short mat_nr;
        char flag, pad;
    };
}

//
// The following function takes a series of pointers to Blender mesh data
// and modifies the appleseed MeshObject entity.
//
// blender_mesh: the appleseed MeshObject created earlier in the export process
// bl_looptris: Pointer to the loop triangle array of the Blender mesh.  Each looptri stores the index of the three loops it references plus the polygon
// bl_loops: Pointer to the loops array of the mesh.  Each loop stores an index into the vertex array of the Blender mesh
// bl_polys: Pointer to the polygon array of the Blender mesh.  This poly contains the material index
// bl_loops_uv: Pointer to the loop UV array.  Each stores a singe UV coordinate.  This array is kept in sync with the loops array
//

void convert_bl_mesh(
    MeshObject*         blender_mesh,
    const size_t        bl_looptri_count,
    const uintptr_t     bl_looptri_ptr,
    const size_t        bl_loop_count,
    const uintptr_t     bl_loops_ptr,
    const uintptr_t     bl_polys_ptr,
    const uintptr_t     bl_vertices_ptr,
    const uintptr_t     bl_loops_uv_ptr,
    const bool          export_normals,
    const bool          export_uvs)
{
    // Convert uintptr_t numbers to actual pointers.
    const MLoopTri* bl_looptri_array = reinterpret_cast<MLoopTri*>(bl_looptri_ptr);
    const MLoop* bl_loop_array = reinterpret_cast<MLoop*>(bl_loops_ptr);
    const MPoly* bl_poly_array = reinterpret_cast<MPoly*>(bl_polys_ptr);
    const MVert* bl_vert_array = reinterpret_cast<MVert*>(bl_vertices_ptr);
    const MLoopUV* bl_loop_uv_array = reinterpret_cast<MLoopUV*>(bl_loops_uv_ptr);

    // Push vertices
    blender_mesh->reserve_vertices(bl_loop_count);

    for (size_t loop_index = 0; loop_index < bl_loop_count; ++loop_index)
    {
        const MLoop& bl_loop = bl_loop_array[loop_index];
        const MVert& bl_vert = bl_vert_array[bl_loop.v];
        blender_mesh->push_vertex(
            GVector3(
                bl_vert.co[0],
                bl_vert.co[1],
                bl_vert.co[2]));
    }

    // Push faces
    blender_mesh->reserve_triangles(bl_looptri_count);

    for (size_t looptri_index = 0; looptri_index < bl_looptri_count; ++looptri_index)
    {
        const MLoopTri& bl_looptri = bl_looptri_array[looptri_index];
        short mat_index = bl_poly_array[bl_looptri.poly].mat_nr;
        blender_mesh->push_triangle(
            Triangle(
                bl_looptri.tri[0],
                bl_looptri.tri[1],
                bl_looptri.tri[2],
                mat_index));
    }

    // Push normals
    if (export_normals)
    {
        for (size_t loop_index = 0; loop_index < bl_loop_count; ++loop_index)
        {
            const MLoop& bl_loop = bl_loop_array[loop_index];
            const MVert& bl_vert = bl_vert_array[bl_loop.v];
            blender_mesh->push_vertex_normal(
                GVector3(
                    bl_vert.no[0],
                    bl_vert.no[1],
                    bl_vert.no[2]));
        }

        for (size_t looptri_index = 0; looptri_index < bl_looptri_count; ++looptri_index)
        {
            const MLoopTri& bl_looptri = bl_looptri_array[looptri_index];
            Triangle& as_tri = blender_mesh->get_triangle(looptri_index);
            as_tri.m_n0 = bl_looptri.tri[0];
            as_tri.m_n1 = bl_looptri.tri[1];
            as_tri.m_n2 = bl_looptri.tri[2];
        }
    }

    // Push UV coordinates
    if (export_uvs)
    {
        for (size_t loop_index = 0; loop_index < bl_loop_count; ++loop_index)
        {
            const MLoopUV& bl_loop_uv = bl_loop_uv_array[loop_index];
            blender_mesh->push_tex_coords(
                GVector2(
                    bl_loop_uv.uv[0],
                    bl_loop_uv.uv[1]));
        }

        for (size_t looptri_index = 0; looptri_index < bl_looptri_count; ++looptri_index)
        {
            const MLoopTri& bl_looptri = bl_looptri_array[looptri_index];
            Triangle& as_tri = blender_mesh->get_triangle(looptri_index);
            as_tri.m_a0 = bl_looptri.tri[0];
            as_tri.m_a1 = bl_looptri.tri[1];
            as_tri.m_a2 = bl_looptri.tri[2];
        }
    }
}

void convert_bl_vertex_pose(
    MeshObject*         blender_mesh,
    const size_t        pose,
    const uintptr_t     bl_loop_ptr,
    const size_t        bl_loop_count,
    const uintptr_t     bl_vert_ptr,
    const bool          export_normals)
{
    // Convert uintptr_t numbers to actual pointers.
    const MVert* bl_vert_array = reinterpret_cast<MVert*>(bl_vert_ptr);
    const MLoop* bl_loop_array = reinterpret_cast<MLoop *>(bl_loop_ptr);

    // Push vertices.
    for (size_t loop_index = 0; loop_index < bl_loop_count; ++loop_index)
    {
        const MLoop &bl_loop = bl_loop_array[loop_index];
        const MVert &bl_vert = bl_vert_array[bl_loop.v];
        blender_mesh->set_vertex_pose(
            loop_index,
            pose,
            GVector3(
                bl_vert.co[0],
                bl_vert.co[1],
                bl_vert.co[2]));
    }

    // Push normals.
    if (export_normals)
    {
        for (size_t loop_index = 0; loop_index < bl_loop_count; ++loop_index)
        {
            const MLoop &bl_loop = bl_loop_array[loop_index];
            const MVert &bl_vert = bl_vert_array[bl_loop.v];
            blender_mesh->set_vertex_normal_pose(
                loop_index,
                pose,
                GVector3(
                    bl_vert.no[0],
                    bl_vert.no[1],
                    bl_vert.no[2]));
        }
    }
}

void bind_blender_mesh_converter()
{
    bpy::def("convert_bl_mesh", &convert_bl_mesh);
    bpy::def("convert_bl_vertex_pose", &convert_bl_vertex_pose);
}
