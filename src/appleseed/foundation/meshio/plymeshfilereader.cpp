
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
#include "plymeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/vector.h"
#include "foundation/memory/memory.h"
#include "foundation/meshio/imeshbuilder.h"

// Standard headers.
#include <cstring>
#include <map>
#include <utility>
#include <vector>

namespace foundation
{
    struct PLYMeshFileReader::Impl
    {
        const int       m_options;
        IMeshBuilder&   m_builder;

        // Current state.
        bool            m_inside_mesh_def;      // currently inside a mesh definition?
        std::string     m_current_mesh_name;    // name of the current mesh

        // Features defined in the file.
        std::vector<Vector3d>   m_vertices;
        std::vector<Vector3d>   m_normals;
        std::vector<Vector2d>   m_tex_coords;

        Impl(
            const int options,
            IMeshBuilder& builder)
        : m_options(options)
        , m_builder(builder)
        , m_inside_mesh_def(false)
        , m_current_mesh_name("default_mesh_name")
        {
        }

        void ensure_mesh_def()
        {
            if (!m_inside_mesh_def)
            {
                // Begin the definition of the new mesh.
                m_builder.begin_mesh(m_current_mesh_name.c_str());
                m_inside_mesh_def = true;
            }
        }

        void end_mesh_def()
        {
            if (m_inside_mesh_def)
            {
                // End the current mesh definition.
                m_builder.end_mesh();
                m_inside_mesh_def = false;
            }
        }
    };

    PLYMeshFileReader::PLYMeshFileReader(
        const std::string&  filename,
        const int           options
    )
    : m_filename(filename)
    , m_options(options)
    {
    }

    void PLYMeshFileReader::read(IMeshBuilder& builder)
    {
        Impl impl(m_options, builder);

        bool verbose = true;
        happly::PLYData plyIn(m_filename, verbose);

        // 1) Read all vertices and faces as one mesh.
        // 2) Add group mapping to ply.
        // 3) Create a mesh for each group.

        std::vector<std::array<double, 3>> vertices = plyIn.getVertexPositions<double>();
        std::vector<std::array<double, 3>> vertex_normals = plyIn.getVertexNormals<double>();
        std::vector<std::vector<size_t>> faces = plyIn.getFaceIndices();

        impl.m_vertices.reserve(vertices.size());
        for (const auto& v : vertices) {
            impl.m_vertices.emplace_back(v[0], v[1], v[2]);
        }

        impl.m_normals.reserve(vertex_normals.size());
        for (const auto& vn : vertex_normals) {
            impl.m_normals.emplace_back(vn[0], vn[1], vn[2]);
        }

        if (plyIn.hasElement("group"))
        {
            std::vector<std::string> group_names = get_group_names(plyIn);
            std::vector<std::vector<size_t>> group_faces = get_group_faces(plyIn);

            size_t              index_in_mesh; // index in the internal mesh (in `impl.m_builder`), as opposed to the index in the ply file
            std::vector<size_t> face_vertex_indices;

            for (size_t i = 0; i < group_names.size(); i++)
            {
                impl.m_current_mesh_name = group_names[i];
                impl.ensure_mesh_def();
                const size_t default_slot = impl.m_builder.push_material_slot("default");
    
                for (const auto& face_idx : group_faces[i])
                {

                    const std::vector<size_t> face_vertices = faces[face_idx];
                    for (const auto& v_idx : face_vertices)
                    {
                        index_in_mesh = impl.m_builder.push_vertex(
                            Vector3d(
                                vertices[v_idx][0],
                                vertices[v_idx][1],
                                vertices[v_idx][2]));
                                
                        impl.m_builder.push_vertex_normal(
                            Vector3d(
                                vertex_normals[v_idx][0],
                                vertex_normals[v_idx][1],
                                vertex_normals[v_idx][2]));

                        face_vertex_indices.emplace_back(index_in_mesh);
                    }

                    impl.m_builder.begin_face(face_vertex_indices.size());
                    impl.m_builder.set_face_vertices(&face_vertex_indices.front());
                    impl.m_builder.set_face_vertex_normals(&face_vertex_indices.front());
                    impl.m_builder.set_face_material(default_slot);
                    impl.m_builder.end_face();

                    face_vertex_indices.clear();
                }
                impl.end_mesh_def();
            }
        }
        else
        {
            impl.ensure_mesh_def();
            const size_t default_slot = impl.m_builder.push_material_slot("default");

            for (const auto& vertex : vertices)
            {
                impl.m_builder.push_vertex(Vector3d(vertex[0], vertex[1], vertex[2]));
            }
    
            for (const auto& vertex_normal : vertex_normals)
            {
                impl.m_builder.push_vertex_normal(Vector3d(vertex_normal[0], vertex_normal[1], vertex_normal[2]));
            }
    
            for (const auto& face : faces)
            {
                impl.m_builder.begin_face(face.size());
                impl.m_builder.set_face_vertices(&face.front());
                impl.m_builder.set_face_vertex_normals(&face.front());
                impl.m_builder.set_face_material(default_slot);
                impl.m_builder.end_face();
            }
            impl.end_mesh_def();

            impl.ensure_mesh_def();
        }
    }
    
    std::vector<std::string> PLYMeshFileReader::get_group_names(
        happly::PLYData& ply_input) const
    {
        std::vector<
            std::vector<size_t>> group_names_raw =
                ply_input.getElement("group").getListProperty<size_t>("group_name");
        
        // Works for ASCII (0–127) and UTF-8 as long as the file stored UTF-8 bytes.
        std::vector<std::string> names;
        names.reserve(group_names_raw.size());
        for (const auto& bytes : group_names_raw)
        {
            names.emplace_back(bytes.begin(), bytes.end());
        }

        return names;
    }
    
    std::vector<std::vector<size_t>> PLYMeshFileReader::get_group_faces(
        happly::PLYData& ply_input) const
    {
        std::vector<size_t> face_groups = ply_input.getElement("face").getProperty<size_t>("group_index");
        const size_t max_group_idx = *std::max_element(face_groups.begin(), face_groups.end());

        // Group by value
        std::vector<std::vector<size_t>> group_faces;
        group_faces.resize(max_group_idx + 1);
        for (size_t face_idx = 0; face_idx < face_groups.size(); face_idx++)
        {
            const size_t group_idx = face_groups[face_idx];
            group_faces[group_idx].push_back(face_idx);
        }
        
        return group_faces;
    }

} // namespace foundation
