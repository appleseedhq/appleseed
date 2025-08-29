
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

// third-party headers.
#include "happly.h"

// Standard headers.
#include <cstring>
#include <map>
#include <utility>
#include <vector>
#include <iostream>

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

        std::cout << "PLY mesh file reader should parse the file now." << std::endl;
        bool verbose = true;
        happly::PLYData plyIn(m_filename, verbose);

        std::vector<std::string> element_names = plyIn.getElementNames();
        for (const auto& element_name : element_names)
        {
            std::cout << "Element name: " << element_name << std::endl;
        }

        const auto plyIn.getElement("group").getProperty("group_id");
        const auto plyIn.getElement("group").getProperty("group_name");
        const auto plyIn.getElement("group").getProperty("face_indices");

        // 1) Read all vertices and faces as one mesh.
        // 2) Add group mapping to ply
        // 3) Create a mesh for each group.

        std::vector<std::array<double, 3>> vertices = plyIn.getVertexPositions<double>();
        std::vector<std::array<double, 3>> vertex_normals = plyIn.getVertexNormals<double>();
        std::vector<std::vector<size_t>> faces = plyIn.getFaceIndices();

        // get groups
        // for each group create a new mesh
        // for each face add face to mesh (it should reference m_vertices from impl to get vertex coordinates and faces to get vertex indices)

        impl.ensure_mesh_def();

        for (const auto& vertex : vertices)
        {
            impl.m_vertices.push_back(Vector3d(vertex[0], vertex[1], vertex[2]));
            impl.m_builder.push_vertex(Vector3d(vertex[0], vertex[1], vertex[2]));
        }

        for (const auto& vertex_normal : vertex_normals)
        {
            impl.m_normals.push_back(Vector3d(vertex_normal[0], vertex_normal[1], vertex_normal[2]));
            impl.m_builder.push_vertex_normal(Vector3d(vertex_normal[0], vertex_normal[1], vertex_normal[2]));
        }
        
        for (const auto& face : faces)
        {
            impl.m_builder.begin_face(face.size());
            impl.m_builder.set_face_vertices(&face.front());
            impl.m_builder.end_face();
        }

        impl.end_mesh_def();

        std::cout << "happly works!" << std::endl;
    }

    } // namespace foundation
