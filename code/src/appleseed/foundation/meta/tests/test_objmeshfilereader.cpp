
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/mesh/meshbuilderbase.h"
#include "foundation/mesh/objmeshfilereader.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Mesh_OBJMeshFileReader)
{
    struct Face {};

    struct Mesh
    {
        string              m_name;
        vector<Vector3d>    m_vertices;
        vector<Vector3d>    m_vertex_normals;
        vector<Vector2d>    m_tex_coords;
        vector<Face>        m_faces;
    };

    struct MeshBuilder
      : public MeshBuilderBase
    {
        vector<Mesh>        m_meshes;

        void begin_mesh(const char* name) override
        {
            m_meshes.emplace_back();
            m_meshes.back().m_name = name;
        }

        size_t push_vertex(const Vector3d& v) override
        {
            m_meshes.back().m_vertices.push_back(v);
            return m_meshes.back().m_vertices.size() - 1;
        }

        size_t push_vertex_normal(const Vector3d& v) override
        {
            m_meshes.back().m_vertex_normals.push_back(safe_normalize(v));
            return m_meshes.back().m_vertex_normals.size() - 1;
        }

        size_t push_tex_coords(const Vector2d& v) override
        {
            m_meshes.back().m_tex_coords.push_back(v);
            return m_meshes.back().m_tex_coords.size() - 1;
        }

        void begin_face(const size_t vertex_count) override
        {
            m_meshes.back().m_faces.emplace_back();
        }
    };

    TEST_CASE(ReadCubeMeshFile)
    {
        OBJMeshFileReader reader("unit tests/inputs/test_objmeshfilereader_cube.obj");
        MeshBuilder builder;
        reader.read(builder);

        EXPECT_EQ(1, builder.m_meshes.size());

        Mesh& mesh = builder.m_meshes.front();
        EXPECT_EQ("", mesh.m_name);
        EXPECT_EQ(20, mesh.m_vertices.size());
        EXPECT_EQ(6, mesh.m_vertex_normals.size());
        EXPECT_EQ(20, mesh.m_tex_coords.size());
        EXPECT_EQ(12, mesh.m_faces.size());
    }

    TEST_CASE(ReadQuadMeshFile)
    {
        OBJMeshFileReader reader("unit tests/inputs/test_objmeshfilereader_quad.obj");
        MeshBuilder builder;
        reader.read(builder);

        EXPECT_EQ(1, builder.m_meshes.size());

        Mesh& mesh = builder.m_meshes.front();
        EXPECT_EQ("quad", mesh.m_name);
        EXPECT_EQ(4, mesh.m_vertices.size());
        EXPECT_EQ(0, mesh.m_vertex_normals.size());
        EXPECT_EQ(4, mesh.m_tex_coords.size());
        EXPECT_EQ(1, mesh.m_faces.size());
    }
}
