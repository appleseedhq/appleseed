
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

TEST_SUITE(Foundation_Mesh_OBJMeshFileReader)
{
    using namespace foundation;
    using namespace std;

    struct Face
    {
        vector<size_t>      m_vertices;
        vector<size_t>      m_vertex_normals;
        vector<size_t>      m_tex_coords;
        size_t              m_material;
    };

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

        // Begin the definition of a mesh.
        virtual void begin_mesh(const string& name)
        {
            m_meshes.push_back(Mesh());
            m_meshes.back().m_name = name;
        }

        // Append a vertex to the mesh.
        virtual size_t push_vertex(const Vector3d& v)
        {
            m_meshes.back().m_vertices.push_back(v);
            return m_meshes.back().m_vertices.size() - 1;
        }

        // Append a vertex normal to the mesh. The normal is unit-length.
        virtual size_t push_vertex_normal(const Vector3d& v)
        {
            m_meshes.back().m_vertex_normals.push_back(v);
            return m_meshes.back().m_vertex_normals.size() - 1;
        }

        // Append a texture coordinate to the mesh.
        virtual size_t push_tex_coords(const Vector2d& v)
        {
            m_meshes.back().m_tex_coords.push_back(v);
            return m_meshes.back().m_tex_coords.size() - 1;
        }

        // Begin the definition of a face.
        virtual void begin_face(const size_t vertex_count)
        {
            Face face;
            face.m_vertices.resize(vertex_count);
            face.m_vertex_normals.resize(vertex_count);
            face.m_tex_coords.resize(vertex_count);
            face.m_material = ~size_t(0);
            m_meshes.back().m_faces.push_back(face);
        }

        // Assign vertices to the face.
        virtual void set_face_vertices(const size_t vertices[])
        {
            Face& face = m_meshes.back().m_faces.back();
            for (size_t i = 0; i < face.m_vertices.size(); ++i)
                face.m_vertices[i] = vertices[i];
        }

        // Assign vertex normals to the face.
        virtual void set_face_vertex_normals(const size_t vertex_normals[])
        {
            Face& face = m_meshes.back().m_faces.back();
            for (size_t i = 0; i < face.m_vertex_normals.size(); ++i)
                face.m_vertex_normals[i] = vertex_normals[i];
        }

        // Assign texture coordinates to the face.
        virtual void set_face_vertex_tex_coords(const size_t tex_coords[])
        {
            Face& face = m_meshes.back().m_faces.back();
            for (size_t i = 0; i < face.m_tex_coords.size(); ++i)
                face.m_tex_coords[i] = tex_coords[i];
        }

        // Assign a material to the face.
        virtual void set_face_material(const size_t material)
        {
            m_meshes.back().m_faces.back().m_material = material;
        }
    };

    struct Fixture
    {
        OBJMeshFileReader   m_reader;
        MeshBuilder         m_builder;
    };

    TEST_CASE_WITH_FIXTURE(ReadCubeMeshFile, Fixture)
    {
        m_reader.read("data/cube.obj", m_builder);

        EXPECT_EQ(1, m_builder.m_meshes.size());

        Mesh& mesh = m_builder.m_meshes.front();

        EXPECT_EQ("", mesh.m_name);
        EXPECT_EQ(20, mesh.m_vertices.size());
        EXPECT_EQ(6, mesh.m_vertex_normals.size());
        EXPECT_EQ(20, mesh.m_tex_coords.size());
        EXPECT_EQ(12, mesh.m_faces.size());
    }

    TEST_CASE_WITH_FIXTURE(ReadQuadMeshFile, Fixture)
    {
        m_reader.read("data/quad.obj", m_builder);

        EXPECT_EQ(1, m_builder.m_meshes.size());

        Mesh& mesh = m_builder.m_meshes.front();

        EXPECT_EQ(1, m_builder.m_meshes.size());
        EXPECT_EQ("quad", mesh.m_name);
        EXPECT_EQ(4, mesh.m_vertices.size());
        EXPECT_EQ(0, mesh.m_vertex_normals.size());
        EXPECT_EQ(4, mesh.m_tex_coords.size());
        EXPECT_EQ(1, mesh.m_faces.size());
    }
}
