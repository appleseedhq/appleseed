
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/meshio/imeshwalker.h"
#include "foundation/meshio/meshbuilderbase.h"
#include "foundation/meshio/objmeshfilereader.h"
#include "foundation/meshio/objmeshfilewriter.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Mesh_OBJMeshFileWriter)
{
    struct Face
    {
        size_t m_v0, m_v1, m_v2;
    };

    struct Mesh
    {
        std::string              m_name;
        std::vector<Vector3d>    m_vertices;
        std::vector<Face>        m_faces;
    };

    struct MeshBuilder
      : public MeshBuilderBase
    {
        std::vector<Mesh> m_meshes;

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

        void begin_face(const size_t vertex_count) override
        {
            assert(vertex_count == 3);
            m_meshes.back().m_faces.emplace_back();
        }

        void set_face_vertices(const size_t vertices[]) override
        {
            Face& face = m_meshes.back().m_faces.back();
            face.m_v0 = vertices[0];
            face.m_v1 = vertices[1];
            face.m_v2 = vertices[2];
        }
    };

    struct MeshWalker
      : public IMeshWalker
    {
        const Mesh& m_mesh;

        explicit MeshWalker(const Mesh& mesh)
          : m_mesh(mesh)
        {
        }

        const char* get_name() const override
        {
            return m_mesh.m_name.c_str();
        }

        size_t get_vertex_count() const override
        {
            return m_mesh.m_vertices.size();
        }

        Vector3d get_vertex(const size_t i) const override
        {
            return m_mesh.m_vertices[i];
        }

        size_t get_vertex_normal_count() const override
        {
            return 0;
        }

        Vector3d get_vertex_normal(const size_t i) const override
        {
            return Vector3d();
        }

        size_t get_tex_coords_count() const override
        {
            return 0;
        }

        Vector2d get_tex_coords(const size_t i) const override
        {
            return Vector2d();
        }

        size_t get_material_slot_count() const override
        {
            return 0;
        }

        const char* get_material_slot(const size_t i) const override
        {
            return nullptr;
        }

        size_t get_face_count() const override
        {
            return m_mesh.m_faces.size();
        }

        size_t get_face_vertex_count(const size_t face_index) const override
        {
            return 3;
        }

        size_t get_face_vertex(const size_t face_index, const size_t vertex_index) const override
        {
            assert(vertex_index < 3);
            return (&m_mesh.m_faces[face_index].m_v0)[vertex_index];
        }

        size_t get_face_vertex_normal(const size_t face_index, const size_t vertex_index) const override
        {
            return None;
        }

        size_t get_face_tex_coords(const size_t face_index, const size_t vertex_index) const override
        {
            return None;
        }

        size_t get_face_material(const size_t face_index) const override
        {
            return None;
        }
    };

    Mesh create_mesh(const std::string& name)
    {
        Mesh mesh;
        mesh.m_name = name;

        mesh.m_vertices.emplace_back(0.0, 0.0, 0.0);
        mesh.m_vertices.emplace_back(1.0, 0.0, 0.0);
        mesh.m_vertices.emplace_back(1.0, 1.0, 0.0);

        Face face;
        face.m_v0 = 0;
        face.m_v1 = 1;
        face.m_v2 = 2;
        mesh.m_faces.push_back(face);

        return mesh;
    }

    TEST_CASE(WriteOneObjectToFile)
    {
        const Mesh mesh = create_mesh("mesh");

        OBJMeshFileWriter writer("unit tests/outputs/test_objmeshfilewriter_oneobject.obj");
        MeshWalker walker(mesh);
        writer.write(walker);
        writer.close();

        OBJMeshFileReader reader("unit tests/outputs/test_objmeshfilewriter_oneobject.obj");
        MeshBuilder builder;
        reader.read(builder);

        ASSERT_EQ(1, builder.m_meshes.size());

        Mesh& output_mesh = builder.m_meshes[0];
        EXPECT_EQ(mesh.m_name, output_mesh.m_name);
        EXPECT_EQ(mesh.m_vertices.size(), output_mesh.m_vertices.size());
        EXPECT_EQ(mesh.m_faces.size(), output_mesh.m_faces.size());
    }

    TEST_CASE(WriteTwoObjectsToFile)
    {
        const Mesh mesh1 = create_mesh("mesh1");
        const Mesh mesh2 = create_mesh("mesh2");

        OBJMeshFileWriter writer("unit tests/outputs/test_objmeshfilewriter_twoobjects.obj");
        MeshWalker walker1(mesh1);
        writer.write(walker1);
        MeshWalker walker2(mesh2);
        writer.write(walker2);
        writer.close();

        OBJMeshFileReader reader("unit tests/outputs/test_objmeshfilewriter_twoobjects.obj");
        MeshBuilder builder;
        reader.read(builder);

        ASSERT_EQ(2, builder.m_meshes.size());

        Mesh& output_mesh1 = builder.m_meshes[0];
        EXPECT_EQ(mesh1.m_name, output_mesh1.m_name);
        EXPECT_EQ(mesh1.m_vertices.size(), output_mesh1.m_vertices.size());
        EXPECT_EQ(mesh1.m_faces.size(), output_mesh1.m_faces.size());

        Mesh& output_mesh2 = builder.m_meshes[1];
        EXPECT_EQ(mesh2.m_name, output_mesh2.m_name);
        EXPECT_EQ(mesh2.m_vertices.size(), output_mesh2.m_vertices.size());
        EXPECT_EQ(mesh2.m_faces.size(), output_mesh2.m_faces.size());
    }
}
