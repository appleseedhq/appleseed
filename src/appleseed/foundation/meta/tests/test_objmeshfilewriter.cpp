
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
#include "foundation/mesh/imeshwalker.h"
#include "foundation/mesh/meshbuilderbase.h"
#include "foundation/mesh/objmeshfilereader.h"
#include "foundation/mesh/objmeshfilewriter.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Mesh_OBJMeshFileWriter)
{
    struct Face
    {
        size_t m_v0, m_v1, m_v2;
    };

    struct Mesh
    {
        string              m_name;
        vector<Vector3d>    m_vertices;
        vector<Face>        m_faces;
    };

    struct MeshBuilder
      : public MeshBuilderBase
    {
        vector<Mesh> m_meshes;

        virtual void begin_mesh(const char* name) APPLESEED_OVERRIDE
        {
            m_meshes.push_back(Mesh());
            m_meshes.back().m_name = name;
        }

        virtual size_t push_vertex(const Vector3d& v) APPLESEED_OVERRIDE
        {
            m_meshes.back().m_vertices.push_back(v);
            return m_meshes.back().m_vertices.size() - 1;
        }

        virtual void begin_face(const size_t vertex_count) APPLESEED_OVERRIDE
        {
            assert(vertex_count == 3);
            m_meshes.back().m_faces.push_back(Face());
        }

        virtual void set_face_vertices(const size_t vertices[]) APPLESEED_OVERRIDE
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

        virtual const char* get_name() const APPLESEED_OVERRIDE
        {
            return m_mesh.m_name.c_str();
        }

        virtual size_t get_vertex_count() const APPLESEED_OVERRIDE
        {
            return m_mesh.m_vertices.size();
        }

        virtual Vector3d get_vertex(const size_t i) const APPLESEED_OVERRIDE
        {
            return m_mesh.m_vertices[i];
        }

        virtual size_t get_vertex_normal_count() const APPLESEED_OVERRIDE
        {
            return 0;
        }

        virtual Vector3d get_vertex_normal(const size_t i) const APPLESEED_OVERRIDE
        {
            return Vector3d();
        }

        virtual size_t get_tex_coords_count() const APPLESEED_OVERRIDE
        {
            return 0;
        }

        virtual Vector2d get_tex_coords(const size_t i) const APPLESEED_OVERRIDE
        {
            return Vector2d();
        }

        virtual size_t get_material_slot_count() const APPLESEED_OVERRIDE
        {
            return 0;
        }

        virtual const char* get_material_slot(const size_t i) const APPLESEED_OVERRIDE
        {
            return 0;
        }

        virtual size_t get_face_count() const APPLESEED_OVERRIDE
        {
            return m_mesh.m_faces.size();
        }

        virtual size_t get_face_vertex_count(const size_t face_index) const APPLESEED_OVERRIDE
        {
            return 3;
        }

        virtual size_t get_face_vertex(const size_t face_index, const size_t vertex_index) const APPLESEED_OVERRIDE
        {
            assert(vertex_index < 3);
            return (&m_mesh.m_faces[face_index].m_v0)[vertex_index];
        }

        virtual size_t get_face_vertex_normal(const size_t face_index, const size_t vertex_index) const APPLESEED_OVERRIDE
        {
            return None;
        }

        virtual size_t get_face_tex_coords(const size_t face_index, const size_t vertex_index) const APPLESEED_OVERRIDE
        {
            return None;
        }

        virtual size_t get_face_material(const size_t face_index) const APPLESEED_OVERRIDE
        {
            return None;
        }
    };

    Mesh create_mesh(const string& name)
    {
        Mesh mesh;
        mesh.m_name = name;

        mesh.m_vertices.push_back(Vector3d(0.0, 0.0, 0.0));
        mesh.m_vertices.push_back(Vector3d(1.0, 0.0, 0.0));
        mesh.m_vertices.push_back(Vector3d(1.0, 1.0, 0.0));

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
