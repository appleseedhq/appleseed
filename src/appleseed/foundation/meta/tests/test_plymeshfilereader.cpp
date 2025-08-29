#include "foundation/meshio/meshbuilderbase.h"
#include "foundation/meshio/plymeshfilereader.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

#include <iostream>

using namespace foundation;

TEST_SUITE(Foundation_Mesh_PLYMeshFileReader)
{
    struct Face
    {
        std::vector<size_t>     m_vertices;
    };

    struct Mesh
    {
        std::string m_name;
        std::vector<Vector3d>   m_vertices;
        std::vector<Vector3d>   m_vertex_normals;
        std::vector<Vector2d>   m_tex_coords;
        std::vector<Face>       m_faces;
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
            assert(!m_meshes.empty());
            m_meshes.back().m_vertices.push_back(v);
            return m_meshes.back().m_vertices.size() - 1;
        }

        size_t push_vertex_normal(const Vector3d& v) override
        {
            assert(!m_meshes.empty());
            m_meshes.back().m_vertex_normals.push_back(safe_normalize(v));
            return m_meshes.back().m_vertex_normals.size() - 1;
        }

        size_t push_tex_coords(const Vector2d& v) override
        {
            assert(!m_meshes.empty());
            m_meshes.back().m_tex_coords.push_back(v);
            return m_meshes.back().m_tex_coords.size() - 1;
        }

        void begin_face(const size_t vertex_count) override
        {
            assert(!m_meshes.empty());
            m_meshes.back().m_faces.emplace_back();
            Face& face = m_meshes.back().m_faces.back();
            face.m_vertices.resize(vertex_count);
        }

        void set_face_vertices(const size_t vertices[]) override
        {
            assert(!meshes.empty());
            assert(!m_meshes.back().m_faces.empty());
            Face& face = m_meshes.back().m_faces.back();
            for (size_t i = 0; i < face.m_vertices.size(); ++i)
                face.m_vertices[i] = vertices[i];
        }

    };

    TEST_CASE(ReadCubeMeshFile)
    {
        std::cout << "TEST PLY!" << std::endl;

        const std::string igd_ply = "/home/petra/Documents/Projects/SimVision/2025-21-04 Example ply with additional data/Endmill_48_everything.ply";
        const std::string test_ply = "unit tests/inputs/test_plymeshfilereader_cube.ply";
        PLYMeshFileReader reader(igd_ply);
        MeshBuilder builder;
        reader.read(builder);

        assert(builder.m_meshes.size() == 1);
        
        FILE* f = fopen("output.cpp", "wt");
        assert(f);

        const Mesh& m = builder.m_meshes.front();

        fprintf(f, "static const float Vertices[] =\n{\n");
        for (const auto& v : m.m_vertices)
        {
            fprintf(f, "    %.7ff, %.7ff, %.7ff,\n", v.x, v.y, v.z);
        }
        fprintf(f, "};\n\n");

        fprintf(f, "static const size_t Triangles[] =\n{\n");
        for (const auto& face : m.m_faces)
        {
            fprintf(
                f,
                "    " FMT_SIZE_T ", " FMT_SIZE_T ", " FMT_SIZE_T ",\n",
                face.m_vertices[0], face.m_vertices[1], face.m_vertices[2]);
        }
        fprintf(f, "};\n");

        fclose(f);
    }
}
