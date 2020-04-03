
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

// convertmeshfile headers.
#include "commandlinehandler.h"

// appleseed.common headers.
#include "application/application.h"
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/meshio/genericmeshfilereader.h"
#include "foundation/meshio/genericmeshfilewriter.h"
#include "foundation/meshio/imeshbuilder.h"
#include "foundation/meshio/imeshwalker.h"
#include "foundation/platform/compiler.h"
#include "foundation/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/log/log.h"

// Standard headers.
#include <cstddef>
#include <deque>
#include <exception>
#include <list>
#include <string>
#include <vector>

using namespace appleseed::convertmeshfile;
using namespace appleseed::common;
using namespace foundation;

namespace
{
    struct Face
    {
        std::vector<size_t>        m_vertices;
        std::vector<size_t>        m_vertex_normals;
        std::vector<size_t>        m_vertex_tex_coords;
        size_t                     m_material;
    };

    struct Mesh
    {
        std::string                m_name;
        std::vector<Vector3d>      m_vertices;
        std::vector<Vector3d>      m_vertex_normals;
        std::vector<Vector2d>      m_tex_coords;
        std::vector<std::string>   m_material_slots;
        std::deque<Face>           m_faces;
    };

    class MeshBuilder
      : public IMeshBuilder
    {
      public:
        const std::list<Mesh>& get_meshes() const
        {
            return m_meshes;
        }

        void begin_mesh(const char* name) override
        {
            m_current_mesh = Mesh();
            m_current_mesh.m_name = name;
        }

        size_t push_vertex(const Vector3d& v) override
        {
            m_current_mesh.m_vertices.push_back(v);
            return m_current_mesh.m_vertices.size() - 1;
        }

        size_t push_vertex_normal(const Vector3d& v) override
        {
            m_current_mesh.m_vertex_normals.push_back(safe_normalize(v));
            return m_current_mesh.m_vertex_normals.size() - 1;
        }

        size_t push_tex_coords(const Vector2d& v) override
        {
            m_current_mesh.m_tex_coords.push_back(v);
            return m_current_mesh.m_tex_coords.size() - 1;
        }

        size_t push_material_slot(const char* name) override
        {
            m_current_mesh.m_material_slots.emplace_back(name);
            return m_current_mesh.m_material_slots.size() - 1;
        }

        void begin_face(const size_t vertex_count) override
        {
            m_current_face = Face();
            m_current_face.m_vertices.resize(vertex_count);
            m_current_face.m_vertex_normals.resize(vertex_count);
            m_current_face.m_vertex_tex_coords.resize(vertex_count);
            m_current_face.m_material = 0;
        }

        void set_face_vertices(const size_t vertices[]) override
        {
            for (size_t i = 0; i < m_current_face.m_vertices.size(); ++i)
                m_current_face.m_vertices[i] = vertices[i];
        }

        void set_face_vertex_normals(const size_t vertex_normals[]) override
        {
            for (size_t i = 0; i < m_current_face.m_vertex_normals.size(); ++i)
                m_current_face.m_vertex_normals[i] = vertex_normals[i];
        }

        void set_face_vertex_tex_coords(const size_t tex_coords[]) override
        {
            for (size_t i = 0; i < m_current_face.m_vertex_tex_coords.size(); ++i)
                m_current_face.m_vertex_tex_coords[i] = tex_coords[i];
        }

        void set_face_material(const size_t material) override
        {
            m_current_face.m_material = material;
        }

        void end_face() override
        {
            m_current_mesh.m_faces.push_back(m_current_face);
        }

        void end_mesh() override
        {
            m_meshes.push_back(m_current_mesh);
        }

      private:
        std::list<Mesh>  m_meshes;
        Mesh             m_current_mesh;
        Face             m_current_face;
    };

    class MeshWalker
      : public IMeshWalker
    {
      public:
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
            return m_mesh.m_vertex_normals.size();
        }

        Vector3d get_vertex_normal(const size_t i) const override
        {
            return m_mesh.m_vertex_normals[i];
        }

        size_t get_tex_coords_count() const override
        {
            return m_mesh.m_tex_coords.size();
        }

        Vector2d get_tex_coords(const size_t i) const override
        {
            return m_mesh.m_tex_coords[i];
        }

        size_t get_material_slot_count() const override
        {
            return m_mesh.m_material_slots.size();
        }

        const char* get_material_slot(const size_t i) const override
        {
            return m_mesh.m_material_slots[i].c_str();
        }

        size_t get_face_count() const override
        {
            return m_mesh.m_faces.size();
        }

        size_t get_face_vertex_count(const size_t face_index) const override
        {
            return m_mesh.m_faces[face_index].m_vertices.size();
        }

        size_t get_face_vertex(const size_t face_index, const size_t vertex_index) const override
        {
            return m_mesh.m_faces[face_index].m_vertices[vertex_index];
        }

        size_t get_face_vertex_normal(const size_t face_index, const size_t vertex_index) const override
        {
            return m_mesh.m_faces[face_index].m_vertex_normals[vertex_index];
        }

        size_t get_face_tex_coords(const size_t face_index, const size_t vertex_index) const override
        {
            return m_mesh.m_faces[face_index].m_vertex_tex_coords[vertex_index];
        }

        size_t get_face_material(const size_t face_index) const override
        {
            return m_mesh.m_faces[face_index].m_material;
        }

      private:
        const Mesh& m_mesh;
    };

    void print_bbox(Logger& logger, const Mesh& mesh)
    {
        AABB3d bbox;
        bbox.invalidate();

        for (size_t i = 0; i < mesh.m_vertices.size(); ++i)
            bbox.insert(mesh.m_vertices[i]);

        LOG_INFO(
            logger,
            "mesh \"%s\" bounding box: (%f, %f, %f)-(%f, %f, %f).",
            mesh.m_name.c_str(),
            bbox.min[0], bbox.min[1], bbox.min[2],
            bbox.max[0], bbox.max[1], bbox.max[2]);
    }
}


//
// Entry point of convertmeshfile.
//

int main(int argc, char* argv[])
{
    // Construct the logger that will be used throughout the program.
    SuperLogger logger;

    // Make sure this build can run on this host.
    Application::check_compatibility_with_host(logger);

    // Make sure appleseed is correctly installed.
    Application::check_installation(logger);

    // Parse the command line.
    CommandLineHandler cl;
    cl.parse(argc, argv, logger);

    // Load an apply settings from the settings file.
    Dictionary settings;
    Application::load_settings("appleseed.tools.xml", settings, logger);
    logger.configure_from_settings(settings);

    // Apply command line arguments.
    cl.apply(logger);

    // Retrieve the input and output file paths.
    const std::string& input_filepath = cl.m_filenames.values()[0];
    const std::string& output_filepath = cl.m_filenames.values()[1];

    // Read the input mesh file.
    MeshBuilder builder;
    try
    {
        GenericMeshFileReader reader(input_filepath.c_str());
        reader.read(builder);
    }
    catch (const std::exception& e)
    {
        LOG_FATAL(
            logger,
            "could not read mesh file %s (%s).",
            input_filepath.c_str(),
            e.what());
    }

    // Print a warning message and exit if no mesh were defined in the input file.
    if (builder.get_meshes().empty())
    {
        LOG_WARNING(logger, "no mesh defined.");
        return 0;
    }

    // Optionally print the bounding box of each loaded mesh.
    if (cl.m_print_bboxes.is_set())
    {
        for (const_each<std::list<Mesh>> i = builder.get_meshes(); i; ++i)
            print_bbox(logger, *i);
    }

    // Write the output mesh file.
    GenericMeshFileWriter writer(output_filepath.c_str());
    try
    {
        for (const_each<std::list<Mesh>> i = builder.get_meshes(); i; ++i)
        {
            const MeshWalker walker(*i);
            writer.write(walker);
        }
    }
    catch (const std::exception& e)
    {
        LOG_FATAL(
            logger,
            "could not write mesh file %s (%s).",
            output_filepath.c_str(),
            e.what());
    }

    return 0;
}
