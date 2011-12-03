
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

// Interface header.
#include "meshobjectreader.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/triangulator.h"
#include "foundation/mesh/genericmeshfilereader.h"
#include "foundation/mesh/imeshbuilder.h"
#include "foundation/mesh/imeshfilereader.h"
#include "foundation/mesh/objmeshfilereader.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <exception>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MeshObjectArray class implementation.
//

DEFINE_ARRAY(MeshObjectArray);


//
// MeshObjectReader class implementation.
//

namespace
{
    class MeshObjectBuilder
      : public IMeshBuilder
    {
      public:
        typedef vector<MeshObject*> MeshObjectVector;

        MeshObjectBuilder(
            const ParamArray&   params,
            const string&       base_object_name)
          : m_params(params)
          , m_ignore_vertex_normals(m_params.get_optional<bool>("ignore_vertex_normals"))
          , m_base_object_name(base_object_name)
          , m_untitled_mesh_counter(0)
          , m_vertex_count(0)
          , m_face_material(0)
          , m_face_count(0)
          , m_triangulation_error_count(0)
        {
        }

        const MeshObjectVector& get_objects() const
        {
            return m_objects;
        }

        virtual void begin_mesh(const string& name)
        {
            // If the mesh has no name, assign it a number (starting with 0).
            const string mesh_name = name.empty() ? to_string(m_untitled_mesh_counter++) : name;

            // Construct the final object name from the base object name and the mesh name.
            const string object_name = m_base_object_name + "." + mesh_name;

            // Create an empty mesh object.
            m_objects.push_back(
                MeshObjectFactory::create(object_name.c_str(), m_params).release());

            m_face_count = 0;
            m_triangulation_error_count = 0;
        }

        virtual void end_mesh()
        {
            // Print the number of faces that could not be triangulated (if any).
            if (m_triangulation_error_count > 0)
            {
                RENDERER_LOG_WARNING(
                    "%s polygonal %s (out of %s) could not be triangulated",
                    pretty_int(m_triangulation_error_count).c_str(),
                    plural(m_triangulation_error_count, "face").c_str(),
                    pretty_int(m_face_count).c_str());
            }

            // Print the number of vertices and triangles in the mesh.
            const size_t vertex_count = m_objects.back()->get_vertex_count();
            const size_t triangle_count = m_objects.back()->get_triangle_count();
            RENDERER_LOG_INFO(
                "loaded mesh object \"%s\" (%s %s, %s %s)",
                m_objects.back()->get_name(),
                pretty_int(vertex_count).c_str(),
                plural(vertex_count, "vertex", "vertices").c_str(),
                pretty_int(triangle_count).c_str(),
                plural(triangle_count, "triangle").c_str());
        }

        virtual size_t push_vertex(const Vector3d& v)
        {
            return m_objects.back()->push_vertex(GVector3(v));
        }

        virtual size_t push_vertex_normal(const Vector3d& v)
        {
            return m_objects.back()->push_vertex_normal(GVector3(v));
        }

        virtual size_t push_tex_coords(const Vector2d& v)
        {
            return m_objects.back()->push_tex_coords(GVector2(v));
        }

        virtual void begin_face(const size_t vertex_count)
        {
            assert(vertex_count >= 3);

            clear_keep_memory(m_face_vertices);
            clear_keep_memory(m_face_normals);
            clear_keep_memory(m_face_tex_coords);

            ++m_face_count;

            m_vertex_count = vertex_count;
            m_face_material = Triangle::None;
        }

        virtual void end_face()
        {
            assert(m_face_vertices.size() == m_vertex_count);
            assert(m_face_normals.size() == 0 || m_face_normals.size() == m_vertex_count);
            assert(m_face_tex_coords.size() == 0 || m_face_tex_coords.size() == m_vertex_count);

            if (m_vertex_count > 3)
            {
                // Create the polygon to triangulate.
                clear_keep_memory(m_polygon);
                for (size_t i = 0; i < m_vertex_count; ++i)
                {
                    m_polygon.push_back(
                        Vector3d(m_objects.back()->get_vertex(m_face_vertices[i])));
                }

                // Triangulate the polygon.
                clear_keep_memory(m_triangles);
                if (!m_triangulator.triangulate(m_polygon, m_triangles))
                {
                    // Skip problematic polygonal faces.
                    ++m_triangulation_error_count;
                    return;
                }

                // Insert all triangles of the triangulation into the mesh.
                const size_t m_triangle_count = m_triangles.size();
                for (size_t i = 0; i < m_triangle_count; i += 3)
                {
                    insert_triangle(
                        m_triangles[i + 0],
                        m_triangles[i + 1],
                        m_triangles[i + 2]);
                }
            }
            else
            {
                // The face is already a triangle, no triangulation is necessary.
                insert_triangle(0, 1, 2);
            }
        }

        virtual void set_face_vertices(const size_t vertices[])
        {
            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_vertices.push_back(static_cast<uint32>(vertices[i]));
        }

        virtual void set_face_vertex_normals(const size_t vertex_normals[])
        {
            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_normals.push_back(static_cast<uint32>(vertex_normals[i]));
        }

        virtual void set_face_vertex_tex_coords(const size_t tex_coords[])
        {
            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_tex_coords.push_back(static_cast<uint32>(tex_coords[i]));
        }

        virtual void set_face_material(const size_t material)
        {
            m_face_material = static_cast<uint32>(material);
        }

      private:
        const ParamArray        m_params;
        const bool              m_ignore_vertex_normals;
        const string            m_base_object_name;

        size_t                  m_untitled_mesh_counter;
        MeshObjectVector        m_objects;

        size_t                  m_vertex_count;
        vector<uint32>          m_face_vertices;
        vector<uint32>          m_face_normals;
        vector<uint32>          m_face_tex_coords;
        uint32                  m_face_material;

        Triangulator<double>    m_triangulator;
        vector<Vector3d>        m_polygon;
        vector<size_t>          m_triangles;

        size_t                  m_face_count;
        size_t                  m_triangulation_error_count;

        void insert_triangle(
            const size_t        v0_index,
            const size_t        v1_index,
            const size_t        v2_index)
        {
            Triangle triangle;

            // Set triangle vertices.
            triangle.m_v0 = m_face_vertices[v0_index];
            triangle.m_v1 = m_face_vertices[v1_index];
            triangle.m_v2 = m_face_vertices[v2_index];

            // Set triangle vertex normals.
            if (!m_ignore_vertex_normals && m_face_normals.size() == m_vertex_count)
            {
                triangle.m_n0 = m_face_normals[v0_index];
                triangle.m_n1 = m_face_normals[v1_index];
                triangle.m_n2 = m_face_normals[v2_index];
            }
            else
            {
                // Fetch the triangle vertices.
                const Vector3d v0 = Vector3d(m_objects.back()->get_vertex(triangle.m_v0));
                const Vector3d v1 = Vector3d(m_objects.back()->get_vertex(triangle.m_v1));
                const Vector3d v2 = Vector3d(m_objects.back()->get_vertex(triangle.m_v2));

                // Compute the geometric normal to the triangle.
                const Vector3d geometric_normal = normalize(cross(v1 - v0, v2 - v0));

                // Insert the geometric normal into the mesh.
                const size_t geometric_normal_index =
                    m_objects.back()->push_vertex_normal(GVector3(geometric_normal));

                // Assign the geometric normal to all vertices of the triangle.
                triangle.m_n0 = geometric_normal_index;
                triangle.m_n1 = geometric_normal_index;
                triangle.m_n2 = geometric_normal_index;
            }

            // Set triangle vertex texture coordinates (if any).
            if (m_face_tex_coords.size() == m_vertex_count)
            {
                triangle.m_a0 = m_face_tex_coords[v0_index];
                triangle.m_a1 = m_face_tex_coords[v1_index];
                triangle.m_a2 = m_face_tex_coords[v2_index];
            }
            else
            {
                triangle.m_a0 = Triangle::None;
                triangle.m_a1 = Triangle::None;
                triangle.m_a2 = Triangle::None;
            }

            // Set triangle material.
//          triangle.m_pa = m_face_material;
            triangle.m_pa = 0;

            // Insert the triangle into the mesh.
            m_objects.back()->push_triangle(triangle);
        }
    };
}

MeshObjectArray MeshObjectReader::read(
    const char*         filename,
    const char*         base_object_name,
    const ParamArray&   params)
{
    assert(filename);
    assert(base_object_name);

    GenericMeshFileReader reader;
    MeshObjectBuilder builder(params, base_object_name);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        reader.read(filename, builder);
    }
    catch (const OBJMeshFileReader::ExceptionInvalidFaceDef& e)
    {
        RENDERER_LOG_ERROR(
            "failed to load mesh file %s: invalid face definition on line " FMT_SIZE_T,
            filename,
            e.m_line);
        return MeshObjectArray();
    }
    catch (const OBJMeshFileReader::ExceptionParseError& e)
    {
        RENDERER_LOG_ERROR(
            "failed to load mesh file %s: parse error on line " FMT_SIZE_T,
            filename,
            e.m_line);
        return MeshObjectArray();
    }
    catch (const ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
            "failed to load mesh file %s: i/o error",
            filename);
        return MeshObjectArray();
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to load mesh file %s: %s",
            filename,
            e.what());
        return MeshObjectArray();
    }

    stopwatch.measure();

    MeshObjectArray objects;
    for (const_each<vector<MeshObject*> > i = builder.get_objects(); i; ++i)
        objects.push_back(*i);

    // Print the number of loaded objects.
    RENDERER_LOG_INFO(
        "loaded mesh file %s (%s %s) in %s",
        filename,
        pretty_int(objects.size()).c_str(),
        plural(objects.size(), "object").c_str(),
        pretty_time(stopwatch.get_seconds()).c_str());

    return objects;
}

}   // namespace renderer
