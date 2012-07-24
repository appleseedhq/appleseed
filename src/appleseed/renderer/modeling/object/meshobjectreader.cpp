
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/scalar.h"
#include "foundation/math/triangulator.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/genericmeshfilereader.h"
#include "foundation/mesh/imeshbuilder.h"
#include "foundation/mesh/imeshfilereader.h"
#include "foundation/mesh/objmeshfilereader.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <exception>
#include <map>
#include <string>
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
          , m_triangulator(Triangulator<double>::KeepDegenerateTriangles)
          , m_face_count(0)
          , m_triangulation_error_count(0)
          , m_total_vertex_count(0)
          , m_total_triangle_count(0)
        {
        }

        const MeshObjectVector& get_objects() const
        {
            return m_objects;
        }

        size_t get_total_vertex_count() const
        {
            return m_total_vertex_count;
        }

        size_t get_total_triangle_count() const
        {
            return m_total_triangle_count;
        }

        virtual void begin_mesh(const string& mesh_name)
        {
            // Construct the object name.
            const string object_name = m_base_object_name + "." + make_unique_mesh_name(mesh_name);

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
                    "while loading mesh object \"%s\": %s polygonal %s (out of %s) could not be triangulated.",
                    m_objects.back()->get_name(),
                    pretty_int(m_triangulation_error_count).c_str(),
                    plural(m_triangulation_error_count, "face").c_str(),
                    pretty_int(m_face_count).c_str());
            }

            // Keep track of the total number of vertices and triangles that were loaded.
            m_total_vertex_count += m_objects.back()->get_vertex_count();
            m_total_triangle_count += m_objects.back()->get_triangle_count();
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
                assert(m_polygon.empty());
                assert(m_triangles.empty());

                // Create the polygon to triangulate.
                for (size_t i = 0; i < m_vertex_count; ++i)
                {
                    m_polygon.push_back(
                        Vector3d(m_objects.back()->get_vertex(m_face_vertices[i])));
                }

                // Triangulate the polygon.
                if (m_triangulator.triangulate(m_polygon, m_triangles))
                {
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
                    // The polygon could not be triangulated.
                    ++m_triangulation_error_count;

                    // Insert 0-area triangle to ensure the right number of triangles in the result mesh.
                    for (size_t i = 0; i < m_vertex_count - 2; ++i)
                        insert_triangle(0, 0, 0);
                }

                clear_keep_memory(m_polygon);
                clear_keep_memory(m_triangles);
            }
            else
            {
                // The face is already a triangle, no triangulation is necessary.
                insert_triangle(0, 1, 2);
            }
        }

        virtual void set_face_vertices(const size_t vertices[])
        {
            m_face_vertices.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_vertices[i] = static_cast<uint32>(vertices[i]);
        }

        virtual void set_face_vertex_normals(const size_t vertex_normals[])
        {
            m_face_normals.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_normals[i] = static_cast<uint32>(vertex_normals[i]);
        }

        virtual void set_face_vertex_tex_coords(const size_t tex_coords[])
        {
            m_face_tex_coords.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_tex_coords[i] = static_cast<uint32>(tex_coords[i]);
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
        map<string, size_t>     m_mesh_counters;

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

        size_t                  m_total_vertex_count;
        size_t                  m_total_triangle_count;

        string make_unique_mesh_name(string mesh_name)
        {
            if (mesh_name.empty())
                mesh_name = to_string(m_untitled_mesh_counter++);

            if (m_mesh_counters.find(mesh_name) == m_mesh_counters.end())
            {
                m_mesh_counters[mesh_name] = 0;
                return mesh_name;
            }

            return mesh_name + "." + to_string(m_mesh_counters[mesh_name]++);
        }

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
                const uint32 geometric_normal_index =
                    static_cast<uint32>(m_objects.back()->push_vertex_normal(GVector3(geometric_normal)));

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

    bool read_mesh_object(
        const char*             filename,
        const char*             base_object_name,
        const ParamArray&       params,
        MeshObjectArray&        objects)
    {
        GenericMeshFileReader reader(filename);

        const string obj_parsing_mode = params.get_optional<string>("obj_parsing_mode", "fast");

        if (obj_parsing_mode == "fast")
        {
            reader.set_obj_options(
                reader.get_obj_options() | OBJMeshFileReader::FavorSpeedOverPrecision);
        }
        else if (obj_parsing_mode == "precise")
        {
            // This is the default in foundation::OBJMeshFileReader.
        }
        else
        {
            RENDERER_LOG_WARNING(
                "while reading geometry for object \"%s\" from mesh file %s: "
                "invalid OBJ parsing mode: \"%s\"; valid values are \"precise\" and \"fast\", "
                "using default value \"fast\".",
                base_object_name,
                filename,
                obj_parsing_mode.c_str());

            reader.set_obj_options(
                reader.get_obj_options() | OBJMeshFileReader::FavorSpeedOverPrecision);
        }

        MeshObjectBuilder builder(params, base_object_name);

        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        try
        {
            reader.read(builder);
        }
        catch (const OBJMeshFileReader::ExceptionInvalidFaceDef& e)
        {
            RENDERER_LOG_ERROR(
                "failed to load mesh file %s: invalid face definition on line " FMT_SIZE_T ".",
                filename,
                e.m_line);

            return false;
        }
        catch (const OBJMeshFileReader::ExceptionParseError& e)
        {
            RENDERER_LOG_ERROR(
                "failed to load mesh file %s: parse error on line " FMT_SIZE_T ".",
                filename,
                e.m_line);

            return false;
        }
        catch (const ExceptionIOError&)
        {
            RENDERER_LOG_ERROR(
                "failed to load mesh file %s: i/o error.",
                filename);

            return false;
        }
        catch (const exception& e)
        {
            RENDERER_LOG_ERROR(
                "failed to load mesh file %s: %s.",
                filename,
                e.what());

            return false;
        }

        stopwatch.measure();

        RENDERER_LOG_INFO(
            "loaded mesh file %s (%s %s, %s %s, %s %s) in %s.",
            filename,
            pretty_int(builder.get_objects().size()).c_str(),
            plural(builder.get_objects().size(), "object").c_str(),
            pretty_int(builder.get_total_vertex_count()).c_str(),
            plural(builder.get_total_vertex_count(), "vertex", "vertices").c_str(),
            pretty_int(builder.get_total_triangle_count()).c_str(),
            plural(builder.get_total_triangle_count(), "triangle").c_str(),
            pretty_time(stopwatch.get_seconds()).c_str());

        objects = array_vector<MeshObjectArray>(builder.get_objects());

        return true;
    }

    bool set_vertex_poses(
        const MeshObjectArray&  objects,
        const MeshObjectArray&  objects_next,
        const size_t            motion_segment_index,
        const char*             filename,
        const char*             base_object_name)
    {
        if (objects.size() != objects_next.size())
        {
            RENDERER_LOG_ERROR(
                "while reading key frame for object \"%s\" from mesh file %s: "
                "expected " FMT_SIZE_T " object%s, got " FMT_SIZE_T ".",
                base_object_name,
                filename,
                objects.size(),
                objects.size() > 1 ? "s" : "",
                objects_next.size());

            return false;
        }

        for (size_t i = 0; i < objects.size(); ++i)
        {
            MeshObject* object = objects[i];
            const MeshObject* object_next = objects_next[i];

            if (object->get_vertex_count() != object_next->get_vertex_count())
            {
                RENDERER_LOG_ERROR(
                    "while reading key frame for object \"%s\" from mesh file %s: "
                    "expected " FMT_SIZE_T " %s, got " FMT_SIZE_T ".",
                    object->get_name(),
                    filename,
                    object->get_vertex_count(),
                    object->get_vertex_count() > 1 ? "vertices" : "vertex",
                    object_next->get_vertex_count());

                return false;
            }

            if (object->get_triangle_count() != object_next->get_triangle_count())
            {
                RENDERER_LOG_WARNING(
                    "while reading key frame for object \"%s\" from mesh file %s: "
                    "expected " FMT_SIZE_T " %s, got " FMT_SIZE_T ".",
                    object->get_name(),
                    filename,
                    object->get_triangle_count(),
                    object->get_triangle_count() > 1 ? "triangles" : "triangle",
                    object_next->get_triangle_count());
            }

            const size_t vertex_count = object_next->get_vertex_count();

            for (size_t j = 0; j < vertex_count; ++j)
                object->set_vertex_pose(j, motion_segment_index, object_next->get_vertex(j));
        }

        return true;
    }

    struct MeshObjectKeyFrame
    {
        double  m_key;
        string  m_filename;

        MeshObjectKeyFrame() {}

        MeshObjectKeyFrame(const double key, const string& filename)
          : m_key(key)
          , m_filename(filename)
        {
        }

        MeshObjectKeyFrame(const string& key, const string& filename)
          : m_key(from_string<double>(key))
          , m_filename(filename)
        {
        }

        bool operator<(const MeshObjectKeyFrame& rhs) const
        {
            return m_key < rhs.m_key;
        }
    };

    bool read_key_framed_mesh_object(
        const SearchPaths&      search_paths,
        const StringDictionary& filenames,
        const char*             base_object_name,
        const ParamArray&       params,
        MeshObjectArray&        objects)
    {
        vector<MeshObjectKeyFrame> key_frames;

        for (const_each<StringDictionary> i = filenames; i; ++i)
            key_frames.push_back(MeshObjectKeyFrame(i->name(), i->value<string>()));

        sort(key_frames.begin(), key_frames.end());

        if (!read_mesh_object(
                search_paths.qualify(key_frames[0].m_filename).c_str(),
                base_object_name,
                params,
                objects))
            return false;

        for (size_t i = 0; i < objects.size(); ++i)
            objects[i]->set_motion_segment_count(key_frames.size() - 1);

        for (size_t i = 1; i < key_frames.size(); ++i)
        {
            const string& filename = key_frames[i].m_filename;

            MeshObjectArray key_frame;

            if (!read_mesh_object(
                    search_paths.qualify(filename).c_str(),
                    base_object_name,
                    params,
                    key_frame))
                return false;

            if (!set_vertex_poses(objects, key_frame, i - 1, filename.c_str(), base_object_name))
                return false;
        }

        return true;
    }
}

bool MeshObjectReader::read(
    const SearchPaths&  search_paths,
    const char*         base_object_name,
    const ParamArray&   params,
    MeshObjectArray&    objects)
{
    assert(base_object_name);

    // Tag objects with the name of their parent.
    ParamArray completed_params(params);
    completed_params.insert("__base_object_name", base_object_name);

    if (params.strings().exist("filename"))
    {
        if (params.dictionaries().exist("filename"))
        {
            RENDERER_LOG_ERROR(
                "while reading geometry for object \"%s\": conflicting presence "
                "of both a \"filename\" parameter and a \"filename\" parameter group.",
                base_object_name);

            return false;
        }

        // Single-pose object.
        if (!read_mesh_object(
                search_paths.qualify(params.strings().get<string>("filename")).c_str(),
                base_object_name,
                completed_params,
                objects))
            return false;
    }
    else
    {
        if (params.dictionaries().exist("filename"))
        {
            const StringDictionary& filenames = params.dictionaries().get("filename").strings();

            if (filenames.empty())
            {
                RENDERER_LOG_ERROR(
                    "while reading geometry for object \"%s\": no pose defined "
                    "(the \"filename\" parameter group is empty).",
                    base_object_name);

                return false;
            }

            if (!is_pow2(filenames.size()))
            {
                RENDERER_LOG_ERROR(
                    "while reading geometry for object \"%s\": the number of poses must be a power of two, "
                    "but " FMT_SIZE_T " poses were defined.",
                    base_object_name,
                    filenames.size());

                return false;
            }

            // Multi-pose (motion-blurred) object.
            if (!read_key_framed_mesh_object(
                    search_paths,
                    filenames,
                    base_object_name,
                    completed_params,
                    objects))
                return false;
        }
        else
        {
            RENDERER_LOG_ERROR(
                "while reading geometry for object \"%s\": no \"filename\" parameter or "
                "\"filename\" parameter group found.",
                base_object_name);

            return false;
        }
    }

    return true;
}

}   // namespace renderer
