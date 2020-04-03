
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
#include "meshobjectreader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectoperations.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/math/scalar.h"
#include "foundation/math/triangulator.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/memory/memory.h"
#include "foundation/meshio/genericmeshfilereader.h"
#include "foundation/meshio/imeshbuilder.h"
#include "foundation/meshio/imeshfilereader.h"
#include "foundation/meshio/objmeshfilereader.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/filter.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <map>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// MeshObjectArray class implementation.
//

APPLESEED_DEFINE_APIARRAY(MeshObjectArray);


//
// MeshObjectReader class implementation.
//

namespace
{
    class MeshObjectBuilder
      : public IMeshBuilder
    {
      public:
        typedef std::vector<MeshObject*> MeshObjectVector;

        MeshObjectBuilder(
            const ParamArray&   params,
            const std::string&  base_object_name)
          : m_params(params)
          , m_ignore_vertex_normals(m_params.get_optional<bool>("ignore_vertex_normals"))
          , m_base_object_name(base_object_name)
          , m_untitled_mesh_counter(0)
          , m_vertex_count(0)
          , m_face_material(0)
          , m_triangulator(Triangulator<double>::KeepDegenerateTriangles)
          , m_total_vertex_count(0)
          , m_total_triangle_count(0)
        {
            reset_mesh_stats();
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

        void begin_mesh(const char* mesh_name) override
        {
            // Construct the object name.
            const std::string object_name = m_base_object_name + "." + make_unique_mesh_name(mesh_name);

            // Create an empty mesh object.
            m_objects.push_back(
                static_cast<MeshObject*>(
                    MeshObjectFactory().create(object_name.c_str(), m_params).release()));

            // Reset mesh statistics.
            reset_mesh_stats();
        }

        void end_mesh() override
        {
            // Print the number of faces that could not be triangulated, if any.
            if (m_triangulation_error_count > 0)
            {
                RENDERER_LOG_WARNING(
                    "while loading mesh object \"%s\": %s polygonal %s (out of %s) could not be triangulated and have been replaced by zero-area triangles.",
                    m_objects.back()->get_path().c_str(),
                    pretty_uint(m_triangulation_error_count).c_str(),
                    m_triangulation_error_count > 1 ? "faces" : "face",
                    pretty_uint(m_face_count).c_str());
            }

            // Print the number of null normal vectors, if any.
            if (m_null_normal_vector_count > 0)
            {
                RENDERER_LOG_WARNING(
                    "while loading mesh object \"%s\": %s normal %s (out of %s) were null and have been replaced by arbitrary unit-length vectors.",
                    m_objects.back()->get_path().c_str(),
                    pretty_uint(m_null_normal_vector_count).c_str(),
                    m_null_normal_vector_count > 1 ? "vectors" : "vector",
                    pretty_uint(m_normal_count).c_str());
            }

            // Keep track of the total number of vertices and triangles that were loaded.
            m_total_vertex_count += m_objects.back()->get_vertex_count();
            m_total_triangle_count += m_objects.back()->get_triangle_count();
        }

        size_t push_vertex(const Vector3d& v) override
        {
            return m_objects.back()->push_vertex(GVector3(v));
        }

        size_t push_vertex_normal(const Vector3d& v) override
        {
            GVector3 n(v);

            const GScalar norm_n = norm(n);

            if (norm_n > GScalar(0.0))
                n /= norm_n;
            else
            {
                ++m_null_normal_vector_count;
                n = GVector3(GScalar(1.0), GScalar(0.0), GScalar(0.0));
            }

            ++m_normal_count;

            return m_objects.back()->push_vertex_normal(n);
        }

        size_t push_tex_coords(const Vector2d& v) override
        {
            return m_objects.back()->push_tex_coords(GVector2(v));
        }

        size_t push_material_slot(const char* name) override
        {
            return m_objects.back()->push_material_slot(name);
        }

        void begin_face(const size_t vertex_count) override
        {
            assert(vertex_count >= 3);

            m_vertex_count = vertex_count;
            clear_keep_memory(m_face_vertices);
            clear_keep_memory(m_face_normals);
            clear_keep_memory(m_face_tex_coords);
            m_face_material = Triangle::None;

            ++m_face_count;
        }

        void end_face() override
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
                    m_polygon.emplace_back(m_objects.back()->get_vertex(m_face_vertices[i]));

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

        void set_face_vertices(const size_t vertices[]) override
        {
            m_face_vertices.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_vertices[i] = static_cast<std::uint32_t>(vertices[i]);
        }

        void set_face_vertex_normals(const size_t vertex_normals[]) override
        {
            m_face_normals.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_normals[i] = static_cast<std::uint32_t>(vertex_normals[i]);
        }

        void set_face_vertex_tex_coords(const size_t tex_coords[]) override
        {
            m_face_tex_coords.resize(m_vertex_count);

            for (size_t i = 0; i < m_vertex_count; ++i)
                m_face_tex_coords[i] = static_cast<std::uint32_t>(tex_coords[i]);
        }

        void set_face_material(const size_t material) override
        {
            m_face_material = static_cast<std::uint32_t>(material);
        }

      private:
        const ParamArray                  m_params;
        const bool                        m_ignore_vertex_normals;
        const std::string                 m_base_object_name;

        MeshObjectVector                  m_objects;

        // Support data for untitled meshes.
        size_t                            m_untitled_mesh_counter;
        std::map<std::string, size_t>     m_mesh_counters;

        // Face definition.
        size_t                            m_vertex_count;
        std::vector<std::uint32_t>        m_face_vertices;
        std::vector<std::uint32_t>        m_face_normals;
        std::vector<std::uint32_t>        m_face_tex_coords;
        std::uint32_t                     m_face_material;

        // Support data for face triangulation.
        Triangulator<double>              m_triangulator;
        std::vector<Vector3d>             m_polygon;
        std::vector<size_t>               m_triangles;

        // Mesh statistics.
        size_t                            m_normal_count;
        size_t                            m_face_count;
        size_t                            m_triangulation_error_count;
        size_t                            m_null_normal_vector_count;

        // Global statistics.
        size_t                            m_total_vertex_count;
        size_t                            m_total_triangle_count;

        void reset_mesh_stats()
        {
            m_normal_count = 0;
            m_face_count = 0;
            m_triangulation_error_count = 0;
            m_null_normal_vector_count = 0;
        }

        std::string make_unique_mesh_name(std::string mesh_name)
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
                triangle.m_n0 = Triangle::None;
                triangle.m_n1 = Triangle::None;
                triangle.m_n2 = Triangle::None;
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
            triangle.m_pa = m_face_material;

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

        const std::string obj_parsing_mode = params.get_optional<std::string>("obj_parsing_mode", "fast");

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
        catch (const std::exception& e)
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
            builder.get_objects().size() > 1 ? "objects" : "object",
            pretty_int(builder.get_total_vertex_count()).c_str(),
            builder.get_total_vertex_count() > 1 ? "vertices" : "vertex",
            pretty_int(builder.get_total_triangle_count()).c_str(),
            builder.get_total_triangle_count() > 1 ? "triangles" : "triangle",
            pretty_time(stopwatch.get_seconds()).c_str());

        objects = array_vector<MeshObjectArray>(builder.get_objects());

        return true;
    }

    void emit_mismatching_feature_count_error_message(
        const char*             object_path,
        const char*             filename,
        const char*             feature_name_singular,
        const char*             feature_name_plural,
        const size_t            expected_count,
        const size_t            actual_count)
    {
        RENDERER_LOG_ERROR(
            "while reading key frame for object \"%s\" from mesh file %s: "
            "expected " FMT_SIZE_T " %s, got " FMT_SIZE_T ".",
            object_path,
            filename,
            expected_count,
            expected_count > 1 ? feature_name_plural : feature_name_singular,
            actual_count);
    }

    bool have_identical_topology(
        const MeshObject&       object,
        const char*             filename,
        const MeshObject&       pose)
    {
        if (object.get_vertex_count() != pose.get_vertex_count())
        {
            emit_mismatching_feature_count_error_message(
                object.get_path().c_str(),
                filename,
                "vertex", "vertices",
                object.get_vertex_count(),
                pose.get_vertex_count());
            return false;
        }

        if (object.get_vertex_normal_count() != pose.get_vertex_normal_count())
        {
            emit_mismatching_feature_count_error_message(
                object.get_path().c_str(),
                filename,
                "vertex normal", "vertex normals",
                object.get_vertex_normal_count(),
                pose.get_vertex_normal_count());
            return false;
        }

        if (object.get_triangle_count() != pose.get_triangle_count())
        {
            emit_mismatching_feature_count_error_message(
                object.get_path().c_str(),
                filename,
                "triangle", "triangles",
                object.get_vertex_normal_count(),
                pose.get_vertex_normal_count());
            return false;
        }

        return true;
    }

    bool set_vertex_poses(
        MeshObjectArray&        objects,
        const MeshObjectArray&  poses,
        const size_t            motion_segment_index,
        const char*             filename,
        const char*             base_object_name)
    {
        if (objects.size() != poses.size())
        {
            emit_mismatching_feature_count_error_message(
                base_object_name,
                filename,
                "objects", "object",
                objects.size(),
                poses.size());
            return false;
        }

        for (size_t i = 0; i < objects.size(); ++i)
        {
            MeshObject& object = *objects[i];
            const MeshObject& pose = *poses[i];

            if (!have_identical_topology(object, filename, pose))
                return false;

            for (size_t j = 0, e = pose.get_vertex_count(); j < e; ++j)
                object.set_vertex_pose(j, motion_segment_index, pose.get_vertex(j));

            for (size_t j = 0, e = pose.get_vertex_normal_count(); j < e; ++j)
                object.set_vertex_normal_pose(j, motion_segment_index, pose.get_vertex_normal(j));
        }

        return true;
    }

    struct MeshObjectKeyFrame
    {
        double       m_key;
        std::string  m_filename;

        MeshObjectKeyFrame() {}

        MeshObjectKeyFrame(const double key, const std::string& filename)
          : m_key(key)
          , m_filename(filename)
        {
        }

        bool operator<(const MeshObjectKeyFrame& rhs) const
        {
            return m_key < rhs.m_key;
        }
    };

    bool has_actual_motion(const MeshObject& object)
    {
        const size_t motion_segment_count = object.get_motion_segment_count();
        const size_t vertex_count = object.get_vertex_count();

        for (size_t i = 0; i < vertex_count; ++i)
        {
            // Check vertices.

            if (object.get_vertex_pose(i, 0) != object.get_vertex(i))
                return true;

            for (size_t j = 1; j < motion_segment_count; ++j)
            {
                if (object.get_vertex_pose(i, j) != object.get_vertex_pose(i, j - 1))
                    return true;
            }

            // Check vertex normals.
            if (object.get_vertex_normal_count() > 0)
            {
                if (object.get_vertex_normal_pose(i, 0) != object.get_vertex_normal(i))
                    return true;

                for (size_t j = 1; j < motion_segment_count; ++j)
                {
                    if (object.get_vertex_normal_pose(i, j) != object.get_vertex_normal_pose(i, j - 1))
                        return true;
                }
            }

            // Check vertex tangents.
            if (object.get_vertex_tangent_count() > 0)
            {
                if (object.get_vertex_tangent_pose(i, 0) != object.get_vertex_tangent(i))
                    return true;

                for (size_t j = 1; j < motion_segment_count; ++j)
                {
                    if (object.get_vertex_tangent_pose(i, j) != object.get_vertex_tangent_pose(i, j - 1))
                        return true;
                }
            }
        }

        return false;
    }

    void try_collapsing_to_static_object(MeshObject& object)
    {
        if (object.get_motion_segment_count() == 0)
            return;

        if (has_actual_motion(object))
            return;

        RENDERER_LOG_INFO(
            "collapsing mesh object \"%s\" (%s %s, %s %s, %s %s) to static mesh.",
            object.get_path().c_str(),
            pretty_int(object.get_vertex_count()).c_str(),
            object.get_vertex_count() > 1 ? "vertices" : "vertex",
            pretty_int(object.get_triangle_count()).c_str(),
            object.get_triangle_count() > 1 ? "triangles" : "triangle",
            pretty_int(object.get_motion_segment_count()).c_str(),
            object.get_motion_segment_count() > 1 ? "motion segments" : "motion segment");

        object.clear_vertex_poses();
        object.set_motion_segment_count(0);
    }

    void unshare_normals(MeshObject& object)
    {
        std::vector<GVector3> old_normals(object.get_vertex_normal_count());
        for (size_t i = 0, e = object.get_vertex_normal_count(); i < e; ++i)
            old_normals[i] = object.get_vertex_normal(i);

        const size_t triangle_count = object.get_triangle_count();

        std::vector<Triangle> old_triangles(triangle_count);
        for (size_t i = 0, e = triangle_count; i < e; ++i)
            old_triangles[i] = object.get_triangle(i);

        object.clear_vertex_normals();
        object.reserve_vertex_normals(triangle_count * 3);

        object.clear_triangles();
        object.reserve_triangles(triangle_count);

        for (size_t i = 0, e = triangle_count; i < e; ++i)
        {
            Triangle tri = old_triangles[i];

            tri.m_n0 = static_cast<std::uint32_t>(object.push_vertex_normal(old_normals[tri.m_n0]));
            tri.m_n1 = static_cast<std::uint32_t>(object.push_vertex_normal(old_normals[tri.m_n1]));
            tri.m_n2 = static_cast<std::uint32_t>(object.push_vertex_normal(old_normals[tri.m_n2]));

            object.push_triangle(tri);
        }
    }

    bool read_key_framed_mesh_object(
        const SearchPaths&      search_paths,
        const StringDictionary& filenames,
        const char*             base_object_name,
        const ParamArray&       params,
        MeshObjectArray&        objects)
    {
        assert(filenames.size() >= 2);

        std::vector<MeshObjectKeyFrame> key_frames;
        key_frames.reserve(filenames.size());

        for (const_each<StringDictionary> i = filenames; i; ++i)
        {
            const double key = from_string<double>(i->key());
            const std::string filename = i->value<std::string>();
            key_frames.emplace_back(key, filename);
        }

        sort(key_frames.begin(), key_frames.end());

        if (!read_mesh_object(
                search_paths.qualify(key_frames[0].m_filename).c_str(),
                base_object_name,
                params,
                objects))
            return false;

        for (size_t i = 0; i < objects.size(); ++i)
            objects[i]->set_motion_segment_count(key_frames.size() - 1);

        for (size_t i = 0; i < objects.size(); ++i)
        {
            if (objects[i]->get_vertex_normal_count() != 0)
                unshare_normals(*objects[i]);
        }

        for (size_t i = 1; i < key_frames.size(); ++i)
        {
            const std::string& filename = key_frames[i].m_filename;

            MeshObjectArray poses;

            if (!read_mesh_object(
                    search_paths.qualify(filename).c_str(),
                    base_object_name,
                    params,
                    poses))
                return false;

            for (size_t j = 0; j < poses.size(); ++j)
            {
                if (poses[j]->get_vertex_normal_count() != 0)
                    unshare_normals(*poses[j]);
            }

            if (!set_vertex_poses(
                    objects,
                    poses,
                    i - 1,
                    filename.c_str(),
                    base_object_name))
                return false;
        }

        for (size_t i = 0; i < objects.size(); ++i)
            try_collapsing_to_static_object(*objects[i]);

        return true;
    }

    void compute_smooth_normals(MeshObject& object)
    {
        if (object.get_vertex_normal_count() > 0)
        {
            RENDERER_LOG_WARNING(
                "skipping computation of smooth normal vectors for mesh object \"%s\" because it already has normal vectors.",
                object.get_path().c_str());
            return;
        }

        RENDERER_LOG_INFO("computing smooth normal vectors for mesh object \"%s\"...", object.get_path().c_str());

        compute_smooth_vertex_normals(object);
    }

    void compute_smooth_tangents(MeshObject& object)
    {
        if (object.get_vertex_tangent_count() > 0)
        {
            RENDERER_LOG_WARNING(
                "skipping computation of smooth tangent vectors for mesh object \"%s\" because it already has tangent vectors.",
                object.get_path().c_str());
            return;
        }

        if (object.get_tex_coords_count() == 0)
        {
            RENDERER_LOG_WARNING(
                "cannot compute smooth tangent vectors for mesh object \"%s\" because it lacks texture coordinates.",
                object.get_path().c_str());
            return;
        }

        RENDERER_LOG_INFO("computing smooth tangent vectors for mesh object \"%s\"...", object.get_path().c_str());

        compute_smooth_vertex_tangents(object);
    }
}

bool MeshObjectReader::read(
    const SearchPaths&  search_paths,
    const char*         base_object_name,
    const ParamArray&   params,
    MeshObjectArray&    objects)
{
    assert(base_object_name);

    // Objects will be tagged with the name of their parent.
    ParamArray completed_params(params);
    completed_params.insert("__base_object_name", base_object_name);

    // Read object(s) from disk.
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
                search_paths.qualify(params.strings().get<std::string>("filename")).c_str(),
                base_object_name,
                completed_params,
                objects))
            return false;
    }
    else if (params.dictionaries().exist("filename"))
    {
        const StringDictionary& filenames = params.dictionaries().get("filename").strings();
        switch (filenames.size())
        {
          case 0:
            {
                RENDERER_LOG_ERROR(
                    "while reading geometry for object \"%s\": no pose defined "
                    "(the \"filename\" parameter group is empty).",
                    base_object_name);
                return false;
            }
            break;

          case 1:
            {
                // Single-pose object.
                if (!read_mesh_object(
                        search_paths.qualify(filenames.begin().value()).c_str(),
                        base_object_name,
                        completed_params,
                        objects))
                    return false;
            }
            break;

          default:
            {
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
            break;
        }
    }

    // Compute smooth normals.
    if (params.strings().exist("compute_smooth_normals"))
    {
        const RegExFilter filter(params.get("compute_smooth_normals"));
        for (size_t i = 0, e = objects.size(); i < e; ++i)
        {
            MeshObject& object = *objects[i];
            if (filter.accepts(object.get_name()))
                compute_smooth_normals(object);
        }
    }

    // Compute smooth tangents.
    if (params.strings().exist("compute_smooth_tangents"))
    {
        const RegExFilter filter(params.get("compute_smooth_tangents"));
        for (size_t i = 0, e = objects.size(); i < e; ++i)
        {
            MeshObject& object = *objects[i];
            if (filter.accepts(object.get_name()))
                compute_smooth_tangents(object);
        }
    }

    return true;
}

}   // namespace renderer
