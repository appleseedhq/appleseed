
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
#include "meshobjectwriter.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/meshio/genericmeshfilewriter.h"
#include "foundation/meshio/imeshwalker.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/string/string.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <exception>
#include <string>

using namespace foundation;

namespace renderer
{

//
// MeshObjectWriter class implementation.
//

namespace
{
    //
    // Mesh object walker.
    //

    class MeshObjectWalker
      : public IMeshWalker
    {
      public:
        MeshObjectWalker(
            const MeshObject&   object,
            const char*         object_name)
          : m_object(object)
          , m_object_name(object_name)
        {
        }

        const char* get_name() const override
        {
            return m_object_name.c_str();
        }

        size_t get_vertex_count() const override
        {
            return m_object.get_vertex_count();
        }

        Vector3d get_vertex(const size_t i) const override
        {
            return Vector3d(m_object.get_vertex(i));
        }

        size_t get_vertex_normal_count() const override
        {
            return m_object.get_vertex_normal_count();
        }

        Vector3d get_vertex_normal(const size_t i) const override
        {
            return Vector3d(m_object.get_vertex_normal(i));
        }

        size_t get_tex_coords_count() const override
        {
            return m_object.get_tex_coords_count();
        }

        Vector2d get_tex_coords(const size_t i) const override
        {
            return Vector2d(m_object.get_tex_coords(i));
        }

        size_t get_material_slot_count() const override
        {
            return m_object.get_material_slot_count();
        }

        const char* get_material_slot(const size_t i) const override
        {
            return m_object.get_material_slot(i);
        }

        size_t get_face_count() const override
        {
            return m_object.get_triangle_count();
        }

        size_t get_face_vertex_count(const size_t face_index) const override
        {
            return 3;
        }

        size_t get_face_vertex(const size_t face_index, const size_t vertex_index) const override
        {
            assert(vertex_index < 3);
            const Triangle& triangle = m_object.get_triangle(face_index);
            return static_cast<size_t>((&triangle.m_v0)[vertex_index]);
        }

        size_t get_face_vertex_normal(const size_t face_index, const size_t vertex_index) const override
        {
            assert(vertex_index < 3);
            const Triangle& triangle = m_object.get_triangle(face_index);
            const size_t n = static_cast<size_t>((&triangle.m_n0)[vertex_index]);
            return n == Triangle::None ? None : n;
        }

        size_t get_face_tex_coords(const size_t face_index, const size_t vertex_index) const override
        {
            assert(vertex_index < 3);
            const Triangle& triangle = m_object.get_triangle(face_index);
            const size_t n = static_cast<size_t>((&triangle.m_a0)[vertex_index]);
            return n == Triangle::None ? None : n;
        }

        size_t get_face_material(const size_t face_index) const override
        {
            return static_cast<size_t>(m_object.get_triangle(face_index).m_pa);
        }

      private:
        const MeshObject&   m_object;
        const std::string   m_object_name;
    };
}

bool MeshObjectWriter::write(
    const MeshObject&   object,
    const char*         object_name,
    const char*         filename)
{
    assert(filename);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        GenericMeshFileWriter writer(filename);
        MeshObjectWalker walker(object, object_name);
        writer.write(walker);
    }
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write mesh file %s: %s.",
            filename,
            e.what());
        return false;
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "wrote mesh file %s in %s.",
        filename,
        pretty_time(stopwatch.get_seconds()).c_str());

    return true;
}

}   // namespace renderer
