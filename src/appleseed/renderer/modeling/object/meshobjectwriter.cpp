
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
#include "meshobjectwriter.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/mesh/imeshfilewriter.h"
#include "foundation/mesh/imeshwalker.h"
#include "foundation/mesh/objmeshfilewriter.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

using namespace foundation;
using namespace std;

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
        // Constructor.
        explicit MeshObjectWalker(const MeshObject& object)
          : m_object(object)
        {
        }

        // Return the name of the mesh.
        virtual string get_name() const
        {
            return m_object.get_name();
        }

        // Return the number of vertices in the mesh.
        virtual size_t get_vertex_count() const
        {
            return m_object.get_vertex_count();
        }

        // Return a given vertex from the mesh.
        virtual Vector3d get_vertex(const size_t i) const
        {
            return Vector3d(m_object.get_vertex(i));
        }

        // Return the number of vertex normals in the mesh.
        virtual size_t get_vertex_normal_count() const
        {
            return m_object.get_vertex_normal_count();
        }

        // Return a given vertex normal from the mesh.
        virtual Vector3d get_vertex_normal(const size_t i) const
        {
            return Vector3d(m_object.get_vertex_normal(i));
        }

        // Return the number of texture coordinates in the mesh.
        virtual size_t get_tex_coords_count() const
        {
            return m_object.get_tex_coords_count();
        }

        // Return a given texture coordinate from the mesh.
        virtual Vector2d get_tex_coords(const size_t i) const
        {
            return Vector2d(m_object.get_tex_coords(i));
        }

        // Return the number of faces in the mesh.
        virtual size_t get_face_count() const
        {
            return m_object.get_triangle_count();
        }

        // Return a given face from the mesh.
        virtual Face get_face(const size_t i) const
        {
            const Triangle& triangle = m_object.get_triangle(i);

            Face face;

            face.m_v0 = static_cast<size_t>(triangle.m_v0);
            face.m_v1 = static_cast<size_t>(triangle.m_v1);
            face.m_v2 = static_cast<size_t>(triangle.m_v2);

            face.m_n0 = triangle.m_n0 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_n0);
            face.m_n1 = triangle.m_n1 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_n1);
            face.m_n2 = triangle.m_n2 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_n2);

            face.m_t0 = triangle.m_a0 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_a0);
            face.m_t1 = triangle.m_a1 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_a1);
            face.m_t2 = triangle.m_a2 == Triangle::None ? Face::None : static_cast<size_t>(triangle.m_a2);

            face.m_material = static_cast<size_t>(triangle.m_pa);

            return face;
        }

      private:
        const MeshObject& m_object;
    };
}

// Write a mesh object to disk.
bool MeshObjectWriter::write(
    const MeshObject&   object,
    const char*         filename)
{
    assert(filename);

    OBJMeshFileWriter writer;
    MeshObjectWalker walker(object);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    try
    {
        writer.write(filename, walker);
    }
    catch (const ExceptionIOError&)
    {
        RENDERER_LOG_ERROR(
            "failed to write mesh file %s: i/o error",
            filename);
        return false;
    }
    catch (const Exception& e)
    {
        RENDERER_LOG_ERROR(
            "failed to write mesh file %s: %s",
            filename,
            e.what());
        return false;
    }

    stopwatch.measure();

    RENDERER_LOG_INFO(
        "wrote mesh file %s in %s",
        filename,
        pretty_time(stopwatch.get_seconds()).c_str());

    return true;
}

}   // namespace renderer
