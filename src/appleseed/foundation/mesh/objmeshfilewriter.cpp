
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "objmeshfilewriter.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/core/appleseed.h"
#include "foundation/math/vector.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstddef>
#include <cstdio>

using namespace std;

namespace foundation
{

//
// OBJMeshFileWriter class implementation.
//

namespace
{
    //
    // IMeshWalker to IOBJMeshWalker adaptor.
    //

    class OBJMeshBuilderWalker
      : public IOBJMeshWalker
    {
      public:
        explicit OBJMeshBuilderWalker(const IMeshWalker& walker)
          : m_walker(walker)
        {
        }

        virtual string get_name() const
        {
            return m_walker.get_name();
        }

        virtual size_t get_vertex_count() const
        {
            return m_walker.get_vertex_count();
        }

        virtual Vector3d get_vertex(const size_t i) const
        {
            return m_walker.get_vertex(i);
        }

        virtual size_t get_vertex_normal_count() const
        {
            return m_walker.get_vertex_normal_count();
        }

        virtual Vector3d get_vertex_normal(const size_t i) const
        {
            return m_walker.get_vertex_normal(i);
        }

        virtual size_t get_tex_coords_count() const
        {
            return m_walker.get_tex_coords_count();
        }

        virtual Vector2d get_tex_coords(const size_t i) const
        {
            return m_walker.get_tex_coords(i);
        }

        virtual size_t get_face_count() const
        {
            return m_walker.get_face_count();
        }

        virtual Face get_face(const size_t i) const
        {
            return m_walker.get_face(i);
        }

      private:
        const IMeshWalker& m_walker;
    };

    // Floating-point formatting settings.
    #define VectorFormat "%.15f"

    void write_vector(FILE* file, const char* prefix, const Vector2d& v)
    {
        fprintf(
            file,
            "%s " VectorFormat " " VectorFormat "\n",
            prefix, v[0], v[1]);
    }

    void write_vector(FILE* file, const char* prefix, const Vector3d& v)
    {
        fprintf(
            file,
            "%s " VectorFormat " " VectorFormat " " VectorFormat "\n",
            prefix, v[0], v[1], v[2]);
    }

    void write_vertices(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t vertex_count = walker.get_vertex_count();

        fprintf(
            file,
            "# %s %s.\n",
            pretty_int(vertex_count).c_str(),
            plural(vertex_count, "vertex", "vertices").c_str());

        for (size_t i = 0; i < vertex_count; ++i)
        {
            const Vector3d v = walker.get_vertex(i);
            write_vector(file, "v", v);
        }
    }

    void write_vertex_normals(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t vertex_normal_count = walker.get_vertex_normal_count();

        if (vertex_normal_count == 0)
            return;

        fprintf(
            file,
            "# %s %s.\n",
            pretty_int(vertex_normal_count).c_str(),
            plural(vertex_normal_count, "vertex normal").c_str());

        for (size_t i = 0; i < vertex_normal_count; ++i)
        {
            const Vector3d vn = walker.get_vertex_normal(i);
            write_vector(file, "vn", vn);
        }
    }

    void write_texture_coordinates(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t tex_coords_count = walker.get_tex_coords_count();

        if (tex_coords_count == 0)
            return;

        fprintf(
            file,
            "# %s %s.\n",
            pretty_int(tex_coords_count).c_str(),
            plural(tex_coords_count, "texture coordinate").c_str());

        for (size_t i = 0; i < tex_coords_count; ++i)
        {
            const Vector2d vt = walker.get_tex_coords(i);
            write_vector(file, "vt", vt);
        }
    }

    void write_faces_no_vn_no_vt(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t face_count = walker.get_face_count();

        for (size_t i = 0; i < face_count; ++i)
        {
            const IOBJMeshWalker::Face face = walker.get_face(i);
            fprintf(
                file,
                "f " FMT_SIZE_T
                 " " FMT_SIZE_T
                 " " FMT_SIZE_T "\n",
                face.m_v0 + 1,
                face.m_v1 + 1,
                face.m_v2 + 1);
        }
    }

    void write_faces_vn_no_vt(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t face_count = walker.get_face_count();

        for (size_t i = 0; i < face_count; ++i)
        {
            const IOBJMeshWalker::Face face = walker.get_face(i);
            fprintf(
                file,
                "f " FMT_SIZE_T "//" FMT_SIZE_T
                 " " FMT_SIZE_T "//" FMT_SIZE_T
                 " " FMT_SIZE_T "//" FMT_SIZE_T "\n",
                face.m_v0 + 1, face.m_n0 + 1,
                face.m_v1 + 1, face.m_n1 + 1,
                face.m_v2 + 1, face.m_n2 + 1);
        }
    }

    void write_faces_no_vn_vt(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t face_count = walker.get_face_count();

        for (size_t i = 0; i < face_count; ++i)
        {
            const IOBJMeshWalker::Face face = walker.get_face(i);
            fprintf(
                file,
                "f " FMT_SIZE_T "/" FMT_SIZE_T
                 " " FMT_SIZE_T "/" FMT_SIZE_T
                 " " FMT_SIZE_T "/" FMT_SIZE_T "\n",
                face.m_v0 + 1, face.m_t0 + 1,
                face.m_v1 + 1, face.m_t1 + 1,
                face.m_v2 + 1, face.m_t2 + 1);
        }
    }

    void write_faces_vn_vt(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t face_count = walker.get_face_count();

        for (size_t i = 0; i < face_count; ++i)
        {
            const IOBJMeshWalker::Face face = walker.get_face(i);
            fprintf(
                file,
                "f " FMT_SIZE_T "/" FMT_SIZE_T "/" FMT_SIZE_T
                 " " FMT_SIZE_T "/" FMT_SIZE_T "/" FMT_SIZE_T
                 " " FMT_SIZE_T "/" FMT_SIZE_T "/" FMT_SIZE_T "\n",
                face.m_v0 + 1, face.m_t0 + 1, face.m_n0 + 1,
                face.m_v1 + 1, face.m_t1 + 1, face.m_n1 + 1,
                face.m_v2 + 1, face.m_t2 + 1, face.m_n2 + 1);
        }
    }

    void write_faces(FILE* file, const IOBJMeshWalker& walker)
    {
        const size_t face_count = walker.get_face_count();

        fprintf(
            file,
            "# %s %s.\n",
            pretty_int(face_count).c_str(),
            plural(face_count, "face").c_str());

        const size_t feature_mask =
            (walker.get_vertex_normal_count() > 0 ? 1 : 0) +
            (walker.get_tex_coords_count() > 0 ? 2 : 0);

        switch (feature_mask)
        {
          case 0: write_faces_no_vn_no_vt(file, walker); break;
          case 1: write_faces_vn_no_vt(file, walker); break;
          case 2: write_faces_no_vn_vt(file, walker); break;
          case 3: write_faces_vn_vt(file, walker); break;
          assert_otherwise;
        }
    }
}

void OBJMeshFileWriter::write(
    const string&           filename,
    const IMeshWalker&      walker)
{
    OBJMeshBuilderWalker adaptor(walker);
    write(filename, adaptor);
}

void OBJMeshFileWriter::write(
    const string&           filename,
    const IOBJMeshWalker&   walker)
{
    FILE* file = fopen(filename.c_str(), "wt");

    if (file == 0)
        throw ExceptionIOError();

    // Write the file header.
    fprintf(
        file,
        "# File generated by %s.\n",
        Appleseed::get_synthetic_version_string());

    // Write the mesh object name.
    fprintf(
        file,
        "o %s\n",
        walker.get_name().c_str());

    write_vertices(file, walker);
    write_vertex_normals(file, walker);
    write_texture_coordinates(file, walker);
    write_faces(file, walker);

    fclose(file);
}

}   // namespace foundation
