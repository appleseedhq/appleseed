
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "alembicmeshfilereader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"
#include "foundation/mesh/imeshbuilder.h"
#include "foundation/platform/types.h"
#include "foundation/utility/memory.h"

// Alembic headers.
#include "Alembic/Abc/Foundation.h"
#include "Alembic/Abc/IArchive.h"
#include "Alembic/Abc/IObject.h"
#include "Alembic/Abc/TypedArraySample.h"
#include "Alembic/AbcCoreAbstract/ObjectHeader.h"
#include "Alembic/AbcCoreHDF5/ReadWrite.h"
#include "Alembic/AbcGeom/IGeomParam.h"
#include "Alembic/AbcGeom/IPolyMesh.h"

// OpenEXR headers.
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImathVec.h"
END_EXR_INCLUDES

// Standard headers.
#include <cstddef>
#include <vector>

using namespace Alembic;
using namespace Alembic::AbcGeom;
using namespace std;

namespace foundation
{

namespace
{
    class MeshObjectReader
      : public NonCopyable
    {
      public:
        explicit MeshObjectReader(IMeshBuilder& mesh_builder)
          : m_mesh_builder(mesh_builder)
          , m_has_vertex_normals(false)
          , m_has_uv(false)
        {
        }

        void read(IPolyMesh mesh)
        {
            IPolyMeshSchema& mesh_schema = mesh.getSchema();

            // todo: pass as parameter.
            const size_t sample_index = 0;

            IPolyMeshSchema::Sample mesh_sample;
            mesh_schema.get(mesh_sample, sample_index);

            if (!mesh_sample)
                return;

            // Skip degenerate mesh objects.
            if (mesh_sample.getPositions()->size() < 3)
                return;

            m_mesh_builder.begin_mesh(mesh.getName().c_str());

            read_vertices(mesh_sample);
            read_vertex_normals(mesh_schema);
            read_uv(mesh_schema);

            // Use the mesh topology of the first sample.
            mesh_sample.reset();
            mesh_schema.get(mesh_sample, 0);

            read_face_indices(mesh_sample);

            m_mesh_builder.end_mesh();
        }

      private:
        IMeshBuilder&   m_mesh_builder;
        bool            m_has_vertex_normals;
        bool            m_has_uv;

        void read_vertices(IPolyMeshSchema::Sample mesh_sample)
        {
            const Imath::V3f* vertices = mesh_sample.getPositions()->get();
            const size_t vertex_count = mesh_sample.getPositions()->size();

            for (size_t i = 0; i < vertex_count; ++i)
            {
                const Vector3d v(vertices[i]);      // todo: transform to world space using matrix stack
                m_mesh_builder.push_vertex(v);
            }
        }

        void read_vertex_normals(IPolyMeshSchema& mesh_schema)
        {
            IN3fGeomParam normal_param = mesh_schema.getNormalsParam();
            if (!normal_param.valid())
                return;

            IN3fGeomParam::Sample normal_sample(normal_param.getIndexedValue());
            if (!normal_sample.valid())
                return;

            const N3f* normals = normal_sample.getVals()->get();
            const size_t normal_count = normal_sample.getVals()->size();

            for (size_t i = 0; i < normal_count; ++i)
            {
                const Vector3d n(normals[i]);       // todo: transform to world space using matrix stack
                m_mesh_builder.push_vertex_normal(n);
            }

            m_has_vertex_normals = normal_count > 0;
        }

        void read_uv(IPolyMeshSchema& mesh_schema)
        {
            IV2fGeomParam uv_param = mesh_schema.getUVsParam();
            if (!uv_param.valid())
                return;

            IV2fGeomParam::Sample uv_sample(uv_param.getIndexedValue());
            if (!uv_sample.valid())
                return;

            const V2f* uv = uv_sample.getVals()->get();
            const size_t uv_count = uv_sample.getVals()->size();

            for (size_t i = 0; i < uv_count; ++i)
            {
                const Vector2d v(uv[i]);
                m_mesh_builder.push_tex_coords(v);
            }

            m_has_uv = uv_count > 0;
        }

        void read_face_indices(IPolyMeshSchema::Sample mesh_sample)
        {
            const int32* face_sizes = mesh_sample.getFaceCounts()->get();
            const size_t face_count = mesh_sample.getFaceCounts()->size();
            const int32* face_indices = mesh_sample.getFaceIndices()->get();

            size_t current_vertex_index = 0;
            vector<size_t> indices;

            for (size_t i = 0; i < face_count; ++i)
            {
                const size_t face_size = static_cast<size_t>(face_sizes[i]);

                // Skip degenerate faces.
                if (face_size < 3)
                    continue;

                // Collect feature indices for this face.
                ensure_minimum_size(indices, face_size);
                for (size_t j = 0; j < face_size; ++j)
                    indices[j] = static_cast<size_t>(face_indices[current_vertex_index + j]);

                m_mesh_builder.begin_face(face_size);
                m_mesh_builder.set_face_material(0);

                // Vertices.
                m_mesh_builder.set_face_vertices(&indices[0]);

                // Vertex normals.
                if (m_has_vertex_normals)
                    m_mesh_builder.set_face_vertex_normals(&indices[0]);

                // Texture coordinates.
                if (m_has_uv)
                    m_mesh_builder.set_face_vertex_tex_coords(&indices[0]);

                m_mesh_builder.end_face();

                current_vertex_index += face_size;
            }
        }
    };

    void read_object(IObject object, IMeshBuilder& builder)
    {
        const size_t children_count = object.getNumChildren();

        for (size_t i = 0; i < children_count; ++i)
        {
            const ObjectHeader& child_header = object.getChildHeader(i);

            if (IPolyMesh::matches(child_header))
            {
                MeshObjectReader mesh_reader(builder);
                mesh_reader.read(IPolyMesh(object, child_header.getName()));
            }

            read_object(object.getChild(i), builder);
        }
    }
}

AlembicMeshFileReader::AlembicMeshFileReader(const string& filename)
  : m_filename(filename)
{
}

void AlembicMeshFileReader::read(IMeshBuilder& builder)
{
    const IArchive archive(AbcCoreHDF5::ReadArchive(), m_filename);

    read_object(IObject(archive, kTop), builder);
}

}   // namespace foundation
