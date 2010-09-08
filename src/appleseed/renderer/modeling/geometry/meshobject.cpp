
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "meshobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/geometry/iregion.h"
#include "renderer/modeling/geometry/regionkit.h"
#include "renderer/modeling/geometry/triangle.h"

// appleseed.foundation headers.
#include "foundation/utility/attributeset.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/numerictype.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MeshObject class implementation.
//

namespace
{
    // A dummy region that simply wraps the tessellation stored in the object.
    class MeshRegion
      : public IRegion
    {
      public:
        // Constructor.
        MeshRegion(
            const GAABB3*               local_bbox,
            const StaticTriangleTess*   tess)
          : m_uid(g_uid_source.get())
          , m_local_bbox(local_bbox)
          , m_lazy_tess(tess)
        {
        }

        // Return the unique ID of this object.
        virtual UniqueID get_uid() const
        {
            return m_uid;
        }

        // Return the local space bounding box of the region.
        virtual const GAABB3& get_local_bbox() const
        {
            return *m_local_bbox;
        }

        // Return the static triangle tessellation of the region.
        virtual Lazy<StaticTriangleTess>& get_static_triangle_tess() const
        {
            return m_lazy_tess;
        }

      private:
        const UniqueID                      m_uid;
        const GAABB3*                       m_local_bbox;
        mutable Lazy<StaticTriangleTess>    m_lazy_tess;
    };
}

struct MeshObject::Impl
{
    const string                m_name;
    GAABB3                      m_bbox;
    StaticTriangleTess          m_tess;
    MeshRegion                  m_region;
    RegionKit                   m_region_kit;
    mutable Lazy<RegionKit>     m_lazy_region_kit;

    AttributeSet::ChannelID     m_uv0_channel_id;

    // Constructor.
    explicit Impl(const char* name)
      : m_name(name)
      , m_region(&m_bbox, &m_tess)
      , m_lazy_region_kit(&m_region_kit)
      , m_uv0_channel_id(AttributeSet::InvalidChannelID)
    {
        m_bbox.invalidate();
        m_region_kit.push_back(&m_region);
    }
};

// Constructor.
MeshObject::MeshObject(
    const char*         name,
    const ParamArray&   params)
  : Object(params)
  , impl(new Impl(name))
{
}

// Destructor.
MeshObject::~MeshObject()
{
    delete impl;
}

// Delete this instance.
void MeshObject::release()
{
    delete this;
}

// Return a string identifying the model of this object.
const char* MeshObject::get_model() const
{
    return MeshObjectFactory::get_model();
}

// Return the name of this object.
const char* MeshObject::get_name() const
{
    return impl->m_name.c_str();
}

// Return the local space bounding box of the object.
const GAABB3& MeshObject::get_local_bbox() const
{
    return impl->m_bbox;
}

// Return the region kit of the object.
Lazy<RegionKit>& MeshObject::get_region_kit()
{
    return impl->m_lazy_region_kit;
}

// Insert and access vertices.
void MeshObject::reserve_vertices(const size_t count)
{
    impl->m_tess.m_vertices.reserve(count);
}
size_t MeshObject::push_vertex(const GVector3& vertex)
{
    const size_t index = impl->m_tess.m_vertices.size();
    impl->m_tess.m_vertices.push_back(vertex);
    impl->m_bbox.insert(vertex);
    return index;
}
size_t MeshObject::get_vertex_count() const
{
    return impl->m_tess.m_vertices.size();
}
const GVector3& MeshObject::get_vertex(const size_t index) const
{
    return impl->m_tess.m_vertices[index];
}

// Insert and access vertex normals.
void MeshObject::reserve_vertex_normals(const size_t count)
{
    impl->m_tess.m_vertex_normals.reserve(count);
}
size_t MeshObject::push_vertex_normal(const GVector3& normal)
{
    const size_t index = impl->m_tess.m_vertex_normals.size();
    impl->m_tess.m_vertex_normals.push_back(normal);
    return index;
}
size_t MeshObject::get_vertex_normal_count() const
{
    return impl->m_tess.m_vertex_normals.size();
}
const GVector3& MeshObject::get_vertex_normal(const size_t index) const
{
    return impl->m_tess.m_vertex_normals[index];
}

// Insert and access texture coordinates.
void MeshObject::reserve_tex_coords(const size_t count)
{
}
size_t MeshObject::push_tex_coords(const GVector2& tex_coords)
{
    if (impl->m_uv0_channel_id == AttributeSet::InvalidChannelID)
    {
        impl->m_uv0_channel_id =
            impl->m_tess.m_vertex_attributes.create_channel(
                "uv0",
                NumericType::id<GVector2::ValueType>(),
                2);
    }

    return
        impl->m_tess.m_vertex_attributes.push_attribute(
            impl->m_uv0_channel_id,
            tex_coords);
}
size_t MeshObject::get_tex_coords_count() const
{
    if (impl->m_uv0_channel_id == AttributeSet::InvalidChannelID)
        return 0;
    else
    {
        return impl->m_tess.m_vertex_attributes.get_attribute_count(
            impl->m_uv0_channel_id);
    }
}
GVector2 MeshObject::get_tex_coords(const size_t index) const
{
    if (impl->m_uv0_channel_id == AttributeSet::InvalidChannelID)
        return GVector2(0.0);
    else
    {
        GVector2 tex_coords;
        impl->m_tess.m_vertex_attributes.get_attribute(
            impl->m_uv0_channel_id,
            index,
            &tex_coords);
        return tex_coords;
    }
}

// Insert and access triangles.
void MeshObject::reserve_triangles(const size_t count)
{
    impl->m_tess.m_primitives.reserve(count);
}
size_t MeshObject::push_triangle(const Triangle& triangle)
{
    const size_t index = impl->m_tess.m_primitives.size();
    impl->m_tess.m_primitives.push_back(triangle);
    return index;
}
size_t MeshObject::get_triangle_count() const
{
    return impl->m_tess.m_primitives.size();
}
const Triangle& MeshObject::get_triangle(const size_t index) const
{
    return impl->m_tess.m_primitives[index];
}


//
// MeshObjectFactory class implementation.
//

// Return a string identifying this object model.
const char* MeshObjectFactory::get_model()
{
    return "mesh_object";
}

// Create a new mesh object.
auto_release_ptr<MeshObject> MeshObjectFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<MeshObject>(
            new MeshObject(name, params));
}

}   // namespace renderer
