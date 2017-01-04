
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MeshObject class implementation.
//

namespace
{
    // A region that simply wraps a static tessellation.
    class MeshRegion
      : public IRegion
    {
      public:
        explicit MeshRegion(StaticTriangleTess* tess)
          : m_tess(tess)
          , m_lazy_tess(tess)
        {
        }

        virtual GAABB3 compute_local_bbox() const APPLESEED_OVERRIDE
        {
            return m_tess->compute_local_bbox();
        }

        virtual Lazy<StaticTriangleTess>& get_static_triangle_tess() const APPLESEED_OVERRIDE
        {
            return m_lazy_tess;
        }

      private:
        StaticTriangleTess*                 m_tess;
        mutable Lazy<StaticTriangleTess>    m_lazy_tess;
    };
}

struct MeshObject::Impl
{
    StaticTriangleTess          m_tess;
    MeshRegion                  m_region;
    RegionKit                   m_region_kit;
    mutable Lazy<RegionKit>     m_lazy_region_kit;
    vector<string>              m_material_slots;

    Impl()
      : m_region(&m_tess)
      , m_lazy_region_kit(&m_region_kit)
    {
        m_region_kit.push_back(&m_region);
    }
};

MeshObject::MeshObject(
    const char*             name,
    const ParamArray&       params)
  : Object(name, params)
  , impl(new Impl())
{
    m_inputs.declare("alpha_map", InputFormatFloat, "");
}

MeshObject::~MeshObject()
{
    delete impl;
}

void MeshObject::release()
{
    delete this;
}

const char* MeshObject::get_model() const
{
    return MeshObjectFactory::get_model();
}

bool MeshObject::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Object::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_alpha_map = get_uncached_alpha_map();
    m_shade_alpha_cutouts = m_params.get_optional<bool>("shade_alpha_cutouts", false);

    return true;
}

void MeshObject::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    m_alpha_map = 0;
    m_shade_alpha_cutouts = false;

    Object::on_frame_end(project, parent);
}

bool MeshObject::has_alpha_map() const
{
    if (!m_params.strings().exist("alpha_map"))
        return false;

    const char* value = m_params.strings().get("alpha_map");

    return !is_empty_string(value);
}

const Source* MeshObject::get_uncached_alpha_map() const
{
    return m_inputs.source("alpha_map");
}

GAABB3 MeshObject::compute_local_bbox() const
{
    return impl->m_tess.compute_local_bbox();
}

Lazy<RegionKit>& MeshObject::get_region_kit()
{
    return impl->m_lazy_region_kit;
}

void MeshObject::reserve_vertices(const size_t count)
{
    impl->m_tess.m_vertices.reserve(count);
}

size_t MeshObject::push_vertex(const GVector3& vertex)
{
    const size_t index = impl->m_tess.m_vertices.size();
    impl->m_tess.m_vertices.push_back(vertex);
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

void MeshObject::reserve_vertex_normals(const size_t count)
{
    impl->m_tess.m_vertex_normals.reserve(count);
}

size_t MeshObject::push_vertex_normal(const GVector3& normal)
{
    assert(is_normalized(normal));

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

void MeshObject::clear_vertex_normals()
{
    impl->m_tess.m_vertex_normals.clear();
}

void MeshObject::reserve_vertex_tangents(const size_t count)
{
    impl->m_tess.reserve_vertex_tangents(count);
}

size_t MeshObject::push_vertex_tangent(const GVector3& tangent)
{
    return impl->m_tess.push_vertex_tangent(tangent);
}

size_t MeshObject::get_vertex_tangent_count() const
{
    return impl->m_tess.get_vertex_tangent_count();
}

GVector3 MeshObject::get_vertex_tangent(const size_t index) const
{
    return impl->m_tess.get_vertex_tangent(index);
}

void MeshObject::reserve_tex_coords(const size_t count)
{
    impl->m_tess.reserve_tex_coords(count);
}

size_t MeshObject::push_tex_coords(const GVector2& tex_coords)
{
    return impl->m_tess.push_tex_coords(tex_coords);
}

size_t MeshObject::get_tex_coords_count() const
{
    return impl->m_tess.get_tex_coords_count();
}

GVector2 MeshObject::get_tex_coords(const size_t index) const
{
    return impl->m_tess.get_tex_coords(index);
}

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

Triangle& MeshObject::get_triangle(const size_t index)
{
    return impl->m_tess.m_primitives[index];
}

void MeshObject::clear_triangles()
{
    impl->m_tess.m_primitives.clear();
}

void MeshObject::set_motion_segment_count(const size_t count)
{
    impl->m_tess.set_motion_segment_count(count);
}

size_t MeshObject::get_motion_segment_count() const
{
    return impl->m_tess.get_motion_segment_count();
}

void MeshObject::set_vertex_pose(
    const size_t            vertex_index,
    const size_t            motion_segment_index,
    const GVector3&         vertex)
{
    impl->m_tess.set_vertex_pose(vertex_index, motion_segment_index, vertex);
}

GVector3 MeshObject::get_vertex_pose(
    const size_t            vertex_index,
    const size_t            motion_segment_index) const
{
    return impl->m_tess.get_vertex_pose(vertex_index, motion_segment_index);
}

void MeshObject::clear_vertex_poses()
{
    impl->m_tess.clear_vertex_poses();
}

void MeshObject::set_vertex_normal_pose(
    const size_t            normal_index,
    const size_t            motion_segment_index,
    const GVector3&         normal)
{
    impl->m_tess.set_vertex_normal_pose(normal_index, motion_segment_index, normal);
}

GVector3 MeshObject::get_vertex_normal_pose(
    const size_t            normal_index,
    const size_t            motion_segment_index) const
{
    return impl->m_tess.get_vertex_normal_pose(normal_index, motion_segment_index);
}

void MeshObject::clear_vertex_normal_poses()
{
    impl->m_tess.clear_vertex_normal_poses();
}

void MeshObject::set_vertex_tangent_pose(
    const size_t            tangent_index,
    const size_t            motion_segment_index,
    const GVector3&         tangent)
{
    impl->m_tess.set_vertex_tangent_pose(tangent_index, motion_segment_index, tangent);
}

GVector3 MeshObject::get_vertex_tangent_pose(
    const size_t            tangent_index,
    const size_t            motion_segment_index) const
{
    return impl->m_tess.get_vertex_tangent_pose(tangent_index, motion_segment_index);
}

void MeshObject::clear_vertex_tangent_poses()
{
    impl->m_tess.clear_vertex_tangent_poses();
}

void MeshObject::reserve_material_slots(const size_t count)
{
    impl->m_material_slots.reserve(count);
}

size_t MeshObject::push_material_slot(const char* name)
{
    const size_t index = impl->m_material_slots.size();
    impl->m_material_slots.push_back(name);
    return index;
}

size_t MeshObject::get_material_slot_count() const
{
    return impl->m_material_slots.size();
}

const char* MeshObject::get_material_slot(const size_t index) const
{
    return impl->m_material_slots[index].c_str();
}

void MeshObject::collect_asset_paths(StringArray& paths) const
{
    if (m_params.strings().exist("filename"))
        paths.push_back(m_params.get("filename"));
    else if (m_params.dictionaries().exist("filename"))
    {
        const StringDictionary& filepaths = m_params.dictionaries().get("filename").strings();
        for (const_each<StringDictionary> i = filepaths; i; ++i)
            paths.push_back(i->value());
    }
}

void MeshObject::update_asset_paths(const StringDictionary& mappings)
{
    if (m_params.strings().exist("filename"))
        m_params.set("filename", mappings.get(m_params.get("filename")));
    else if (m_params.dictionaries().exist("filename"))
    {
        StringDictionary& filepaths = m_params.dictionaries().get("filename").strings();
        for (const_each<StringDictionary> i = filepaths; i; ++i)
            filepaths.set(i->key(), mappings.get(i->value()));
    }
}


//
// MeshObjectFactory class implementation.
//

const char* MeshObjectFactory::get_model()
{
    return "mesh_object";
}

auto_release_ptr<MeshObject> MeshObjectFactory::create(
    const char*             name,
    const ParamArray&       params)
{
    return auto_release_ptr<MeshObject>(new MeshObject(name, params));
}

}   // namespace renderer
