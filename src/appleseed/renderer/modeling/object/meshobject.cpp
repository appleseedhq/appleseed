
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
#include "meshobject.h"

// appleseed.renderer headers.
#include "renderer/kernel/rasterization/objectrasterizer.h"
#include "renderer/modeling/object/meshobjectprimitives.h"
#include "renderer/modeling/object/meshobjectreader.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/api/apiarray.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// MeshObject class implementation.
//

namespace
{
    const char* Model = "mesh_object";
}

struct MeshObject::Impl
{
    StaticTriangleTess          m_tess;
    std::vector<std::string>    m_material_slots;
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
    return Model;
}

const Source* MeshObject::get_uncached_alpha_map() const
{
    return m_inputs.source("alpha_map");
}

GAABB3 MeshObject::compute_local_bbox() const
{
    return impl->m_tess.compute_local_bbox();
}

const StaticTriangleTess& MeshObject::get_static_triangle_tess() const
{
    return impl->m_tess;
}

void MeshObject::rasterize(ObjectRasterizer& rasterizer) const
{
    rasterizer.begin_object(impl->m_tess.m_primitives.size());

    for (const auto& prim : impl->m_tess.m_primitives)
    {
        const auto& v0 = impl->m_tess.m_vertices[prim.m_v0];
        const auto& v1 = impl->m_tess.m_vertices[prim.m_v1];
        const auto& v2 = impl->m_tess.m_vertices[prim.m_v2];

        // todo: check that vertex normals are available.
        const auto& n0 = impl->m_tess.m_vertex_normals[prim.m_n0];
        const auto& n1 = impl->m_tess.m_vertex_normals[prim.m_n1];
        const auto& n2 = impl->m_tess.m_vertex_normals[prim.m_n2];

        ObjectRasterizer::Triangle triangle;

        triangle.m_v0[0] = v0[0];
        triangle.m_v0[1] = v0[1];
        triangle.m_v0[2] = v0[2];

        triangle.m_v1[0] = v1[0];
        triangle.m_v1[1] = v1[1];
        triangle.m_v1[2] = v1[2];

        triangle.m_v2[0] = v2[0];
        triangle.m_v2[1] = v2[1];
        triangle.m_v2[2] = v2[2];

        triangle.m_n0[0] = n0[0];
        triangle.m_n0[1] = n0[1];
        triangle.m_n0[2] = n0[2];

        triangle.m_n1[0] = n1[0];
        triangle.m_n1[1] = n1[1];
        triangle.m_n1[2] = n1[2];

        triangle.m_n2[0] = n2[0];
        triangle.m_n2[1] = n2[1];
        triangle.m_n2[2] = n2[2];

        rasterizer.rasterize(triangle);
    }

    rasterizer.end_object();
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
    impl->m_material_slots.emplace_back(name);
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

void MeshObjectFactory::release()
{
    delete this;
}

const char* MeshObjectFactory::get_model() const
{
    return Model;
}

Dictionary MeshObjectFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Mesh Object");
}

DictionaryArray MeshObjectFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_map")
            .insert("label", "Alpha Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<Object> MeshObjectFactory::create(
    const char*             name,
    const ParamArray&       params) const
{
    return auto_release_ptr<Object>(new MeshObject(name, params));
}

bool MeshObjectFactory::create(
    const char*             name,
    const ParamArray&       params,
    const SearchPaths&      search_paths,
    const bool              omit_loading_assets,
    ObjectArray&            objects) const
{
    if (params.strings().exist("primitive"))
    {
        auto_release_ptr<MeshObject> mesh = create_primitive_mesh(name, params);
        if (mesh.get() == nullptr)
            return false;

        objects.push_back(mesh.release());
        return true;
    }

    if (omit_loading_assets)
    {
        objects.push_back(create(name, params).release());
        return true;
    }

    MeshObjectArray object_array;
    if (!MeshObjectReader::read(
            search_paths,
            name,
            params,
            object_array))
        return false;

    objects = array_vector<ObjectArray>(object_array);
    return true;
}

}   // namespace renderer
