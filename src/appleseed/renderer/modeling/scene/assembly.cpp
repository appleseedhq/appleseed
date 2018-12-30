
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
#include "assembly.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/proceduralobject.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/volume/volume.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/job/abortswitch.h"

using namespace foundation;
using namespace std;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(IndexedObjectInstanceArray);


//
// Assembly class implementation.
//

const char* Model = "generic_assembly";

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Assembly::get_class_uid()
{
    return g_class_uid;
}

struct Assembly::Impl
{
    BSDFContainer               m_bsdfs;
    BSSRDFContainer             m_bssrdfs;
    EDFContainer                m_edfs;
    SurfaceShaderContainer      m_surface_shaders;
    MaterialContainer           m_materials;
    LightContainer              m_lights;
    ObjectContainer             m_objects;
    ObjectInstanceContainer     m_object_instances;
    VolumeContainer             m_volumes;

    explicit Impl(Entity* parent)
      : m_bsdfs(parent)
      , m_bssrdfs(parent)
      , m_edfs(parent)
      , m_surface_shaders(parent)
      , m_materials(parent)
      , m_lights(parent)
      , m_objects(parent)
      , m_object_instances(parent)
      , m_volumes(parent)
    {
    }
};

Assembly::Assembly(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , BaseGroup(this)
  , impl(new Impl(this))
  , m_has_render_data(false)
{
    set_name(name);
}

Assembly::~Assembly()
{
    delete impl;
}

void Assembly::release()
{
    delete this;
}

const char* Assembly::get_model() const
{
    return Model;
}

BSDFContainer& Assembly::bsdfs() const
{
    return impl->m_bsdfs;
}

BSSRDFContainer& Assembly::bssrdfs() const
{
    return impl->m_bssrdfs;
}

EDFContainer& Assembly::edfs() const
{
    return impl->m_edfs;
}

SurfaceShaderContainer& Assembly::surface_shaders() const
{
    return impl->m_surface_shaders;
}

MaterialContainer& Assembly::materials() const
{
    return impl->m_materials;
}

LightContainer& Assembly::lights() const
{
    return impl->m_lights;
}

ObjectContainer& Assembly::objects() const
{
    return impl->m_objects;
}

ObjectInstanceContainer& Assembly::object_instances() const
{
    return impl->m_object_instances;
}

VolumeContainer& Assembly::volumes() const
{
    return impl->m_volumes;
}

void Assembly::clear()
{
    BaseGroup::clear();

    impl->m_bsdfs.clear();
    impl->m_bssrdfs.clear();
    impl->m_edfs.clear();
    impl->m_surface_shaders.clear();
    impl->m_materials.clear();
    impl->m_lights.clear();
    impl->m_objects.clear();
    impl->m_object_instances.clear();
    impl->m_volumes.clear();
}

GAABB3 Assembly::compute_local_bbox() const
{
    GAABB3 bbox = compute_non_hierarchical_local_bbox();

    bbox.insert(
        compute_parent_bbox<GAABB3>(
            assembly_instances().begin(),
            assembly_instances().end()));

    return bbox;
}

GAABB3 Assembly::compute_non_hierarchical_local_bbox() const
{
    return
        compute_parent_bbox<GAABB3>(
            impl->m_object_instances.begin(),
            impl->m_object_instances.end());
}

void Assembly::collect_asset_paths(StringArray& paths) const
{
    BaseGroup::collect_asset_paths(paths);

    invoke_collect_asset_paths(bsdfs(), paths);
    invoke_collect_asset_paths(bssrdfs(), paths);
    invoke_collect_asset_paths(edfs(), paths);
    invoke_collect_asset_paths(surface_shaders(), paths);
    invoke_collect_asset_paths(materials(), paths);
    invoke_collect_asset_paths(lights(), paths);
    invoke_collect_asset_paths(objects(), paths);
    invoke_collect_asset_paths(object_instances(), paths);
    invoke_collect_asset_paths(volumes(), paths);
}

void Assembly::update_asset_paths(const StringDictionary& mappings)
{
    BaseGroup::update_asset_paths(mappings);

    invoke_update_asset_paths(bsdfs(), mappings);
    invoke_update_asset_paths(bssrdfs(), mappings);
    invoke_update_asset_paths(edfs(), mappings);
    invoke_update_asset_paths(surface_shaders(), mappings);
    invoke_update_asset_paths(materials(), mappings);
    invoke_update_asset_paths(lights(), mappings);
    invoke_update_asset_paths(objects(), mappings);
    invoke_update_asset_paths(object_instances(), mappings);
    invoke_update_asset_paths(volumes(), mappings);
}

bool Assembly::on_render_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnRenderBeginRecorder&  recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_render_begin(project, parent, recorder, abort_switch))
        return false;

    if (!BaseGroup::on_render_begin(project, parent, recorder, abort_switch))
        return false;

    bool success = true;
    success = success && invoke_on_render_begin(bsdfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(bssrdfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(edfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(surface_shaders(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(materials(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(lights(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(objects(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(object_instances(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(volumes(), project, this, recorder, abort_switch);

    return success;
}

bool Assembly::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (!BaseGroup::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    bool success = true;
    success = success && invoke_on_frame_begin(bsdfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(bssrdfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(edfs(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(surface_shaders(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(materials(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(lights(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(objects(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(object_instances(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(volumes(), project, this, recorder, abort_switch);
    if (!success)
        return false;

    // Collect procedural object instances.
    assert(!m_has_render_data);
    for (size_t i = 0, e = object_instances().size(); i < e; ++i)
    {
        const ObjectInstance* object_instance = object_instances().get_by_index(i);
        const Object& object = object_instance->get_object();
        if (dynamic_cast<const ProceduralObject*>(&object) != nullptr)
            m_render_data.m_procedural_object_instances.push_back(make_pair(object_instance, i));
    }
    m_has_render_data = true;

    return true;
}

void Assembly::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    // `m_has_render_data` may be false if `on_frame_begin()` failed.
    if (m_has_render_data)
    {
        m_render_data.m_procedural_object_instances.clear();
        m_has_render_data = false;
    }

    Entity::on_frame_end(project, parent);
}


//
// AssemblyFactory class implementation.
//

void AssemblyFactory::release()
{
    delete this;
}

const char* AssemblyFactory::get_model() const
{
    return Model;
}

Dictionary AssemblyFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Generic Assembly");
}

DictionaryArray AssemblyFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<Assembly> AssemblyFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Assembly>(new Assembly(name, params));
}

}   // namespace renderer
