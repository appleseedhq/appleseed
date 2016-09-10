
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
#include "assembly.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"

using namespace foundation;
using namespace std;

namespace renderer
{

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

    explicit Impl(Entity* parent)
      : m_bsdfs(parent)
      , m_bssrdfs(parent)
      , m_edfs(parent)
      , m_surface_shaders(parent)
      , m_materials(parent)
      , m_lights(parent)
      , m_objects(parent)
      , m_object_instances(parent)
    {
    }
};

Assembly::Assembly(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , BaseGroup(this)
  , impl(new Impl(this))
{
    set_name(name);

    m_flushable = m_params.get_optional<bool>("flushable", false);
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

namespace
{
    template <typename EntityCollection>
    void do_collect_asset_paths(
        StringArray&            paths,
        const EntityCollection& entities)
    {
        for (const_each<EntityCollection> i = entities; i; ++i)
            i->collect_asset_paths(paths);
    }

    template <typename EntityCollection>
    void do_update_asset_paths(
        const StringDictionary& mappings,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->update_asset_paths(mappings);
    }
}

void Assembly::collect_asset_paths(StringArray& paths) const
{
    BaseGroup::collect_asset_paths(paths);

    do_collect_asset_paths(paths, bsdfs());
    do_collect_asset_paths(paths, bssrdfs());
    do_collect_asset_paths(paths, edfs());
    do_collect_asset_paths(paths, surface_shaders());
    do_collect_asset_paths(paths, materials());
    do_collect_asset_paths(paths, lights());
    do_collect_asset_paths(paths, objects());
    do_collect_asset_paths(paths, object_instances());
}

void Assembly::update_asset_paths(const StringDictionary& mappings)
{
    BaseGroup::update_asset_paths(mappings);

    do_update_asset_paths(mappings, bsdfs());
    do_update_asset_paths(mappings, bssrdfs());
    do_update_asset_paths(mappings, edfs());
    do_update_asset_paths(mappings, surface_shaders());
    do_update_asset_paths(mappings, materials());
    do_update_asset_paths(mappings, lights());
    do_update_asset_paths(mappings, objects());
    do_update_asset_paths(mappings, object_instances());
}

namespace
{
    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        EntityCollection&       entities,
        IAbortSwitch*           abort_switch)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
        {
            if (is_aborted(abort_switch))
                break;

            success = success && i->on_frame_begin(project, abort_switch);
        }

        return success;
    }

    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        const Assembly&         assembly,
        EntityCollection&       entities,
        IAbortSwitch*           abort_switch)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
        {
            if (is_aborted(abort_switch))
                break;

            success = success && i->on_frame_begin(project, assembly, abort_switch);
        }

        return success;
    }

    template <typename EntityCollection>
    void invoke_on_frame_end(
        const Project&          project,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project);
    }

    template <typename EntityCollection>
    void invoke_on_frame_end(
        const Project&          project,
        const Assembly&         assembly,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project, assembly);
    }
}

bool Assembly::on_frame_begin(
    const Project&      project,
    IAbortSwitch*       abort_switch)
{
    bool success = true;
    success = success && invoke_on_frame_begin(project, textures(), abort_switch);
    success = success && invoke_on_frame_begin(project, texture_instances(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, surface_shaders(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, bsdfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, bssrdfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, edfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, materials(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, lights(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, objects(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, object_instances(), abort_switch);
    success = success && invoke_on_frame_begin(project, assemblies(), abort_switch);
    success = success && invoke_on_frame_begin(project, assembly_instances(), abort_switch);
    return success;
}

void Assembly::on_frame_end(const Project& project)
{
    invoke_on_frame_end(project, assembly_instances());
    invoke_on_frame_end(project, assemblies());
    invoke_on_frame_end(project, object_instances());
    invoke_on_frame_end(project, objects());
    invoke_on_frame_end(project, *this, lights());
    invoke_on_frame_end(project, *this, materials());
    invoke_on_frame_end(project, *this, edfs());
    invoke_on_frame_end(project, *this, bssrdfs());
    invoke_on_frame_end(project, *this, bsdfs());
    invoke_on_frame_end(project, *this, surface_shaders());
    invoke_on_frame_end(project, texture_instances());
    invoke_on_frame_end(project, textures());
}


//
// AssemblyFactory class implementation.
//

const char* AssemblyFactory::get_model() const
{
    return Model;
}

auto_release_ptr<Assembly> AssemblyFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Assembly>(new Assembly(name, params));
}

auto_release_ptr<Assembly> AssemblyFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Assembly>(new Assembly(name, params));
}

}   // namespace renderer
