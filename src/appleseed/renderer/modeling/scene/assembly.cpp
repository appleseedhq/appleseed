
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#ifdef WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Assembly class implementation.
//

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
    EDFContainer                m_edfs;
    SurfaceShaderContainer      m_surface_shaders;
    MaterialContainer           m_materials;
    LightContainer              m_lights;
    ObjectContainer             m_objects;
    ObjectInstanceContainer     m_object_instances;

#ifdef WITH_OSL
    ShaderGroupContainer        m_shader_groups;
#endif
    
    explicit Impl(Entity* parent)
      : m_bsdfs(parent)
      , m_edfs(parent)
      , m_surface_shaders(parent)
      , m_materials(parent)
      , m_lights(parent)
      , m_objects(parent)
      , m_object_instances(parent)
#ifdef WITH_OSL
      , m_shader_groups(parent)
#endif
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

BSDFContainer& Assembly::bsdfs() const
{
    return impl->m_bsdfs;
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

#ifdef WITH_OSL

ShaderGroupContainer& Assembly::shader_groups() const
{
    return impl->m_shader_groups;
}

#endif

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
    bool invoke_on_frame_begin(
        const Project&          project,
        EntityCollection&       entities,
        AbortSwitch*            abort_switch)
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
        AbortSwitch*            abort_switch)
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

#ifdef WITH_OSL
    
    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        EntityCollection&       entities,
        OSL::ShadingSystem*     shading_system,
        AbortSwitch*            abort_switch)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
        {
            if (is_aborted(abort_switch))
                break;

            success = success && i->on_frame_begin(project, shading_system, abort_switch);
        }

        return success;
    }

    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        const Assembly&         assembly,
        OSL::ShadingSystem*     shading_system,
        EntityCollection&       entities,
        AbortSwitch*            abort_switch)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
        {
            if (is_aborted(abort_switch))
                break;

            success = success && i->on_frame_begin(project, assembly, shading_system, abort_switch);
        }

        return success;
    }
#endif
    
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
#ifdef WITH_OSL
    OSL::ShadingSystem* shading_system,
#endif
    AbortSwitch*        abort_switch)
{
    bool success = true;

    success = success && invoke_on_frame_begin(project, texture_instances(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, surface_shaders(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, bsdfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, edfs(), abort_switch);
    
#ifdef WITH_OSL
    success = success && invoke_on_frame_begin(project, *this, shading_system, shader_groups(), abort_switch);
#endif
    
    success = success && invoke_on_frame_begin(project, *this, materials(), abort_switch);
    success = success && invoke_on_frame_begin(project, *this, lights(), abort_switch);

#ifdef WITH_OSL
    success = success && invoke_on_frame_begin(project, assemblies(), shading_system, abort_switch);
#else
    success = success && invoke_on_frame_begin(project, assemblies(), abort_switch);
#endif
    
    success = success && invoke_on_frame_begin(project, assembly_instances(), abort_switch);

    return success;
}

void Assembly::on_frame_end(const Project& project)
{
    invoke_on_frame_end(project, assembly_instances());
    invoke_on_frame_end(project, assemblies());
    invoke_on_frame_end(project, *this, lights());
    invoke_on_frame_end(project, *this, materials());
    invoke_on_frame_end(project, *this, edfs());
    invoke_on_frame_end(project, *this, bsdfs());
    invoke_on_frame_end(project, *this, surface_shaders());
    invoke_on_frame_end(project, texture_instances());
}


//
// AssemblyFactory class implementation.
//

auto_release_ptr<Assembly> AssemblyFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Assembly>(new Assembly(name, params));
}

}   // namespace renderer
