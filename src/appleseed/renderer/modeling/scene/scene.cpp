
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
#include "scene.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/proceduralassembly.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"

// Standard headers.
#include <set>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Scene class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Scene::get_class_uid()
{
    return g_class_uid;
}

struct Scene::Impl
{
    UniqueID                        m_uid;
    CameraContainer                 m_cameras;
    auto_release_ptr<Environment>   m_environment;
    EnvironmentEDFContainer         m_environment_edfs;
    EnvironmentShaderContainer      m_environment_shaders;
    auto_release_ptr<SurfaceShader> m_default_surface_shader;

    explicit Impl(Entity* parent)
      : m_cameras(parent)
      , m_environment_edfs(parent)
      , m_environment_shaders(parent)
      , m_default_surface_shader(
            PhysicalSurfaceShaderFactory().create(
                "default_surface_shader",
                ParamArray()))
    {
    }
};

Scene::Scene()
  : Entity(g_class_uid)
  , BaseGroup(this)
  , impl(new Impl(this))
  , m_has_render_data(false)
  , m_camera(0)
{
    set_name("scene");
}

Scene::~Scene()
{
    delete impl;
}

void Scene::release()
{
    delete this;
}

Camera* Scene::get_active_camera() const
{
    return m_camera;
}

CameraContainer& Scene::cameras() const
{
    return impl->m_cameras;
}

void Scene::set_environment(auto_release_ptr<Environment> environment)
{
    impl->m_environment = environment;

    if (impl->m_environment.get())
        impl->m_environment->set_parent(this);
}

Environment* Scene::get_environment() const
{
    return impl->m_environment.get();
}

EnvironmentEDFContainer& Scene::environment_edfs() const
{
    return impl->m_environment_edfs;
}

EnvironmentShaderContainer& Scene::environment_shaders() const
{
    return impl->m_environment_shaders;
}

SurfaceShader* Scene::get_default_surface_shader() const
{
    return impl->m_default_surface_shader.get();
}

GAABB3 Scene::compute_bbox() const
{
    const AssemblyInstanceContainer& instances = assembly_instances();
    const GAABB3 bbox = compute_parent_bbox<GAABB3>(instances.begin(), instances.end());
    return bbox.is_valid() ? bbox : GAABB3(GVector3(0.0f), GVector3(0.0f));
}

namespace
{
    bool assembly_instances_use_alpha_mapping(
        const AssemblyInstanceContainer&  assembly_instances,
        set<UniqueID>&                    visited_assemblies)
    {
        // Regarding transparency in the Tracer,
        // we only care about camera and shadow rays.
        const uint32 visibility_mask = VisibilityFlags::CameraRay | VisibilityFlags::ShadowRay;

        for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
        {
            // Retrieve the assembly instance.
            const AssemblyInstance& assembly_instance = *i;

            // Skip invisible assembly instances.
            if ((assembly_instance.get_vis_flags() & visibility_mask) == 0)
                continue;

            // Retrieve the assembly.
            const Assembly& assembly = assembly_instance.get_assembly();

            if (visited_assemblies.find(assembly.get_uid()) == visited_assemblies.end())
            {
                visited_assemblies.insert(assembly.get_uid());

                // Check the assembly contents.
                for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
                {
                    // Skip invisible object instances.
                    if ((i->get_vis_flags() & visibility_mask) == 0)
                        continue;

                    if (i->uses_alpha_mapping())
                        return true;
                }

                // Recurse into child assembly instances.
                if (assembly_instances_use_alpha_mapping(assembly.assembly_instances(), visited_assemblies))
                    return true;
            }
        }

        return false;
    }
}

bool Scene::uses_alpha_mapping() const
{
    set<UniqueID> visited_assemblies;
    return assembly_instances_use_alpha_mapping(assembly_instances(), visited_assemblies);
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

void Scene::collect_asset_paths(StringArray& paths) const
{
    BaseGroup::collect_asset_paths(paths);

    do_collect_asset_paths(paths, cameras());

    if (impl->m_environment.get())
        impl->m_environment->collect_asset_paths(paths);

    do_collect_asset_paths(paths, environment_edfs());
    do_collect_asset_paths(paths, environment_shaders());
}

void Scene::update_asset_paths(const StringDictionary& mappings)
{
    BaseGroup::update_asset_paths(mappings);

    do_update_asset_paths(mappings, cameras());

    if (impl->m_environment.get())
        impl->m_environment->update_asset_paths(mappings);

    do_update_asset_paths(mappings, environment_edfs());
    do_update_asset_paths(mappings, environment_shaders());
}

bool Scene::on_render_begin(
    const Project&          project,
    IAbortSwitch*           abort_switch)
{
    bool success = true;

    create_render_data();

    for (each<CameraContainer> i = cameras(); i; ++i)
        success = success && i->on_render_begin(project, abort_switch);

    return success;
}

void Scene::on_render_end(const Project& project)
{
    for (each<CameraContainer> i = cameras(); i; ++i)
        i->on_render_end(project);

    m_has_render_data = false;
}

namespace
{
    bool invoke_procedural_expand(
        Assembly&               assembly,
        const Project&          project,
        const Assembly*         parent,
        IAbortSwitch*           abort_switch)
    {
        ProceduralAssembly* proc_assembly =
            dynamic_cast<ProceduralAssembly*>(&assembly);

        if (proc_assembly)
        {
            if (!proc_assembly->expand_contents(project, parent, abort_switch))
                return false;
        }

        for (each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
        {
            if (!invoke_procedural_expand(*i, project, &assembly, abort_switch))
                return false;
        }

        return true;
    }
}

bool Scene::expand_procedural_assemblies(
    const Project&          project,
    IAbortSwitch*           abort_switch)
{
    for (each<AssemblyContainer> i = assemblies(); i; ++i)
    {
        if (!invoke_procedural_expand(*i, project, 0, abort_switch))
            return false;
    }

    return true;
}

namespace
{
    template <typename EntityCollection>
    bool invoke_on_frame_begin(
        const Project&          project,
        const BaseGroup*        parent,
        EntityCollection&       entities,
        OnFrameBeginRecorder&   recorder,
        IAbortSwitch*           abort_switch)
    {
        bool success = true;

        for (each<EntityCollection> i = entities; i; ++i)
        {
            if (is_aborted(abort_switch))
                break;

            success = success && i->on_frame_begin(project, parent, recorder, abort_switch);
        }

        return success;
    }
}

bool Scene::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_camera = project.get_uncached_active_camera();

    // Fail if we don't have a camera.
    if (m_camera == 0)
    {
        RENDERER_LOG_ERROR("no cameras in scene or no camera specified in the frame entity.");
        return false;
    }

    bool success = true;

    success = success && impl->m_default_surface_shader->on_frame_begin(project, this, recorder, abort_switch);

    success = success && invoke_on_frame_begin(project, this, colors(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, textures(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, texture_instances(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, shader_groups(), recorder, abort_switch);

    success = success && invoke_on_frame_begin(project, this, cameras(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, environment_edfs(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, environment_shaders(), recorder, abort_switch);

    if (!is_aborted(abort_switch) && impl->m_environment.get())
        success = success && impl->m_environment->on_frame_begin(project, this, recorder, abort_switch);

    success = success && invoke_on_frame_begin(project, this, assemblies(), recorder, abort_switch);
    success = success && invoke_on_frame_begin(project, this, assembly_instances(), recorder, abort_switch);

    return success;
}

void Scene::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    m_camera = 0;

    Entity::on_frame_end(project, parent);
}

void Scene::create_render_data()
{
    assert(!m_has_render_data);

    m_render_data.m_bbox = compute_bbox();
    m_render_data.m_center = m_render_data.m_bbox.center();
    m_render_data.m_radius = m_render_data.m_bbox.radius();
    m_render_data.m_diameter = m_render_data.m_bbox.diameter();
    m_render_data.m_safe_diameter = m_render_data.m_diameter * GScalar(1.01);

    m_has_render_data = true;
}


//
// SceneFactory class implementation.
//

auto_release_ptr<Scene> SceneFactory::create()
{
    return auto_release_ptr<Scene>(new Scene());
}

}   // namespace renderer
