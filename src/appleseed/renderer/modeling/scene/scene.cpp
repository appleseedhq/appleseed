
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/surfaceshader/physicalsurfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/foreach.h"

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
    auto_release_ptr<Camera>        m_camera;
    auto_release_ptr<Environment>   m_environment;
    EnvironmentEDFContainer         m_environment_edfs;
    EnvironmentShaderContainer      m_environment_shaders;
    auto_release_ptr<SurfaceShader> m_default_surface_shader;

    explicit Impl(Entity* parent)
      : m_environment_edfs(parent)
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

void Scene::set_camera(auto_release_ptr<Camera> camera)
{
    impl->m_camera = camera;

    if (impl->m_camera.get())
        impl->m_camera->set_parent(this);
}

Camera* Scene::get_camera() const
{
    return impl->m_camera.get();
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
    return compute_parent_bbox<GAABB3>(instances.begin(), instances.end());
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
    bool invoke_on_frame_begin(
        const Project&      project,
        EntityCollection&   entities,
        IAbortSwitch*       abort_switch)
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
    void invoke_on_frame_end(
        const Project&      project,
        EntityCollection&   entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project);
    }
}

bool Scene::on_frame_begin(
    const Project&          project,
    IAbortSwitch*           abort_switch)
{
    create_render_data();

    bool success = true;

    if (impl->m_camera.get())
        success = success && impl->m_camera->on_frame_begin(project, abort_switch);

    success = success && invoke_on_frame_begin(project, texture_instances(), abort_switch);
    success = success && invoke_on_frame_begin(project, environment_edfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, environment_shaders(), abort_switch);

    if (!is_aborted(abort_switch) && impl->m_environment.get())
        success = success && impl->m_environment->on_frame_begin(project, abort_switch);

    success = success && invoke_on_frame_begin(project, assemblies(), abort_switch);
    success = success && invoke_on_frame_begin(project, assembly_instances(), abort_switch);

    return success;
}

void Scene::on_frame_end(const Project& project)
{
    invoke_on_frame_end(project, assembly_instances());
    invoke_on_frame_end(project, assemblies());

    if (impl->m_environment.get())
        impl->m_environment->on_frame_end(project);

    invoke_on_frame_end(project, environment_shaders());
    invoke_on_frame_end(project, environment_edfs());
    invoke_on_frame_end(project, texture_instances());

    if (impl->m_camera.get())
        impl->m_camera->on_frame_end(project);

    m_has_render_data = false;
}

void Scene::create_render_data()
{
    assert(!m_has_render_data);

    m_render_data.m_bbox = compute_bbox();

    if (m_render_data.m_bbox.is_valid())
    {
        m_render_data.m_center = m_render_data.m_bbox.center();
        m_render_data.m_radius = m_render_data.m_bbox.radius();
        m_render_data.m_diameter = m_render_data.m_bbox.diameter();
        m_render_data.m_safe_diameter = m_render_data.m_diameter * GScalar(1.01);
    }
    else
    {
        m_render_data.m_center = GVector3(0.0);
        m_render_data.m_radius = GScalar(0.0);
        m_render_data.m_diameter = GScalar(0.0);
        m_render_data.m_safe_diameter = GScalar(0.0);
    }

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
