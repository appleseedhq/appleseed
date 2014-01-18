
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
#include "scene.h"

// appleseed.renderer headers.
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Scene class implementation.
//

struct Scene::Impl
{
    UniqueID                        m_uid;
    auto_release_ptr<Camera>        m_camera;
    auto_release_ptr<Environment>   m_environment;
    EnvironmentEDFContainer         m_environment_edfs;
    EnvironmentShaderContainer      m_environment_shaders;

    explicit Impl(Entity* parent)
      : m_environment_edfs(parent)
      , m_environment_shaders(parent)
    {
    }
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Scene::Scene()
  : Entity(g_class_uid)
  , BaseGroup(this)
  , impl(new Impl(this))
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

GAABB3 Scene::compute_bbox() const
{
    const AssemblyInstanceContainer& instances = assembly_instances();
    return compute_parent_bbox<GAABB3>(instances.begin(), instances.end());
}

double Scene::compute_radius() const
{
    double square_radius = 0.0;
    const GAABB3 bbox = compute_bbox();

    if (bbox.is_valid())
    {
        for (size_t i = 0; i < 8; ++i)
        {
            const double square_distance =
                static_cast<double>(square_norm(bbox.compute_corner(i)));

            square_radius = max(square_radius, square_distance);
        }
    }

    return sqrt(square_radius);
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
    void invoke_on_frame_end(
        const Project&          project,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project);
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
#endif
}

bool Scene::on_frame_begin(
    const Project&          project, 
#ifdef WITH_OSL
    OSL::ShadingSystem*     shading_system,
#endif            
    AbortSwitch*            abort_switch)
{
    bool success = true;

    if (impl->m_camera.get())
        success = success && impl->m_camera->on_frame_begin(project, abort_switch);

    success = success && invoke_on_frame_begin(project, texture_instances(), abort_switch);
    success = success && invoke_on_frame_begin(project, environment_edfs(), abort_switch);
    success = success && invoke_on_frame_begin(project, environment_shaders(), abort_switch);

    if (is_aborted(abort_switch))
        return success;

    if (impl->m_environment.get())
        success = success && impl->m_environment->on_frame_begin(project, abort_switch);

#ifdef WITH_OSL
    success = success && invoke_on_frame_begin(project, assemblies(), shading_system, abort_switch);
#else
    success = success && invoke_on_frame_begin(project, assemblies(), abort_switch);
#endif

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
}


//
// SceneFactory class implementation.
//

auto_release_ptr<Scene> SceneFactory::create()
{
    return auto_release_ptr<Scene>(new Scene());
}

}   // namespace renderer
