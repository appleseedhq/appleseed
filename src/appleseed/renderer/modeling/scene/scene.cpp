
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/foreach.h"

// Standard headers.
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
    UniqueID                                m_uid;
    VersionID                               m_geometry_version_id;
    auto_release_ptr<Camera>                m_camera;
    auto_release_ptr<Environment>           m_environment;
    ColorContainer                          m_colors;
    TextureContainer                        m_textures;
    TextureInstanceContainer                m_texture_instances;
    EnvironmentEDFContainer                 m_environment_edfs;
    EnvironmentShaderContainer              m_environment_shaders;
    AssemblyContainer                       m_assemblies;
    AssemblyInstanceContainer               m_assembly_instances;
};

Scene::Scene()
  : impl(new Impl())
{
    impl->m_geometry_version_id = 0;
}

Scene::~Scene()
{
    delete impl;
}

void Scene::release()
{
    delete this;
}

VersionID Scene::get_geometry_version_id() const
{
    return impl->m_geometry_version_id;
}

void Scene::bump_geometry_version_id()
{
    ++impl->m_geometry_version_id;
}

void Scene::set_camera(auto_release_ptr<Camera> camera)
{
    impl->m_camera = camera;
}

Camera* Scene::get_camera() const
{
    return impl->m_camera.get();
}

void Scene::set_environment(auto_release_ptr<Environment> environment)
{
    impl->m_environment = environment;
}

Environment* Scene::get_environment() const
{
    return impl->m_environment.get();
}

ColorContainer& Scene::colors() const
{
    return impl->m_colors;
}

TextureContainer& Scene::textures() const
{
    return impl->m_textures;
}

TextureInstanceContainer& Scene::texture_instances() const
{
    return impl->m_texture_instances;
}

EnvironmentEDFContainer& Scene::environment_edfs() const
{
    return impl->m_environment_edfs;
}

EnvironmentShaderContainer& Scene::environment_shaders() const
{
    return impl->m_environment_shaders;
}

AssemblyContainer& Scene::assemblies() const
{
    return impl->m_assemblies;
}

AssemblyInstanceContainer& Scene::assembly_instances() const
{
    return impl->m_assembly_instances;
}

GAABB3 Scene::compute_bbox() const
{
    return
        compute_parent_bbox<GAABB3>(
            impl->m_assembly_instances.begin(),
            impl->m_assembly_instances.end());
}

double Scene::compute_radius() const
{
    double square_radius = 0.0;

    const GAABB3 bbox = compute_bbox();

    if (bbox.is_valid())
    {
        GVector3 corners[8];
        bbox.compute_corners(corners);

        for (size_t i = 0; i < 8; ++i)
        {
            const double square_distance = square_norm(corners[i]);

            if (square_radius < square_distance)
                square_radius = square_distance;
        }
    }

    return sqrt(square_radius);
}

namespace
{
    template <typename EntityCollection>
    void invoke_on_frame_begin(
        const Project&          project,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_begin(project);
    }

    template <typename EntityCollection>
    void invoke_on_frame_end(
        const Project&          project,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->on_frame_end(project);
    }
}

void Scene::on_frame_begin(const Project& project)
{
    if (impl->m_camera.get())
        impl->m_camera->on_frame_begin(project);

    invoke_on_frame_begin(project, environment_edfs());
    invoke_on_frame_begin(project, environment_shaders());
    invoke_on_frame_begin(project, assemblies());
}

void Scene::on_frame_end(const Project& project)
{
    invoke_on_frame_end(project, assemblies());
    invoke_on_frame_end(project, environment_shaders());
    invoke_on_frame_end(project, environment_edfs());

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
