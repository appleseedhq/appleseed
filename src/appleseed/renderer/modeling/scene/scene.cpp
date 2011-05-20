
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/scene/assemblyinstance.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

using namespace foundation;
using namespace std;

namespace renderer
{

struct Scene::Impl
{
    UniqueID                                m_uid;
    VersionID                               m_asm_inst_version_id;
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
    impl->m_asm_inst_version_id = 0;
    impl->m_geometry_version_id = 0;
}

Scene::~Scene()
{
    delete impl;
}

VersionID Scene::get_assembly_instances_version_id() const
{
    return impl->m_asm_inst_version_id;
}

void Scene::increase_assembly_instances_version_id()
{
    ++impl->m_asm_inst_version_id;
}

VersionID Scene::get_geometry_version_id() const
{
    return impl->m_geometry_version_id;
}

void Scene::increase_geometry_version_id()
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

double Scene::compute_radius() const
{
    double square_radius = 0.0;

    for (const_each<AssemblyInstanceContainer> i = impl->m_assembly_instances; i; ++i)
    {
        const AssemblyInstance& inst = *i;
        const GAABB3 inst_bbox = inst.compute_parent_bbox();

        GVector3 corners[8];
        inst_bbox.compute_corners(corners);

        for (size_t j = 0; j < 8; ++j)
        {
            const double square_distance = square_norm(corners[i]);

            if (square_radius < square_distance)
                square_radius = square_distance;
        }
    }

    return sqrt(square_radius);
}

}   // namespace renderer
