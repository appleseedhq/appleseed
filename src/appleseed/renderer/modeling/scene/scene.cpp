
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

// Constructor.
Scene::Scene()
  : impl(new Impl())
{
    impl->m_uid = g_uid_source.get();
    impl->m_asm_inst_version_id = 0;
    impl->m_geometry_version_id = 0;
}

// Destructor.
Scene::~Scene()
{
    delete impl;
}

// Return the unique ID of this object.
UniqueID Scene::get_uid() const
{
    return impl->m_uid;
}

// Return/increase the version ID of the assembly instances.
VersionID Scene::get_assembly_instances_version_id() const
{
    return impl->m_asm_inst_version_id;
}
void Scene::increase_assembly_instances_version_id()
{
    ++impl->m_asm_inst_version_id;
}

// Return/increase the version ID of the scene geometry.
VersionID Scene::get_geometry_version_id() const
{
    return impl->m_geometry_version_id;
}
void Scene::increase_geometry_version_id()
{
    ++impl->m_geometry_version_id;
}

// Set the camera.
void Scene::set_camera(auto_release_ptr<Camera> camera)
{
    impl->m_camera = camera;
}

// Access the camera.
Camera* Scene::get_camera() const
{
    return impl->m_camera.get();
}

// Set the environment.
void Scene::set_environment(auto_release_ptr<Environment> environment)
{
    impl->m_environment = environment;
}

// Access the environment.
Environment* Scene::get_environment() const
{
    return impl->m_environment.get();
}

// Access the colors.
ColorContainer& Scene::colors() const
{
    return impl->m_colors;
}

// Access the textures.
TextureContainer& Scene::textures() const
{
    return impl->m_textures;
}

// Access the texture instances.
TextureInstanceContainer& Scene::texture_instances() const
{
    return impl->m_texture_instances;
}

// Access the environment EDFs.
EnvironmentEDFContainer& Scene::environment_edfs() const
{
    return impl->m_environment_edfs;
}

// Access the environment shaders.
EnvironmentShaderContainer& Scene::environment_shaders() const
{
    return impl->m_environment_shaders;
}

// Access the assemblies.
AssemblyContainer& Scene::assemblies() const
{
    return impl->m_assemblies;
}

// Access the assembly instances.
AssemblyInstanceContainer& Scene::assembly_instances() const
{
    return impl->m_assembly_instances;
}

}   // namespace renderer
