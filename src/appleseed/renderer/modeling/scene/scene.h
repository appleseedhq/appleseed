
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_SCENE_H
#define APPLESEED_RENDERER_MODELING_SCENE_SCENE_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/utility/version.h"

// Forward declarations.
namespace renderer  { class Camera; }
namespace renderer  { class Environment; }

namespace renderer
{

//
// A scene. Self-contained.
//

class RENDERERDLL Scene
  : public foundation::IIdentifiable
{
  public:
    // Constructor. Initially, the scene is empty.
    Scene();

    // Destructor.
    ~Scene();

    // Return the unique ID of this object.
    virtual foundation::UniqueID get_uid() const;

    // Return/increase the version ID of the assembly instances.
    foundation::VersionID get_assembly_instances_version_id() const;
    void increase_assembly_instances_version_id();

    // Return/increase the version ID of the scene geometry.
    foundation::VersionID get_geometry_version_id() const;
    void increase_geometry_version_id();

    // Set the camera.
    void set_camera(foundation::auto_release_ptr<Camera> camera);

    // Access the camera.
    // Return 0 if the camera does not exist.
    Camera* get_camera() const;

    // Set the environment.
    void set_environment(foundation::auto_release_ptr<Environment> environment);

    // Access the environment.
    // Return 0 if the environment does not exist.
    Environment* get_environment() const;

    // Access the colors.
    ColorContainer& colors() const;

    // Access the textures.
    TextureContainer& textures() const;

    // Access the texture instances.
    TextureInstanceContainer& texture_instances() const;

    // Access the environment EDFs.
    EnvironmentEDFContainer& environment_edfs() const;

    // Access the environment shaders.
    EnvironmentShaderContainer& environment_shaders() const;

    // Access the assemblies.
    AssemblyContainer& assemblies() const;

    // Access the assembly instances.
    AssemblyInstanceContainer& assembly_instances() const;

  private:
    // Private implementation.
    struct Impl;
    Impl* impl;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_SCENE_H
