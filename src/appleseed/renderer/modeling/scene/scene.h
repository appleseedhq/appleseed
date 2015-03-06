
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_SCENE_H
#define APPLESEED_RENDERER_MODELING_SCENE_SCENE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }
namespace renderer      { class SurfaceShader; }

namespace renderer
{

//
// A scene. Self-contained.
//

class APPLESEED_DLLSYMBOL Scene
  : public Entity
  , public BaseGroup
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

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

    // Access the environment EDFs.
    EnvironmentEDFContainer& environment_edfs() const;

    // Access the environment shaders.
    EnvironmentShaderContainer& environment_shaders() const;

    // Return the surface shader that is used by materials without one.
    SurfaceShader* get_default_surface_shader() const;

    // Compute and return the bounding box of the scene.
    GAABB3 compute_bbox() const;

    // Return true if at least one of the object instance references a material with an alpha map set.
    bool uses_alpha_mapping() const;

    // Perform pre-frame rendering actions.
    // Returns true on success, false otherwise.
    bool on_frame_begin(
        const Project&              project,
        foundation::IAbortSwitch*   abort_switch = 0);

    // Perform post-frame rendering actions.
    void on_frame_end(const Project& project);

  private:
    friend class SceneFactory;

    struct Impl;
    Impl* impl;

    // Constructor. Initially, the scene is empty.
    Scene();

    // Destructor.
    ~Scene();
};


//
// SceneFactory.
//

class APPLESEED_DLLSYMBOL SceneFactory
{
  public:
    // Create a new scene.
    static foundation::auto_release_ptr<Scene> create();
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_SCENE_H
