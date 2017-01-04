
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

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class OnFrameBeginRecorder; }
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

    // Access the active camera.
    // Return 0 if the camera does not exist.
    Camera* get_active_camera() const;

    // Access the cameras.
    CameraContainer& cameras() const;

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

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const APPLESEED_OVERRIDE;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings) APPLESEED_OVERRIDE;

    // Perform pre-render rendering actions.
    // Returns true on success, false otherwise.
    bool on_render_begin(
        const Project&              project,
        foundation::IAbortSwitch*   abort_switch = 0);

    // Perform post-render rendering actions.
    void on_render_end(const Project& project);

    // Expand all procedural assemblies in the scene.
    virtual bool expand_procedural_assemblies(
        const Project&              project,
        foundation::IAbortSwitch*   abort_switch = 0);

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = 0) APPLESEED_OVERRIDE;

    // This method is called once after rendering each frame (only if on_frame_begin() was called).
    virtual void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) APPLESEED_OVERRIDE;

    struct RenderData
    {
        GAABB3      m_bbox;
        GVector3    m_center;
        GScalar     m_radius;
        GScalar     m_diameter;
        GScalar     m_safe_diameter;
    };

    // Return render-time data of this entity.
    // Render-time data are available between on_frame_begin() and on_frame_end() calls.
    const RenderData& get_render_data() const;

  private:
    friend class SceneFactory;

    struct Impl;
    Impl* impl;

    Camera*     m_camera;
    bool        m_has_render_data;
    RenderData  m_render_data;

    // Constructor. Initially, the scene is empty.
    Scene();

    // Destructor.
    ~Scene();

    // Create render data, which are then available through get_render_data().
    void create_render_data();
};


//
// Scene factory.
//

class APPLESEED_DLLSYMBOL SceneFactory
{
  public:
    // Create a new scene.
    static foundation::auto_release_ptr<Scene> create();
};


//
// Scene class implementation.
//

inline const Scene::RenderData& Scene::get_render_data() const
{
    assert(m_has_render_data);
    return m_render_data;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_SCENE_H
