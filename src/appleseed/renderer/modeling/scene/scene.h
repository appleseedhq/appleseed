
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace renderer      { class Camera; }
#ifdef APPLESEED_WITH_EMBREE
namespace renderer      { class EmbreeDevice; }
#endif
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
namespace renderer      { class Project; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
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
    void release() override;

    // Access the cameras.
    CameraContainer& cameras() const;

    // Set the environment.
    void set_environment(foundation::auto_release_ptr<Environment> environment);

    // Access the environment.
    // Return nullptr if the environment does not exist.
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

    // Return true if the scene contains participating media.
    bool has_participating_media() const;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

    // Expand all procedural assemblies in the scene.
    bool expand_procedural_assemblies(
        const Project&              project,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    bool on_render_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_render_end(
        const Project&              project,
        const BaseGroup*            parent) override;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

#ifdef APPLESEED_WITH_EMBREE

    const EmbreeDevice& get_embree_device() const;

#endif

    struct RenderData
    {
        GAABB3      m_bbox;
        GVector3    m_center;
        GScalar     m_radius;
        GScalar     m_diameter;
        GScalar     m_safe_diameter;
        Camera*     m_active_camera;

        // Cannot add methods to this class without first DLL-exporting GAABB3, GVector3, etc.
    };

    // Return render-time data of this entity.
    // Render-time data are available between on_render_begin() and on_render_end() calls.
    const RenderData& get_render_data() const;

  private:
    friend class SceneFactory;

    struct Impl;
    Impl* impl;

    RenderData m_render_data;

    // Constructor. Initially, the scene is empty.
    Scene();

    // Destructor.
    ~Scene() override;

    void clear_render_data();
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
    return m_render_data;
}

}   // namespace renderer
