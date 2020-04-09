
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
#include "renderer/modeling/aov/aovtraits.h"
#include "renderer/modeling/bsdf/bsdftraits.h"
#include "renderer/modeling/bssrdf/bssrdftraits.h"
#include "renderer/modeling/camera/cameratraits.h"
#include "renderer/modeling/edf/edftraits.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entitytraits.h"
#include "renderer/modeling/environmentedf/environmentedftraits.h"
#include "renderer/modeling/environmentshader/environmentshadertraits.h"
#include "renderer/modeling/light/lighttraits.h"
#include "renderer/modeling/material/materialtraits.h"
#include "renderer/modeling/object/objecttraits.h"
#include "renderer/modeling/postprocessingstage/postprocessingstagetraits.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/renderingtimer.h"
#include "renderer/modeling/scene/assemblytraits.h"
#include "renderer/modeling/surfaceshader/surfaceshadertraits.h"
#include "renderer/modeling/texture/texturetraits.h"
#include "renderer/modeling/volume/volumetraits.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class AOV; }
namespace renderer      { class Assembly; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class BSDF; }
namespace renderer      { class BSSRDF; }
namespace renderer      { class Camera; }
namespace renderer      { class Display; }
namespace renderer      { class EDF; }
namespace renderer      { class EnvironmentEDF; }
namespace renderer      { class EnvironmentShader; }
namespace renderer      { class Frame; }
namespace renderer      { class Light; }
namespace renderer      { class LightPathRecorder; }
namespace renderer      { class Material; }
namespace renderer      { class Object; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class PluginStore; }
namespace renderer      { class PostProcessingStage; }
namespace renderer      { class Scene; }
namespace renderer      { class SurfaceShader; }
namespace renderer      { class Texture; }
namespace renderer      { class TextureStore; }
namespace renderer      { class TraceContext; }
namespace renderer      { class Volume; }

namespace renderer
{

//
// A rendering project.
//

class APPLESEED_DLLSYMBOL Project
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Set/get the format revision of the project.
    // The default value is renderer::ProjectFormatRevision (see projectformatrevision.h).
    void set_format_revision(const size_t format_revision);
    size_t get_format_revision() const;

    // Set/get the project path.
    bool has_path() const;
    void set_path(const char* path);
    const char* get_path() const;

    // Access the search paths.
    foundation::SearchPaths& search_paths() const;

    // Access the plugin store.
    PluginStore& get_plugin_store() const;

    // Return the factory registrar for a given entity type (e.g. `renderer::BSDF`).
    template <typename Entity>
    const typename EntityTraits<Entity>::FactoryRegistrarType& get_factory_registrar() const;

    // Set the scene, replacing the existing scene.
    void set_scene(foundation::auto_release_ptr<Scene> scene);

    // Access the scene.
    // Return nullptr if the project does not contain a scene.
    Scene* get_scene() const;

    // Access the camera specified in the frame as active.
    // Return nullptr if the scene does not contain cameras or if
    // no cameras are specified in the frame.
    Camera* get_uncached_active_camera() const;

    // Set the frame, replacing the existing frame.
    void set_frame(foundation::auto_release_ptr<Frame> frame);

    // Access the frame.
    // Return nullptr if the project does not contain a frame.
    Frame* get_frame() const;

    // Set the display, replacing the existing display.
    void set_display(foundation::auto_release_ptr<Display> display);

    // Access the display.
    // Return nullptr if the project does not contain a display.
    Display* get_display() const;

    // Access the configurations.
    ConfigurationContainer& configurations() const;

    // Add the default configurations to the project.
    void add_default_configurations();

    // Access the light path recorder.
    LightPathRecorder& get_light_path_recorder() const;

#ifdef APPLESEED_WITH_EMBREE
    // Set use Embree flag for trace context
    void set_use_embree(const bool value);
#endif

    // Return true if the trace context has already been built.
    bool has_trace_context() const;

    // Get the trace context.
    const TraceContext& get_trace_context() const;

    // Synchronize the trace context with the scene.
    void update_trace_context();

    // Access the timer used to track and measure frame rendering time.
    RenderingTimer& get_rendering_timer();
    const RenderingTimer& get_rendering_timer() const;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

    bool on_frame_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnFrameBeginRecorder&           recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    void on_frame_end(
        const Project&                  project,
        const BaseGroup*                parent) override;

    // Initialize the texture store.
    void initialize_texture_store(const ParamArray& params);

    // Return true if the texture store has already been initialized.
    bool has_texture_store() const;

    // Get the texture store.
    TextureStore& get_texture_store() const;

  private:
    friend class ProjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    explicit Project(const char* name);

    // Destructor.
    ~Project() override;

    void add_base_configurations();
    void add_default_configuration(const char* name, const char* base_name);
    void register_plugin_handlers();
};

template <> APPLESEED_DLLSYMBOL const EntityTraits<AOV>::FactoryRegistrarType& Project::get_factory_registrar<AOV>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Assembly>::FactoryRegistrarType& Project::get_factory_registrar<Assembly>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<BSDF>::FactoryRegistrarType& Project::get_factory_registrar<BSDF>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<BSSRDF>::FactoryRegistrarType& Project::get_factory_registrar<BSSRDF>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Camera>::FactoryRegistrarType& Project::get_factory_registrar<Camera>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<EDF>::FactoryRegistrarType& Project::get_factory_registrar<EDF>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<EnvironmentEDF>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentEDF>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<EnvironmentShader>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentShader>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Light>::FactoryRegistrarType& Project::get_factory_registrar<Light>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Material>::FactoryRegistrarType& Project::get_factory_registrar<Material>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Object>::FactoryRegistrarType& Project::get_factory_registrar<Object>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<PostProcessingStage>::FactoryRegistrarType& Project::get_factory_registrar<PostProcessingStage>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<SurfaceShader>::FactoryRegistrarType& Project::get_factory_registrar<SurfaceShader>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Texture>::FactoryRegistrarType& Project::get_factory_registrar<Texture>() const;
template <> APPLESEED_DLLSYMBOL const EntityTraits<Volume>::FactoryRegistrarType& Project::get_factory_registrar<Volume>() const;


//
// Project factory.
//

class APPLESEED_DLLSYMBOL ProjectFactory
{
  public:
    // Create a new project.
    static foundation::auto_release_ptr<Project> create(const char* name);
};

}   // namespace renderer
