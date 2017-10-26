
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H
#define APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H

// appleseed.renderer headers.
#include "renderer/modeling/aov/aovfactoryregistrar.h"
#include "renderer/modeling/aov/aovtraits.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdftraits.h"
#include "renderer/modeling/bssrdf/bssrdffactoryregistrar.h"
#include "renderer/modeling/bssrdf/bssrdftraits.h"
#include "renderer/modeling/camera/camerafactoryregistrar.h"
#include "renderer/modeling/camera/cameratraits.h"
#include "renderer/modeling/edf/edffactoryregistrar.h"
#include "renderer/modeling/edf/edftraits.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entitytraits.h"
#include "renderer/modeling/environmentedf/environmentedffactoryregistrar.h"
#include "renderer/modeling/environmentedf/environmentedftraits.h"
#include "renderer/modeling/environmentshader/environmentshaderfactoryregistrar.h"
#include "renderer/modeling/environmentshader/environmentshadertraits.h"
#include "renderer/modeling/light/lightfactoryregistrar.h"
#include "renderer/modeling/light/lighttraits.h"
#include "renderer/modeling/material/materialfactoryregistrar.h"
#include "renderer/modeling/material/materialtraits.h"
#include "renderer/modeling/object/objectfactoryregistrar.h"
#include "renderer/modeling/object/objecttraits.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/scene/assemblyfactoryregistrar.h"
#include "renderer/modeling/scene/assemblytraits.h"
#include "renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.h"
#include "renderer/modeling/surfaceshader/surfaceshadertraits.h"
#include "renderer/modeling/texture/texturefactoryregistrar.h"
#include "renderer/modeling/texture/texturetraits.h"
#include "renderer/modeling/volume/volumefactoryregistrar.h"
#include "renderer/modeling/volume/volumetraits.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSDF; }
namespace renderer      { class BSSRDF; }
namespace renderer      { class Camera; }
namespace renderer      { class Display; }
namespace renderer      { class EDF; }
namespace renderer      { class EnvironmentEDF; }
namespace renderer      { class EnvironmentShader; }
namespace renderer      { class Frame; }
namespace renderer      { class Light; }
namespace renderer      { class Material; }
namespace renderer      { class Object; }
namespace renderer      { class Scene; }
namespace renderer      { class SurfaceShader; }
namespace renderer      { class Texture; }
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

    // Set the scene, replacing the existing scene.
    void set_scene(foundation::auto_release_ptr<Scene> scene);

    // Access the scene.
    // Return 0 if the project does not contain a scene.
    Scene* get_scene() const;

    // Set the frame, replacing the existing frame.
    void set_frame(foundation::auto_release_ptr<Frame> frame);

    // Access the frame.
    // Return 0 if the project does not contain a frame.
    Frame* get_frame() const;

    // Set the display, replacing the existing display.
    void set_display(foundation::auto_release_ptr<Display> display);

    // Access the display.
    // Return 0 if the project does not contain a display.
    Display* get_display() const;

    // Access the camera specified in the frame as active.
    // Return 0 if the scene does not contain cameras or if
    // no cameras are specified in the frame.
    Camera* get_uncached_active_camera() const;

    // Access the configurations.
    ConfigurationContainer& configurations() const;

    // Add the default configurations to the project.
    void add_default_configurations();

    // Return the factory registrar for a given entity type (e.g. `renderer::BSDF`).
    template <typename Entity>
    const typename EntityTraits<Entity>::FactoryRegistrarType& get_factory_registrar() const;

    // Reinitialize all factory registrars; load plugins found in project's search paths.
    void reinitialize_factory_registrars();

    // Return true if the trace context has already been built.
    bool has_trace_context() const;

    // Get the trace context.
    const TraceContext& get_trace_context() const;

    // Synchronize the trace context with the scene.
    void update_trace_context();

  private:
    friend class ProjectFactory;

    // Must be declared first.
    struct Impl;
    Impl* impl;

    AOVFactoryRegistrar                 m_aov_factory_registrar;
    AssemblyFactoryRegistrar            m_assembly_factory_registrar;
    BSDFFactoryRegistrar                m_bsdf_factory_registrar;
    BSSRDFFactoryRegistrar              m_bssrdf_factory_registrar;
    CameraFactoryRegistrar              m_camera_factory_registrar;
    EDFFactoryRegistrar                 m_edf_factory_registrar;
    EnvironmentEDFFactoryRegistrar      m_environment_edf_factory_registrar;
    EnvironmentShaderFactoryRegistrar   m_environment_shader_factory_registrar;
    LightFactoryRegistrar               m_light_factory_registrar;
    MaterialFactoryRegistrar            m_material_factory_registrar;
    ObjectFactoryRegistrar              m_object_factory_registrar;
    SurfaceShaderFactoryRegistrar       m_surface_shader_factory_registrar;
    TextureFactoryRegistrar             m_texture_factory_registrar;
    VolumeFactoryRegistrar              m_volume_factory_registrar;

    // Constructor.
    explicit Project(const char* name);

    // Destructor.
    ~Project() override;

    void add_base_configurations();
    void add_default_configuration(const char* name, const char* base_name);
};


//
// Project factory.
//

class APPLESEED_DLLSYMBOL ProjectFactory
{
  public:
    // Create a new project.
    static foundation::auto_release_ptr<Project> create(const char* name);
};


//
// Project class implementation.
//

template <>
inline const EntityTraits<AOV>::FactoryRegistrarType& Project::get_factory_registrar<AOV>() const
{
    return m_aov_factory_registrar;
}

template <>
inline const EntityTraits<Assembly>::FactoryRegistrarType& Project::get_factory_registrar<Assembly>() const
{
    return m_assembly_factory_registrar;
}

template <>
inline const EntityTraits<BSDF>::FactoryRegistrarType& Project::get_factory_registrar<BSDF>() const
{
    return m_bsdf_factory_registrar;
}

template <>
inline const EntityTraits<BSSRDF>::FactoryRegistrarType& Project::get_factory_registrar<BSSRDF>() const
{
    return m_bssrdf_factory_registrar;
}

template <>
inline const EntityTraits<Camera>::FactoryRegistrarType& Project::get_factory_registrar<Camera>() const
{
    return m_camera_factory_registrar;
}

template <>
inline const EntityTraits<EDF>::FactoryRegistrarType& Project::get_factory_registrar<EDF>() const
{
    return m_edf_factory_registrar;
}

template <>
inline const EntityTraits<EnvironmentEDF>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentEDF>() const
{
    return m_environment_edf_factory_registrar;
}

template <>
inline const EntityTraits<EnvironmentShader>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentShader>() const
{
    return m_environment_shader_factory_registrar;
}

template <>
inline const EntityTraits<Light>::FactoryRegistrarType& Project::get_factory_registrar<Light>() const
{
    return m_light_factory_registrar;
}

template <>
inline const EntityTraits<Material>::FactoryRegistrarType& Project::get_factory_registrar<Material>() const
{
    return m_material_factory_registrar;
}

template <>
inline const EntityTraits<Object>::FactoryRegistrarType& Project::get_factory_registrar<Object>() const
{
    return m_object_factory_registrar;
}

template <>
inline const EntityTraits<SurfaceShader>::FactoryRegistrarType& Project::get_factory_registrar<SurfaceShader>() const
{
    return m_surface_shader_factory_registrar;
}

template <>
inline const EntityTraits<Texture>::FactoryRegistrarType& Project::get_factory_registrar<Texture>() const
{
    return m_texture_factory_registrar;
}

template <>
inline const EntityTraits<Volume>::FactoryRegistrarType& Project::get_factory_registrar<Volume>() const
{
    return m_volume_factory_registrar;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_PROJECT_H
