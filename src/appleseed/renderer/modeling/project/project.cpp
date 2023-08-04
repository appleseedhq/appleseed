
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

// Interface header.
#include "project.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/lighting/lightpathrecorder.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/aov/aovfactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bssrdf/bssrdffactoryregistrar.h"
#include "renderer/modeling/camera/camerafactoryregistrar.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/edf/edffactoryregistrar.h"
#include "renderer/modeling/environmentedf/environmentedffactoryregistrar.h"
#include "renderer/modeling/environmentshader/environmentshaderfactoryregistrar.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/lightfactoryregistrar.h"
#include "renderer/modeling/material/materialfactoryregistrar.h"
#include "renderer/modeling/object/objectfactoryregistrar.h"
#include "renderer/modeling/postprocessingstage/postprocessingstagefactoryregistrar.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/projectformatrevision.h"
#include "renderer/modeling/scene/assemblyfactoryregistrar.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.h"
#include "renderer/modeling/texture/texturefactoryregistrar.h"
#include "renderer/modeling/volume/volumefactoryregistrar.h"
#include "renderer/utility/pluginstore.h"

// appleseed.foundation headers.
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

//
// Project class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Project::get_class_uid()
{
    return g_class_uid;
}

struct Project::Impl
{
    // Project metadata.
    size_t                              m_format_revision;
    std::string                         m_path;
    SearchPaths                         m_search_paths;

    // The plugin store must live at least as long as the factory registrars
    // and the scene entities since some factory registrars and some entities
    // may be defined by plugins. It thus must be constructed first in order
    // to be destroyed last.
    PluginStore                         m_plugin_store;

    // Entity factory registrars.
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
    PostProcessingStageFactoryRegistrar m_post_processing_stage_factory_registrar;
    SurfaceShaderFactoryRegistrar       m_surface_shader_factory_registrar;
    TextureFactoryRegistrar             m_texture_factory_registrar;
    VolumeFactoryRegistrar              m_volume_factory_registrar;

    // Scene description.
    auto_release_ptr<Scene>             m_scene;
    auto_release_ptr<Frame>             m_frame;
    auto_release_ptr<Display>           m_display;
    ConfigurationContainer              m_configurations;

    // Project-specific components.
    LightPathRecorder                   m_light_path_recorder;
    std::unique_ptr<TraceContext>       m_trace_context;
    RenderingTimer                      m_rendering_timer;
    std::unique_ptr<TextureStore>       m_texture_store;

    explicit Impl(const Project& project)
      : m_format_revision(ProjectFormatRevision)
      , m_search_paths("APPLESEED_SEARCHPATH", SearchPaths::environment_path_separator())
      , m_light_path_recorder(project)
    {
    }
};

Project::Project(const char* name)
  : Entity(g_class_uid)
  , impl(new Impl(*this))
{
    set_name(name);
    add_base_configurations();
    register_plugin_handlers();
}

Project::~Project()
{
    delete impl;
}

void Project::release()
{
    delete this;
}

void Project::set_format_revision(const size_t format_revision)
{
    impl->m_format_revision = format_revision;
}

size_t Project::get_format_revision() const
{
    return impl->m_format_revision;
}

bool Project::has_path() const
{
    return !impl->m_path.empty();
}

void Project::set_path(const char* path)
{
    assert(path);
    impl->m_path = path;
}

const char* Project::get_path() const
{
    return impl->m_path.c_str();
}

SearchPaths& Project::search_paths() const
{
    return impl->m_search_paths;
}

PluginStore& Project::get_plugin_store() const
{
    return impl->m_plugin_store;
}

template <>
const EntityTraits<AOV>::FactoryRegistrarType& Project::get_factory_registrar<AOV>() const
{
    return impl->m_aov_factory_registrar;
}

template <>
const EntityTraits<Assembly>::FactoryRegistrarType& Project::get_factory_registrar<Assembly>() const
{
    return impl->m_assembly_factory_registrar;
}

template <>
const EntityTraits<BSDF>::FactoryRegistrarType& Project::get_factory_registrar<BSDF>() const
{
    return impl->m_bsdf_factory_registrar;
}

template <>
const EntityTraits<BSSRDF>::FactoryRegistrarType& Project::get_factory_registrar<BSSRDF>() const
{
    return impl->m_bssrdf_factory_registrar;
}

template <>
const EntityTraits<Camera>::FactoryRegistrarType& Project::get_factory_registrar<Camera>() const
{
    return impl->m_camera_factory_registrar;
}

template <>
const EntityTraits<EDF>::FactoryRegistrarType& Project::get_factory_registrar<EDF>() const
{
    return impl->m_edf_factory_registrar;
}

template <>
const EntityTraits<EnvironmentEDF>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentEDF>() const
{
    return impl->m_environment_edf_factory_registrar;
}

template <>
const EntityTraits<EnvironmentShader>::FactoryRegistrarType& Project::get_factory_registrar<EnvironmentShader>() const
{
    return impl->m_environment_shader_factory_registrar;
}

template <>
const EntityTraits<Light>::FactoryRegistrarType& Project::get_factory_registrar<Light>() const
{
    return impl->m_light_factory_registrar;
}

template <>
const EntityTraits<Material>::FactoryRegistrarType& Project::get_factory_registrar<Material>() const
{
    return impl->m_material_factory_registrar;
}

template <>
const EntityTraits<Object>::FactoryRegistrarType& Project::get_factory_registrar<Object>() const
{
    return impl->m_object_factory_registrar;
}

template <>
const EntityTraits<PostProcessingStage>::FactoryRegistrarType& Project::get_factory_registrar<PostProcessingStage>() const
{
    return impl->m_post_processing_stage_factory_registrar;
}

template <>
const EntityTraits<SurfaceShader>::FactoryRegistrarType& Project::get_factory_registrar<SurfaceShader>() const
{
    return impl->m_surface_shader_factory_registrar;
}

template <>
const EntityTraits<Texture>::FactoryRegistrarType& Project::get_factory_registrar<Texture>() const
{
    return impl->m_texture_factory_registrar;
}

template <>
const EntityTraits<Volume>::FactoryRegistrarType& Project::get_factory_registrar<Volume>() const
{
    return impl->m_volume_factory_registrar;
}

void Project::set_scene(auto_release_ptr<Scene> scene)
{
    impl->m_scene = scene;
}

Scene* Project::get_scene() const
{
    return impl->m_scene.get();
}

Camera* Project::get_uncached_active_camera() const
{
    if (const Scene* scene = get_scene())
    {
        if (const Frame* frame = get_frame())
        {
            if (const char* camera_name = frame->get_active_camera_name())
                return scene->cameras().get_by_name(camera_name);
        }
    }

    return nullptr;
}

void Project::set_frame(auto_release_ptr<Frame> frame)
{
    impl->m_frame = frame;
}

Frame* Project::get_frame() const
{
    return impl->m_frame.get();
}

void Project::set_display(auto_release_ptr<Display> display)
{
    impl->m_display = display;
}

Display* Project::get_display() const
{
    return impl->m_display.get();
}

ConfigurationContainer& Project::configurations() const
{
    return impl->m_configurations;
}

void Project::add_default_configurations()
{
    add_default_configuration("final", "base_final");
    add_default_configuration("interactive", "base_interactive");
}

LightPathRecorder& Project::get_light_path_recorder() const
{
    return impl->m_light_path_recorder;
}

#ifdef APPLESEED_WITH_EMBREE

void Project::set_use_embree(const bool value)
{
    if (impl->m_trace_context)
        impl->m_trace_context->set_use_embree(value);
}

#endif

bool Project::has_trace_context() const
{
    return impl->m_trace_context.get() != nullptr;
}

const TraceContext& Project::get_trace_context() const
{
    if (!impl->m_trace_context)
        impl->m_trace_context.reset(new TraceContext(*this));

    return *impl->m_trace_context;
}

void Project::update_trace_context()
{
    if (impl->m_trace_context)
        impl->m_trace_context->update();
}

RenderingTimer& Project::get_rendering_timer()
{
    return impl->m_rendering_timer;
}

const RenderingTimer& Project::get_rendering_timer() const
{
    return impl->m_rendering_timer;
}

void Project::collect_asset_paths(StringArray& paths) const
{
    if (impl->m_scene)
        impl->m_scene->collect_asset_paths(paths);

    if (impl->m_frame)
        impl->m_frame->collect_asset_paths(paths);

    for (const Configuration& config : configurations())
        config.collect_asset_paths(paths);
}

void Project::update_asset_paths(const StringDictionary& mappings)
{
    if (impl->m_scene)
        impl->m_scene->update_asset_paths(mappings);

    if (impl->m_frame)
        impl->m_frame->update_asset_paths(mappings);

    for (Configuration& config : configurations())
        config.update_asset_paths(mappings);
}

bool Project::on_frame_begin(
    const Project&              project,
    const BaseGroup*            parent,
    OnFrameBeginRecorder&       recorder,
    IAbortSwitch*               abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    impl->m_rendering_timer.start();

    assert(impl->m_scene);
    assert(impl->m_frame);

    if (!impl->m_scene->on_frame_begin(project, nullptr, recorder, abort_switch))
        return false;

    if (!impl->m_frame->on_frame_begin(project, nullptr, recorder, abort_switch))
        return false;

    return true;
}

void Project::on_frame_end(
    const Project&              project,
    const BaseGroup*            parent)
{
    impl->m_rendering_timer.measure();
}

void Project::initialize_texture_store(const ParamArray& params)
{
    assert(impl->m_scene.get());

    impl->m_texture_store.reset(new TextureStore(*impl->m_scene, params));
}

bool Project::has_texture_store() const
{
    return impl->m_texture_store.get() != nullptr;
}

TextureStore& Project::get_texture_store() const
{
    assert(impl->m_texture_store);

    return *impl->m_texture_store;
}

void Project::add_base_configurations()
{
    impl->m_configurations.insert(BaseConfigurationFactory::create_base_final());
    impl->m_configurations.insert(BaseConfigurationFactory::create_base_interactive());
}

void Project::add_default_configuration(const char* name, const char* base_name)
{
    Configuration* base_configuration = impl->m_configurations.get_by_name(base_name);
    assert(base_configuration);

    auto_release_ptr<Configuration> configuration = ConfigurationFactory::create(name);
    configuration->set_base(base_configuration);

    impl->m_configurations.insert(configuration);
}

namespace
{
    template <typename EntityFactoryRegistrarType>
    void register_plugin_handler(PluginStore& plugin_store, EntityFactoryRegistrarType& registrar)
    {
        typedef typename EntityFactoryRegistrarType::EntityType EntityType;

        const std::string entry_point_name =
            format("appleseed_create_{0}_factory", EntityTraits<EntityType>::get_entity_type_name());

        plugin_store.register_plugin_handler(
            entry_point_name.c_str(),
            [&registrar](Plugin* plugin, void* plugin_entry_point)
            {
                registrar.register_factory_plugin(plugin, plugin_entry_point);
            });
    }
}

void Project::register_plugin_handlers()
{
    register_plugin_handler(impl->m_plugin_store, impl->m_aov_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_assembly_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_bsdf_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_bssrdf_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_camera_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_edf_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_environment_edf_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_environment_shader_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_light_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_material_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_object_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_surface_shader_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_texture_factory_registrar);
    register_plugin_handler(impl->m_plugin_store, impl->m_volume_factory_registrar);
}


//
// ProjectFactory class implementation.
//

auto_release_ptr<Project> ProjectFactory::create(const char* name)
{
    return auto_release_ptr<Project>(new Project(name));
}

}   // namespace renderer
