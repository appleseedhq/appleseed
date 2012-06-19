
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
#include "project.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/modeling/aov/aovcollection.h"
#include "renderer/modeling/aov/aovimagecollection.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <map>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Project class implementation.
//

struct Project::Impl
{
    string                          m_path;
    auto_release_ptr<Scene>         m_scene;
    auto_release_ptr<Frame>         m_frame;
    ConfigurationContainer          m_configurations;
    SearchPaths                     m_search_paths;
    auto_ptr<TraceContext>          m_trace_context;
};

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Project::Project(const char* name)
  : Entity(g_class_uid)
  , impl(new Impl())
{
    set_name(name);
    add_base_configurations();
}

Project::~Project()
{
    delete impl;
}

void Project::release()
{
    delete this;
}

bool Project::has_path() const
{
    return impl->m_path.size() > 0;
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

void Project::set_scene(auto_release_ptr<Scene> scene)
{
    impl->m_scene = scene;
}

Scene* Project::get_scene() const
{
    return impl->m_scene.get();
}

void Project::set_frame(auto_release_ptr<Frame> frame)
{
    impl->m_frame = frame;
}

Frame* Project::get_frame() const
{
    return impl->m_frame.get();
}

namespace
{
    typedef map<string, size_t> RenderLayerMapping;

    void assign_entity_to_render_layer(
        AOVImageCollection&     aov_images,
        RenderLayerMapping&     mapping,
        const PixelFormat       format,
        Entity&                 entity)
    {
        const string render_layer_name = entity.get_render_layer_name();

        if (render_layer_name.empty())
        {
            entity.set_render_layer_index(~0);
            return;
        }

        const RenderLayerMapping::const_iterator i = mapping.find(render_layer_name);

        if (i != mapping.end())
        {
            entity.set_render_layer_index(i->second);
            return;
        }

        assert(mapping.size() <= AOVCollection::MaxSize);

        if (mapping.size() == AOVCollection::MaxSize)
        {
            RENDERER_LOG_ERROR(
                "while assigning entity \"%s\" to render layer \"%s\": "
                "could not create render layer, maximum number of AOVs (" FMT_SIZE_T ") reached.",
                entity.get_name(),
                render_layer_name.c_str(),
                AOVCollection::MaxSize);
            entity.set_render_layer_index(~0);
            return;
        }

        const size_t aov_image_index = aov_images.size();
        aov_images.append(render_layer_name.c_str(), format);
        mapping[render_layer_name] = aov_image_index;

        entity.set_render_layer_index(aov_image_index);
    }

    template <typename EntityCollection>
    void assign_entities_to_render_layers(
        AOVImageCollection&     aov_images,
        RenderLayerMapping&     mapping,
        const PixelFormat       format,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            assign_entity_to_render_layer(aov_images, mapping, format, *i);
    }

    void assign_entities_to_render_layers(
        AOVImageCollection&     aov_images,
        const PixelFormat       format,
        const Scene&            scene)
    {
        RenderLayerMapping mapping;

        assign_entities_to_render_layers(aov_images, mapping, format, scene.assemblies());
        assign_entities_to_render_layers(aov_images, mapping, format, scene.assembly_instances());

        for (const_each<AssemblyContainer> i = scene.assemblies(); i; ++i)
        {
            assign_entities_to_render_layers(aov_images, mapping, format, i->edfs());
            assign_entities_to_render_layers(aov_images, mapping, format, i->lights());
            assign_entities_to_render_layers(aov_images, mapping, format, i->objects());
            assign_entities_to_render_layers(aov_images, mapping, format, i->object_instances());
        }

        EnvironmentEDF* env_edf = scene.get_environment()->get_environment_edf();

        if (env_edf)
            assign_entity_to_render_layer(aov_images, mapping, format, *env_edf);
    }
}

void Project::create_aov_images()
{
    assert(impl->m_frame.get());

    const PixelFormat format =
        impl->m_frame->image().properties().m_pixel_format;

    AOVImageCollection& aov_images = impl->m_frame->aov_images();

    aov_images.clear();

    assign_entities_to_render_layers(
        aov_images,
        format,
        impl->m_scene.ref());
}

ConfigurationContainer& Project::configurations()
{
    return impl->m_configurations;
}

const ConfigurationContainer& Project::configurations() const
{
    return impl->m_configurations;
}

void Project::add_default_configurations()
{
    add_default_configuration("final", "base_final");
    add_default_configuration("interactive", "base_interactive");
}

SearchPaths& Project::get_search_paths()
{
    return impl->m_search_paths;
}

const SearchPaths& Project::get_search_paths() const
{
    return impl->m_search_paths;
}

const TraceContext& Project::get_trace_context() const
{
    if (impl->m_trace_context.get() == 0)
    {
        assert(impl->m_scene.get());
        impl->m_trace_context.reset(new TraceContext(*impl->m_scene));
    }

    return *impl->m_trace_context;
}

void Project::update_trace_context()
{
    if (impl->m_trace_context.get())
        impl->m_trace_context->update();
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


//
// ProjectFactory class implementation.
//

auto_release_ptr<Project> ProjectFactory::create(const char* name)
{
    return auto_release_ptr<Project>(new Project(name));
}

}   // namespace renderer
