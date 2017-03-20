
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

// Interface header.
#include "project.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/projectformatrevision.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <memory>

using namespace foundation;
using namespace std;

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
    size_t                      m_format_revision;
    string                      m_path;
    auto_release_ptr<Scene>     m_scene;
    auto_release_ptr<Frame>     m_frame;
    auto_release_ptr<Display>   m_display;
    RenderLayerRuleContainer    m_render_layer_rules;
    ConfigurationContainer      m_configurations;
    SearchPaths                 m_search_paths;
    auto_ptr<TraceContext>      m_trace_context;

    Impl()
      : m_format_revision(ProjectFormatRevision)
      , m_search_paths("APPLESEED_SEARCHPATH", SearchPaths::environment_path_separator())
    {
    }
};

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

string Project::make_search_path_string() const
{
    return to_string(impl->m_search_paths.to_string_reversed(SearchPaths::osl_path_separator()));
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

void Project::set_display(foundation::auto_release_ptr<Display> display)
{
    impl->m_display = display;
}

Display* Project::get_display() const
{
    return impl->m_display.get();
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

    return 0;
}

void Project::add_render_layer_rule(foundation::auto_release_ptr<RenderLayerRule> rule)
{
    impl->m_render_layer_rules.insert(rule);
}

RenderLayerRuleContainer& Project::render_layer_rules() const
{
    return impl->m_render_layer_rules;
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

bool Project::has_trace_context() const
{
    return impl->m_trace_context.get() != 0;
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
