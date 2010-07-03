
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
#include "project.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/scene/scene.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Project class implementation.
//

struct Project::Impl
{
    string                  m_name;
    string                  m_path;
    auto_ptr<Scene>         m_scene;
    ConfigurationContainer  m_configurations;
    auto_ptr<Frame>         m_frame;
    auto_ptr<TraceContext>  m_trace_context;
};

// Constructor.
Project::Project(const char* name)
  : impl(new Impl())
{
    assert(name);

    impl->m_name = name;

    add_base_configurations();
}

// Destructor.
Project::~Project()
{
    delete impl;
}

// Delete this instance.
void Project::release()
{
    delete this;
}

// Return the name of this project.
const char* Project::get_name() const
{
    return impl->m_name.c_str();
}

// Set or get the project path.
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

// Set the scene, replacing the existing scene.
void Project::set_scene(auto_ptr<Scene> scene)
{
    impl->m_scene = scene;
}

// Access the scene.
Scene* Project::get_scene() const
{
    return impl->m_scene.get();
}

// Set the frame, replacing the existing frame.
void Project::set_frame(auto_ptr<Frame> frame)
{
    impl->m_frame = frame;
}

// Access the frame.
Frame* Project::get_frame() const
{
    return impl->m_frame.get();
}

// Access the configurations.
ConfigurationContainer& Project::configurations()
{
    return impl->m_configurations;
}
const ConfigurationContainer& Project::configurations() const
{
    return impl->m_configurations;
}

// Add the default configurations to the project.
void Project::add_default_configurations()
{
    add_default_configuration("final", "base_final");
    add_default_configuration("interactive", "base_interactive");
}

// Get the trace context.
const TraceContext& Project::get_trace_context()
{
    // Create the trace context if it doesn't exist yet.
    if (impl->m_trace_context.get() == 0)
    {
        // Create a fresh trace context.
        assert(impl->m_scene.get());
        impl->m_trace_context.reset(new TraceContext(*impl->m_scene));
    }

    return *impl->m_trace_context;
}

void Project::add_base_configurations()
{
    impl->m_configurations.insert(BaseConfigurationFactory::create_base_final());
    impl->m_configurations.insert(BaseConfigurationFactory::create_base_interactive());
}

void Project::add_default_configuration(const char* name, const char* base_name)
{
    Configuration* base_configuration = impl->m_configurations.get(base_name);
    assert(base_configuration);

    auto_release_ptr<Configuration> configuration = ConfigurationFactory::create(name);
    configuration->set_base(base_configuration);

    impl->m_configurations.insert(configuration);
}


//
// ProjectFactory class implementation.
//

// Create a new project.
auto_release_ptr<Project> ProjectFactory::create(const char* name)
{
    return auto_release_ptr<Project>(new Project(name));
}

}   // namespace renderer
