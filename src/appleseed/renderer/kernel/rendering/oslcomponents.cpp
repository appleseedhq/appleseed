
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// Interface header.
#include "oslcomponents.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/utility/job/iabortswitch.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// OSLComponents class implementation.
//

OSLComponents::OSLComponents(
    const Project&          project,
    TextureStore&           texture_store,
    OIIO::TextureSystem&    texture_system)
  : m_project(project)
  , m_texture_store(texture_store)
  , m_texture_system(texture_system)
{
#ifndef NDEBUG
    // While debugging, we want all possible outputs.
    m_error_handler.verbosity(OIIO::ErrorHandler::VERBOSE);
#endif

    m_renderer_services.reset(
        new RendererServices(
            m_project,
            m_texture_system,
            m_texture_store));

    m_shading_system =
        OSL::ShadingSystem::create(
            m_renderer_services.get(),
            &m_texture_system,
            &m_error_handler);

    const string search_paths = project.make_search_path_string();

    if (!search_paths.empty())
        m_shading_system->attribute("searchpath:shader", search_paths);

    m_shading_system->attribute("lockgeom", 1);
    m_shading_system->attribute("colorspace", "Linear");
    m_shading_system->attribute("commonspace", "world");
    m_shading_system->attribute("statistics:level", 1);

    m_shading_system->attribute(
        "raytypes",
        OSL::TypeDesc(
            OSL::TypeDesc::STRING,
            static_cast<int>(VisibilityFlags::Count)),
        VisibilityFlags::Names);

#ifndef NDEBUG
    // While debugging, we want all possible outputs.
    m_shading_system->attribute("debug", 1);
    m_shading_system->attribute("compile_report", 1);
    m_shading_system->attribute("countlayerexecs", 1);
    m_shading_system->attribute("clearmemory", 1);
#endif

    // Register appleseed's closures into OSL's shading system.
    register_closures(*m_shading_system);
}

OSLComponents::~OSLComponents()
{
    RENDERER_LOG_DEBUG("releasing osl shader groups...");
    m_project.get_scene()->release_osl_shader_groups();

    OSL::ShadingSystem::destroy(m_shading_system);
}

RendererServices& OSLComponents::get_renderer_services()
{
    return *m_renderer_services.get();
}

OSL::ShadingSystem& OSLComponents::get_shading_system()
{
    return *m_shading_system;
}

bool OSLComponents::compile_osl_shaders(IAbortSwitch* abort_switch)
{
    return
        m_project.get_scene()->create_osl_shader_groups(
            *m_shading_system,
            abort_switch);
}

}   // namespace renderer
