
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "baserenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#ifdef APPLESEED_WITH_OIIO
#include "renderer/kernel/rendering/oiioerrorhandler.h"
#endif
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/closures.h"
#endif
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <exception>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BaseRenderer class implementation.
//

BaseRenderer::BaseRenderer(
    Project&                project,
    const ParamArray&       params)
  : m_project(project)
  , m_params(params)
{
#ifdef APPLESEED_WITH_OIIO
    m_error_handler = new OIIOErrorHandler();

#ifndef NDEBUG
    // While debugging, we want all possible outputs.
    m_error_handler->verbosity(OIIO::ErrorHandler::VERBOSE);
#endif

    RENDERER_LOG_DEBUG("creating OpenImageIO texture system...");
    m_texture_system = OIIO::TextureSystem::create(false);

    m_texture_system->attribute("automip", 0);
    m_texture_system->attribute("accept_untiled", 1);
    m_texture_system->attribute("accept_unmipped", 1);
    m_texture_system->attribute("gray_to_rgb", 1);
    m_texture_system->attribute("latlong_up", "y");
#endif

#ifdef APPLESEED_WITH_OSL
    RENDERER_LOG_DEBUG("creating OSL shading system...");
    m_renderer_services = new RendererServices(m_project, *m_texture_system);

    m_shading_system = OSL::ShadingSystem::create(
        m_renderer_services,
        m_texture_system,
        m_error_handler);

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
#endif
}

BaseRenderer::~BaseRenderer()
{
#ifdef APPLESEED_WITH_OSL
    RENDERER_LOG_DEBUG("destroying OSL shading system...");
    m_project.get_scene()->release_optimized_osl_shader_groups();
    OSL::ShadingSystem::destroy(m_shading_system);
    delete m_renderer_services;
#endif

#ifdef APPLESEED_WITH_OIIO
    const string stats = m_texture_system->getstats();
    const string trimmed_stats = trim_right(stats, "\r\n");
    RENDERER_LOG_INFO("%s", trimmed_stats.c_str());

    RENDERER_LOG_DEBUG("destroying OpenImageIO texture system...");
    OIIO::TextureSystem::destroy(m_texture_system);
    delete m_error_handler;
#endif
}

ParamArray& BaseRenderer::get_parameters()
{
    return m_params;
}

const ParamArray& BaseRenderer::get_parameters() const
{
    return m_params;
}

bool BaseRenderer::initialize_shading_system(
    TextureStore& texture_store,
    IAbortSwitch& abort_switch)
{
#ifdef APPLESEED_WITH_OIIO
    initialize_oiio();
#endif

#ifdef APPLESEED_WITH_OSL
    return initialize_osl(texture_store, abort_switch);
#else
    return true;
#endif
}

#ifdef APPLESEED_WITH_OIIO
void BaseRenderer::initialize_oiio()
{
    const ParamArray& params = m_params.child("texture_store");

    const size_t texture_cache_size_bytes =
        params.get_optional<size_t>("max_size", 256 * 1024 * 1024);

    RENDERER_LOG_INFO(
        "setting OpenImageIO texture cache size to %s.",
        pretty_size(texture_cache_size_bytes).c_str());

    const float texture_cache_size_mb =
        static_cast<float>(texture_cache_size_bytes) / (1024 * 1024);

    m_texture_system->attribute("max_memory_MB", texture_cache_size_mb);

    // search paths
    string prev_search_path;
    m_texture_system->getattribute("searchpath", prev_search_path);
    const string new_search_path = m_project.make_search_path_string();

    if (new_search_path != prev_search_path)
    {
        RENDERER_LOG_INFO(
            "setting OpenImageIO search path to %s.",
            new_search_path.c_str());

        m_texture_system->clear();
        m_texture_system->attribute("searchpath", new_search_path);
    }
}
#endif

#ifdef APPLESEED_WITH_OSL
bool BaseRenderer::initialize_osl(TextureStore& texture_store, IAbortSwitch& abort_switch)
{
    m_renderer_services->initialize(texture_store);

    // search paths
    string prev_search_path;
    m_shading_system->getattribute("searchpath:shader", prev_search_path);
    const string new_search_path = m_project.make_search_path_string();

    if (new_search_path != prev_search_path)
    {
        RENDERER_LOG_INFO(
            "setting OSL shader search path to %s.",
            new_search_path.c_str());

        m_project.get_scene()->release_optimized_osl_shader_groups();
        m_shading_system->attribute("searchpath:shader", new_search_path);
    }

    // Re-optimize the shadergroups that needs updating.
    return
        m_project.get_scene()->create_optimized_osl_shader_groups(
            *m_shading_system,
            &abort_switch);
}
#endif

}   // namespace renderer
