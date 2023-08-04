
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "cpurenderdevice.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/oiioerrorhandler.h"
#include "renderer/kernel/rendering/renderercomponents.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

namespace renderer
{

CPURenderDevice::CPURenderDevice(
    Project&                project,
    const ParamArray&       params)
  : RenderDeviceBase(project, params)
  , m_texture_store(project.get_texture_store())
{
    m_error_handler = new OIIOErrorHandler();
#ifndef NDEBUG
    m_error_handler->verbosity(OIIO::ErrorHandler::VERBOSE);
#endif

    RENDERER_LOG_DEBUG("creating oiio texture system...");
    m_texture_system = OIIOTextureSystemFactory::create(false);
    m_texture_system->attribute("accept_untiled", 1);
    m_texture_system->attribute("accept_unmipped", 1);
    m_texture_system->attribute("gray_to_rgb", 1);
    m_texture_system->attribute("latlong_up", "y");
    m_texture_system->attribute("flip_t", 1);

    m_renderer_services =
        new RendererServices(
            project,
            reinterpret_cast<OIIO::TextureSystem&>(*m_texture_system));

    RENDERER_LOG_DEBUG("creating osl shading system...");
    m_shading_system =
        OSLShadingSystemFactory::create(
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
    m_shading_system->attribute("compile_report", 1);
    m_shading_system->attribute("countlayerexecs", 1);
    m_shading_system->attribute("clearmemory", 1);
#endif

    // Register appleseed's closures into OSL's shading system.
    register_closures(*m_shading_system);
}

CPURenderDevice::~CPURenderDevice()
{
    m_components.reset();

    RENDERER_LOG_DEBUG("destroying osl shading system...");
    get_project().get_scene()->release_optimized_osl_shader_groups();
    m_shading_system->release();
    delete m_renderer_services;

    const std::string stats = m_texture_system->getstats();
    const std::string modified_stats = prefix_all_lines(trim_both(stats), "oiio: ");
    RENDERER_LOG_DEBUG("%s", modified_stats.c_str());

    RENDERER_LOG_DEBUG("destroying oiio texture system...");
    m_texture_system->release();
    delete m_error_handler;

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", m_texture_store.get_statistics().to_string().c_str());
}

bool CPURenderDevice::initialize(
    const SearchPaths&      resource_search_paths,
    ITileCallbackFactory*   tile_callback_factory,
    IAbortSwitch&           abort_switch)
{
    // Construct a search paths string from the project's search paths.
    const std::string project_search_paths =
        to_string(get_project().search_paths().to_string_reversed(SearchPaths::osl_path_separator()));

    // Initialize OIIO.
    const size_t texture_cache_size_bytes =
        get_params().child("texture_store").get_optional<size_t>(
            "max_size",
            TextureStore::get_default_size());
    RENDERER_LOG_INFO(
        "setting oiio texture cache size to %s.",
        pretty_size(texture_cache_size_bytes).c_str());
    const float texture_cache_size_mb =
        static_cast<float>(texture_cache_size_bytes) / (1024 * 1024);
    m_texture_system->attribute("max_memory_MB", texture_cache_size_mb);

    // Set OIIO search paths.
    std::string prev_oiio_search_path;
    m_texture_system->getattribute("searchpath", prev_oiio_search_path);
    if (prev_oiio_search_path != project_search_paths)
    {
        RENDERER_LOG_INFO("setting oiio search paths to %s", project_search_paths.c_str());
        m_texture_system->invalidate_all(true);
        m_texture_system->attribute("searchpath", project_search_paths);
    }

    // Also use the project search paths to look for OpenImageIO plugins.
    m_texture_system->attribute("plugin_searchpath", project_search_paths);

    // Initialize OSL.
    m_renderer_services->initialize(m_texture_store);

    // Create renderer components.
    m_components.reset(
        new RendererComponents(
            get_project(),
            get_params(),
            tile_callback_factory,
            m_texture_store,
            *m_texture_system,
            *m_shading_system));

    // Set OSL search paths.
    std::string prev_osl_search_paths;
    m_shading_system->getattribute("searchpath:shader", prev_osl_search_paths);
    if (prev_osl_search_paths != project_search_paths)
    {
        RENDERER_LOG_INFO("setting osl shader search paths to %s", project_search_paths.c_str());
        get_project().get_scene()->release_optimized_osl_shader_groups();
        m_shading_system->attribute("searchpath:shader", project_search_paths);
    }

    // Initialize the shader compiler, if the OSL headers are found.
    if (resource_search_paths.exist("stdosl.h"))
    {
        const APIString stdosl_path = resource_search_paths.qualify("stdosl.h");
        RENDERER_LOG_INFO("found OSL headers in %s", stdosl_path.c_str());
        m_osl_compiler = ShaderCompilerFactory::create(stdosl_path.c_str());
    }
    else
        RENDERER_LOG_INFO("OSL headers not found.");

    // Re-optimize shader groups that need updating.
    if (!get_project().get_scene()->create_optimized_osl_shader_groups(
            *m_shading_system,
            m_osl_compiler.get(),
            &abort_switch))
    {
        return false;
    }

    return m_components->create();
}

bool CPURenderDevice::build_or_update_scene()
{
    // Updating the trace context causes ray tracing acceleration structures to be updated or rebuilt.
    get_project().update_trace_context();
    return true;
}

bool CPURenderDevice::load_checkpoint(Frame& frame, const size_t pass_count)
{
    return
        frame.load_checkpoint(
            &m_components->get_shading_result_framebuffer_factory(),
            pass_count);
}

IRendererController* CPURenderDevice::get_frame_renderer_controller()
{
    return m_components->get_frame_renderer().get_renderer_controller();
}

bool CPURenderDevice::on_render_begin(
    OnRenderBeginRecorder&  recorder,
    IAbortSwitch*           abort_switch)
{
    return m_components->on_render_begin(recorder, abort_switch);
}

bool CPURenderDevice::on_frame_begin(
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    return m_components->on_frame_begin(recorder, abort_switch);
}

const IRenderContext& CPURenderDevice::get_render_context() const
{
    return m_context;
}

IRendererController::Status CPURenderDevice::render_frame(
    ITileCallbackFactory*   tile_callback_factory,
    IRendererController&    renderer_controller,
    IAbortSwitch&           abort_switch)
{
    IFrameRenderer& frame_renderer = m_components->get_frame_renderer();
    assert(!frame_renderer.is_rendering());

    // Start rendering the frame.
    frame_renderer.start_rendering();

    // Wait until the frame is completed or rendering is aborted.
    const IRendererController::Status status =
        wait_for_event(frame_renderer, renderer_controller);

    switch (status)
    {
      case IRendererController::TerminateRendering:
      case IRendererController::AbortRendering:
      case IRendererController::ReinitializeRendering:
        frame_renderer.terminate_rendering();
        break;

      case IRendererController::RestartRendering:
        frame_renderer.stop_rendering();
        break;

      assert_otherwise;
    }

    assert(!frame_renderer.is_rendering());

    return status;
}

void CPURenderDevice::print_settings() const
{
    m_components->print_settings();
}

Dictionary CPURenderDevice::get_metadata()
{
    Dictionary metadata;
    return metadata;
}

}   // namespace renderer
