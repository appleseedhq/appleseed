//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/drt/drtlightingengine.h"
#include "renderer/kernel/lighting/lighttracing/lighttracingsamplegenerator.h"
#include "renderer/kernel/lighting/pt/ptlightingengine.h"
#include "renderer/kernel/lighting/sppm/sppmlightingengine.h"
#include "renderer/kernel/lighting/sppm/sppmparameters.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/rendering/debug/blanksamplerenderer.h"
#include "renderer/kernel/rendering/debug/blanktilerenderer.h"
#include "renderer/kernel/rendering/debug/debugsamplerenderer.h"
#include "renderer/kernel/rendering/debug/debugtilerenderer.h"
#include "renderer/kernel/rendering/final/adaptivepixelrenderer.h"
#include "renderer/kernel/rendering/final/uniformpixelrenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/genericsamplegenerator.h"
#include "renderer/kernel/rendering/generic/genericsamplerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/kernel/rendering/ephemeralshadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/ipasscallback.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/rendering/permanentshadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#ifdef WITH_OSL
#include "renderer/kernel/rendering/oiioerrorhandler.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/closures.h"
#endif

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/searchpaths.h"

// boost headers.
#include "boost/filesystem/path.hpp"
#include "boost/shared_ptr.hpp"
#include "boost/bind.hpp"

// Standard headers.
#include <deque>
#include <exception>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// MasterRenderer class implementation.
//

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory,
    AbortSwitch*            abort_switch)
  : m_project(project)
  , m_params(params)
  , m_renderer_controller(renderer_controller)
  , m_tile_callback_factory(tile_callback_factory)
  , m_abort_switch(abort_switch)
  , m_serial_renderer_controller(0)
  , m_serial_tile_callback_factory(0)
#ifdef WITH_OSL
  , m_texture_system(0)
#endif
{
}

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallback*          tile_callback,
    AbortSwitch*            abort_switch)
  : m_project(project)
  , m_params(params)
  , m_abort_switch(abort_switch)
  , m_serial_renderer_controller(new SerialRendererController(renderer_controller, tile_callback))
  , m_serial_tile_callback_factory(new SerialTileCallbackFactory(m_serial_renderer_controller))
#ifdef WITH_OSL
  , m_texture_system(0)
#endif
{
    m_renderer_controller = m_serial_renderer_controller;
    m_tile_callback_factory = m_serial_tile_callback_factory;
}

MasterRenderer::~MasterRenderer()
{
    delete m_serial_tile_callback_factory;
    delete m_serial_renderer_controller;

#ifdef WITH_OIIO
    if (m_texture_system)
        OIIO::TextureSystem::destroy(m_texture_system);
#endif
}

ParamArray& MasterRenderer::get_parameters()
{
    return m_params;
}

const ParamArray& MasterRenderer::get_parameters() const
{
    return m_params;
}

bool MasterRenderer::render()
{
    try
    {
        do_render();
        return true;
    }
    catch (const bad_alloc&)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (ran out of memory).");
        return false;
    }
#ifdef NDEBUG
    catch (const std::exception& e)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
        return false;
    }
    catch (...)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (unknown exception).");
        return false;
    }
#endif
}

void MasterRenderer::do_render()
{
    while (true)
    {
        m_renderer_controller->on_rendering_begin();

        const IRendererController::Status status = initialize_and_render_frame_sequence();

        switch (status)
        {
          case IRendererController::TerminateRendering:
            m_renderer_controller->on_rendering_success();
            return;

          case IRendererController::AbortRendering:
            m_renderer_controller->on_rendering_abort();
            return;

          case IRendererController::ReinitializeRendering:
            break;

          assert_otherwise;
        }
    }
}

namespace
{
    void copy_param(
        ParamArray&             dest,
        const ParamArray&       source,
        const char*             param_name)
    {
        if (source.strings().exist(param_name))
            dest.strings().insert(param_name, source.strings().get(param_name));
    }

#ifdef WITH_OSL

    void destroy_osl_shading_system(
        OSL::ShadingSystem*     s,
        OIIO::TextureSystem*    tx)
    {
        RENDERER_LOG_INFO("%s", tx->getstats().c_str());
        tx->reset_stats();
        OSL::ShadingSystem::destroy(s);
    }

#endif
}

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence()
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

    // Reset the abort switch.
    if (m_abort_switch)
        m_abort_switch->clear();

    // Create OpenImageIO's texture system.
#ifdef WITH_OIIO

    if (!m_texture_system)
        m_texture_system = OIIO::TextureSystem::create();

    const size_t texture_cache_size =
        m_params.get_optional<size_t>("texture_cache_size",  256 * 1024 * 1024);

    m_texture_system->attribute("max_memory_MB", static_cast<float>(texture_cache_size / 1024));

    string search_paths;

    // Skip search paths for builtin projects.
    if (m_project.search_paths().has_root_path())
    {
        // Setup texture / shader search paths.
        // In OIIO / OSL, the path priorities are the opposite of appleseed,
        // so we copy the paths in reverse order.

        const filesystem::path root_path = m_project.search_paths().get_root_path();

        if (!m_project.search_paths().empty())
        {
            for (size_t i = 0, e = m_project.search_paths().size(); i != e; ++i)
            {
                filesystem::path p(m_project.search_paths()[e - 1 - i]);

                if (p.is_relative())
                   p = root_path / p;

                search_paths.append(p.string());
                search_paths.append(";");
            }
        }

        search_paths.append(root_path.string());
    }

    if (!search_paths.empty())
        m_texture_system->attribute("searchpath", search_paths);

    // Set other texture system options here.
    m_texture_system->attribute("automip", 0);
    m_texture_system->attribute("accept_untiled", 1);
    m_texture_system->attribute("accept_unmipped", 1);
    m_texture_system->attribute("gray_to_rgb", 1);
    m_texture_system->attribute("latlong_up", "y");

#endif  // WITH_OIIO

    // We start by binding entities inputs. This must be done before creating/updating the trace context.
    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    m_project.create_aov_images();
    m_project.update_trace_context();

    const Scene& scene = *m_project.get_scene();

    Frame& frame = *m_project.get_frame();
    frame.print_settings();

    const TraceContext& trace_context = m_project.get_trace_context();

    // Create the texture store.
    TextureStore texture_store(scene, m_params.child("texture_store"));

    // Create the light sampler.
    LightSampler light_sampler(scene, m_params.child("light_sampler"));

    // Create the shading engine.
    ShadingEngine shading_engine(m_params.child("shading_engine"));

    // Create OSL's shading system.
#ifdef WITH_OSL

    // Create the error handler.
    OIIOErrorHandler error_handler;

    // While debugging, we want all possible outputs.
#ifndef NDEBUG
    error_handler.verbosity(OIIO::ErrorHandler::VERBOSE);
#endif

    // Create our renderer services.
    RendererServices services(m_project, *m_texture_system);

    // Create our OSL shading system.
    boost::shared_ptr<OSL::ShadingSystem> shading_system(
        OSL::ShadingSystem::create(
            &services,
            m_texture_system,
            &error_handler),
            bind(&destroy_osl_shading_system, _1, m_texture_system));

    if (!search_paths.empty())
        shading_system->attribute("searchpath:shader", search_paths);

    shading_system->attribute("lockgeom", 1);
    shading_system->attribute("colorspace", "Linear");
    shading_system->attribute("commonspace", "world");

    // This array needs to be kept in sync with the ShadingRay::Type enumeration.
    static const char* ray_type_labels[] =
    {
        "camera",
        "light",
        "shadow",
        "probe",
        "diffuse",
        "glossy",
        "specular"
    };

    shading_system->attribute(
        "raytypes",
        OSL::TypeDesc(
            OSL::TypeDesc::STRING,
            sizeof(ray_type_labels) / sizeof(ray_type_labels[0])),
        ray_type_labels);

#ifndef NDEBUG
    // While debugging, we want all possible outputs.
    shading_system->attribute("debug", 1);
    shading_system->attribute("statistics:level", 1);
    shading_system->attribute("compile_report", 1);
    shading_system->attribute("countlayerexecs", 1);
    shading_system->attribute("clearmemory", 1);
#endif

    register_closures(*shading_system);

#endif  // WITH_OSL

    //
    // Create a lighting engine factory.
    //

    auto_ptr<IPassCallback> pass_callback;
    auto_ptr<ILightingEngineFactory> lighting_engine_factory;
    {
        const string value = m_params.get_required<string>("lighting_engine", "pt");

        if (value == "drt")
        {
            lighting_engine_factory.reset(
                new DRTLightingEngineFactory(
                    light_sampler,
                    m_params.child("drt")));    // todo: change to "drt_lighting_engine" -- or?
        }
        else if (value == "pt")
        {
            lighting_engine_factory.reset(
                new PTLightingEngineFactory(
                    light_sampler,
                    m_params.child("pt")));     // todo: change to "pt_lighting_engine" -- or?
        }
        else if (value == "sppm")
        {
            const SPPMParameters params(m_params.child("sppm"));

            SPPMPassCallback* sppm_pass_callback =
                new SPPMPassCallback(
                    scene,
                    light_sampler,
                    trace_context,
                    texture_store,
#ifdef WITH_OSL
                    *shading_system,
#endif
                    params);
            pass_callback.reset(sppm_pass_callback);

            lighting_engine_factory.reset(
                new SPPMLightingEngineFactory(
                    *sppm_pass_callback,
                    light_sampler,
                    params));
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"lighting_engine\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a sample renderer factory.
    //

    auto_ptr<ISampleRendererFactory> sample_renderer_factory;
    {
        const string value = m_params.get_required<string>("sample_renderer", "generic");

        if (value == "generic")
        {
            sample_renderer_factory.reset(
                new GenericSampleRendererFactory(
                    scene,
                    frame,
                    trace_context,
                    texture_store,
                    lighting_engine_factory.get(),
                    shading_engine,
#ifdef WITH_OSL
                    *shading_system,
#endif
                    m_params.child("generic_sample_renderer")));
        }
        else if (value == "blank")
        {
            sample_renderer_factory.reset(new BlankSampleRendererFactory());
        }
        else if (value == "debug")
        {
            sample_renderer_factory.reset(new DebugSampleRendererFactory());
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"sample_renderer\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a sample generator factory.
    //

    auto_ptr<ISampleGeneratorFactory> sample_generator_factory;
    {
        const string value = m_params.get_optional<string>("sample_generator", "");

        if (value == "generic")
        {
            sample_generator_factory.reset(
                new GenericSampleGeneratorFactory(
                    frame,
                    sample_renderer_factory.get()));
        }
        else if (value == "lighttracing")
        {
            sample_generator_factory.reset(
                new LightTracingSampleGeneratorFactory(
                    scene,
                    frame,
                    trace_context,
                    texture_store,
                    light_sampler,
#ifdef WITH_OSL
                    *shading_system,
#endif
                    m_params.child("lighttracing_sample_generator")));
        }
        else if (!value.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"sample_generator\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a pixel renderer factory.
    //

    auto_ptr<IPixelRendererFactory> pixel_renderer_factory;
    {
        const string value = m_params.get_optional<string>("pixel_renderer", "");

        if (value == "uniform")
        {
            ParamArray params = m_params.child("uniform_pixel_renderer");
            copy_param(params, m_params.child("generic_frame_renderer"), "passes");

            pixel_renderer_factory.reset(
                new UniformPixelRendererFactory(
                    sample_renderer_factory.get(),
                    params));
        }
        else if (value == "adaptive")
        {
            pixel_renderer_factory.reset(
                new AdaptivePixelRendererFactory(
                    frame,
                    sample_renderer_factory.get(),
                    m_params.child("adaptive_pixel_renderer")));
        }
        else if (!value.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"pixel_renderer\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a shading result framebuffer factory.
    //

    auto_ptr<IShadingResultFrameBufferFactory> shading_result_framebuffer_factory;
    {
        const string value =
            m_params.get_optional<string>("shading_result_framebuffer", "ephemeral");

        if (value == "ephemeral")
        {
            shading_result_framebuffer_factory.reset(
                new EphemeralShadingResultFrameBufferFactory());
        }
        else if (value == "permanent")
        {
            shading_result_framebuffer_factory.reset(
                new PermanentShadingResultFrameBufferFactory(frame));
        }
        else if (!value.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"shading_result_framebuffer\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a tile renderer factory.
    //

    auto_ptr<ITileRendererFactory> tile_renderer_factory;
    {
        const string value = m_params.get_optional<string>("tile_renderer", "");

        if (value == "generic")
        {
            tile_renderer_factory.reset(
                new GenericTileRendererFactory(
                    frame,
                    pixel_renderer_factory.get(),
                    shading_result_framebuffer_factory.get(),
                    m_params.child("generic_tile_renderer")));
        }
        else if (value == "blank")
        {
            tile_renderer_factory.reset(new BlankTileRendererFactory());
        }
        else if (value == "debug")
        {
            tile_renderer_factory.reset(new DebugTileRendererFactory());
        }
        else if (!value.empty())
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"tile_renderer\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    //
    // Create a frame renderer.
    //

    auto_release_ptr<IFrameRenderer> frame_renderer;
    {
        const string value = m_params.get_required<string>("frame_renderer", "generic");

        if (value == "generic")
        {
            ParamArray params = m_params.child("generic_frame_renderer");
            copy_param(params, m_params, "rendering_threads");

            frame_renderer.reset(
                GenericFrameRendererFactory::create(
                    frame,
                    tile_renderer_factory.get(),
                    m_tile_callback_factory,
                    pass_callback.get(),
                    params));
        }
        else if (value == "progressive")
        {
            ParamArray params = m_params.child("progressive_frame_renderer");
            copy_param(params, m_params, "rendering_threads");

            frame_renderer.reset(
                ProgressiveFrameRendererFactory::create(
                    m_project,
                    sample_generator_factory.get(),
                    m_tile_callback_factory,
                    params));
        }
        else
        {
            RENDERER_LOG_ERROR(
                "invalid value for \"frame_renderer\" parameter: \"%s\".",
                value.c_str());

            return IRendererController::AbortRendering;
        }
    }

    // Execute the main rendering loop.
    const IRendererController::Status status =
        render_frame_sequence(
            frame_renderer.get()
#ifdef WITH_OSL
            , *shading_system
#endif
            );

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

    return status;
}

IRendererController::Status MasterRenderer::render_frame_sequence(
    IFrameRenderer*         frame_renderer
#ifdef WITH_OSL
    , OSL::ShadingSystem&   shading_system
#endif
    )
{
    while (true)
    {
        assert(!frame_renderer->is_rendering());

        // The on_frame_begin() method of the renderer controller might alter the scene
        // (e.g. transform the camera), thus it needs to be called before the on_frame_begin()
        // of the scene which assumes the scene is up-to-date and ready to be rendered.
        m_renderer_controller->on_frame_begin();

        // Prepare the scene for rendering. Don't proceed if that failed.
#ifdef WITH_OSL
        if (!m_project.get_scene()->on_frame_begin(m_project, &shading_system, m_abort_switch))
#else
        if (!m_project.get_scene()->on_frame_begin(m_project, m_abort_switch))
#endif
        {
            m_renderer_controller->on_frame_end();
            return IRendererController::AbortRendering;
        }

        // Don't proceed with rendering if scene preparation was aborted.
        if (is_aborted(m_abort_switch))
        {
            m_renderer_controller->on_frame_end();
            return m_renderer_controller->on_progress();
        }

        frame_renderer->start_rendering();

        const IRendererController::Status status = wait_for_event(frame_renderer);

        switch (status)
        {
          case IRendererController::TerminateRendering:
          case IRendererController::AbortRendering:
          case IRendererController::ReinitializeRendering:
            frame_renderer->terminate_rendering();
            break;

          case IRendererController::RestartRendering:
            frame_renderer->stop_rendering();
            break;

          assert_otherwise;
        }

        assert(!frame_renderer->is_rendering());

        m_project.get_scene()->on_frame_end(m_project);
        m_renderer_controller->on_frame_end();

        switch (status)
        {
          case IRendererController::TerminateRendering:
          case IRendererController::AbortRendering:
          case IRendererController::ReinitializeRendering:
            return status;

          case IRendererController::RestartRendering:
            break;

          assert_otherwise;
        }
    }
}

IRendererController::Status MasterRenderer::wait_for_event(IFrameRenderer* frame_renderer) const
{
    while (frame_renderer->is_rendering())
    {
        // Make sure to retrieve the status *after* having checked the abort switch.
        // Clients are required to configure their renderer controller such that it
        // reports the desired status before they trigger the abort switch.
        const bool aborted = is_aborted(m_abort_switch);
        const IRendererController::Status status = m_renderer_controller->on_progress();

        if (aborted || status != IRendererController::ContinueRendering)
            return status;
    }

    return IRendererController::TerminateRendering;
}

bool MasterRenderer::bind_scene_entities_inputs() const
{
    InputBinder input_binder;
    input_binder.bind(*m_project.get_scene());
    return input_binder.get_error_count() == 0;
}

}   // namespace renderer
