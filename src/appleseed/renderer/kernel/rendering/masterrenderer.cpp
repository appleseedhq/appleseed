
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/rendering/debug/ewatesttilerenderer.h"
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
#ifdef WITH_OSL
#include "renderer/kernel/rendering/rendererservices.h"
#endif
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/searchpaths.h"

// OIIO headers.
#ifdef WITH_OSL
#include "OpenImageIO/texture.h"
#endif

// boost headers.
#include "boost/shared_ptr.hpp"
#include "boost/bind.hpp"

// Standard headers.
#include <deque>
#include <exception>

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
    ITileCallbackFactory*   tile_callback_factory)
  : m_project(project)
  , m_params(params)
  , m_renderer_controller(renderer_controller)
  , m_tile_callback_factory(tile_callback_factory)
  , m_serial_renderer_controller(0)
  , m_serial_tile_callback_factory(0)
{
}

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallback*          tile_callback)
  : m_project(project)
  , m_params(params)
  , m_serial_renderer_controller(new SerialRendererController(renderer_controller, tile_callback))
  , m_serial_tile_callback_factory(new SerialTileCallbackFactory(m_serial_renderer_controller))
{
    m_renderer_controller = m_serial_renderer_controller;
    m_tile_callback_factory = m_serial_tile_callback_factory;
}

MasterRenderer::~MasterRenderer()
{
    delete m_serial_tile_callback_factory;
    delete m_serial_renderer_controller;
}

ParamArray& MasterRenderer::get_parameters()
{
    return m_params;
}

const ParamArray& MasterRenderer::get_parameters() const
{
    return m_params;
}

bool MasterRenderer::render() const
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
    catch (const exception& e)
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

void MasterRenderer::do_render() const
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
        ParamArray&         dest,
        const ParamArray&   source,
        const char*         param_name)
    {
        if (source.strings().exist(param_name))
            dest.strings().insert(param_name, source.strings().get(param_name));
    }
}

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence() const
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

#ifdef WITH_OSL

    // Create our renderer services.
    RendererServices services;

    // Create the error handler
    // TODO: replace this by an appropiate error handler...
    OIIO::ErrorHandler error_handler;

    // Create the OIIO texture system.
    boost::shared_ptr<OIIO::TextureSystem> texture_system(
        OIIO::TextureSystem::create(false),
        boost::bind(&OIIO::TextureSystem::destroy, _1));

    // Set texture system mem limit.
    {
        const size_t max_size = m_params.get_optional<size_t>("texture_cache_size", 256 * 1024 * 1024);
        texture_system->attribute("max_memory_MB", static_cast<float>(max_size / 1024));
    }

    std::string osl_search_path;
    for (size_t i = 0, e = m_project.get_search_paths().size(); i < e; ++i)
    {
        osl_search_path.append(m_project.get_search_paths()[i]);

        // Do not append a colon after the last path.
        if (i != e - 1)
            osl_search_path.append(";");
    }

    // Setup search paths.
    texture_system->attribute("searchpath", osl_search_path);

    // TODO: set other texture system options here...

    // Create our OSL shading system.
    boost::shared_ptr<OSL::ShadingSystem> shading_system(
        OSL::ShadingSystem::create(
            &services,
            texture_system.get(),
            &error_handler),
        boost::bind(&OSL::ShadingSystem::destroy, _1));
    shading_system->attribute("searchpath:shader", osl_search_path);
    shading_system->attribute("lockgeom", 1);
    shading_system->attribute("colorspace", "Linear");
    shading_system->attribute("commonspace", "world");
    // TODO: set more shading system options here...
    // string[] raytypes      Array of ray type names
    // ...

    register_closures(*shading_system);

#endif

    // We start by binding entities inputs. This must be done before creating/updating the trace context.
    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    m_project.create_aov_images();
    m_project.update_trace_context();

    const Scene& scene = *m_project.get_scene();
    Frame& frame = *m_project.get_frame();
    const TraceContext& trace_context = m_project.get_trace_context();

    // Create the texture store.
    TextureStore texture_store(scene, m_params.child("texture_store"));

    // Create the light sampler.
    LightSampler light_sampler(scene, m_params.child("light_sampler"));

    // Create the shading engine.
    ShadingEngine shading_engine(m_params.child("shading_engine"));

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
            pixel_renderer_factory.reset(
                new UniformPixelRendererFactory(
                    sample_renderer_factory.get(),
                    m_params.child("uniform_pixel_renderer")));
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
        else if (value == "ewatest")
        {
            tile_renderer_factory.reset(
                new EWATestTileRendererFactory(
                    scene,
                    trace_context,
                    texture_store,
                    m_params.child("ewatest_tile_renderer")));
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
    const IRendererController::Status status = render_frame_sequence(frame_renderer.get());

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

    return status;
}

IRendererController::Status MasterRenderer::render_frame_sequence(IFrameRenderer* frame_renderer) const
{
    while (true) 
    {
        assert(!frame_renderer->is_rendering());

        // The renderer controller might alter the scene (like the transform of the camera).
        // It needs to be called before renderer::Scene::on_frame_begin() which assumes the
        // scene is up-to-date and ready to be rendered.
        m_renderer_controller->on_frame_begin();

        if (!m_project.get_scene()->on_frame_begin(m_project))
        {
            m_renderer_controller->on_frame_end();
            return IRendererController::AbortRendering;
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

        if (status != IRendererController::RestartRendering)
            return status;
    }
}

IRendererController::Status MasterRenderer::wait_for_event(IFrameRenderer* frame_renderer) const
{
    while (frame_renderer->is_rendering())
    {
        const IRendererController::Status status = m_renderer_controller->on_progress();

        if (status != IRendererController::ContinueRendering)
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

#ifdef WITH_OSL

void MasterRenderer::register_closures(OSL::ShadingSystem& shading_sys) const
{
    // OSL TODO: implement this...
}

#endif

}   // namespace renderer
