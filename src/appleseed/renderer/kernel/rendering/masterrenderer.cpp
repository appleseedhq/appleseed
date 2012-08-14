
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/drt/drtlightingengine.h"
#include "renderer/kernel/lighting/pt/ptlightingengine.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/rendering/debug/blanktilerenderer.h"
#include "renderer/kernel/rendering/debug/debugtilerenderer.h"
#include "renderer/kernel/rendering/debug/ewatesttilerenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/genericsamplegenerator.h"
#include "renderer/kernel/rendering/generic/genericsamplerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/lighttracing/lighttracingsamplegenerator.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/stringexception.h"

// Standard headers.
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
{
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
    catch (const StringException& e)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (%s: %s).", e.what(), e.string());
        return false;
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

    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    m_project.create_aov_images();
    m_project.update_trace_context();

    const Scene& scene = *m_project.get_scene();
    Frame& frame = *m_project.get_frame();

    // Create the texture store.
    TextureStore texture_store(
        scene,
        m_params.get_optional<size_t>("texture_cache_size", 256 * 1024 * 1024));

    // Create the light sampler.
    LightSampler light_sampler(scene);

    // Create the shading engine.
    ShadingEngine shading_engine(m_params.child("shading_engine"));

    //
    // Create a lighting engine factory.
    //

    auto_ptr<ILightingEngineFactory> lighting_engine_factory;

    const string lighting_engine_param =
        m_params.get_required<string>("lighting_engine", "pt");

    if (lighting_engine_param == "drt")
    {
        lighting_engine_factory.reset(
            new DRTLightingEngineFactory(
                light_sampler,
                m_params.child("drt")));
    }
    else if (lighting_engine_param == "pt")
    {
        lighting_engine_factory.reset(
            new PTLightingEngineFactory(
                light_sampler,
                m_params.child("pt")));
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"lighting_engine\" parameter: \"%s\".",
            lighting_engine_param.c_str());

        return IRendererController::AbortRendering;
    }

    //
    // Create a sample renderer factory.
    //

    auto_ptr<ISampleRendererFactory> sample_renderer_factory;

    const string sample_renderer_param =
        m_params.get_required<string>("sample_renderer", "generic");

    if (sample_renderer_param == "generic")
    {
        sample_renderer_factory.reset(
            new GenericSampleRendererFactory(
                scene,
                frame,
                m_project.get_trace_context(),
                texture_store,
                lighting_engine_factory.get(),
                shading_engine,
                m_params.child("generic_sample_renderer")));
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"sample_renderer\" parameter: \"%s\".",
            sample_renderer_param.c_str());

        return IRendererController::AbortRendering;
    }

    //
    // Create a tile renderer factory.
    //

    auto_ptr<ITileRendererFactory> tile_renderer_factory;

    const string tile_renderer_param =
        m_params.get_required<string>("tile_renderer", "generic");

    if (tile_renderer_param == "generic")
    {
        tile_renderer_factory.reset(
            new GenericTileRendererFactory(
                frame,
                sample_renderer_factory.get(),
                m_params.child("generic_tile_renderer")));
    }
    else if (tile_renderer_param == "blank")
    {
        tile_renderer_factory.reset(new BlankTileRendererFactory());
    }
    else if (tile_renderer_param == "debug")
    {
        tile_renderer_factory.reset(new DebugTileRendererFactory());
    }
    else if (tile_renderer_param == "ewatest")
    {
        tile_renderer_factory.reset(
            new EWATestTileRendererFactory(
                scene,
                m_project.get_trace_context(),
                texture_store,
                m_params.child("ewatest_tile_renderer")));
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"tile_renderer\" parameter: \"%s\".",
            tile_renderer_param.c_str());

        return IRendererController::AbortRendering;
    }

    //
    // Create a sample generator factory.
    //

    auto_ptr<ISampleGeneratorFactory> sample_generator_factory;

    const string sample_generator_param =
        m_params.get_optional<string>("sample_generator", "generic");

    if (sample_generator_param == "generic")
    {
        sample_generator_factory.reset(
            new GenericSampleGeneratorFactory(
                frame,
                sample_renderer_factory.get()));
    }
    else if (sample_generator_param == "lighttracing")
    {
        sample_generator_factory.reset(
            new LightTracingSampleGeneratorFactory(
                scene,
                frame,
                m_project.get_trace_context(),
                texture_store,
                light_sampler,
                m_params.child("lighttracing_sample_generator")));
    }
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value for \"sample_generator\" parameter: \"%s\".",
            sample_generator_param.c_str());

        return IRendererController::AbortRendering;
    }

    //
    // Create a frame renderer.
    //

    auto_release_ptr<IFrameRenderer> frame_renderer;

    const string frame_renderer_param =
        m_params.get_required<string>("frame_renderer", "generic");

    if (frame_renderer_param == "generic")
    {
        ParamArray params = m_params.child("generic_frame_renderer");
        copy_param(params, m_params, "rendering_threads");

        frame_renderer.reset(
            GenericFrameRendererFactory::create(
                frame,
                tile_renderer_factory.get(),
                m_tile_callback_factory,
                params));
    }
    else if (frame_renderer_param == "progressive")
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
            frame_renderer_param.c_str());

        return IRendererController::AbortRendering;
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

        if (!m_project.get_scene()->on_frame_begin(m_project))
            return IRendererController::AbortRendering;

        m_renderer_controller->on_frame_begin();

        frame_renderer->start_rendering();

        const IRendererController::Status status = render_frame(frame_renderer);

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

IRendererController::Status MasterRenderer::render_frame(IFrameRenderer* frame_renderer) const
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

}   // namespace renderer
