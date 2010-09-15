
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/drt/drt.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracing/pathtracing.h"
#include "renderer/kernel/rendering/debug/blanktilerenderer.h"
#include "renderer/kernel/rendering/debug/debugtilerenderer.h"
#include "renderer/kernel/rendering/generic/genericframerenderer.h"
#include "renderer/kernel/rendering/generic/genericsamplerenderer.h"
#include "renderer/kernel/rendering/generic/generictilerenderer.h"
#include "renderer/kernel/rendering/progressive/progressiveframerenderer.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/itilerenderer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingengine.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/input/uniforminputevaluator.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/utility/foreach.h"

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
    const Mode              mode,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory)
  : m_project(project)
  , m_params(params)
  , m_mode(mode)
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

void MasterRenderer::render()
{
    try
    {
        do_render();
    }
    catch (const Exception& e)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed: %s", e.what());
    }
    catch (const bad_alloc&)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed: ran out of memory");
    }
    catch (...)
    {
        m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed: unknown exception");
    }
}

void MasterRenderer::do_render()
{
    while (true)
    {
        // Notify the rendering controller that rendering is beginning.
        m_renderer_controller->on_rendering_begin();

        // Initialize the rendering components and render until completed or aborted.
        const IRendererController::Status status = render_from_scratch();

        // In non-continuous rendering mode, stop after one frame.
        if (m_mode == RenderOnce)
        {
            m_renderer_controller->on_rendering_success();
            break;
        }

        // If rendering was aborted, stop.
        if (status == IRendererController::AbortRendering)
        {
            m_renderer_controller->on_rendering_abort();
            break;
        }
    }
}

IRendererController::Status MasterRenderer::render_from_scratch()
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

    // Bind all scene entities inputs.
    if (!bind_inputs())
        return IRendererController::AbortRendering;

    // Create a light sampler.
    LightSampler light_sampler(*m_project.get_scene());

    // Create a lighting engine factory.
    auto_ptr<ILightingEngineFactory> lighting_engine_factory;
    const string lighting_engine_param =
        m_params.get_required<string>("lighting_engine", "pt");
    if (lighting_engine_param == "drt")
    {
        // Distribution ray tracing.
        lighting_engine_factory.reset(
            new DRTLightingEngineFactory(light_sampler, m_params.child("drt")));
    }
    else
    {
        // Path tracing.
        lighting_engine_factory.reset(
            new PTLightingEngineFactory(light_sampler, m_params.child("pt")));
    }

    // Create a shading engine.
    ShadingEngine shading_engine(m_params.child("shading_engine"));

    // Create a sample renderer factory.
    auto_ptr<ISampleRendererFactory> sample_renderer_factory;
    const string sample_renderer_param =
        m_params.get_required<string>("sample_renderer", "generic");
    {
        // Generic sample renderer.
        sample_renderer_factory.reset(
            new GenericSampleRendererFactory(
                *m_project.get_scene(),
                m_project.get_trace_context(),
                lighting_engine_factory.get(),
                shading_engine,
                m_params.child("generic_sample_renderer")));
    }

    // Create a tile renderer factory.
    auto_ptr<ITileRendererFactory> tile_renderer_factory;
    const string tile_renderer_param =
        m_params.get_required<string>("tile_renderer", "generic");
    if (tile_renderer_param == "blank")
    {
        // Blank tile renderer factory.
        tile_renderer_factory.reset(new BlankTileRendererFactory());
    }
    else if (tile_renderer_param == "debug")
    {
        // Debug tile renderer factory.
        tile_renderer_factory.reset(new DebugTileRendererFactory());
    }
    else
    {
        // Generic tile renderer factory.
        tile_renderer_factory.reset(
            new GenericTileRendererFactory(
                *m_project.get_frame(),
                sample_renderer_factory.get(),
                m_params.child("generic_tile_renderer")));
    }

    // Create a frame renderer.
    auto_release_ptr<IFrameRenderer> frame_renderer;
    const string frame_renderer_param =
        m_params.get_required<string>("frame_renderer", "generic");
    if (frame_renderer_param == "progressive")
    {
        // Progressive frame renderer.
        frame_renderer.reset(
            ProgressiveFrameRendererFactory::create(
                *m_project.get_frame(),
                sample_renderer_factory.get(),
                m_tile_callback_factory,
                m_params.child("progressive_frame_renderer")));
    }
    else
    {
        // Generic frame renderer.
        frame_renderer.reset(
            GenericFrameRendererFactory::create(
                *m_project.get_frame(),
                tile_renderer_factory.get(),
                m_tile_callback_factory,
                m_params.child("generic_frame_renderer")));
    }

    // Execute the main rendering loop.
    return render_until_completed_or_aborted(frame_renderer.get());
}

bool MasterRenderer::bind_inputs() const
{
    InputBinder input_binder;
    input_binder.bind(*m_project.get_scene());
    return input_binder.get_error_count() == 0;
}

IRendererController::Status MasterRenderer::render_until_completed_or_aborted(
    IFrameRenderer* frame_renderer)
{
    while (true) 
    {
        assert(!frame_renderer->is_rendering());

        // Notify the rendering controller that a frame is about to be rendered.
        m_renderer_controller->on_frame_begin();

        // Perform pre-frame rendering actions.
        on_frame_begin();

        // Start rendering the frame.
        frame_renderer->start_rendering();

        // Wait until rendering of the frame is complete.
        const IRendererController::Status status = wait_until_frame_complete(frame_renderer);
        assert(!frame_renderer->is_rendering());

        // Perform post-frame rendering actions.
        on_frame_end();

        // Notify the rendering controller that a frame is finished rendering.
        m_renderer_controller->on_frame_end();

        // In non-continuous rendering mode, return after one frame.
        if (m_mode == RenderOnce)
            return IRendererController::ContinueRendering;

        // If rendering was aborted or reinitialized, return.
        if (status == IRendererController::AbortRendering ||
            status == IRendererController::ReinitializeRendering)
            return status;
    }
}

IRendererController::Status MasterRenderer::wait_until_frame_complete(
    IFrameRenderer* frame_renderer)
{
    while (frame_renderer->is_rendering())
    {
        // Report progress and retrieve status.
        const IRendererController::Status status = m_renderer_controller->on_progress();

        // Return if rendering was aborted or restarted.
        if (status != IRendererController::ContinueRendering)
        {
            frame_renderer->stop_rendering();
            return status;
        }
    }

    return IRendererController::ContinueRendering;
}

namespace
{
    //
    // Invoke on_frame_begin() / on_frame_end() on all entities of a collection.
    //

    template <typename EntityCollection>
    void invoke_on_frame_begin_collection(
        EntityCollection&       entities,
        const Scene&            scene)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_begin(*i, scene);
    }

    template <typename EntityCollection>
    void invoke_on_frame_begin_collection(
        EntityCollection&       entities,
        const Scene&            scene,
        UniformInputEvaluator&  input_evaluator)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_begin(*i, scene, input_evaluator);
    }

    template <typename EntityCollection>
    void invoke_on_frame_end_collection(
        EntityCollection&       entities,
        const Scene&            scene)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_end(*i, scene);
    }

    //
    // Invoke on_frame_begin() / on_frame_end() on a single entity.
    //

    template <typename Entity>
    void invoke_on_frame_begin(
        Entity&                 entity,
        const Scene&            scene)
    {
        entity.on_frame_begin(scene);
    }

    template <typename Entity>
    void invoke_on_frame_begin(
        Entity&                 entity,
        const Scene&            scene,
        UniformInputEvaluator&  input_evaluator)
    {
        const void* data =
            input_evaluator.evaluate(entity.get_inputs());

        entity.on_frame_begin(scene, data);
    }

    template <typename Entity>
    void invoke_on_frame_end(
        Entity&                 entity,
        const Scene&            scene)
    {
        entity.on_frame_end(scene);
    }

    //
    // Invoke on_frame_begin() / on_frame_end() on all entities of an assembly.
    //

    template <>
    void invoke_on_frame_begin(
        Assembly&               assembly,
        const Scene&            scene,
        UniformInputEvaluator&  input_evaluator)
    {
        invoke_on_frame_begin_collection(assembly.surface_shaders(), scene);
        invoke_on_frame_begin_collection(assembly.bsdfs(), scene, input_evaluator);
        invoke_on_frame_begin_collection(assembly.edfs(), scene, input_evaluator);
    }

    template <>
    void invoke_on_frame_end(
        Assembly&               assembly,
        const Scene&            scene)
    {
        invoke_on_frame_end_collection(assembly.edfs(), scene);
        invoke_on_frame_end_collection(assembly.bsdfs(), scene);
        invoke_on_frame_end_collection(assembly.surface_shaders(), scene);
    }
}

void MasterRenderer::on_frame_begin() const
{
    const Scene& scene = *m_project.get_scene();

    assert(scene.get_camera());
    Intersector intersector(m_project.get_trace_context(), false);
    scene.get_camera()->on_frame_begin(scene, intersector);

    UniformInputEvaluator input_evaluator;

    invoke_on_frame_begin_collection(scene.environment_edfs(), scene);
    invoke_on_frame_begin_collection(scene.environment_shaders(), scene);
    invoke_on_frame_begin_collection(scene.assemblies(), scene, input_evaluator);
}

void MasterRenderer::on_frame_end() const
{
    const Scene& scene = *m_project.get_scene();

    invoke_on_frame_end_collection(scene.assemblies(), scene);
    invoke_on_frame_end_collection(scene.environment_shaders(), scene);
    invoke_on_frame_end_collection(scene.environment_edfs(), scene);

    assert(scene.get_camera());
    scene.get_camera()->on_frame_end(scene);
}

}   // namespace renderer
