
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence()
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

    // Bind all scene entities inputs.
    if (!bind_inputs())
        return IRendererController::AbortRendering;

    // Create the light sampler.
    LightSampler light_sampler(*m_project.get_scene());

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
            new DRTLightingEngineFactory(light_sampler, m_params.child("drt")));
    }
    else if (lighting_engine_param == "pt")
    {
        lighting_engine_factory.reset(
            new PTLightingEngineFactory(light_sampler, m_params.child("pt")));
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
                *m_project.get_scene(),
                m_project.get_trace_context(),
                lighting_engine_factory.get(),
                shading_engine,
                m_params.child("generic_sample_renderer")));
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
                *m_project.get_frame(),
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
                *m_project.get_frame(),
                sample_renderer_factory.get()));
    }
    else if (sample_generator_param == "lighttracing")
    {
        sample_generator_factory.reset(
            new LightTracingSampleGeneratorFactory(
                *m_project.get_frame()));
    }

    //
    // Create a frame renderer.
    //

    auto_release_ptr<IFrameRenderer> frame_renderer;

    const string frame_renderer_param =
        m_params.get_required<string>("frame_renderer", "generic");

    if (frame_renderer_param == "generic")
    {
        frame_renderer.reset(
            GenericFrameRendererFactory::create(
                *m_project.get_frame(),
                tile_renderer_factory.get(),
                m_tile_callback_factory,
                m_params.child("generic_frame_renderer")));
    }
    else if (frame_renderer_param == "progressive")
    {
        frame_renderer.reset(
            ProgressiveFrameRendererFactory::create(
                *m_project.get_frame(),
                sample_generator_factory.get(),
                m_tile_callback_factory,
                m_params.child("progressive_frame_renderer")));
    }

    // Execute the main rendering loop.
    return render_frame_sequence(frame_renderer.get());
}

IRendererController::Status MasterRenderer::render_frame_sequence(IFrameRenderer* frame_renderer)
{
    while (true) 
    {
        assert(!frame_renderer->is_rendering());

        m_renderer_controller->on_frame_begin();

        on_frame_begin();

        const IRendererController::Status status = render_frame(frame_renderer);
        assert(!frame_renderer->is_rendering());

        on_frame_end();

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

IRendererController::Status MasterRenderer::render_frame(IFrameRenderer* frame_renderer)
{
    frame_renderer->start_rendering();

    while (frame_renderer->is_rendering())
    {
        const IRendererController::Status status = m_renderer_controller->on_progress();

        if (status == IRendererController::ContinueRendering)
            continue;

        frame_renderer->stop_rendering();

        return status;
    }

    return IRendererController::TerminateRendering;
}

bool MasterRenderer::bind_inputs() const
{
    InputBinder input_binder;
    input_binder.bind(*m_project.get_scene());
    return input_binder.get_error_count() == 0;
}

namespace
{
    //
    // Invoke on_frame_begin() / on_frame_end() on all entities of a collection.
    //

    template <typename EntityCollection>
    void invoke_on_frame_begin_collection(
        EntityCollection&       entities,
        const Project&          project)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_begin(*i, project);
    }

    template <typename EntityCollection>
    void invoke_on_frame_begin_collection(
        EntityCollection&       entities,
        const Project&          project,
        UniformInputEvaluator&  input_evaluator)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_begin(*i, project, input_evaluator);
    }

    template <typename EntityCollection>
    void invoke_on_frame_end_collection(
        EntityCollection&       entities,
        const Project&          project)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            invoke_on_frame_end(*i, project);
    }

    //
    // Invoke on_frame_begin() / on_frame_end() on a single entity.
    //

    template <typename Entity>
    void invoke_on_frame_begin(
        Entity&                 entity,
        const Project&          project)
    {
        entity.on_frame_begin(project);
    }

    template <typename Entity>
    void invoke_on_frame_begin(
        Entity&                 entity,
        const Project&          project,
        UniformInputEvaluator&  input_evaluator)
    {
        const void* data =
            input_evaluator.evaluate(entity.get_inputs());

        entity.on_frame_begin(project, data);
    }

    template <typename Entity>
    void invoke_on_frame_end(
        Entity&                 entity,
        const Project&          project)
    {
        entity.on_frame_end(project);
    }

    //
    // Invoke on_frame_begin() / on_frame_end() on all entities of an assembly.
    //

    template <>
    void invoke_on_frame_begin(
        Assembly&               assembly,
        const Project&          project,
        UniformInputEvaluator&  input_evaluator)
    {
        invoke_on_frame_begin_collection(assembly.surface_shaders(), project);
        invoke_on_frame_begin_collection(assembly.bsdfs(), project, input_evaluator);
        invoke_on_frame_begin_collection(assembly.edfs(), project, input_evaluator);
    }

    template <>
    void invoke_on_frame_end(
        Assembly&               assembly,
        const Project&          project)
    {
        invoke_on_frame_end_collection(assembly.edfs(), project);
        invoke_on_frame_end_collection(assembly.bsdfs(), project);
        invoke_on_frame_end_collection(assembly.surface_shaders(), project);
    }
}

void MasterRenderer::on_frame_begin() const
{
    const Scene& scene = *m_project.get_scene();

    Camera* camera = scene.get_camera();
    assert(camera);

    Intersector intersector(m_project.get_trace_context(), false);
    camera->on_frame_begin(m_project, intersector);

    invoke_on_frame_begin_collection(scene.environment_edfs(), m_project);
    invoke_on_frame_begin_collection(scene.environment_shaders(), m_project);

    UniformInputEvaluator input_evaluator;
    invoke_on_frame_begin_collection(scene.assemblies(), m_project, input_evaluator);
}

void MasterRenderer::on_frame_end() const
{
    const Scene& scene = *m_project.get_scene();

    invoke_on_frame_end_collection(scene.assemblies(), m_project);
    invoke_on_frame_end_collection(scene.environment_shaders(), m_project);
    invoke_on_frame_end_collection(scene.environment_edfs(), m_project);

    Camera* camera = scene.get_camera();
    assert(camera);

    camera->on_frame_end(m_project);
}

}   // namespace renderer
