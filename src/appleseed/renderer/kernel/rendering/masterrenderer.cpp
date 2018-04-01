
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/renderercomponents.h"
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <cassert>
#include <exception>
#include <new>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// MasterRenderer class implementation.
//

struct MasterRenderer::Impl
{
    IRendererController*        m_renderer_controller;
    ITileCallbackFactory*       m_tile_callback_factory;

    SerialRendererController*   m_serial_renderer_controller;
    ITileCallbackFactory*       m_serial_tile_callback_factory;

    Display*                    m_display;
};

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory)
  : BaseRenderer(project, params)
  , impl(new Impl())
{
    impl->m_renderer_controller = renderer_controller;
    impl->m_tile_callback_factory = tile_callback_factory;

    impl->m_serial_renderer_controller = nullptr;
    impl->m_serial_tile_callback_factory = nullptr;

    impl->m_display = nullptr;

    if (impl->m_tile_callback_factory == nullptr)
    {
        // Try to use the display if there is one in the project
        // and no tile callback factory was specified.
        Display* display = m_project.get_display();
        if (display != nullptr && display->open(m_project))
        {
            impl->m_tile_callback_factory = display->get_tile_callback_factory();
            impl->m_display = display;
        }
    }
}

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallback*          tile_callback)
  : BaseRenderer(project, params)
  , impl(new Impl())
{
    impl->m_renderer_controller =
    impl->m_serial_renderer_controller =
        new SerialRendererController(renderer_controller, tile_callback);

    impl->m_tile_callback_factory =
    impl->m_serial_tile_callback_factory =
        new SerialTileCallbackFactory(impl->m_serial_renderer_controller);

    impl->m_display = nullptr;
}

MasterRenderer::~MasterRenderer()
{
    if (impl->m_display)
        impl->m_display->close();

    delete impl->m_serial_tile_callback_factory;
    delete impl->m_serial_renderer_controller;
    delete impl;
}

MasterRenderer::RenderingResult MasterRenderer::render()
{
    // Initialize thread-local variables.
    Spectrum::set_mode(get_spectrum_mode(m_params));

    RenderingResult result;

    try
    {
        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        result.m_status = do_render();

        stopwatch.measure();
        result.m_render_time = stopwatch.get_seconds();

        if (result.m_status == RenderingResult::Succeeded)
            add_render_stamp(result.m_render_time);

        return result;
    }
    catch (const bad_alloc&)
    {
        impl->m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (ran out of memory).");
        result.m_status = RenderingResult::Failed;
    }
#ifdef NDEBUG
    catch (const exception& e)
    {
        impl->m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
        result.m_status = RenderingResult::Failed;
    }
    catch (...)
    {
        impl->m_renderer_controller->on_rendering_abort();
        RENDERER_LOG_ERROR("rendering failed (unknown exception).");
        result.m_status = RenderingResult::Failed;
    }
#endif

    return result;
}

MasterRenderer::RenderingResult::Status MasterRenderer::do_render()
{
    while (true)
    {
        impl->m_renderer_controller->on_rendering_begin();

        const IRendererController::Status status = initialize_and_render_frame_sequence();

        switch (status)
        {
          case IRendererController::TerminateRendering:
            impl->m_renderer_controller->on_rendering_success();
            return RenderingResult::Succeeded;

          case IRendererController::AbortRendering:
            impl->m_renderer_controller->on_rendering_abort();
            return RenderingResult::Aborted;

          case IRendererController::ReinitializeRendering:
            break;

          assert_otherwise;
        }
    }
}

namespace
{
    // An abort switch whose abort status is determined by a renderer::IRendererController.
    class RendererControllerAbortSwitch
      : public IAbortSwitch
    {
      public:
        explicit RendererControllerAbortSwitch(IRendererController& renderer_controller)
          : m_renderer_controller(renderer_controller)
        {
        }

        bool is_aborted() const override
        {
            const IRendererController::Status status = m_renderer_controller.get_status();
            return
                status == IRendererController::TerminateRendering ||
                status == IRendererController::AbortRendering ||
                status == IRendererController::ReinitializeRendering;
        }

      private:
        IRendererController& m_renderer_controller;
    };
}

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence()
{
    // Construct an abort switch that will allow to abort initialization or rendering.
    RendererControllerAbortSwitch abort_switch(*impl->m_renderer_controller);

    // Perform basic integrity checks on the scene.
    if (!check_scene())
        return IRendererController::AbortRendering;

    // Expand all procedural assemblies.
    if (!m_project.get_scene()->expand_procedural_assemblies(m_project, &abort_switch))
        return IRendererController::AbortRendering;

    // Bind entities inputs. This must be done before creating/updating the trace context.
    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    // Build or update ray tracing acceleration structures.
    m_project.update_trace_context();

    // Create the texture store.
    TextureStore texture_store(
        *m_project.get_scene(),
        m_params.child("texture_store"));

    // Initialize OSL's shading system.
    if (!initialize_osl_shading_system(texture_store, abort_switch))
        return IRendererController::AbortRendering;

    // Don't proceed further if initialization was aborted.
    if (abort_switch.is_aborted())
        return impl->m_renderer_controller->get_status();

    // Perform pre-render rendering actions. Don't proceed if that failed.
    if (!m_project.get_scene()->on_render_begin(m_project, &abort_switch))
        return IRendererController::AbortRendering;

    // Create renderer components.
    RendererComponents components(
        m_project,
        m_params,
        impl->m_tile_callback_factory,
        texture_store,
        *m_texture_system,
        *m_shading_system);
    if (!components.create())
        return IRendererController::AbortRendering;

    // Print renderer component settings.
    components.print_settings();

    // Execute the main rendering loop.
    const IRendererController::Status status =
        render_frame_sequence(components, abort_switch);

    // Perform post-render rendering actions.
    m_project.get_scene()->on_render_end(m_project);

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

    return status;
}

bool MasterRenderer::check_scene() const
{
    if (m_project.get_scene() == nullptr)
    {
        RENDERER_LOG_ERROR("project does not contain a scene.");
        return false;
    }

    if (m_project.get_frame() == nullptr)
    {
        RENDERER_LOG_ERROR("project does not contain a frame.");
        return false;
    }

    if (m_project.get_uncached_active_camera() == nullptr)
    {
        RENDERER_LOG_ERROR("no active camera in project.");
        return false;
    }

    return true;
}

bool MasterRenderer::bind_scene_entities_inputs() const
{
    InputBinder input_binder(*m_project.get_scene());
    input_binder.bind();
    return input_binder.get_error_count() == 0;
}

IRendererController::Status MasterRenderer::render_frame_sequence(
    RendererComponents&     components,
    IAbortSwitch&           abort_switch)
{
    while (true)
    {
        // The on_frame_begin() method of the renderer controller might alter the scene
        // (e.g. transform the camera), thus it needs to be called before the on_frame_begin()
        // of the scene which assumes the scene is up-to-date and ready to be rendered.
        impl->m_renderer_controller->on_frame_begin();

        // Perform pre-frame rendering actions. Don't proceed if that failed.
        OnFrameBeginRecorder recorder;
        if (!components.get_shading_engine().on_frame_begin(m_project, recorder, &abort_switch) ||
            !m_project.get_scene()->on_frame_begin(m_project, nullptr, recorder, &abort_switch))
        {
            recorder.on_frame_end(m_project);
            impl->m_renderer_controller->on_frame_end();
            return IRendererController::AbortRendering;
        }

        // Print settings of key entities.
        m_project.get_scene()->get_active_camera()->print_settings();
        m_project.get_frame()->print_settings();

        // Don't proceed with rendering if scene preparation was aborted.
        if (abort_switch.is_aborted())
        {
            recorder.on_frame_end(m_project);
            impl->m_renderer_controller->on_frame_end();
            return impl->m_renderer_controller->get_status();
        }

        IFrameRenderer& frame_renderer = components.get_frame_renderer();
        assert(!frame_renderer.is_rendering());

        frame_renderer.start_rendering();

        const IRendererController::Status status = wait_for_event(frame_renderer);

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

        // Perform post-frame rendering actions
        recorder.on_frame_end(m_project);
        impl->m_renderer_controller->on_frame_end();

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

IRendererController::Status MasterRenderer::wait_for_event(IFrameRenderer& frame_renderer) const
{
    bool is_paused = false;

    while (true)
    {
        if (!frame_renderer.is_rendering())
            return IRendererController::TerminateRendering;

        const IRendererController::Status status = impl->m_renderer_controller->get_status();

        switch (status)
        {
          case IRendererController::ContinueRendering:
            if (is_paused)
            {
                frame_renderer.resume_rendering();
                is_paused = false;
            }
            break;

          case IRendererController::PauseRendering:
            if (!is_paused)
            {
                frame_renderer.pause_rendering();
                is_paused = true;
            }
            break;

          default:
            return status;
        }

        impl->m_renderer_controller->on_progress();

        foundation::sleep(1);   // namespace qualifer required
    }
}

void MasterRenderer::add_render_stamp(const double render_time)
{
    const Frame* frame = m_project.get_frame();

    if (frame == nullptr)
        return;

    if (!frame->is_render_stamp_enabled())
        return;

    Frame::RenderStampInfo info;
    info.m_render_time = render_time;

    frame->add_render_stamp(info);

    if (impl->m_tile_callback_factory)
    {
        // todo: things would be simpler if the tile callback had a method to refresh a whole frame.
        auto_release_ptr<ITileCallback> tile_callback(impl->m_tile_callback_factory->create());
        const CanvasProperties& frame_props = frame->image().properties();

        for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                tile_callback->on_tile_end(frame, tx, ty);
        }
    }
}

}   // namespace renderer
