
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "masterrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#ifdef APPLESEED_WITH_OIIO
#include "renderer/kernel/rendering/oiiocomponents.h"
#endif
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/rendering/oslcomponents.h"
#include "renderer/kernel/rendering/rendererservices.h"
#endif
#include "renderer/kernel/rendering/renderercomponents.h"
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <exception>
#include <string>

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
    // RAII-style handling of display plugins.
    class CloseDisplayPluginOnScopeExit
      : public NonCopyable
    {
      public:
        CloseDisplayPluginOnScopeExit()
          : m_display(0)
        {
        }

        ~CloseDisplayPluginOnScopeExit()
        {
            if (m_display)
                m_display->close();
        }

        void set_display(Display* display)
        {
            m_display = display;
        }

      private:
        Display* m_display;
    };

    // An abort switch whose abort status is determined by a renderer::IRendererController.
    class RendererControllerAbortSwitch
      : public IAbortSwitch
    {
      public:
        explicit RendererControllerAbortSwitch(IRendererController& renderer_controller)
          : m_renderer_controller(renderer_controller)
        {
        }

        virtual bool is_aborted() const APPLESEED_OVERRIDE
        {
            const IRendererController::Status status = m_renderer_controller.get_status();
            return
                status != IRendererController::ContinueRendering &&
                status != IRendererController::RestartRendering;
        }

      private:
        IRendererController& m_renderer_controller;
    };
}

IRendererController::Status MasterRenderer::initialize_and_render_frame_sequence()
{
    assert(m_project.get_scene());
    assert(m_project.get_frame());

    // Construct an abort switch based on the renderer controller.
    RendererControllerAbortSwitch abort_switch(*m_renderer_controller);

    // We start by binding entities inputs. This must be done before creating/updating the trace context.
    if (!bind_scene_entities_inputs())
        return IRendererController::AbortRendering;

    m_project.create_aov_images();
    m_project.update_trace_context();
    m_project.get_frame()->print_settings();

    // Create the texture store.
    TextureStore texture_store(
        *m_project.get_scene(),
        m_params.child("texture_store"));

#ifdef APPLESEED_WITH_OIIO

    // Initialize OIIO.
    OIIOComponents oiio_components(
        m_project,
        m_params.child("texture_store"));

#endif

#ifdef APPLESEED_WITH_OSL

    // Initialize OSL.
    OSLComponents osl_components(
        m_project,
        texture_store,
        oiio_components.get_texture_system());

    // Compile OSL shaders.
    if (!osl_components.compile_osl_shaders(&abort_switch))
        return IRendererController::AbortRendering;

    // Don't proceed further if rendering was aborted.
    if (abort_switch.is_aborted())
        return m_renderer_controller->get_status();

#endif

    // If needed, open the display plugin.
    CloseDisplayPluginOnScopeExit close_display;
    ITileCallbackFactory* tile_callback_factory = m_tile_callback_factory;
    if (tile_callback_factory == 0)
    {
        if (Display* display = m_project.get_display())
        {
            if (!display->open(m_project))
                return IRendererController::AbortRendering;

            close_display.set_display(display);
            tile_callback_factory = display->get_tile_callback_factory();
        }
    }

    // Create the renderer components.
    RendererComponents components(
        m_project,
        m_params,
        tile_callback_factory,
        texture_store
#ifdef APPLESEED_WITH_OIIO
        , oiio_components.get_texture_system()
#endif
#ifdef APPLESEED_WITH_OSL
        , osl_components.get_shading_system()
#endif
        );
    if (!components.initialize())
        return IRendererController::AbortRendering;

    // Execute the main rendering loop.
    const IRendererController::Status status =
        render_frame_sequence(
            components.get_frame_renderer()
#ifdef APPLESEED_WITH_OSL
            , osl_components.get_renderer_services()
#endif
            , abort_switch);

    // Print texture store performance statistics.
    RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

    return status;
}

IRendererController::Status MasterRenderer::render_frame_sequence(
    IFrameRenderer&         frame_renderer
#ifdef APPLESEED_WITH_OSL
    , RendererServices&     renderer_services
#endif
    , IAbortSwitch&         abort_switch)
{
    while (true)
    {
        assert(!frame_renderer.is_rendering());

        // The on_frame_begin() method of the renderer controller might alter the scene
        // (e.g. transform the camera), thus it needs to be called before the on_frame_begin()
        // of the scene which assumes the scene is up-to-date and ready to be rendered.
        m_renderer_controller->on_frame_begin();

        // Prepare the scene for rendering. Don't proceed if that failed.
        if (!m_project.get_scene()->on_frame_begin(m_project, &abort_switch))
        {
            m_renderer_controller->on_frame_end();
            return IRendererController::AbortRendering;
        }

        // Don't proceed with rendering if scene preparation was aborted.
        if (abort_switch.is_aborted())
        {
            m_renderer_controller->on_frame_end();
            return m_renderer_controller->get_status();
        }

#ifdef APPLESEED_WITH_OSL
        renderer_services.initialize();
#endif

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

IRendererController::Status MasterRenderer::wait_for_event(IFrameRenderer& frame_renderer) const
{
    while (true)
    {
        if (!frame_renderer.is_rendering())
            return IRendererController::TerminateRendering;

        const IRendererController::Status status = m_renderer_controller->get_status();

        if (status != IRendererController::ContinueRendering)
            return status;

        m_renderer_controller->on_progress();

        foundation::sleep(1);   // namespace qualifer required
    }
}

bool MasterRenderer::bind_scene_entities_inputs() const
{
    InputBinder input_binder;
    input_binder.bind(*m_project.get_scene());
    return input_binder.get_error_count() == 0;
}

}   // namespace renderer
