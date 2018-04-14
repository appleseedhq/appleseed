
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
#include "renderer/kernel/lighting/lightpathrecorder.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/oiioerrorhandler.h"
#include "renderer/kernel/rendering/renderercomponents.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
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

namespace
{
    string make_search_path_string(const SearchPaths& search_paths)
    {
        return search_paths.to_string_reversed(SearchPaths::osl_path_separator()).c_str();
    }

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

struct MasterRenderer::Impl
{
    Project&                    m_project;
    ParamArray                  m_params;

    OIIOErrorHandler*           m_error_handler;

    OIIOTextureSystem*          m_texture_system;

    RendererServices*           m_renderer_services;
    OSLShadingSystem*           m_shading_system;

    IRendererController*        m_renderer_controller;
    ITileCallbackFactory*       m_tile_callback_factory;

    SerialRendererController*   m_serial_renderer_controller;
    ITileCallbackFactory*       m_serial_tile_callback_factory;

    Display*                    m_display;

    Impl(
        Project&          project,
        const ParamArray& params)
      : m_project(project)
      , m_params(params)
      , m_serial_renderer_controller(nullptr)
      , m_serial_tile_callback_factory(nullptr)
      , m_display(nullptr)
    {
        m_error_handler = new OIIOErrorHandler();
    #ifndef NDEBUG
        m_error_handler->verbosity(OIIO::ErrorHandler::VERBOSE);
    #endif

        RENDERER_LOG_DEBUG("creating oiio texture system...");
        m_texture_system = OIIOTextureSystemFactory::create(false);
        m_texture_system->attribute("automip", 0);
        m_texture_system->attribute("accept_untiled", 1);
        m_texture_system->attribute("accept_unmipped", 1);
        m_texture_system->attribute("gray_to_rgb", 1);
        m_texture_system->attribute("latlong_up", "y");
        m_texture_system->attribute("flip_t", 1);

        m_renderer_services = new RendererServices(
            m_project,
            reinterpret_cast<OIIO::TextureSystem&>(*m_texture_system));

        RENDERER_LOG_DEBUG("creating osl shading system...");
        m_shading_system = OSLShadingSystemFactory::create(
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

    ~Impl()
    {
        if (m_display)
            m_display->close();

        delete m_serial_tile_callback_factory;
        delete m_serial_renderer_controller;

        RENDERER_LOG_DEBUG("destroying osl shading system...");
        m_project.get_scene()->release_optimized_osl_shader_groups();
        m_shading_system->release();
        delete m_renderer_services;

        const string stats = m_texture_system->getstats();
        const string modified_stats = prefix_all_lines(trim_both(stats), "oiio: ");
        RENDERER_LOG_DEBUG("%s", modified_stats.c_str());

        RENDERER_LOG_DEBUG("destroying oiio texture system...");
        m_texture_system->release();
        delete m_error_handler;
    }

    // Initialize OSL's shading system.
    bool initialize_osl_shading_system(
        TextureStore&               texture_store,
        foundation::IAbortSwitch&   abort_switch)
    {
        // Initialize OIIO.
        const size_t texture_cache_size_bytes =
            m_params.child("texture_store").get_optional<size_t>(
                "max_size",
                TextureStore::get_default_size());
        RENDERER_LOG_INFO(
            "setting oiio texture cache size to %s.",
            pretty_size(texture_cache_size_bytes).c_str());
        const float texture_cache_size_mb =
            static_cast<float>(texture_cache_size_bytes) / (1024 * 1024);
        m_texture_system->attribute("max_memory_MB", texture_cache_size_mb);

        string prev_search_path;
        m_texture_system->getattribute("searchpath", prev_search_path);

        string new_search_path = make_search_path_string(m_project.search_paths());
        if (new_search_path != prev_search_path)
        {
            RENDERER_LOG_INFO("setting oiio search path to %s", new_search_path.c_str());
            m_texture_system->invalidate_all(true);
            m_texture_system->attribute("searchpath", new_search_path);
        }

        // Initialize OSL.
        m_renderer_services->initialize(texture_store);

        m_shading_system->getattribute("searchpath:shader", prev_search_path);

        new_search_path = make_search_path_string(m_project.search_paths());
        if (new_search_path != prev_search_path)
        {
            RENDERER_LOG_INFO("setting osl shader search path to %s", new_search_path.c_str());
            m_project.get_scene()->release_optimized_osl_shader_groups();
            m_shading_system->attribute("searchpath:shader", new_search_path);
        }

        // Re-optimize the shader groups that need updating.
        return
            m_project.get_scene()->create_optimized_osl_shader_groups(
                *m_shading_system,
                &abort_switch);
    }

    // Return true if the scene passes basic integrity checks.
    bool check_scene() const
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

    // Bind all scene entities inputs. Return true on success, false otherwise.
    bool bind_scene_entities_inputs() const
    {
        InputBinder input_binder(*m_project.get_scene());
        input_binder.bind();
        return input_binder.get_error_count() == 0;
    }

    // Render the project.
    MasterRenderer::RenderingResult render()
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
            m_renderer_controller->on_rendering_abort();
            RENDERER_LOG_ERROR("rendering failed (ran out of memory).");
            result.m_status = RenderingResult::Failed;
        }
    #ifdef NDEBUG
        catch (const exception& e)
        {
            m_renderer_controller->on_rendering_abort();
            RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
            result.m_status = RenderingResult::Failed;
        }
        catch (...)
        {
            m_renderer_controller->on_rendering_abort();
            RENDERER_LOG_ERROR("rendering failed (unknown exception).");
            result.m_status = RenderingResult::Failed;
        }
    #endif

        return result;
    }

    // Render a frame until completed or aborted and handle reinitialization events.
    MasterRenderer::RenderingResult::Status do_render()
    {
        while (true)
        {
            m_renderer_controller->on_rendering_begin();

            const IRendererController::Status status = initialize_and_render_frame();

            switch (status)
            {
              case IRendererController::TerminateRendering:
                m_renderer_controller->on_rendering_success();
                return RenderingResult::Succeeded;

              case IRendererController::AbortRendering:
                m_renderer_controller->on_rendering_abort();
                return RenderingResult::Aborted;

              case IRendererController::ReinitializeRendering:
                break;

              assert_otherwise;
            }
        }
    }

    // Initialize rendering components and render a frame.
    IRendererController::Status initialize_and_render_frame()
    {
        // Construct an abort switch that will allow to abort initialization or rendering.
        RendererControllerAbortSwitch abort_switch(*m_renderer_controller);

        // Perform basic integrity checks on the scene.
        if (!check_scene())
            return IRendererController::AbortRendering;

        // Expand all procedural assemblies.
        if (!m_project.get_scene()->expand_procedural_assemblies(m_project, &abort_switch))
            return IRendererController::AbortRendering;

        // Bind entities inputs. This must be done before creating/updating the trace context.
        if (!bind_scene_entities_inputs())
            return IRendererController::AbortRendering;

        // Create the texture store.
        TextureStore texture_store(
            *m_project.get_scene(),
            m_params.child("texture_store"));

        // Initialize OSL's shading system.
        if (!initialize_osl_shading_system(texture_store, abort_switch))
            return IRendererController::AbortRendering;

        // Don't proceed further if initialization was aborted.
        if (abort_switch.is_aborted())
            return m_renderer_controller->get_status();

        // Build or update ray tracing acceleration structures.
        m_project.update_trace_context();

        // Perform pre-render rendering actions.
        if (!m_project.get_scene()->on_render_begin(m_project, &abort_switch))
            return IRendererController::AbortRendering;

        // Create renderer components.
        RendererComponents components(
            m_project,
            m_params,
            m_tile_callback_factory,
            texture_store,
            *m_texture_system,
            *m_shading_system);
        if (!components.create())
            return IRendererController::AbortRendering;

        // Print renderer component settings.
        components.print_settings();

        // Execute the main rendering loop.
        const auto status = render_frame(components, abort_switch);

        // Perform post-render rendering actions.
        m_project.get_scene()->on_render_end(m_project);

        const CanvasProperties& props = m_project.get_frame()->image().properties();
        m_project.get_light_path_recorder().finalize(
            props.m_canvas_width,
            props.m_canvas_height);

        // Print texture store performance statistics.
        RENDERER_LOG_DEBUG("%s", texture_store.get_statistics().to_string().c_str());

        return status;
    }

    // Render a frame until completed or aborted and handle restart events.
    IRendererController::Status render_frame(
        RendererComponents&     components,
        IAbortSwitch&           abort_switch)
    {
        while (true)
        {
            // Discard recorded light paths.
            m_project.get_light_path_recorder().clear();

            // The on_frame_begin() method of the renderer controller might alter the scene
            // (e.g. transform the camera), thus it needs to be called before the on_frame_begin()
            // of the scene which assumes the scene is up-to-date and ready to be rendered.
            m_renderer_controller->on_frame_begin();

            // Perform pre-frame rendering actions. Don't proceed if that failed.
            OnFrameBeginRecorder recorder;
            if (!components.get_shading_engine().on_frame_begin(m_project, recorder, &abort_switch) ||
                !m_project.get_scene()->on_frame_begin(m_project, nullptr, recorder, &abort_switch))
            {
                recorder.on_frame_end(m_project);
                m_renderer_controller->on_frame_end();
                return IRendererController::AbortRendering;
            }

            // Print settings of key entities.
            m_project.get_scene()->get_active_camera()->print_settings();
            m_project.get_frame()->print_settings();

            // Don't proceed with rendering if scene preparation was aborted.
            if (abort_switch.is_aborted())
            {
                recorder.on_frame_end(m_project);
                m_renderer_controller->on_frame_end();
                return m_renderer_controller->get_status();
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

    // Wait until the the frame is completed or rendering is aborted.
    IRendererController::Status wait_for_event(IFrameRenderer& frame_renderer) const
    {
        bool is_paused = false;

        while (true)
        {
            if (!frame_renderer.is_rendering())
                return IRendererController::TerminateRendering;

            const IRendererController::Status status = m_renderer_controller->get_status();

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

            m_renderer_controller->on_progress();

            foundation::sleep(1);   // namespace qualifer required
        }
    }

    void add_render_stamp(const double render_time)
    {
        const Frame* frame = m_project.get_frame();

        if (frame == nullptr)
            return;

        if (!frame->is_render_stamp_enabled())
            return;

        Frame::RenderStampInfo info;
        info.m_render_time = render_time;

        frame->add_render_stamp(info);

        if (m_tile_callback_factory)
        {
            // todo: things would be simpler if the tile callback had a method to refresh a whole frame.
            auto_release_ptr<ITileCallback> tile_callback(m_tile_callback_factory->create());
            const CanvasProperties& frame_props = frame->image().properties();

            for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
            {
                for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                    tile_callback->on_tile_end(frame, tx, ty, 0);
            }
        }
    }
};

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory)
  : impl(new Impl(project, params))
{
    impl->m_renderer_controller = renderer_controller;
    impl->m_tile_callback_factory = tile_callback_factory;

    if (impl->m_tile_callback_factory == nullptr)
    {
        // Try to use the display if there is one in the project
        // and no tile callback factory was specified.
        Display* display = impl->m_project.get_display();
        if (display != nullptr && display->open(impl->m_project))
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
  : impl(new Impl(project, params))
{
    impl->m_renderer_controller =
    impl->m_serial_renderer_controller =
        new SerialRendererController(renderer_controller, tile_callback);

    impl->m_tile_callback_factory =
    impl->m_serial_tile_callback_factory =
        new SerialTileCallbackFactory(impl->m_serial_renderer_controller);
}

MasterRenderer::~MasterRenderer()
{
    delete impl;
}

ParamArray& MasterRenderer::get_parameters()
{
    return impl->m_params;
}

const ParamArray& MasterRenderer::get_parameters() const
{
    return impl->m_params;
}

MasterRenderer::RenderingResult MasterRenderer::render()
{
    return impl->render();
}

}   // namespace renderer
