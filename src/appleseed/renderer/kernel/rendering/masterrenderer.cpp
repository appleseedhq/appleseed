
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/entity/onrenderbeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadercompiler.h"
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
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <exception>
#include <new>
#include <string>
#include <vector>

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

        bool is_aborted() override
        {
            // Don't abort on IRendererController::ReinitializeRendering since this causes render setup to abort.
            const IRendererController::Status status = m_renderer_controller.get_status();
            return
                status == IRendererController::TerminateRendering ||
                status == IRendererController::AbortRendering;
        }

      private:
        IRendererController& m_renderer_controller;
    };
}

struct MasterRenderer::Impl
{
    Project&                            m_project;
    ParamArray                          m_params;
    const SearchPaths&                  m_resource_search_paths;

    OIIOErrorHandler*                   m_error_handler;

    OIIOTextureSystem*                  m_texture_system;

    RendererServices*                   m_renderer_services;
    OSLShadingSystem*                   m_shading_system;
    auto_release_ptr<ShaderCompiler>    m_osl_compiler;

    IRendererController*                m_renderer_controller;
    ITileCallbackFactory*               m_tile_callback_factory;

    SerialRendererController*           m_serial_renderer_controller;
    ITileCallbackFactory*               m_serial_tile_callback_factory;

    Display*                            m_display;

    Stopwatch<DefaultWallclockTimer>    m_stopwatch;

    Impl(
        Project&            project,
        const ParamArray&   params,
        const SearchPaths&  resource_search_paths)
      : m_project(project)
      , m_params(params)
      , m_resource_search_paths(resource_search_paths)
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
        IAbortSwitch&               abort_switch)
    {
        // Construct a search paths string from the project's search paths.
        const string project_search_paths =
            to_string(m_project.search_paths().to_string_reversed(SearchPaths::osl_path_separator()));

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

        // Set OIIO search paths.
        string prev_oiio_search_path;
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
        m_renderer_services->initialize(texture_store);

        // Set OSL search paths.
        string prev_osl_search_paths;
        m_shading_system->getattribute("searchpath:shader", prev_osl_search_paths);
        if (prev_osl_search_paths != project_search_paths)
        {
            RENDERER_LOG_INFO("setting osl shader search paths to %s", project_search_paths.c_str());
            m_project.get_scene()->release_optimized_osl_shader_groups();
            m_shading_system->attribute("searchpath:shader", project_search_paths);
        }

        // Initialize the shader compiler, if the OSL headers are found.
        if (m_resource_search_paths.exist("stdosl.h"))
        {
            const APIString stdosl_path = m_resource_search_paths.qualify("stdosl.h");
            RENDERER_LOG_INFO("found OSL headers in %s", stdosl_path.c_str());
            m_osl_compiler = ShaderCompilerFactory::create(stdosl_path.c_str());
        }
        else
            RENDERER_LOG_INFO("OSL headers not found.");

        // Re-optimize shader groups that need updating.
        return
            m_project.get_scene()->create_optimized_osl_shader_groups(
                *m_shading_system,
                m_osl_compiler.get(),
                &abort_switch);
    }

    // Render the project.
    MasterRenderer::RenderingResult render()
    {
        // RenderingResult is initialized to Failed.
        RenderingResult result;

        // Perform basic integrity checks on the scene.
        if (!check_scene())
            return result;

        // Initialize thread-local variables.
        Spectrum::set_mode(get_spectrum_mode(m_params));

        // Reset the frame's render info.
        m_project.get_frame()->render_info().clear();

        try
        {
            // Render.
            m_stopwatch.start();
            result.m_status = do_render();
            m_stopwatch.measure();
            result.m_render_time = m_stopwatch.get_seconds();

            // Insert render time into the frame's render info.
            // Note that the frame entity may have replaced during rendering.
            ParamArray& render_info = m_project.get_frame()->render_info();
            render_info.insert("render_time", result.m_render_time);

            // Don't proceed further if rendering failed.
            if (result.m_status != RenderingResult::Succeeded)
                return result;

            // Post-process.
            m_stopwatch.start();
            postprocess(result);
            m_stopwatch.measure();
            result.m_post_processing_time = m_stopwatch.get_seconds();
            render_info.insert("post_processing_time", result.m_post_processing_time);
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

    // Render a frame until completed or aborted and handle reinitialization events.
    MasterRenderer::RenderingResult::Status do_render()
    {
        while (true)
        {
            m_renderer_controller->on_rendering_begin();

            // Construct an abort switch that will allow to abort initialization.
            RendererControllerAbortSwitch abort_switch(*m_renderer_controller);

            // Expand procedural assemblies before scene entities inputs are bound.
            if (!m_project.get_scene()->expand_procedural_assemblies(m_project, &abort_switch))
            {
                m_renderer_controller->on_rendering_abort();
                return RenderingResult::Aborted;
            }

            // Bind scene entities inputs.
            if (!bind_scene_entities_inputs())
            {
                m_renderer_controller->on_rendering_abort();
                return RenderingResult::Aborted;
            }

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

    // Bind all scene entities inputs. Return true on success, false otherwise.
    bool bind_scene_entities_inputs() const
    {
        InputBinder input_binder(*m_project.get_scene());
        input_binder.bind();
        return input_binder.get_error_count() == 0;
    }

    // Initialize rendering components and render a frame.
    IRendererController::Status initialize_and_render_frame()
    {
        // Construct an abort switch that will allow to abort initialization or rendering.
        RendererControllerAbortSwitch abort_switch(*m_renderer_controller);

        // Create the texture store.
        TextureStore texture_store(
            *m_project.get_scene(),
            m_params.child("texture_store"));

        // Initialize OSL's shading system.
        if (!initialize_osl_shading_system(texture_store, abort_switch) ||
            abort_switch.is_aborted())
        {
            // todo: there is a bug here: if initialize_osl_shading_system() fails, we return
            // the renderer controller's status which is most likely ContinueRendering, or so
            // we start rendering again, in an infinite loop.
            return m_renderer_controller->get_status();
        }

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

        // Report whether Embree is used or not.
#ifdef APPLESEED_WITH_EMBREE
        const bool use_embree = m_params.get_optional<bool>("use_embree", false);
        m_project.set_use_embree(use_embree);
#else
        const bool use_embree = false;
#endif
        if (use_embree)
             RENDERER_LOG_INFO("using Intel Embree ray tracing kernel.");
        else RENDERER_LOG_INFO("using built-in ray tracing kernel.");

        // Updating the trace context causes ray tracing acceleration structures to be updated or rebuilt.
        m_project.update_trace_context();

        // Load the checkpoint if any.
        Frame& frame = *m_project.get_frame();
        if (!frame.load_checkpoint(&(components.get_shading_result_framebuffer_factory())))
            return IRendererController::AbortRendering;

        // Print renderer component settings.
        components.print_settings();

        // Perform pre-render actions. Don't proceed if that failed.
        // Call on_render_begin() on the shading engine after calling it on the scene because
        // it needs to access the scene's render data (to access the scene's bounding box).
        OnRenderBeginRecorder recorder;
        if (!m_project.get_scene()->on_render_begin(m_project, nullptr, recorder, &abort_switch) ||
            !components.get_shading_engine().on_render_begin(m_project, recorder, &abort_switch) ||
            abort_switch.is_aborted())
        {
            recorder.on_render_end(m_project);
            return m_renderer_controller->get_status();
        }

        // Execute the main rendering loop.
        const auto status = render_frame(components, abort_switch);

        // Perform post-render actions.
        recorder.on_render_end(m_project);

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
            // The `on_frame_begin()` method of the renderer controller might alter the scene
            // (e.g. transform the camera), thus it needs to be called before the `on_frame_begin()`
            // of the scene which assumes the scene is up-to-date and ready to be rendered.
            m_renderer_controller->on_frame_begin();

            // Discard recorded light paths.
            m_project.get_light_path_recorder().clear();

            // Perform pre-frame actions. Don't proceed if that failed.
            OnFrameBeginRecorder recorder;
            if (!components.get_shading_engine().on_frame_begin(m_project, recorder, &abort_switch) ||
                !m_project.on_frame_begin(m_project, nullptr, recorder, &abort_switch) ||
                abort_switch.is_aborted())
            {
                recorder.on_frame_end(m_project);
                m_renderer_controller->on_frame_end();
                return IRendererController::AbortRendering;
            }

            // Print settings of key entities.
            m_project.get_frame()->print_settings();
            m_project.get_scene()->get_active_camera()->print_settings();

            IFrameRenderer& frame_renderer = components.get_frame_renderer();
            assert(!frame_renderer.is_rendering());

            // Start rendering the frame.
            frame_renderer.start_rendering();

            // Wait until the the frame is completed or rendering is aborted.
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

            // Perform post-frame actions.
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
    IRendererController::Status wait_for_event(IFrameRenderer& frame_renderer)
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
                    m_renderer_controller->on_rendering_resume();
                    m_stopwatch.resume();
                    is_paused = false;
                }
                break;

              case IRendererController::PauseRendering:
                if (!is_paused)
                {
                    frame_renderer.pause_rendering();
                    m_renderer_controller->on_rendering_pause();
                    m_stopwatch.pause();
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

    void postprocess(const RenderingResult& rendering_result)
    {
        Frame* frame = m_project.get_frame();
        assert(frame != nullptr);

        // Nothing to do if there are no post-processing stages.
        if (frame->post_processing_stages().empty())
            return;

        // Collect post-processing stages.
        vector<PostProcessingStage*> ordered_stages;
        ordered_stages.reserve(frame->post_processing_stages().size());
        for (auto& stage : frame->post_processing_stages())
            ordered_stages.push_back(&stage);

        // Sort post-processing stages in increasing order.
        sort(
            ordered_stages.begin(),
            ordered_stages.end(),
            [](PostProcessingStage* lhs, PostProcessingStage* rhs)
            {
                return lhs->get_order() < rhs->get_order();
            });

        // Detect post-processing stages with equal order.
        size_t previous_stage_index = 0;
        int previous_order = ordered_stages[0]->get_order();
        for (size_t i = 1, e = ordered_stages.size(); i < e; ++i)
        {
            const int order = ordered_stages[i]->get_order();
            if (order == previous_order)
            {
                RENDERER_LOG_WARNING(
                    "post-processing stages \"%s\" and \"%s\" have equal order (%d); results will be unpredictable.",
                    ordered_stages[previous_stage_index]->get_path().c_str(),
                    ordered_stages[i]->get_path().c_str(),
                    order);
            }
        }

        // Execute post-processing stages.
        for (auto stage : ordered_stages)
        {
            RENDERER_LOG_INFO("executing \"%s\" post-processing stage with order %d on frame \"%s\"...",
                stage->get_path().c_str(), stage->get_order(), frame->get_path().c_str());
            stage->execute(*frame);
            invoke_tile_callbacks(*frame);
        }
    }

    void invoke_tile_callbacks(const Frame& frame)
    {
        if (m_tile_callback_factory)
        {
            // Invoke the tile callbacks on all tiles.
            // todo: things would be simpler if the tile callback had a method to refresh a whole frame.
            auto_release_ptr<ITileCallback> tile_callback(m_tile_callback_factory->create());
            const CanvasProperties& frame_props = frame.image().properties();
            for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
            {
                for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
                    tile_callback->on_tile_end(&frame, tx, ty);
            }

            // If all we have is a serial tile callback (and not a true tile callback factory), updates
            // will be enqueued into the SerialRendererController instead of being executed right away,
            // hence we need to manually execute them here, otherwise the render stamp is not guaranteed
            // to be displayed in client applications.
            if (m_serial_renderer_controller != nullptr)
                m_serial_renderer_controller->exec_callbacks();
        }
    }
};

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    const SearchPaths&      resource_search_paths,
    IRendererController*    renderer_controller,
    ITileCallbackFactory*   tile_callback_factory)
  : impl(new Impl(project, params, resource_search_paths))
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
    const SearchPaths&      resource_search_paths,
    IRendererController*    renderer_controller,
    ITileCallback*          tile_callback)
  : impl(new Impl(project, params, resource_search_paths))
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


//
// MasterRenderer::RenderingResult class implementation.
//

MasterRenderer::RenderingResult::RenderingResult()
  : m_status(Failed)
  , m_render_time(0.0)
  , m_post_processing_time(0.0)
{
}

}   // namespace renderer
