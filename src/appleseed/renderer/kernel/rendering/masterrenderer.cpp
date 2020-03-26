
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
#include "renderer/device/cpu/cpurenderdevice.h"
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/lightpathrecorder.h"
#include "renderer/kernel/rendering/iframerenderer.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/kernel/rendering/renderercontrollercollection.h"
#include "renderer/kernel/rendering/serialrenderercontroller.h"
#include "renderer/kernel/rendering/serialtilecallback.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/entity/onframebeginrecorder.h"
#include "renderer/modeling/entity/onrenderbeginrecorder.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/renderingtimer.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <exception>
#include <memory>
#include <new>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// MasterRenderer class implementation.
//

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
    SearchPaths                         m_resource_search_paths;
    ITileCallbackFactory*               m_tile_callback_factory;
    ITileCallback*                      m_tile_callback;

    Display*                            m_display;
    SerialRendererController*           m_serial_renderer_controller;
    ITileCallbackFactory*               m_serial_tile_callback_factory;

    std::unique_ptr<IRenderDevice>      m_render_device;

    Impl(
        Project&                        project,
        const ParamArray&               params,
        const SearchPaths&              resource_search_paths,
        ITileCallbackFactory*           tile_callback_factory)
      : m_project(project)
      , m_params(params)
      , m_resource_search_paths(resource_search_paths)
      , m_tile_callback_factory(tile_callback_factory)
      , m_tile_callback(nullptr)
      , m_display(nullptr)
      , m_serial_renderer_controller(nullptr)
      , m_serial_tile_callback_factory(nullptr)
    {
        if (m_tile_callback_factory == nullptr)
        {
            // Try to use the display if there is one in the project
            // and no tile callback factory was specified.
            Display* display = m_project.get_display();
            if (display != nullptr && display->open(m_project))
            {
                m_tile_callback_factory = display->get_tile_callback_factory();
                m_display = display;
            }
        }
    }

    Impl(
        Project&                        project,
        const ParamArray&               params,
        const SearchPaths&              resource_search_paths,
        ITileCallback*                  tile_callback)
      : m_project(project)
      , m_params(params)
      , m_resource_search_paths(resource_search_paths)
      , m_tile_callback_factory(nullptr)
      , m_tile_callback(tile_callback)
      , m_display(nullptr)
      , m_serial_renderer_controller(nullptr)
      , m_serial_tile_callback_factory(nullptr)
    {
    }

    ~Impl()
    {
        if (m_display)
            m_display->close();

        delete m_serial_tile_callback_factory;
        delete m_serial_renderer_controller;
    }

    // Render the project.
    MasterRenderer::RenderingResult render(IRendererController& renderer_controller)
    {
        // RenderingResult is initialized to Failed.
        RenderingResult result;

        // Perform basic integrity checks on the scene.
        if (!check_scene())
            return result;

        // Update the render device if needed.
        const std::string device_name = get_render_device(m_params);
        if (device_name == "cpu")
        {
            if (dynamic_cast<const CPURenderDevice*>(m_render_device.get()) == nullptr)
                m_render_device.reset(new CPURenderDevice(m_project, m_params));
        }
        else
        {
            RENDERER_LOG_ERROR("unknown render device: %s.", device_name.c_str());
            return result;
        }

        // Initialize thread-local variables.
        Spectrum::set_mode(get_spectrum_mode(m_params));

        // Reset the frame's render info.
        m_project.get_frame()->render_info().clear();

        // If a single tile callback was provided, wrap the provided renderer controller
        // and single tile callback into their serial counterparts.
        if (m_tile_callback != nullptr)
        {
            m_serial_renderer_controller =
                new SerialRendererController(&renderer_controller, m_tile_callback);
            m_serial_tile_callback_factory =
                new SerialTileCallbackFactory(m_serial_renderer_controller);
            m_tile_callback_factory = m_serial_tile_callback_factory;
        }

        try
        {
            // Render.
            result.m_status =
                do_render(
                    m_serial_renderer_controller != nullptr
                        ? *m_serial_renderer_controller
                        : renderer_controller);

            // Retrieve frame's render info. Note that the frame entity may have been replaced during rendering.
            ParamArray& render_info = m_project.get_frame()->render_info();

            // Insert rendering time into frame's render info.
            render_info.insert("render_time", m_project.get_rendering_timer().get_seconds());

            // Don't proceed further if rendering failed.
            if (result.m_status != RenderingResult::Succeeded)
                return result;

            // Post-process.
            RenderingTimer stopwatch;
            stopwatch.start();
            postprocess();
            stopwatch.measure();

            // Insert post-processing time into frame's render info.
            render_info.insert("post_processing_time", stopwatch.get_seconds());
        }
        catch (const std::bad_alloc&)
        {
            renderer_controller.on_rendering_abort();
            RENDERER_LOG_ERROR("rendering failed (ran out of memory).");
            result.m_status = RenderingResult::Failed;
        }
#ifdef NDEBUG
        catch (const std::exception& e)
        {
            renderer_controller.on_rendering_abort();
            RENDERER_LOG_ERROR("rendering failed (%s).", e.what());
            result.m_status = RenderingResult::Failed;
        }
        catch (...)
        {
            renderer_controller.on_rendering_abort();
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
    MasterRenderer::RenderingResult::Status do_render(IRendererController& renderer_controller)
    {
        while (true)
        {
            renderer_controller.on_rendering_begin();

            // Construct an abort switch that will allow to abort initialization.
            RendererControllerAbortSwitch abort_switch(renderer_controller);

            // Expand procedural assemblies before scene entities inputs are bound.
            if (!m_project.get_scene()->expand_procedural_assemblies(m_project, &abort_switch))
            {
                renderer_controller.on_rendering_abort();
                return RenderingResult::Aborted;
            }

            // Bind scene entities inputs.
            if (!bind_scene_entities_inputs())
            {
                renderer_controller.on_rendering_abort();
                return RenderingResult::Aborted;
            }

            const IRendererController::Status status = initialize_and_render_frame(renderer_controller);

            switch (status)
            {
              case IRendererController::TerminateRendering:
                renderer_controller.on_rendering_success();
                return RenderingResult::Succeeded;

              case IRendererController::AbortRendering:
                renderer_controller.on_rendering_abort();
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

    // Initialize render device and render a frame.
    IRendererController::Status initialize_and_render_frame(IRendererController& renderer_controller)
    {
        // Construct an abort switch that will allow to abort initialization or rendering.
        RendererControllerAbortSwitch abort_switch(renderer_controller);

        // Let scene entities perform their pre-render actions. Don't proceed if that failed.
        // This is done before creating renderer components because renderer components need
        // to access the scene's render data such as the scene's bounding box.
        OnRenderBeginRecorder recorder;
        if (!m_project.get_scene()->on_render_begin(m_project, nullptr, recorder, &abort_switch) ||
            abort_switch.is_aborted())
        {
            recorder.on_render_end(m_project);
            return renderer_controller.get_status();
        }

        // Initialize the render device.
        const bool success =
            m_render_device->initialize(
                m_resource_search_paths,
                m_tile_callback_factory,
                abort_switch);
        if (!success || abort_switch.is_aborted())
        {
            recorder.on_render_end(m_project);

            // If it wasn't an abort, it was a failure.
            return
                abort_switch.is_aborted()
                    ? renderer_controller.get_status()
                    : IRendererController::AbortRendering;
        }

        // Print render device settings.
        m_render_device->print_settings();

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

        // Updating the device scene causes ray tracing acceleration structures to be updated or rebuilt.
        if (!m_render_device->build_or_update_scene())
        {
            recorder.on_render_end(m_project);
            return IRendererController::AbortRendering;
        }

        // Load the checkpoint if any.
        Frame& frame = *m_project.get_frame();
        const size_t pass_count = m_params.get_optional<size_t>("passes", 1);
        if (!m_render_device->load_checkpoint(frame, pass_count))
        {
            recorder.on_render_end(m_project);
            return IRendererController::AbortRendering;
        }

        // Print settings of key entities.
        // todo: move to Project::on_render_begin()?
        m_project.get_frame()->print_settings();
        m_project.get_scene()->get_render_data().m_active_camera->print_settings();

        // Perform pre-render actions.
        if (!m_render_device->on_render_begin(recorder, &abort_switch) ||
            abort_switch.is_aborted())
        {
            recorder.on_render_end(m_project);
            return renderer_controller.get_status();
        }

        // Execute the main rendering loop.
        const auto status = render_frame(renderer_controller, abort_switch);

        // Perform post-render actions.
        recorder.on_render_end(m_project);

        // End light path recording.
        const CanvasProperties& props = m_project.get_frame()->image().properties();
        m_project.get_light_path_recorder().finalize(
            props.m_canvas_width,
            props.m_canvas_height);

        return status;
    }

    // Render a frame until completed or aborted and handle restart events.
    IRendererController::Status render_frame(
        IRendererController&    renderer_controller,
        IAbortSwitch&           abort_switch)
    {
        // Combine the provided renderer controller and the (optional) frame renderer's controller into one.
        RendererControllerCollection combined_renderer_controller;
        combined_renderer_controller.insert(&renderer_controller);
        if (m_render_device->get_frame_renderer_controller())
            combined_renderer_controller.insert(m_render_device->get_frame_renderer_controller());

        while (true)
        {
            // The `on_frame_begin()` method of the renderer controller might alter the scene
            // (e.g. transform the camera), thus it needs to be called before the `on_frame_begin()`
            // of the scene which assumes the scene is up-to-date and ready to be rendered.
            combined_renderer_controller.on_frame_begin();

            // Discard recorded light paths.
            // todo: move to Project::on_frame_begin()?
            m_project.get_light_path_recorder().clear();

            // Perform pre-frame actions. Don't proceed if that failed.
            OnFrameBeginRecorder recorder;
            if (!m_render_device->on_frame_begin(recorder, &abort_switch) ||
                !m_project.on_frame_begin(m_project, nullptr, recorder, &abort_switch) ||
                abort_switch.is_aborted())
            {
                recorder.on_frame_end(m_project);
                combined_renderer_controller.on_frame_end();
                return IRendererController::AbortRendering;
            }

            // Render the frame.
            const IRendererController::Status status =
                m_render_device->render_frame(
                    m_tile_callback_factory,
                    combined_renderer_controller,
                    abort_switch);

            // Perform post-frame actions.
            recorder.on_frame_end(m_project);
            combined_renderer_controller.on_frame_end();

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

    void postprocess()
    {
        Frame* frame = m_project.get_frame();
        assert(frame != nullptr);

        // Nothing to do if there are no post-processing stages.
        if (frame->post_processing_stages().empty())
            return;

        // Collect post-processing stages.
        std::vector<PostProcessingStage*> ordered_stages;
        ordered_stages.reserve(frame->post_processing_stages().size());
        for (PostProcessingStage& stage : frame->post_processing_stages())
            ordered_stages.push_back(&stage);

        // Sort post-processing stages in increasing order.
        std::sort(
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
        for (PostProcessingStage* stage : ordered_stages)
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
    ITileCallbackFactory*   tile_callback_factory)
  : impl(new Impl(project, params, resource_search_paths, tile_callback_factory))
{
}

MasterRenderer::MasterRenderer(
    Project&                project,
    const ParamArray&       params,
    const SearchPaths&      resource_search_paths,
    ITileCallback*          tile_callback)
  : impl(new Impl(project, params, resource_search_paths, tile_callback))
{
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

MasterRenderer::RenderingResult MasterRenderer::render(IRendererController& renderer_controller)
{
    return impl->render(renderer_controller);
}


//
// MasterRenderer::RenderingResult class implementation.
//

MasterRenderer::RenderingResult::RenderingResult()
  : m_status(Failed)
{
}

}   // namespace renderer
