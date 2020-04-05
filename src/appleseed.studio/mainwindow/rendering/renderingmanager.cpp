
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
#include "renderingmanager.h"

// appleseed.studio headers.
#include "mainwindow/rendering/cameracontroller.h"
#include "mainwindow/rendering/qttilecallback.h"
#include "mainwindow/rendering/rendertab.h"
#include "mainwindow/statusbar.h"

// appleseed.qtcommon headers.
#include "widgets/renderwidget.h"

// appleseed.common headers.
#include "application/application.h"
#include "application/progresstilecallback.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/frame.h"
#include "renderer/api/log.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/image/analysis.h"
#include "foundation/image/image.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/iabortswitch.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Qt headers.
#include <QApplication>

// Standard headers.
#include <cassert>

using namespace appleseed::common;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

//
// RenderingManager class implementation.
//

namespace
{
    class MasterRendererThread
      : public QThread
    {
        Q_OBJECT

      public:
        // Constructor.
        MasterRendererThread(
            MasterRenderer&         master_renderer,
            IRendererController&    renderer_controller)
          : m_master_renderer(master_renderer)
          , m_renderer_controller(renderer_controller)
        {
        }

      signals:
        void signal_rendering_failed();

      private:
        MasterRenderer&             m_master_renderer;
        IRendererController&        m_renderer_controller;

        // The entry point for the thread.
        void run() override
        {
            RENDERER_LOG_DEBUG("master renderer thread has started.");

            set_current_thread_name("master_renderer");

            const MasterRenderer::RenderingResult rendering_result =
                m_master_renderer.render(m_renderer_controller);

            if (rendering_result.m_status != MasterRenderer::RenderingResult::Succeeded)
                emit signal_rendering_failed();

            RENDERER_LOG_DEBUG("master renderer thread is ending...");
        }
    };
}

RenderingManager::RenderingManager(StatusBar& status_bar)
  : m_status_bar(status_bar)
  , m_project(nullptr)
  , m_render_tab(nullptr)
{
    Application::initialize_resource_search_paths(m_resource_search_paths);

    //
    // The connections below are using the Qt::BlockingQueuedConnection connection type.
    //
    // They are using a queued connection because the emitting thread is different from
    // the receiving thread (the emitting thread is the master renderer thread, and the
    // receiving thread is the UI thread of the main window (presumably).
    //
    // They are using a blocking queue connection because we need the receiving slot to
    // have returned in the receiving thread before the emitting thread can continue.
    //
    // See https://doc.qt.io/archives/qt-4.8/qt.html#ConnectionType-enum for more details.
    //

    connect(
        &m_renderer_controller, &QtRendererController::signal_frame_begin,
        this, &RenderingManager::slot_frame_begin,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_frame_end,
        this, &RenderingManager::slot_frame_end,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_begin,
        this, &RenderingManager::slot_rendering_begin,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_success,
        this, &RenderingManager::slot_rendering_end,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_abort,
        this, &RenderingManager::slot_rendering_end,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_pause,
        this, &RenderingManager::slot_rendering_pause,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_resume,
        this, &RenderingManager::slot_rendering_resume,
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_success,
        this, &RenderingManager::signal_rendering_end);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_abort,
        this, &RenderingManager::signal_rendering_end);
}

RenderingManager::~RenderingManager()
{
    clear_scheduled_actions();
    clear_sticky_actions();
}

void RenderingManager::start_rendering(
    Project*                    project,
    const ParamArray&           params,
    const RenderingMode         rendering_mode,
    RenderTab*                  render_tab)
{
    m_project = project;
    m_params = params;
    m_rendering_mode = rendering_mode;
    m_render_tab = render_tab;

    m_render_tab->get_render_widget()->start_render();

    TileCallbackCollectionFactory* tile_callback_collection_factory =
        new TileCallbackCollectionFactory();

    tile_callback_collection_factory->insert(
        new QtTileCallbackFactory(
            m_render_tab->get_render_widget()));

    tile_callback_collection_factory->insert(
        new ProgressTileCallbackFactory(
            global_logger(),
            m_params.get_optional<size_t>("passes", 1)));

    m_tile_callback_factory.reset(tile_callback_collection_factory);

    m_master_renderer.reset(
        new MasterRenderer(
            *m_project,
            m_params,
            m_resource_search_paths,
            m_tile_callback_factory.get()));

    m_master_renderer_thread.reset(
        new MasterRendererThread(
            *m_master_renderer,
            m_renderer_controller));

    connect(
        static_cast<MasterRendererThread*>(m_master_renderer_thread.get()), &MasterRendererThread::signal_rendering_failed,
        this, &RenderingManager::slot_frame_end);

    connect(
        static_cast<MasterRendererThread*>(m_master_renderer_thread.get()), &MasterRendererThread::signal_rendering_failed,
        this, &RenderingManager::slot_rendering_failed);

    connect(
        static_cast<MasterRendererThread*>(m_master_renderer_thread.get()), &MasterRendererThread::signal_rendering_failed,
        this, &RenderingManager::signal_rendering_end);

    connect(
        static_cast<MasterRendererThread*>(m_master_renderer_thread.get()), &MasterRendererThread::finished,
        this, &RenderingManager::slot_master_renderer_thread_finished);

    m_master_renderer_thread->start();
}

bool RenderingManager::is_rendering() const
{
    return m_master_renderer.get() != nullptr;
}

bool RenderingManager::is_rendering_paused() const
{
    return m_renderer_controller.get_status() == IRendererController::PauseRendering;
}

void RenderingManager::wait_until_rendering_end()
{
    while (is_rendering())
        QApplication::processEvents();
}

void RenderingManager::abort_rendering()
{
    RENDERER_LOG_INFO("aborting rendering...");
    m_renderer_controller.set_status(IRendererController::AbortRendering);
}

void RenderingManager::restart_rendering()
{
    RENDERER_LOG_DEBUG("restarting rendering...");
    m_renderer_controller.set_status(IRendererController::RestartRendering);
}

void RenderingManager::reinitialize_rendering()
{
    RENDERER_LOG_DEBUG("reinitializing rendering...");
    m_renderer_controller.set_status(IRendererController::ReinitializeRendering);
}

void RenderingManager::pause_rendering()
{
    RENDERER_LOG_INFO("pausing rendering...");
    m_renderer_controller.set_status(IRendererController::PauseRendering);
}

void RenderingManager::resume_rendering()
{
    RENDERER_LOG_INFO("resuming rendering...");
    m_renderer_controller.set_status(IRendererController::ContinueRendering);
}

void RenderingManager::schedule(std::unique_ptr<IScheduledAction> action)
{
    m_scheduled_actions.push_back(action.release());
}

void RenderingManager::schedule_or_execute(std::unique_ptr<IScheduledAction> action)
{
    if (is_rendering())
    {
        // For now, we limit the size of the queue of scheduled actions to 1.
        // Here is why: a scheduled action, when executed, may invalidate one or
        // several other scheduled actions. For instance, a scheduled deletion,
        // when executed, will invalidate all other actions on the same entity
        // (and in particular, other scheduled *deletions* on the same entity).
        // Since we don't check dependencies between scheduled actions, we need
        // to limit the number of scheduled actions to 1 to prevent crashes.
        if (m_scheduled_actions.size() < 1)
        {
            m_scheduled_actions.push_back(action.release());
            reinitialize_rendering();
        }
    }
    else
    {
        (*action)(*m_project);
    }
}

void RenderingManager::clear_scheduled_actions()
{
    for (each<ScheduledActionCollection> i = m_scheduled_actions; i; ++i)
        delete *i;

    m_scheduled_actions.clear();
}

void RenderingManager::set_sticky_action(
    const std::string&               key,
    std::unique_ptr<IStickyAction>   action)
{
    m_sticky_actions[key] = action.release();
}

void RenderingManager::clear_sticky_actions()
{
    for (each<StickyActionCollection> i = m_sticky_actions; i; ++i)
        delete i->second;

    m_sticky_actions.clear();
}

void RenderingManager::slot_abort_rendering()
{
    abort_rendering();
}

void RenderingManager::slot_restart_rendering()
{
    restart_rendering();
}

void RenderingManager::slot_reinitialize_rendering()
{
    reinitialize_rendering();
}

void RenderingManager::print_final_rendering_time()
{
    const double rendering_time = m_project->get_rendering_timer().get_seconds();
    const std::string rendering_time_string = pretty_time(rendering_time, 3);

    RENDERER_LOG_INFO("rendering finished in %s.", rendering_time_string.c_str());

    m_status_bar.set_text("Rendering finished in " + rendering_time_string);
}

void RenderingManager::print_average_luminance()
{
    const double average_luminance = compute_average_luminance(m_project->get_frame()->image());

    RENDERER_LOG_DEBUG(
        "final average luminance is %s.",
        pretty_scalar(average_luminance, 6).c_str());
}

void RenderingManager::print_rms_deviation()
{
    if (!m_project->get_frame()->has_valid_ref_image())
        return;

    const double rmsd = compute_rms_deviation(
        m_project->get_frame()->image(),
        *m_project->get_frame()->ref_image());

    RENDERER_LOG_INFO(
        "final rms deviation is %s.",
        pretty_scalar(rmsd, 6).c_str());
}

void RenderingManager::archive_frame_to_disk()
{
    RENDERER_LOG_INFO("archiving frame to disk...");

    const bf::path autosave_path =
          bf::path(Application::get_root_path())
        / "images" / "autosave";

    m_project->get_frame()->archive(autosave_path.string().c_str());
}

void RenderingManager::run_scheduled_actions()
{
    for (each<ScheduledActionCollection> i = m_scheduled_actions; i; ++i)
    {
        IScheduledAction* action = *i;
        (*action)(*m_project);
        delete action;
    }

    m_scheduled_actions.clear();
}

void RenderingManager::run_sticky_actions()
{
    assert(m_master_renderer.get());

    for (each<StickyActionCollection> i = m_sticky_actions; i; ++i)
    {
        IStickyAction* action = i->second;
        (*action)(*m_master_renderer.get(), *m_project);
    }
}

void RenderingManager::slot_rendering_begin()
{
    assert(m_master_renderer.get());

    run_sticky_actions();
    run_scheduled_actions();

    if (m_rendering_mode == RenderingMode::InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(true);

    m_has_camera_changed = false;

    // Start printing rendering time in the status bar.
    m_status_bar.stop_rendering_time_display();
    m_status_bar.start_rendering_time_display(&m_project->get_rendering_timer());
}

void RenderingManager::slot_rendering_pause()
{
    assert(m_master_renderer.get());

    m_project->get_rendering_timer().pause();
}

void RenderingManager::slot_rendering_resume()
{
    assert(m_master_renderer.get());

    m_project->get_rendering_timer().resume();
}

void RenderingManager::slot_rendering_end()
{
    // Disable camera interaction.
    if (m_rendering_mode == RenderingMode::InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(false);

    // Save the controller target point into the camera.
    m_render_tab->get_camera_controller()->save_camera_target();

    // Stop printing rendering time in the status bar.
    m_status_bar.stop_rendering_time_display();

    print_final_rendering_time();

    if (m_params.get_optional<bool>("print_final_average_luminance", false))
        print_average_luminance();

    print_rms_deviation();

    if (m_params.get_optional<bool>("autosave", true))
        archive_frame_to_disk();
}

void RenderingManager::slot_rendering_failed()
{
    // Disable camera interaction.
    if (m_rendering_mode == RenderingMode::InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(false);

    // Save the controller target point into the camera.
    m_render_tab->get_camera_controller()->save_camera_target();

    // Stop printing rendering time in the status bar.
    m_status_bar.stop_rendering_time_display();
}

void RenderingManager::slot_frame_begin()
{
    // Update the scene's camera before rendering the frame.
    if (m_has_camera_changed)
    {
        m_render_tab->get_camera_controller()->update_camera_transform();
        m_has_camera_changed = false;
    }
}

void RenderingManager::slot_frame_end()
{
    // Ensure that the render widget is up-to-date.
    m_render_tab->get_render_widget()->update();
}

void RenderingManager::slot_camera_change_begin()
{
}

void RenderingManager::slot_camera_changed()
{
    m_has_camera_changed = true;
    restart_rendering();
}

void RenderingManager::slot_camera_change_end()
{
}

void RenderingManager::slot_master_renderer_thread_finished()
{
    m_master_renderer.reset();
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_renderingmanager.cxx"
