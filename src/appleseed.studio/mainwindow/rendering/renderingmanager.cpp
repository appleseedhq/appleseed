
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
#include "mainwindow/rendering/rendertab.h"
#include "mainwindow/rendering/renderwidget.h"
#include "mainwindow/statusbar.h"

// appleseed.shared headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/frame.h"
#include "renderer/api/project.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/image/analysis.h"
#include "foundation/image/image.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Qt headers.
#include <QApplication>

// Standard headers.
#include <cassert>

using namespace appleseed::shared;
using namespace foundation;
using namespace renderer;
using namespace std;
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
        explicit MasterRendererThread(MasterRenderer* master_renderer)
          : m_master_renderer(master_renderer)
        {
        }

      signals:
        void signal_rendering_failed();

      private:
        MasterRenderer* m_master_renderer;

        // The starting point for the thread.
        void run() override
        {
            set_current_thread_name("master_renderer");

            const auto result = m_master_renderer->render();

            if (result.m_status == MasterRenderer::RenderingResult::Failed)
                emit signal_rendering_failed();
        }
    };
}

RenderingManager::RenderingManager(StatusBar& status_bar)
  : m_status_bar(status_bar)
  , m_project(nullptr)
  , m_render_tab(nullptr)
{
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
    // See http://qt-project.org/doc/qt-4.8/qt.html#ConnectionType-enum for more details.
    //

    connect(
        &m_renderer_controller, SIGNAL(signal_frame_begin()),
        SLOT(slot_frame_begin()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_frame_end()),
        SLOT(slot_frame_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_begin()),
        SLOT(slot_rendering_begin()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_success()),
        SLOT(slot_rendering_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_abort()),
        SLOT(slot_rendering_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_success()),
        SIGNAL(signal_rendering_end()));

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_abort()),
        SIGNAL(signal_rendering_end()));
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

    m_tile_callback_factory.reset(
        new QtTileCallbackFactory(
            m_render_tab->get_render_widget()));

    m_master_renderer.reset(
        new MasterRenderer(
            *m_project,
            m_params,
            &m_renderer_controller,
            m_tile_callback_factory.get()));

    m_master_renderer_thread.reset(
        new MasterRendererThread(m_master_renderer.get()));

    connect(
        m_master_renderer_thread.get(), SIGNAL(signal_rendering_failed()),
        SLOT(slot_frame_end()));

    connect(
        m_master_renderer_thread.get(), SIGNAL(signal_rendering_failed()),
        SLOT(slot_rendering_failed()));

    connect(
        m_master_renderer_thread.get(), SIGNAL(signal_rendering_failed()),
        SIGNAL(signal_rendering_end()));

    connect(
        m_master_renderer_thread.get(), SIGNAL(finished()),
        SLOT(slot_master_renderer_thread_finished()));

    m_master_renderer_thread->start();
}

bool RenderingManager::is_rendering() const
{
    return m_master_renderer.get() != nullptr;
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
    m_renderer_controller.set_status(IRendererController::RestartRendering);
}

void RenderingManager::reinitialize_rendering()
{
    m_renderer_controller.set_status(IRendererController::ReinitializeRendering);
}

void RenderingManager::pause_rendering()
{
    m_renderer_controller.set_status(IRendererController::PauseRendering);
}

void RenderingManager::resume_rendering()
{
    m_renderer_controller.set_status(IRendererController::ContinueRendering);
}

void RenderingManager::schedule(unique_ptr<IScheduledAction> action)
{
    m_scheduled_actions.push_back(action.release());
}

void RenderingManager::schedule_or_execute(unique_ptr<IScheduledAction> action)
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
    const string&               key,
    unique_ptr<IStickyAction>   action)
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
    const double rendering_time = m_rendering_timer.get_seconds();
    const string rendering_time_string = pretty_time(rendering_time, 3);

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

    if (m_rendering_mode == InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(true);

    m_rendering_timer.clear();

    m_has_camera_changed = false;
}

void RenderingManager::slot_rendering_end()
{
    if (m_rendering_mode == InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(false);

    // Save the controller target point into the camera when rendering ends.
    m_render_tab->get_camera_controller()->save_camera_target();

    print_final_rendering_time();

    if (m_params.get_optional<bool>("print_final_average_luminance", false))
        print_average_luminance();

    if (m_params.get_optional<bool>("autosave", true))
        archive_frame_to_disk();
}

void RenderingManager::slot_rendering_failed()
{
    if (m_rendering_mode == InteractiveRendering)
        m_render_tab->get_camera_controller()->set_enabled(false);

    // Save the controller target point into the camera when rendering ends.
    m_render_tab->get_camera_controller()->save_camera_target();
}

void RenderingManager::slot_frame_begin()
{
    // Update the scene's camera before rendering the frame.
    if (m_has_camera_changed)
    {
        m_render_tab->get_camera_controller()->update_camera_transform();
        m_has_camera_changed = false;
    }

    // Start printing rendering time in the status bar.
    m_status_bar.start_rendering_time_display(&m_rendering_timer);
    m_rendering_timer.start();
}

void RenderingManager::slot_frame_end()
{
    // Stop printing rendering time in the status bar.
    m_rendering_timer.measure();
    m_status_bar.stop_rendering_time_display();

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
