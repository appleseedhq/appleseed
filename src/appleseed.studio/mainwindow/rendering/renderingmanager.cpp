
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
#include "renderingmanager.h"

// appleseed.studio headers.
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
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Qt headers.
#include <QApplication>
#include <QTimerEvent>

// Standard headers.
#include <cassert>

using namespace appleseed::shared;
using namespace boost;
using namespace foundation;
using namespace renderer;
using namespace std;

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
      public:
        // Constructor.
        explicit MasterRendererThread(MasterRenderer* master_renderer)
          : m_master_renderer(master_renderer)
        {
        }

      private:
        MasterRenderer* m_master_renderer;

        // The starting point for the thread.
        virtual void run()
        {
            m_master_renderer->render();
        }
    };
}

RenderingManager::RenderingManager(StatusBar& status_bar)
  : m_status_bar(status_bar)
  , m_project(0)
  , m_render_widget(0)
  , m_camera_changed(false)
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
        this, SLOT(slot_frame_begin()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_frame_end()),
        this, SLOT(slot_frame_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_begin()),
        this, SLOT(slot_rendering_begin()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_success()),
        this, SLOT(slot_rendering_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_abort()),
        this, SLOT(slot_rendering_end()),
        Qt::BlockingQueuedConnection);

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_success()),
        this, SIGNAL(signal_rendering_end()));

    connect(
        &m_renderer_controller, SIGNAL(signal_rendering_abort()),
        this, SIGNAL(signal_rendering_end()));
}

RenderingManager::~RenderingManager()
{
    clear_permanent_states();
    clear_delayed_actions();
}

void RenderingManager::start_rendering(
    Project*                    project,
    const ParamArray&           params,
    const bool                  interactive,
    RenderWidget*               render_widget)
{
    m_project = project;
    m_params = params;
    m_render_widget = render_widget;
    m_camera_changed = false;

    if (interactive)
    {
        m_camera_controller.reset(
            new CameraController(
                m_render_widget,
                m_project->get_scene()));

        connect(
            m_camera_controller.get(), SIGNAL(signal_camera_change_begin()),
            this, SLOT(slot_camera_change_begin()));

        connect(
            m_camera_controller.get(), SIGNAL(signal_camera_change_end()),
            this, SLOT(slot_camera_change_end()));

        connect(
            m_camera_controller.get(), SIGNAL(signal_camera_changed()),
            this, SLOT(slot_camera_changed()));

        connect(
            m_camera_controller.get(), SIGNAL(signal_camera_changed()),
            this, SIGNAL(signal_camera_changed()));
    }

    m_tile_callbacks_enabled = true;

    m_tile_callback_factory.reset(
        new QtTileCallbackFactory(
            m_render_widget,
            !interactive,
            m_tile_callbacks_enabled));

    m_master_renderer.reset(
        new MasterRenderer(
            *m_project,
            m_params,
            &m_renderer_controller,
            m_tile_callback_factory.get()));

    m_master_renderer_thread.reset(
        new MasterRendererThread(m_master_renderer.get()));

    m_master_renderer_thread->start();

    // todo: refactor.
    QThread::currentThread()->setPriority(QThread::TimeCriticalPriority);
}

bool RenderingManager::is_rendering() const
{
    return m_master_renderer_thread.get() && m_master_renderer_thread->isRunning();
}

void RenderingManager::wait_until_rendering_end()
{
    while (is_rendering())
        QApplication::processEvents();
}

void RenderingManager::push_delayed_action(auto_ptr<IDelayedAction> action)
{
    m_delayed_actions.push_back(action.release());
}

void RenderingManager::clear_delayed_actions()
{
    for (each<DelayedActionCollection> i = m_delayed_actions; i; ++i)
        delete *i;

    m_delayed_actions.clear();
}

void RenderingManager::set_permanent_state(
    const string&               key,
    auto_ptr<IDelayedAction>    action)
{
    m_permanent_states[key] = action.release();
}

void RenderingManager::clear_permanent_states()
{
    for (each<PermanentStateCollection> i = m_permanent_states; i; ++i)
        delete i->second;

    m_permanent_states.clear();
}

void RenderingManager::abort_rendering()
{
    RENDERER_LOG_DEBUG("aborting rendering...");

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

void RenderingManager::timerEvent(QTimerEvent* event)
{
    if (event->timerId() == m_render_widget_update_timer.timerId())
        m_render_widget->update();
    else QObject::timerEvent(event);
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
    Image final_image(m_project->get_frame()->image());
    m_project->get_frame()->transform_to_output_color_space(final_image);

    const double average_luminance = compute_average_luminance(final_image);

    RENDERER_LOG_DEBUG(
        "final average luminance is %s.",
        pretty_scalar(average_luminance, 6).c_str());
}

void RenderingManager::archive_frame_to_disk()
{
    RENDERER_LOG_INFO("archiving frame to disk...");

    const filesystem::path autosave_path =
          filesystem::path(Application::get_root_path())
        / "images" / "autosave";

    m_project->get_frame()->archive(autosave_path.string().c_str());
}

void RenderingManager::apply_permanent_states()
{
    for (each<PermanentStateCollection> i = m_permanent_states; i; ++i)
    {
        IDelayedAction* action = i->second;
        (*action)(*m_master_renderer.get(), *m_project);
    }
}

void RenderingManager::consume_delayed_actions()
{
    for (each<DelayedActionCollection> i = m_delayed_actions; i; ++i)
    {
        IDelayedAction* action = *i;
        (*action)(*m_master_renderer.get(), *m_project);
        delete action;
    }

    m_delayed_actions.clear();
}

void RenderingManager::slot_rendering_begin()
{
    assert(m_master_renderer.get());

    apply_permanent_states();
    consume_delayed_actions();

    m_rendering_timer.clear();

    const int IdleUpdateRate = 15;  // hertz
    m_render_widget_update_timer.start(1000 / IdleUpdateRate, this);
}

void RenderingManager::slot_rendering_end()
{
    m_render_widget_update_timer.stop();

    print_final_rendering_time();

    if (m_params.get_optional<bool>("print_final_average_luminance", false))
        print_average_luminance();

    if (m_params.get_optional<bool>("autosave", true))
        archive_frame_to_disk();

    // Prevent manipulation of the camera after rendering has ended.
    m_camera_controller.reset();
}

void RenderingManager::slot_frame_begin()
{
    if (m_camera_changed)
    {
        // Update the scene's camera before rendering the frame.
        if (m_camera_controller.get())
            m_camera_controller->update_camera_transform();

        if (m_frozen_display_renderer.get())
            m_frozen_display_renderer->render();

        m_camera_changed = false;
    }

    m_status_bar.start_rendering_time_display(&m_rendering_timer);
    m_rendering_timer.start();
}

void RenderingManager::slot_frame_end()
{
    m_rendering_timer.measure();
    m_status_bar.stop_rendering_time_display();

    m_render_widget->update();
}

void RenderingManager::slot_camera_change_begin()
{
    if (m_params.get_optional<bool>("freeze_display_during_navigation", false))
    {
        m_tile_callbacks_enabled = false;
        m_frozen_display_renderer.reset(
            new FrozenDisplayRenderer(
                get_sampling_context_mode(m_params),
                *m_project->get_scene()->get_camera(),
                *m_project->get_frame(),
                *m_render_widget));
        m_frozen_display_renderer->capture();
    }
}

void RenderingManager::slot_camera_changed()
{
    m_camera_changed = true;
    restart_rendering();
}

void RenderingManager::slot_camera_change_end()
{
    if (m_frozen_display_renderer.get())
    {
        m_frozen_display_renderer.reset();
        m_tile_callbacks_enabled = true;
    }
}

}   // namespace studio
}   // namespace appleseed
