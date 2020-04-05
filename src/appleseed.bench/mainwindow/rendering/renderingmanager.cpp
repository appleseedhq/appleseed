
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

// appleseed.bench headers.
#include "mainwindow/rendering/qttilecallback.h"
#include "mainwindow/renderingtimedisplay.h"

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
#include <cstddef>

using namespace appleseed::common;
using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace bench {

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

RenderingManager::RenderingManager(RenderingTimeDisplay& rendering_time_display)
  : m_rendering_time_display(rendering_time_display)
  , m_project(nullptr)
  , m_render_widget(nullptr)
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
        &m_renderer_controller, &QtRendererController::signal_rendering_success,
        this, &RenderingManager::signal_rendering_success);

    connect(
        &m_renderer_controller, &QtRendererController::signal_rendering_abort,
        this, &RenderingManager::signal_rendering_abort);
}

void RenderingManager::start_rendering(
    Project*                    project,
    const ParamArray&           params,
    const RenderingMode         rendering_mode,
    RenderWidget*               render_widget)
{
    m_project = project;
    m_params = params;
    m_rendering_mode = rendering_mode;
    m_render_widget = render_widget;

    m_render_widget->start_render();

    auto tile_callback_collection_factory = new TileCallbackCollectionFactory();

    auto tile_callback_factory = new QtTileCallbackFactory(m_render_widget);
    connect(
        tile_callback_factory, &QtTileCallbackFactory::signal_progressive_frame_update,
        this, &RenderingManager::signal_progressive_frame_update);
    
    tile_callback_collection_factory->insert(tile_callback_factory);

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
        static_cast<MasterRendererThread*>(m_master_renderer_thread.get()), &QThread::finished,
        this, &RenderingManager::slot_master_renderer_thread_finished);

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
    RENDERER_LOG_DEBUG("restarting rendering...");
    m_renderer_controller.set_status(IRendererController::RestartRendering);
}

void RenderingManager::reinitialize_rendering()
{
    RENDERER_LOG_DEBUG("reinitializing rendering...");
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

void RenderingManager::slot_rendering_begin()
{
    assert(m_master_renderer.get());

    // Start printing rendering time.
    m_rendering_time_display.stop();
    m_rendering_time_display.start();
}

void RenderingManager::slot_rendering_end()
{
    // Stop printing rendering time.
    m_rendering_time_display.update();
    m_rendering_time_display.stop();

    const double rendering_time = m_project->get_rendering_timer().get_seconds();
    const std::string rendering_time_string = pretty_time(rendering_time, 3);
    RENDERER_LOG_INFO("rendering finished in %s.", rendering_time_string.c_str());
}

void RenderingManager::slot_rendering_failed()
{
    // Stop printing rendering time.
    m_rendering_time_display.stop();
}

void RenderingManager::slot_frame_begin()
{
}

void RenderingManager::slot_frame_end()
{
    // Ensure that the render widget is up-to-date.
    m_render_widget->update();
}

void RenderingManager::slot_master_renderer_thread_finished()
{
    m_master_renderer.reset();
}

}   // namespace bench
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_renderingmanager.cxx"
