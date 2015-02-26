
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERINGMANAGER_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERINGMANAGER_H

// appleseed.studio headers.
#include "mainwindow/rendering/frozendisplayrenderer.h"
#include "mainwindow/rendering/qtrenderercontroller.h"
#include "mainwindow/rendering/qttilecallback.h"
#include "mainwindow/rendering/renderingtimer.h"

// appleseed.renderer headers.
#include "renderer/api/rendering.h"
#include "renderer/api/utility.h"

// Qt headers.
#include <QBasicTimer>
#include <QObject>
#include <QThread>

// Standard headers.
#include <map>
#include <memory>
#include <string>
#include <vector>

// Forward declarations.
namespace appleseed { namespace studio { class RenderTab; } }
namespace appleseed { namespace studio { class StatusBar; } }
namespace renderer  { class Project; }
class QTimerEvent;

namespace appleseed {
namespace studio {

class RenderingManager
  : public QObject
{
    Q_OBJECT

  public:
    // Constructor.
    explicit RenderingManager(StatusBar& status_bar);

    // Destructor.
    ~RenderingManager();

    // Start rendering.
    void start_rendering(
        renderer::Project*              project,
        const renderer::ParamArray&     params,
        const bool                      interactive,
        RenderTab*                      render_tab);

    // Return true if currently rendering, false otherwise.
    bool is_rendering() const;

    // Wait until rendering has ended.
    void wait_until_rendering_end();

    // Abort rendering.
    void abort_rendering();

    // Restart rendering.
    void restart_rendering();

    // Reinitialize rendering.
    void reinitialize_rendering();

    // Interface of a delayed action. A delayed action is a procedure
    // that gets executed just before rendering begins.
    class IDelayedAction
    {
      public:
        virtual ~IDelayedAction() {}

        virtual void operator()(
            renderer::MasterRenderer&   master_renderer,
            renderer::Project&          project) = 0;
    };

    // Add a delayed action. All delayed actions are deleted after
    // they are executed, just before rendering begins.
    void push_delayed_action(std::auto_ptr<IDelayedAction> action);

    void clear_delayed_actions();

    void set_permanent_state(
        const std::string&              key,
        std::auto_ptr<IDelayedAction>   action);

    void clear_permanent_states();

  signals:
    void signal_camera_changed();
    void signal_rendering_end();

  public slots:
    void slot_abort_rendering();
    void slot_restart_rendering();
    void slot_reinitialize_rendering();

  private:
    StatusBar&                                  m_status_bar;
    QtRendererController                        m_renderer_controller;

    renderer::Project*                          m_project;
    renderer::ParamArray                        m_params;
    RenderTab*                                  m_render_tab;

    bool                                        m_allow_camera_changes;
    bool                                        m_camera_changed;
    volatile bool                               m_tile_callbacks_enabled;

    std::auto_ptr<QtTileCallbackFactory>        m_tile_callback_factory;
    std::auto_ptr<renderer::MasterRenderer>     m_master_renderer;
    std::auto_ptr<QThread>                      m_master_renderer_thread;

    RenderingTimer                              m_rendering_timer;
    QBasicTimer                                 m_render_widget_update_timer;

    typedef std::vector<IDelayedAction*> DelayedActionCollection;
    typedef std::map<std::string, IDelayedAction*> PermanentStateCollection;

    DelayedActionCollection                     m_delayed_actions;
    PermanentStateCollection                    m_permanent_states;

    std::auto_ptr<FrozenDisplayRenderer>        m_frozen_display_renderer;

    virtual void timerEvent(QTimerEvent* event);

    void print_final_rendering_time();
    void print_average_luminance();
    void archive_frame_to_disk();

    void apply_permanent_states();
    void consume_delayed_actions();

  private slots:
    void slot_rendering_begin();
    void slot_rendering_end();
    void slot_frame_begin();
    void slot_frame_end();
    void slot_camera_change_begin();
    void slot_camera_changed();
    void slot_camera_change_end();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERINGMANAGER_H
