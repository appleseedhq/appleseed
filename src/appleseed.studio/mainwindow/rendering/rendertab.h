
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERTAB_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERTAB_H

// appleseed.studio headers.
#include "mainwindow/rendering/renderclipboardhandler.h"
#include "mainwindow/rendering/renderregionhandler.h"
#include "mainwindow/rendering/scenepickinghandler.h"
#include "utility/mousecoordinatestracker.h"
#include "utility/scrollareapanhandler.h"
#include "utility/widgetzoomhandler.h"

// Qt headers.
#include <QObject>
#include <QTextEdit>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace appleseed { namespace studio { class RenderWidget; } }
namespace renderer  { class Project; }
class QComboBox;
class QLabel;
class QPoint;
class QRect;
class QScrollArea;
class QToolBar;
class QToolButton;

namespace appleseed {
namespace studio {

//
// A tab wrapping a render widget and its toolbar.
//

class RenderTab
  : public QWidget
{
    Q_OBJECT

  public:
    RenderTab(
        ProjectExplorer&                    project_explorer,
        renderer::Project&                  project);

    void clear();
    void darken();
    void reset_zoom();

    void update();
    void update_size();

    RenderWidget* get_render_widget() const;

    struct State
    {
        WidgetZoomHandler::State            m_zoom_handler_state;
        ScrollAreaPanHandler::State         m_pan_handler_state;
    };

    State save_state() const;
    void load_state(const State& state);

  signals:
    void signal_save_all_aovs();
    void signal_quicksave_all_aovs();
    void signal_set_render_region(const QRect& rect);
    void signal_clear_render_region();
    void signal_render_widget_context_menu(const QPoint& point);
    void signal_reset_zoom();
    void signal_clear_frame();

  private slots:
    void slot_render_widget_context_menu(const QPoint& point);
    void slot_toggle_render_region(const bool checked);
    void slot_set_render_region(const QRect& rect);

  private:
    RenderWidget*                           m_render_widget;
    QScrollArea*                            m_scroll_area;
    QToolBar*                               m_toolbar;
    QToolButton*                            m_save_aovs_button;
    QToolButton*                            m_quick_save_aovs_button;
    QToolButton*                            m_set_render_region_button;
    QToolButton*                            m_clear_render_region_button;
    QToolButton*                            m_reset_zoom_button;
    QToolButton*                            m_clear_frame_button;
    QComboBox*                              m_picking_mode_combo;
    QLabel*                                 m_info_label;
    QTextEdit*                              m_rgb_text;

    ProjectExplorer&                        m_project_explorer;
    renderer::Project&                      m_project;

    std::auto_ptr<WidgetZoomHandler>        m_zoom_handler;
    std::auto_ptr<ScrollAreaPanHandler>     m_pan_handler;
    std::auto_ptr<MouseCoordinatesTracker>  m_mouse_tracker;
    std::auto_ptr<ScenePickingHandler>      m_picking_handler;
    std::auto_ptr<RenderRegionHandler>      m_render_region_handler;
    std::auto_ptr<RenderClipboardHandler>   m_clipboard_handler;

    void create_render_widget();
    void create_toolbar();
    void create_scrollarea();
    void recreate_handlers();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_RENDERTAB_H
