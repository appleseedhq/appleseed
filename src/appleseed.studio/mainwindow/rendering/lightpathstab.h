
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/rendering/cameracontroller.h"
#include "mainwindow/rendering/lightpathspickinghandler.h"
#include "mainwindow/rendering/renderclipboardhandler.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"
#include "widgets/scrollareapanhandler.h"
#include "widgets/widgetzoomhandler.h"

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace appleseed { namespace studio { class LightPathsWidget; } }
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }
class QLabel;
class QPoint;
class QRect;
class QScrollArea;
class QToolBar;
class QToolButton;

namespace appleseed {
namespace studio {

//
// A tab providing an hardware-accelerated visualization of recorded light paths.
//

class LightPathsTab
  : public QWidget
{
    Q_OBJECT

  public:
    LightPathsTab(
        renderer::Project&      project,
        renderer::ParamArray&   settings);

  public slots:
    void slot_entity_picked(const renderer::ScenePicker::PickingResult& result);
    void slot_rectangle_selection(const QRect& rect);

  private slots:
    void slot_light_path_selection_changed(
        const int               selected_light_path_index,
        const int               total_light_paths) const;
    void slot_context_menu(const QPoint& point);
    void slot_save_light_paths();
    void slot_camera_changed();

  private:
    renderer::Project&                                  m_project;
    renderer::ParamArray&                               m_settings;
    LightPathsWidget*                                   m_light_paths_widget;
    QScrollArea*                                        m_scroll_area;
    QToolBar*                                           m_toolbar;
    QToolButton*                                        m_prev_path_button;
    QToolButton*                                        m_next_path_button;
    QLabel*                                             m_info_label;

    std::unique_ptr<qtcommon::WidgetZoomHandler>        m_zoom_handler;
    std::unique_ptr<qtcommon::ScrollAreaPanHandler>     m_pan_handler;
    std::unique_ptr<qtcommon::MouseCoordinatesTracker>  m_mouse_tracker;
    std::unique_ptr<CameraController>                   m_camera_controller;
    std::unique_ptr<LightPathsPickingHandler>           m_screen_space_paths_picking_handler;
    std::unique_ptr<LightPathsPickingHandler>           m_world_space_paths_picking_handler;
    std::unique_ptr<RenderClipboardHandler>             m_clipboard_handler;

    void create_light_paths_widget();
    void create_toolbar();
    void create_scrollarea();
    void recreate_handlers();

};

}   // namespace studio
}   // namespace appleseed
