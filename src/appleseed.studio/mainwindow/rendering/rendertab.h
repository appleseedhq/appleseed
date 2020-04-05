
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/rendering/cameracontroller.h"
#include "mainwindow/rendering/materialdrophandler.h"
#include "mainwindow/rendering/pixelcolortracker.h"
#include "mainwindow/rendering/pixelinspectorhandler.h"
#include "mainwindow/rendering/renderclipboardhandler.h"
#include "mainwindow/rendering/renderregionhandler.h"
#include "mainwindow/rendering/scenepickinghandler.h"

// appleseed.qtcommon headers.
#include "widgets/mousecoordinatestracker.h"
#include "widgets/scrollareapanhandler.h"
#include "widgets/widgetzoomhandler.h"

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace appleseed { namespace qtcommon { class RenderWidget; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace appleseed { namespace studio { class RenderingManager; } }
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
        ProjectExplorer&        project_explorer,
        renderer::Project&      project,
        RenderingManager&       rendering_manager,
        OCIO::ConstConfigRcPtr  ocio_config);

    qtcommon::RenderWidget* get_render_widget() const;
    CameraController* get_camera_controller() const;
    ScenePickingHandler* get_scene_picking_handler() const;

    void set_clear_frame_button_enabled(const bool enabled);
    void set_render_region_buttons_enabled(const bool enabled);

    void clear();
    void darken();
    void reset_zoom();

    void update();
    void update_size();

    struct State
    {
        qtcommon::WidgetZoomHandler::State      m_zoom_handler_state;
        qtcommon::ScrollAreaPanHandler::State   m_pan_handler_state;
    };

    State save_state() const;
    void load_state(const State& state);

  signals:
    void signal_save_frame_and_aovs();
    void signal_quicksave_frame_and_aovs();
    void signal_set_render_region(const QRect& rect);
    void signal_clear_render_region();
    void signal_render_widget_context_menu(const QPoint& point);
    void signal_reset_zoom();
    void signal_clear_frame();

    void signal_camera_change_begin();
    void signal_camera_changed();
    void signal_camera_change_end();

    void signal_entity_picked(renderer::ScenePicker::PickingResult);
    void signal_rectangle_selection(const QRect& rect);

  private slots:
    void slot_render_widget_context_menu(const QPoint& point);
    void slot_toggle_render_region(const bool checked);
    void slot_set_render_region(const QRect& rect);
    void slot_toggle_pixel_inspector(const bool checked);

  private:
    qtcommon::RenderWidget*                             m_render_widget;
    QScrollArea*                                        m_scroll_area;
    QToolBar*                                           m_toolbar;
    QToolButton*                                        m_set_render_region_button;
    QToolButton*                                        m_clear_render_region_button;
    QToolButton*                                        m_clear_frame_button;
    QComboBox*                                          m_picking_mode_combo;
    QLabel*                                             m_info_label;
    QLabel*                                             m_r_label;
    QLabel*                                             m_g_label;
    QLabel*                                             m_b_label;
    QLabel*                                             m_a_label;

    ProjectExplorer&                                    m_project_explorer;
    renderer::Project&                                  m_project;
    RenderingManager&                                   m_rendering_manager;

    std::unique_ptr<qtcommon::WidgetZoomHandler>        m_zoom_handler;
    std::unique_ptr<qtcommon::ScrollAreaPanHandler>     m_pan_handler;
    std::unique_ptr<MaterialDropHandler>                m_material_drop_handler;
    std::unique_ptr<qtcommon::MouseCoordinatesTracker>  m_mouse_tracker;
    std::unique_ptr<PixelColorTracker>                  m_pixel_color_tracker;
    std::unique_ptr<PixelInspectorHandler>              m_pixel_inspector_handler;
    std::unique_ptr<CameraController>                   m_camera_controller;
    std::unique_ptr<ScenePickingHandler>                m_scene_picking_handler;
    std::unique_ptr<RenderRegionHandler>                m_render_region_handler;
    std::unique_ptr<RenderClipboardHandler>             m_clipboard_handler;

    OCIO::ConstConfigRcPtr                              m_ocio_config;

    void create_render_widget();
    void create_toolbar();
    void create_scrollarea();
    void recreate_handlers();
};

}   // namespace studio
}   // namespace appleseed
