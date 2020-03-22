
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Kevin Masson, The appleseedhq Organization
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
#include "mainwindow/rendering/lightpathsmanager.h"
#include "mainwindow/rendering/viewporttab.h"

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Forward declarations.
namespace appleseed { namespace studio { class LightPathsPickingHandler; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace appleseed { namespace studio { class RenderingManager; } }
namespace renderer  { class Project; }
class QToolButton;

namespace appleseed {
namespace studio {

class FinalRenderViewportTab
  : public ViewportTab
{
    Q_OBJECT

  public:
    FinalRenderViewportTab(
        ProjectExplorer&                    project_explorer,
        renderer::Project&                  project,
        RenderingManager&                   rendering_manager,
        LightPathsManager&                  light_paths_manager,
        OCIO::ConstConfigRcPtr              ocio_config);

    ViewportCanvas* get_viewport_canvas() const override;

    void set_light_paths_toggle_enabled(const bool enabled);

    void render_began() override;
    void update_size() override;
    void on_tab_selected() override;

    CameraController* get_camera_controller() const;
    ScenePickingHandler* get_scene_picking_handler() const;

    void set_clear_frame_button_enabled(const bool enabled);
    void set_render_region_buttons_enabled(const bool enabled);

  signals:
    void signal_save_frame_and_aovs();
    void signal_quicksave_frame_and_aovs();
    void signal_set_render_region(const QRect& rect);
    void signal_clear_render_region();
    void signal_reset_zoom();
    void signal_clear_frame();
    void signal_viewport_canvas_context_menu(const QPoint& point);

    void signal_camera_change_begin();
    void signal_camera_changed();
    void signal_camera_change_end();

    void signal_entity_picked(renderer::ScenePicker::PickingResult);
    void signal_rectangle_selection(const QRect& rect);

  private slots:
    void slot_camera_changed();
    void slot_toggle_pixel_inspector(const bool checked);
    void slot_toggle_render_region(const bool checked);
    void slot_toggle_light_paths(const bool checked);
    void slot_set_render_region(const QRect& rect);
    void slot_viewport_canvas_context_menu(const QPoint& point);
    void slot_clear_frame();

  private:
    ProjectExplorer&                                    m_project_explorer;
    RenderingManager&                                   m_rendering_manager;
    LightPathsManager&                                  m_light_paths_manager;
    OCIO::ConstConfigRcPtr                              m_ocio_config;

    ViewportCanvas*                                     m_viewport_canvas;
    QToolButton*                                        m_set_render_region_button;
    QToolButton*                                        m_clear_render_region_button;
    QToolButton*                                        m_clear_frame_button;
    QScrollArea*                                        m_scroll_area;
    QToolBar*                                           m_toolbar;
    QToolButton*                                        m_light_paths_toggle_button;
    QComboBox*                                          m_picking_mode_combo;
    QComboBox*                                          m_display_transform_combo;
    QComboBox*                                          m_base_layer_combo;
    QLabel*                                             m_info_label;
    QLabel*                                             m_r_label;
    QLabel*                                             m_g_label;
    QLabel*                                             m_b_label;
    QLabel*                                             m_a_label;

    std::unique_ptr<MaterialDropHandler>                m_material_drop_handler;
    std::unique_ptr<qtcommon::MouseCoordinatesTracker>  m_mouse_tracker;
    std::unique_ptr<PixelColorTracker>                  m_pixel_color_tracker;
    std::unique_ptr<PixelInspectorHandler>              m_pixel_inspector_handler;
    std::unique_ptr<CameraController>                   m_camera_controller;
    std::unique_ptr<ScenePickingHandler>                m_scene_picking_handler;
    std::unique_ptr<ViewportRegionSelectionHandler>     m_viewport_selection_handler;
    std::unique_ptr<RenderClipboardHandler>             m_clipboard_handler;

    std::unique_ptr<LightPathsViewportToolbar>          m_light_paths_viewport_toolbar;
    std::unique_ptr<LightPathsPickingHandler>           m_light_paths_picking_handler;

    void create_viewport_canvas();
    void create_toolbar();
    void create_scrollarea();
    void recreate_handlers();
};

}   // namespace studio
}   // namespace appleseed
