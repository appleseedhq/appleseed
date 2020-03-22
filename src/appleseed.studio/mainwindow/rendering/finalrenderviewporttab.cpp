
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

// Interface header.
#include "finalrenderviewporttab.h"

// appleseed.studio headers.
#include "mainwindow/project/projectexplorer.h"
#include "mainwindow/rendering/lightpathspickinghandler.h"
#include "mainwindow/rendering/renderingmanager.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/project.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// Qt headers.
#include <QComboBox>
#include <QGridLayout>
#include <QLabel>
#include <QLayout>
#include <QScrollArea>
#include <QSize>
#include <QString>
#include <Qt>
#include <QToolBar>
#include <QToolButton>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace OCIO = OCIO_NAMESPACE;

namespace appleseed {
namespace studio {

FinalRenderViewportTab::FinalRenderViewportTab(
    ProjectExplorer&                    project_explorer,
    Project&                            project,
    RenderingManager&                   rendering_manager,
    LightPathsManager&                  light_paths_manager,
    OCIO::ConstConfigRcPtr              ocio_config)
    : ViewportTab(
        project,
        rendering_manager,
        ocio_config)
    , m_project_explorer(project_explorer)
    , m_light_paths_manager(light_paths_manager)
{
    setObjectName("final_render_viewport_tab");
    setLayout(new QGridLayout());
    layout()->setSpacing(0);
    layout()->setMargin(0);

    create_viewport_canvas();
    create_toolbar();
    create_scrollarea();

    recreate_handlers();

    layout()->addWidget(m_toolbar);
    layout()->addWidget(m_light_paths_viewport_toolbar->toolbar());
    layout()->addWidget(m_scroll_area);

    get_viewport_canvas()->set_base_layer(ViewportCanvas::BaseLayer::FinalRender);
}

ViewportCanvas* FinalRenderViewportTab::get_viewport_canvas() const
{
    return m_viewport_canvas;
}

void FinalRenderViewportTab::set_clear_frame_button_enabled(const bool enabled)
{
    m_clear_frame_button->setEnabled(enabled);
}

void FinalRenderViewportTab::set_render_region_buttons_enabled(const bool enabled)
{
    m_set_render_region_button->setEnabled(enabled);
    m_clear_render_region_button->setEnabled(enabled);
}

void FinalRenderViewportTab::render_began()
{
    ViewportTab::render_began();

    m_light_paths_viewport_toolbar.get()->reset(&m_project);
}

void FinalRenderViewportTab::update_size()
{
    ViewportTab::update_size();

    m_set_render_region_button->setChecked(false);

    recreate_handlers();
}

void FinalRenderViewportTab::on_tab_selected()
{
    const bool display_light_paths = m_light_paths_manager.should_display_light_paths();
    m_light_paths_viewport_toolbar->set_enabled(display_light_paths);
    m_light_paths_toggle_button->setChecked(display_light_paths);
}

CameraController* FinalRenderViewportTab::get_camera_controller() const
{
    return m_camera_controller.get();
}

ScenePickingHandler* FinalRenderViewportTab::get_scene_picking_handler() const
{
    return m_scene_picking_handler.get();
}

void FinalRenderViewportTab::slot_camera_changed()
{
    m_viewport_canvas->get_light_paths_layer()->set_transform(m_camera_controller->get_transform());
    m_viewport_canvas->get_gl_scene_layer()->set_transform(m_camera_controller->get_transform());
    update();
}

void FinalRenderViewportTab::slot_toggle_render_region(const bool checked)
{
    m_scene_picking_handler->set_enabled(!checked);
    m_viewport_selection_handler->set_mode(
        checked
            ? ViewportRegionSelectionHandler::RenderRegionMode
            : ViewportRegionSelectionHandler::RectangleSelectionMode);
}

void FinalRenderViewportTab::slot_toggle_light_paths(const bool checked)
{
    m_light_paths_picking_handler->set_enabled(checked);
    m_light_paths_viewport_toolbar->set_enabled(checked);
    m_scene_picking_handler->set_enabled(!checked);
    m_light_paths_manager.display_light_paths(checked);
}

void FinalRenderViewportTab::slot_toggle_pixel_inspector(const bool checked)
{
    m_pixel_inspector_handler->set_enabled(checked);
    m_pixel_inspector_handler->update_tooltip_visibility();
}

void FinalRenderViewportTab::slot_set_render_region(const QRect& rect)
{
    m_set_render_region_button->setChecked(false);
    emit signal_set_render_region(rect);
}

void FinalRenderViewportTab::slot_viewport_canvas_context_menu(const QPoint& point)
{
    emit signal_viewport_canvas_context_menu(m_viewport_canvas->mapToGlobal(point));
}

void FinalRenderViewportTab::slot_clear_frame()
{
    m_light_paths_toggle_button->setChecked(false);
    m_light_paths_toggle_button->setEnabled(false);
    m_light_paths_picking_handler->set_enabled(false);
    m_light_paths_viewport_toolbar->set_enabled(false);
    m_scene_picking_handler->set_enabled(false);
    m_light_paths_manager.clear_light_paths_selection();

    emit signal_clear_frame();
}

void FinalRenderViewportTab::create_viewport_canvas()
{
    const CanvasProperties& props = m_project.get_frame()->image().properties();

    m_viewport_canvas =
        new ViewportCanvas(
            m_project,
            props.m_canvas_width,
            props.m_canvas_height,
            m_ocio_config,
            m_light_paths_manager,
            this);

    m_viewport_canvas->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_viewport_canvas, &ViewportCanvas::customContextMenuRequested,
        this, &FinalRenderViewportTab::slot_viewport_canvas_context_menu);

    m_viewport_canvas->setMouseTracking(true);
}

void FinalRenderViewportTab::create_toolbar()
{
    // Create the light path toolbar.
    m_light_paths_viewport_toolbar.reset(
        new LightPathsViewportToolbar(
            this,
            &m_project,
            m_light_paths_manager));

    // Create the render toolbar.
    m_toolbar = new QToolBar();
    m_toolbar->setObjectName("render_toolbar");
    m_toolbar->setIconSize(QSize(18, 18));

    // Display Light Paths button.
    m_light_paths_toggle_button = new QToolButton();
    m_light_paths_toggle_button->setText("Display Light Paths Overlay");
    m_light_paths_toggle_button->setCheckable(true);
    connect(
        m_light_paths_toggle_button, &QToolButton::toggled,
        this, &FinalRenderViewportTab::slot_toggle_light_paths);
    m_toolbar->addWidget(m_light_paths_toggle_button);

    m_toolbar->addSeparator();

    // Save Frame and AOVs button.
    QToolButton* save_aovs_button = new QToolButton();
    save_aovs_button->setIcon(load_icons("rendertab_save_all_aovs"));
    save_aovs_button->setToolTip("Save Frame and AOVs...");
    connect(
        save_aovs_button, &QToolButton::clicked,
        this, &FinalRenderViewportTab::signal_save_frame_and_aovs);
    m_toolbar->addWidget(save_aovs_button);

    // Quicksave Frame and AOVs button.
    QToolButton* quicksave_aovs_button = new QToolButton();
    quicksave_aovs_button->setIcon(load_icons("rendertab_quicksave_all_aovs"));
    quicksave_aovs_button->setToolTip("Quicksave Frame and AOVs");
    connect(
        quicksave_aovs_button, &QToolButton::clicked,
        this, &FinalRenderViewportTab::signal_quicksave_frame_and_aovs);
    m_toolbar->addWidget(quicksave_aovs_button);

    m_toolbar->addSeparator();

    // Set Render Region button.
    m_set_render_region_button = new QToolButton();
    m_set_render_region_button->setIcon(load_icons("rendertab_set_render_region"));
    m_set_render_region_button->setShortcut(Qt::Key_R);
    m_set_render_region_button->setToolTip(combine_name_and_shortcut("Set Render Region", m_set_render_region_button->shortcut()));
    m_set_render_region_button->setCheckable(true);
    connect(
        m_set_render_region_button, &QToolButton::toggled,
        this, &FinalRenderViewportTab::slot_toggle_render_region);
    m_toolbar->addWidget(m_set_render_region_button);

    // Clear Render Region button.
    m_clear_render_region_button = new QToolButton();
    m_clear_render_region_button->setIcon(load_icons("rendertab_clear_render_region"));
    m_clear_render_region_button->setShortcut(Qt::Key_C);
    m_clear_render_region_button->setToolTip(combine_name_and_shortcut("Clear Render Region", m_clear_render_region_button->shortcut()));
    connect(
        m_clear_render_region_button, &QToolButton::clicked,
        this, &FinalRenderViewportTab::signal_clear_render_region);
    m_toolbar->addWidget(m_clear_render_region_button);

    // Clear Frame button.
    m_clear_frame_button = new QToolButton();
    m_clear_frame_button->setIcon(load_icons("rendertab_clear_frame"));
    m_clear_frame_button->setShortcut(Qt::Key_X);
    m_clear_frame_button->setToolTip(combine_name_and_shortcut("Clear Frame", m_clear_frame_button->shortcut()));
    connect(
        m_clear_frame_button, &QToolButton::clicked,
        this, &FinalRenderViewportTab::slot_clear_frame);
    m_toolbar->addWidget(m_clear_frame_button);

    m_toolbar->addSeparator();

    // Reset Zoom button.
    QToolButton* reset_zoom_button = new QToolButton();
    reset_zoom_button->setIcon(load_icons("rendertab_reset_zoom"));
    reset_zoom_button->setShortcut(Qt::Key_Asterisk);
    reset_zoom_button->setToolTip(combine_name_and_shortcut("Reset Zoom", reset_zoom_button->shortcut()));
    connect(
        reset_zoom_button, &QToolButton::clicked,
        this, &FinalRenderViewportTab::signal_reset_zoom);
    m_toolbar->addWidget(reset_zoom_button);

    m_toolbar->addSeparator();

    // Pixel Inspector button.
    QToolButton* pixel_inspector_button = new QToolButton();
    pixel_inspector_button->setIcon(load_icons("rendertab_toggle_pixel_inspector"));
    pixel_inspector_button->setShortcut(Qt::Key_I);
    pixel_inspector_button->setToolTip(combine_name_and_shortcut("Toggle Pixel Inspector", pixel_inspector_button->shortcut()));
    pixel_inspector_button->setCheckable(true);
    pixel_inspector_button->setChecked(false);
    connect(
        pixel_inspector_button, &QToolButton::toggled,
        this, &FinalRenderViewportTab::slot_toggle_pixel_inspector);
    m_toolbar->addWidget(pixel_inspector_button);

    m_toolbar->addSeparator();

    // Create the label preceding the picking mode combobox.
    QLabel* picking_mode_label = new QLabel("Picking Mode:");
    picking_mode_label->setObjectName("picking_mode_label");
    m_toolbar->addWidget(picking_mode_label);

    // Create the picking mode combobox.
    // The combo will be populated by the ScenePickingHandler instantiated below.
    m_picking_mode_combo = new QComboBox();
    m_picking_mode_combo->setObjectName("picking_mode_combo");
    m_toolbar->addWidget(m_picking_mode_combo);

    m_toolbar->addSeparator();

    // Create the label preceding the display combobox.
    QLabel* display_label = new QLabel("Display Transform:");
    display_label->setObjectName("display_label");
    m_toolbar->addWidget(display_label);

    // Create the display combobox.
    m_display_transform_combo = new QComboBox();
    m_display_transform_combo->setObjectName("display_combo");
    {
        const char* display_name = m_ocio_config->getDefaultDisplay();
        const std::string default_transform = m_ocio_config->getDefaultView(display_name);

        int default_index = 0;
        for (int i = 0, e = m_ocio_config->getNumViews(display_name); i < e; ++i)
        {
            const char* name = m_ocio_config->getView(display_name, i);
            m_display_transform_combo->addItem(name);

            if (default_transform == name)
                default_index = i;
        }

        m_display_transform_combo->setCurrentIndex(default_index);
    }
    m_toolbar->addWidget(m_display_transform_combo);
    connect(
        m_display_transform_combo, qOverload<const QString&>(&QComboBox::currentIndexChanged),
        m_viewport_canvas, &ViewportCanvas::slot_display_transform_changed);

    // Add stretchy spacer.
    // This places interactive widgets on the left and info on the right.
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    m_toolbar->addWidget(spacer);

    // Create a label to display various information such as mouse coordinates, etc.
    m_info_label = new QLabel();
    m_info_label->setObjectName("info_label");
    m_toolbar->addWidget(m_info_label);

    m_toolbar->addSeparator();

    // Create labels to display RGBA values.

    m_r_label = new QLabel();
    m_r_label->setObjectName("r_label");
    m_r_label->setScaledContents(true);
    m_toolbar->addWidget(m_r_label);

    m_g_label = new QLabel();
    m_g_label->setObjectName("g_label");
    m_g_label->setScaledContents(true);
    m_toolbar->addWidget(m_g_label);

    m_b_label = new QLabel();
    m_b_label->setObjectName("b_label");
    m_b_label->setScaledContents(true);
    m_toolbar->addWidget(m_b_label);

    m_a_label = new QLabel();
    m_a_label->setObjectName("a_label");
    m_a_label->setScaledContents(true);
    m_toolbar->addWidget(m_a_label);
}

void FinalRenderViewportTab::create_scrollarea()
{
    // Encapsulate the render widget into another widget that adds a margin around it.
    QWidget* render_widget_wrapper = new QWidget();
    render_widget_wrapper->setObjectName("render_widget_wrapper");
    render_widget_wrapper->setLayout(new QGridLayout());
    render_widget_wrapper->layout()->setSizeConstraint(QLayout::SetFixedSize);
    render_widget_wrapper->layout()->setContentsMargins(20, 20, 20, 20);
    render_widget_wrapper->layout()->addWidget(m_viewport_canvas);

    // Wrap the render widget in a scroll area.
    m_scroll_area = new QScrollArea();
    m_scroll_area->setObjectName("render_widget_scrollarea");
    m_scroll_area->setAlignment(Qt::AlignCenter);
    m_scroll_area->setWidget(render_widget_wrapper);
}

void FinalRenderViewportTab::recreate_handlers()
{
    // Handler for zooming the render widget in and out with the keyboard or the mouse wheel.
    m_zoom_handler.reset(
        new WidgetZoomHandler(
            m_scroll_area,
            m_viewport_canvas));

    // Handler for panning the render widget with the mouse.
    m_pan_handler.reset(
        new ScrollAreaPanHandler(
            m_scroll_area));

    // Handler for tracking and displaying mouse coordinates.
    m_mouse_tracker.reset(
        new MouseCoordinatesTracker(
            m_viewport_canvas,
            m_info_label));

    // Handler for tracking and displaying the color of the pixel under the mouse cursor.
    m_pixel_color_tracker.reset(
        new PixelColorTracker(
            m_viewport_canvas,
            m_r_label,
            m_g_label,
            m_b_label,
            m_a_label,
            *m_mouse_tracker.get(),
            m_project));

    // Handler for pixel inspection in the render widget.
    m_pixel_inspector_handler.reset(
        new PixelInspectorHandler(
            m_viewport_canvas,
            *m_mouse_tracker.get(),
            m_project));

    // Camera handler.
    m_camera_controller.reset(
        new CameraController(
            m_viewport_canvas,
            m_project));
    connect(
        m_camera_controller.get(), &CameraController::signal_camera_change_begin,
        this, &FinalRenderViewportTab::signal_camera_change_begin);
    connect(
        m_camera_controller.get(), &CameraController::signal_camera_change_end,
        this, &FinalRenderViewportTab::signal_camera_change_end);
    connect(
        m_camera_controller.get(), &CameraController::signal_camera_changed,
        this, &FinalRenderViewportTab::signal_camera_changed);
    connect(
        m_camera_controller.get(), &CameraController::signal_camera_changed,
        this, &FinalRenderViewportTab::slot_camera_changed);
    connect(
        &m_project_explorer, &ProjectExplorer::signal_frame_modified,
        m_camera_controller.get(), &CameraController::slot_frame_modified);

    // Handler for picking scene entities in the render widget.
    m_scene_picking_handler.reset(
        new ScenePickingHandler(
            m_viewport_canvas,
            m_picking_mode_combo,
            *m_mouse_tracker.get(),
            m_project_explorer,
            m_project));
    connect(
        m_scene_picking_handler.get(), &ScenePickingHandler::signal_entity_picked,
        this, &FinalRenderViewportTab::signal_entity_picked);
    connect(
        m_scene_picking_handler.get(), &ScenePickingHandler::signal_entity_picked,
        m_camera_controller.get(), &CameraController::slot_entity_picked);

    // Light paths picking handler.
    m_light_paths_picking_handler.reset(
        new LightPathsPickingHandler(
                m_light_paths_manager,
                m_viewport_canvas,
                m_project));
    m_light_paths_picking_handler->set_enabled(false);

    // Clipboard handler.
    m_clipboard_handler.reset(new RenderClipboardHandler(m_viewport_canvas, m_viewport_canvas));

    // Material drop handler.
    m_material_drop_handler.reset(
        new MaterialDropHandler(
            m_project,
            m_rendering_manager));

    // Handler for setting render regions with the mouse.
    m_viewport_selection_handler.reset(
        new ViewportRegionSelectionHandler(
            m_viewport_canvas,
            *m_mouse_tracker.get()));
    connect(
        m_viewport_selection_handler.get(), &ViewportRegionSelectionHandler::signal_rectangle_selection,
        m_light_paths_picking_handler.get(), &LightPathsPickingHandler::slot_rectangle_selection);
    connect(
        m_viewport_selection_handler.get(), &ViewportRegionSelectionHandler::signal_render_region,
        this, &FinalRenderViewportTab::slot_set_render_region);
    connect(
        m_viewport_selection_handler.get(), &ViewportRegionSelectionHandler::signal_render_region,
        this, &FinalRenderViewportTab::slot_set_render_region);

    // Set initial state.
    m_pixel_inspector_handler->set_enabled(false);
    m_camera_controller->set_enabled(false);
    m_scene_picking_handler->set_enabled(true);

    connect(
        m_viewport_canvas, &ViewportCanvas::signal_material_dropped,
        m_material_drop_handler.get(), &MaterialDropHandler::slot_material_dropped);
}

void FinalRenderViewportTab::set_light_paths_toggle_enabled(const bool enabled)
{
    if (!enabled)
        m_light_paths_toggle_button->setChecked(false);

    m_light_paths_toggle_button->setDisabled(!enabled);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_finalrenderviewporttab.cxx"
