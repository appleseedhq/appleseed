
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
#include "rendertab.h"

// appleseed.studio headers.
#include "mainwindow/project/projectexplorer.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"
#include "widgets/renderwidget.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>

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

// Standard headers.
#include <string>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;
namespace OCIO = OCIO_NAMESPACE;

namespace appleseed {
namespace studio {

//
// RenderTab class implementation.
//

RenderTab::RenderTab(
    ProjectExplorer&        project_explorer,
    Project&                project,
    RenderingManager&       rendering_manager,
    OCIO::ConstConfigRcPtr  ocio_config)
  : m_project_explorer(project_explorer)
  , m_project(project)
  , m_rendering_manager(rendering_manager)
  , m_ocio_config(ocio_config)
{
    setObjectName("render_widget_tab");
    setLayout(new QGridLayout());
    layout()->setSpacing(0);
    layout()->setMargin(0);

    create_render_widget();
    create_toolbar();
    create_scrollarea();

    layout()->addWidget(m_toolbar);
    layout()->addWidget(m_scroll_area);

    recreate_handlers();
}

RenderWidget* RenderTab::get_render_widget() const
{
    return m_render_widget;
}

CameraController* RenderTab::get_camera_controller() const
{
    return m_camera_controller.get();
}

ScenePickingHandler* RenderTab::get_scene_picking_handler() const
{
    return m_scene_picking_handler.get();
}

void RenderTab::set_clear_frame_button_enabled(const bool enabled)
{
    m_clear_frame_button->setEnabled(enabled);
}

void RenderTab::set_render_region_buttons_enabled(const bool enabled)
{
    m_set_render_region_button->setEnabled(enabled);
    m_clear_render_region_button->setEnabled(enabled);
}

void RenderTab::clear()
{
    m_render_widget->clear();
    m_render_widget->repaint();
}

void RenderTab::darken()
{
    m_render_widget->multiply(0.2f);
}

void RenderTab::reset_zoom()
{
    m_zoom_handler->reset_zoom();
}

void RenderTab::update()
{
    m_render_widget->update();
}

void RenderTab::update_size()
{
    m_set_render_region_button->setChecked(false);

    const CanvasProperties& props = m_project.get_frame()->image().properties();

    m_render_widget->resize(
        props.m_canvas_width,
        props.m_canvas_height);

    recreate_handlers();
}

RenderTab::State RenderTab::save_state() const
{
    State state;
    state.m_zoom_handler_state = m_zoom_handler->save_state();
    state.m_pan_handler_state = m_pan_handler->save_state();
    return state;
}

void RenderTab::load_state(const State& state)
{
    // The order matters here.
    m_zoom_handler->load_state(state.m_zoom_handler_state);
    m_pan_handler->load_state(state.m_pan_handler_state);
}

void RenderTab::slot_render_widget_context_menu(const QPoint& point)
{
    emit signal_render_widget_context_menu(m_render_widget->mapToGlobal(point));
}

void RenderTab::slot_toggle_render_region(const bool checked)
{
    m_scene_picking_handler->set_enabled(!checked);
    m_render_region_handler->set_mode(
        checked
            ? RenderRegionHandler::RenderRegionMode
            : RenderRegionHandler::RectangleSelectionMode);
}

void RenderTab::slot_set_render_region(const QRect& rect)
{
    m_set_render_region_button->setChecked(false);
    emit signal_set_render_region(rect);
}

void RenderTab::slot_toggle_pixel_inspector(const bool checked)
{
    m_pixel_inspector_handler->set_enabled(checked);
    m_pixel_inspector_handler->update_tooltip_visibility();
}

void RenderTab::create_render_widget()
{
    const CanvasProperties& props = m_project.get_frame()->image().properties();

    m_render_widget =
        new RenderWidget(
            props.m_canvas_width,
            props.m_canvas_height,
            m_ocio_config);

    m_render_widget->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(
        m_render_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        SLOT(slot_render_widget_context_menu(const QPoint&)));

    m_render_widget->setMouseTracking(true);
}

void RenderTab::create_toolbar()
{
    // Create the render toolbar.
    m_toolbar = new QToolBar();
    m_toolbar->setObjectName("render_toolbar");
    m_toolbar->setIconSize(QSize(18, 18));

    // Save Frame and AOVs button.
    QToolButton* save_aovs_button = new QToolButton();
    save_aovs_button->setIcon(load_icons("rendertab_save_all_aovs"));
    save_aovs_button->setToolTip("Save Frame and AOVs...");
    connect(
        save_aovs_button, SIGNAL(clicked()),
        SIGNAL(signal_save_frame_and_aovs()));
    m_toolbar->addWidget(save_aovs_button);

    // Quicksave Frame and AOVs button.
    QToolButton* quicksave_aovs_button = new QToolButton();
    quicksave_aovs_button->setIcon(load_icons("rendertab_quicksave_all_aovs"));
    quicksave_aovs_button->setToolTip("Quicksave Frame and AOVs");
    connect(
        quicksave_aovs_button, SIGNAL(clicked()),
        SIGNAL(signal_quicksave_frame_and_aovs()));
    m_toolbar->addWidget(quicksave_aovs_button);

    m_toolbar->addSeparator();

    // Set Render Region button.
    m_set_render_region_button = new QToolButton();
    m_set_render_region_button->setIcon(load_icons("rendertab_set_render_region"));
    m_set_render_region_button->setShortcut(Qt::Key_R);
    m_set_render_region_button->setToolTip(combine_name_and_shortcut("Set Render Region", m_set_render_region_button->shortcut()));
    m_set_render_region_button->setCheckable(true);
    connect(
        m_set_render_region_button, SIGNAL(toggled(bool)),
        SLOT(slot_toggle_render_region(const bool)));
    m_toolbar->addWidget(m_set_render_region_button);

    // Clear Render Region button.
    m_clear_render_region_button = new QToolButton();
    m_clear_render_region_button->setIcon(load_icons("rendertab_clear_render_region"));
    m_clear_render_region_button->setShortcut(Qt::Key_C);
    m_clear_render_region_button->setToolTip(combine_name_and_shortcut("Clear Render Region", m_clear_render_region_button->shortcut()));
    connect(
        m_clear_render_region_button, SIGNAL(clicked()),
        SIGNAL(signal_clear_render_region()));
    m_toolbar->addWidget(m_clear_render_region_button);

    // Clear Frame button.
    m_clear_frame_button = new QToolButton();
    m_clear_frame_button->setIcon(load_icons("rendertab_clear_frame"));
    m_clear_frame_button->setShortcut(Qt::Key_X);
    m_clear_frame_button->setToolTip(combine_name_and_shortcut("Clear Frame", m_clear_frame_button->shortcut()));
    connect(
        m_clear_frame_button, SIGNAL(clicked()),
        SIGNAL(signal_clear_frame()));
    m_toolbar->addWidget(m_clear_frame_button);

    m_toolbar->addSeparator();

    // Reset Zoom button.
    QToolButton* reset_zoom_button = new QToolButton();
    reset_zoom_button->setIcon(load_icons("rendertab_reset_zoom"));
    reset_zoom_button->setShortcut(Qt::Key_Asterisk);
    reset_zoom_button->setToolTip(combine_name_and_shortcut("Reset Zoom", reset_zoom_button->shortcut()));
    connect(
        reset_zoom_button, SIGNAL(clicked()),
        SIGNAL(signal_reset_zoom()));
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
        pixel_inspector_button, SIGNAL(toggled(bool)),
        SLOT(slot_toggle_pixel_inspector(const bool)));
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
    QLabel* display_label = new QLabel("Display:");
    display_label->setObjectName("display_label");
    m_toolbar->addWidget(display_label);

    // Create the display combobox.
    QComboBox* m_display_transform_combo = new QComboBox();
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
        m_display_transform_combo, SIGNAL(currentIndexChanged(QString)),
        m_render_widget, SLOT(slot_display_transform_changed(QString)));

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

void RenderTab::create_scrollarea()
{
    // Encapsulate the render widget into another widget that adds a margin around it.
    QWidget* render_widget_wrapper = new QWidget();
    render_widget_wrapper->setObjectName("render_widget_wrapper");
    render_widget_wrapper->setLayout(new QGridLayout());
    render_widget_wrapper->layout()->setSizeConstraint(QLayout::SetFixedSize);
    render_widget_wrapper->layout()->setContentsMargins(20, 20, 20, 20);
    render_widget_wrapper->layout()->addWidget(m_render_widget);

    // Wrap the render widget in a scroll area.
    m_scroll_area = new QScrollArea();
    m_scroll_area->setObjectName("render_widget_scrollarea");
    m_scroll_area->setAlignment(Qt::AlignCenter);
    m_scroll_area->setWidget(render_widget_wrapper);
}

void RenderTab::recreate_handlers()
{
    // Handler for zooming the render widget in and out with the keyboard or the mouse wheel.
    m_zoom_handler.reset(
        new WidgetZoomHandler(
            m_scroll_area,
            m_render_widget));

    // Handler for panning the render widget with the mouse.
    m_pan_handler.reset(
        new ScrollAreaPanHandler(
            m_scroll_area));

    // Handler for tracking and displaying mouse coordinates.
    m_mouse_tracker.reset(
        new MouseCoordinatesTracker(
            m_render_widget,
            m_info_label));

    // Handle for tracking and displaying the color of the pixel under the mouse cursor.
    m_pixel_color_tracker.reset(
        new PixelColorTracker(
            m_render_widget,
            m_r_label,
            m_g_label,
            m_b_label,
            m_a_label,
            *m_mouse_tracker.get(),
            m_project));

    // Handler for pixel inspection in the render widget.
    m_pixel_inspector_handler.reset(
        new PixelInspectorHandler(
            m_render_widget,
            *m_mouse_tracker.get(),
            m_project));

    // Camera handler.
    m_camera_controller.reset(
        new CameraController(
            m_render_widget,
            m_project));
    connect(
        m_camera_controller.get(), SIGNAL(signal_camera_change_begin()),
        SIGNAL(signal_camera_change_begin()));
    connect(
        m_camera_controller.get(), SIGNAL(signal_camera_change_end()),
        SIGNAL(signal_camera_change_end()));
    connect(
        m_camera_controller.get(), SIGNAL(signal_camera_changed()),
        SIGNAL(signal_camera_changed()));
    connect(
        &m_project_explorer, SIGNAL(signal_frame_modified()),
        m_camera_controller.get(), SLOT(slot_frame_modified()));

    // Handler for picking scene entities in the render widget.
    m_scene_picking_handler.reset(
        new ScenePickingHandler(
            m_render_widget,
            m_picking_mode_combo,
            *m_mouse_tracker.get(),
            m_project_explorer,
            m_project));
    connect(
        m_scene_picking_handler.get(), SIGNAL(signal_entity_picked(renderer::ScenePicker::PickingResult)),
        SIGNAL(signal_entity_picked(renderer::ScenePicker::PickingResult)));
    connect(
        m_scene_picking_handler.get(), SIGNAL(signal_entity_picked(renderer::ScenePicker::PickingResult)),
        m_camera_controller.get(), SLOT(slot_entity_picked(renderer::ScenePicker::PickingResult)));

    // Handler for setting render regions with the mouse.
    m_render_region_handler.reset(
        new RenderRegionHandler(
            m_render_widget,
            *m_mouse_tracker.get()));
    connect(
        m_render_region_handler.get(), SIGNAL(signal_rectangle_selection(const QRect&)),
        SIGNAL(signal_rectangle_selection(const QRect&)));
    connect(
        m_render_region_handler.get(), SIGNAL(signal_render_region(const QRect&)),
        SLOT(slot_set_render_region(const QRect&)));

    // Clipboard handler.
    m_clipboard_handler.reset(new RenderClipboardHandler(m_render_widget, m_render_widget));

    // Material drop handler.
    m_material_drop_handler.reset(
        new MaterialDropHandler(
            m_project,
            m_rendering_manager));
    connect(
        m_render_widget, SIGNAL(signal_material_dropped(const foundation::Vector2d&, const QString&)),
        m_material_drop_handler.get(), SLOT(slot_material_dropped(const foundation::Vector2d&, const QString&)));

    // Set initial state.
    m_pixel_inspector_handler->set_enabled(false);
    m_camera_controller->set_enabled(false);
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_rendertab.cxx"
