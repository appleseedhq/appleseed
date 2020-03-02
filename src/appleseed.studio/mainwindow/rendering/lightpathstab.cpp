
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

// Interface header.
#include "lightpathstab.h"

// appleseed.studio headers.
#include "mainwindow/rendering/lightpathspickinghandler.h"
#include "mainwindow/rendering/lightpathswidget.h"
#include "utility/settingskeys.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"
#include "renderer/api/lighting.h"
#include "renderer/api/project.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QApplication>
#include <QDir>
#include <QFileInfo>
#include <QGridLayout>
#include <QLabel>
#include <QMenu>
#include <QScrollArea>
#include <QSize>
#include <QString>
#include <Qt>
#include <QToolBar>
#include <QToolButton>

// Standard headers.
#include <cassert>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// LightPathsTab class implementation.
//

LightPathsTab::LightPathsTab(Project& project, ParamArray& settings)
  : m_project(project)
  , m_settings(settings)
{
    setObjectName("render_widget_tab");
    setLayout(new QGridLayout());
    layout()->setSpacing(0);
    layout()->setMargin(0);

    create_light_paths_widget();
    create_toolbar();
    create_scrollarea();

    layout()->addWidget(m_toolbar);
    layout()->addWidget(m_scroll_area);

    recreate_handlers();
}

void LightPathsTab::slot_entity_picked(const ScenePicker::PickingResult& result)
{
    const CanvasProperties& props = m_project.get_frame()->image().properties();

    m_screen_space_paths_picking_handler->pick(
        Vector2i(
            result.m_ndc[0] * static_cast<int>(props.m_canvas_width),
            result.m_ndc[1] * static_cast<int>(props.m_canvas_height)));
}

void LightPathsTab::slot_rectangle_selection(const QRect& rect)
{
    m_screen_space_paths_picking_handler->pick(
        AABB2i(
            Vector2i(rect.x(), rect.y()),
            Vector2i(rect.x() + rect.width() - 1, rect.y() + rect.height() - 1)));
}

void LightPathsTab::slot_light_path_selection_changed(
    const int       selected_light_path_index,
    const int       total_light_paths) const
{
    if (total_light_paths > 0)
    {
        m_prev_path_button->setEnabled(selected_light_path_index > -1);
        m_next_path_button->setEnabled(selected_light_path_index < total_light_paths - 1);
    }
    else
    {
        m_prev_path_button->setEnabled(false);
        m_next_path_button->setEnabled(false);
    }
}

void LightPathsTab::slot_context_menu(const QPoint& point)
{
    if (!(QApplication::keyboardModifiers() & Qt::ShiftModifier))
        return;

    QMenu* menu = new QMenu(this);

    const auto light_path_count = m_project.get_light_path_recorder().get_light_path_count();
    menu->addAction(
        QString("Save %1 Light Path%2...")
            .arg(QString::fromStdString(pretty_uint(light_path_count)))
            .arg(light_path_count > 1 ? "s" : ""),
        this, SLOT(slot_save_light_paths()));

    menu->exec(m_light_paths_widget->mapToGlobal(point));
}

void LightPathsTab::slot_save_light_paths()
{
    QString filepath =
        get_save_filename(
            this,
            "Save Light Paths As...",
            "Light Paths Files (*.aspaths);;All Files (*.*)",
            m_settings,
            SETTINGS_FILE_DIALOG_LIGHT_PATHS);

    if (filepath.isEmpty())
        return;

    if (QFileInfo(filepath).suffix().isEmpty())
        filepath += ".aspaths";

    // Write light paths to disk.
    m_project.get_light_path_recorder().write(filepath.toUtf8().constData());
}

void LightPathsTab::slot_camera_changed()
{
    m_light_paths_widget->set_transform(m_camera_controller->get_transform());
    m_light_paths_widget->update();
}

void LightPathsTab::create_light_paths_widget()
{
    // Create the OpenGL widget.
    const CanvasProperties& props = m_project.get_frame()->image().properties();
    m_light_paths_widget =
        new LightPathsWidget(
            m_project,
            props.m_canvas_width,
            props.m_canvas_height);

    // Enable context menu on the OpenGL widget.
    m_light_paths_widget->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(
        m_light_paths_widget, SIGNAL(signal_light_path_selection_changed(const int, const int)),
        SLOT(slot_light_path_selection_changed(const int, const int)));
    connect(
        m_light_paths_widget, SIGNAL(customContextMenuRequested(const QPoint&)),
        SLOT(slot_context_menu(const QPoint&)));
}

void LightPathsTab::create_toolbar()
{
    // Create the render toolbar.
    m_toolbar = new QToolBar();
    m_toolbar->setObjectName("render_toolbar");
    m_toolbar->setIconSize(QSize(18, 18));

    // Save Light Paths button.
    QToolButton* save_light_paths_button = new QToolButton();
    save_light_paths_button->setIcon(load_icons("lightpathstab_save_light_paths"));
    const auto light_path_count = m_project.get_light_path_recorder().get_light_path_count();
    save_light_paths_button->setToolTip(
        QString("Save %1 Light Path%2...")
            .arg(QString::fromStdString(pretty_uint(light_path_count)))
            .arg(light_path_count > 1 ? "s" : ""));
    connect(
        save_light_paths_button , SIGNAL(clicked()),
        SLOT(slot_save_light_paths()));
    m_toolbar->addWidget(save_light_paths_button);

    m_toolbar->addSeparator();

    // Previous Light Path button.
    m_prev_path_button = new QToolButton();
    m_prev_path_button->setIcon(load_icons("lightpathstab_prev_light_path"));
    m_prev_path_button->setToolTip("Display previous light path");
    m_prev_path_button->setEnabled(false);
    connect(
        m_prev_path_button, SIGNAL(clicked()),
        m_light_paths_widget, SLOT(slot_display_previous_light_path()));
    m_toolbar->addWidget(m_prev_path_button);

    // Next Light Path button.
    m_next_path_button = new QToolButton();
    m_next_path_button->setIcon(load_icons("lightpathstab_next_light_path"));
    m_next_path_button->setToolTip("Display next light path");
    m_next_path_button->setEnabled(false);
    connect(
        m_next_path_button, SIGNAL(clicked()),
        m_light_paths_widget, SLOT(slot_display_next_light_path()));
    m_toolbar->addWidget(m_next_path_button);

    m_toolbar->addSeparator();

    // Toggle Backface Culling button.
    QToolButton* backface_culling_button = new QToolButton();
    backface_culling_button->setIcon(load_icons("lightpathstab_toggle_backface_culling"));
    backface_culling_button->setToolTip("Show/hide backfacing surfaces");
    backface_culling_button->setCheckable(true);
    backface_culling_button->setChecked(false);
    connect(
        backface_culling_button, SIGNAL(toggled(bool)),
        m_light_paths_widget, SLOT(slot_toggle_backface_culling(const bool)));
    m_toolbar->addWidget(backface_culling_button);

    // Synchronize Camera button.
    QToolButton* sync_camera_button = new QToolButton();
    sync_camera_button->setIcon(load_icons("lightpathstab_synchronize_camera"));
    sync_camera_button->setToolTip("Synchronize the rendering camera with this camera");
    connect(
        sync_camera_button, SIGNAL(clicked()),
        m_light_paths_widget, SLOT(slot_synchronize_camera()));
    m_toolbar->addWidget(sync_camera_button);

    // Add stretchy spacer.
    // This places interactive widgets on the left and info on the right.
    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    m_toolbar->addWidget(spacer);

    // Create a label to display various information such as mouse coordinates, etc.
    m_info_label = new QLabel();
    m_info_label->setObjectName("info_label");
    m_toolbar->addWidget(m_info_label);
}

void LightPathsTab::create_scrollarea()
{
    // Encapsulate the OpenGL widget into another widget that adds a margin around it.
    QWidget* gl_widget_wrapper = new QWidget();
    gl_widget_wrapper->setObjectName("render_widget_wrapper");
    gl_widget_wrapper->setLayout(new QGridLayout());
    gl_widget_wrapper->layout()->setSizeConstraint(QLayout::SetFixedSize);
    gl_widget_wrapper->layout()->setContentsMargins(20, 20, 20, 20);
    gl_widget_wrapper->layout()->addWidget(m_light_paths_widget);

    // Wrap the OpenGL widget in a scroll area.
    m_scroll_area = new QScrollArea();
    m_scroll_area->setObjectName("render_widget_scrollarea");
    m_scroll_area->setAlignment(Qt::AlignCenter);
    m_scroll_area->setWidget(gl_widget_wrapper);
}

void LightPathsTab::recreate_handlers()
{
    // Handler for zooming the render widget in and out with the keyboard or the mouse wheel.
    m_zoom_handler.reset(
        new WidgetZoomHandler(
            m_scroll_area,
            m_light_paths_widget));

    // Handler for panning the render widget with the mouse.
    m_pan_handler.reset(
        new ScrollAreaPanHandler(
            m_scroll_area));

    // Handler for tracking and displaying mouse coordinates.
    m_mouse_tracker.reset(
        new MouseCoordinatesTracker(
            m_light_paths_widget,
            m_info_label));

    // The screen-space paths picking handler is used to pick paths from the render widget.
    m_screen_space_paths_picking_handler.reset(
        new LightPathsPickingHandler(
            m_light_paths_widget,
            *m_mouse_tracker.get(),
            m_project));
    m_screen_space_paths_picking_handler->set_enabled(false);

    // The world-space paths picking handler is used to pick paths in the light paths widget.
    // Commented out because we don't want to allow that.
    // m_world_space_paths_picking_handler.reset(
    //     new LightPathsPickingHandler(
    //         m_light_paths_widget,
    //         *m_mouse_tracker.get(),
    //         m_project));

    // Camera handler.
    m_light_paths_widget->setMouseTracking(true);
    m_camera_controller.reset(
        new CameraController(
            m_light_paths_widget,
            m_project,
            m_project.get_uncached_active_camera()));
    connect(
        m_camera_controller.get(), SIGNAL(signal_camera_changed()),
        SLOT(slot_camera_changed()));

    // Clipboard handler.
    m_clipboard_handler.reset(new RenderClipboardHandler(m_light_paths_widget, m_light_paths_widget));
}

}   // namespace studio
}   // namespace appleseed

#include "mainwindow/rendering/moc_cpp_lightpathstab.cxx"
