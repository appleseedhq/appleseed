
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
#include "openglviewporttab.h"

// appleseed.studio headers.
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

OpenGLViewportTab::OpenGLViewportTab(
    Project&                            project,
    RenderingManager&                   rendering_manager,
    LightPathsManager&                  light_paths_manager,
    OCIO::ConstConfigRcPtr              ocio_config)
    : ViewportTab(
        project,
        rendering_manager,
        ocio_config)
    , m_light_paths_manager(light_paths_manager)
{
    setObjectName("opengl_viewport_tab");
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

    m_camera_controller->set_enabled(true);

    get_viewport_canvas()->set_base_layer(ViewportCanvas::BaseLayer::OpenGL);
}

ViewportCanvas* OpenGLViewportTab::get_viewport_canvas() const
{
    return m_viewport_canvas;
}

void OpenGLViewportTab::render_began()
{
    ViewportTab::render_began();

    m_light_paths_viewport_toolbar.get()->reset(&m_project);
}

void OpenGLViewportTab::update_size()
{
    ViewportTab::update_size();

    recreate_handlers();
}

void OpenGLViewportTab::on_tab_selected()
{
    const bool display_light_paths = m_light_paths_manager.should_display_light_paths();
    m_light_paths_viewport_toolbar->set_enabled(display_light_paths);
    m_light_paths_toggle_button->setChecked(display_light_paths);
}

void OpenGLViewportTab::slot_camera_changed()
{
    m_viewport_canvas->get_light_paths_layer()->set_transform(m_camera_controller->get_transform());
    m_viewport_canvas->get_gl_scene_layer()->set_transform(m_camera_controller->get_transform());
    update();
}

void OpenGLViewportTab::slot_viewport_canvas_context_menu(const QPoint& point)
{
    emit signal_viewport_canvas_context_menu(m_viewport_canvas->mapToGlobal(point));
}

void OpenGLViewportTab::create_viewport_canvas()
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
        this, &OpenGLViewportTab::slot_viewport_canvas_context_menu);

    m_viewport_canvas->setMouseTracking(true);
}

void OpenGLViewportTab::create_toolbar()
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
        this, &OpenGLViewportTab::slot_toggle_light_paths);
    m_toolbar->addWidget(m_light_paths_toggle_button);

    m_toolbar->addSeparator();

    // Reset Zoom button.
    QToolButton* reset_zoom_button = new QToolButton();
    reset_zoom_button->setIcon(load_icons("rendertab_reset_zoom"));
    reset_zoom_button->setShortcut(Qt::Key_Asterisk);
    reset_zoom_button->setToolTip(combine_name_and_shortcut("Reset Zoom", reset_zoom_button->shortcut()));
    connect(
        reset_zoom_button, &QToolButton::clicked,
        this, &OpenGLViewportTab::signal_reset_zoom);
    m_toolbar->addWidget(reset_zoom_button);

    m_toolbar->addSeparator();

    // Toggle Backface Culling button.
    QToolButton* backface_culling_button = new QToolButton();
    backface_culling_button->setIcon(load_icons("opengl_viewport_tab_toggle_backface_culling"));
    backface_culling_button->setToolTip("Show/hide backfacing surfaces");
    backface_culling_button->setCheckable(true);
    backface_culling_button->setChecked(false);
    connect(
        backface_culling_button, &QToolButton::toggled ,
        m_viewport_canvas, &ViewportCanvas::slot_toggle_backface_culling);
    m_toolbar->addWidget(backface_culling_button);

    // Synchronize Camera button.
    QToolButton* sync_camera_button = new QToolButton();
    sync_camera_button->setIcon(load_icons("opengl_viewport_tab_synchronize_camera"));
    sync_camera_button->setToolTip("Synchronize the rendering camera with this camera");
    connect(
        sync_camera_button, &QToolButton::clicked,
        m_viewport_canvas->get_light_paths_layer(), &LightPathsLayer::slot_synchronize_camera);
    m_toolbar->addWidget(sync_camera_button);
}

void OpenGLViewportTab::create_scrollarea()
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

void OpenGLViewportTab::recreate_handlers()
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

    // Camera handler.
    m_camera_controller.reset(
        new CameraController(
            m_viewport_canvas,
            m_project));

    connect(
        m_camera_controller.get(), &CameraController::signal_camera_changed,
        this, &OpenGLViewportTab::slot_camera_changed);

    // Clipboard handler.
    m_clipboard_handler.reset(new RenderClipboardHandler(m_viewport_canvas, m_viewport_canvas));
}

void OpenGLViewportTab::slot_toggle_light_paths(const bool checked)
{
    m_light_paths_viewport_toolbar->set_enabled(checked);
    m_light_paths_manager.display_light_paths(checked);
}

}   // namespace studio
}   // namespace appleseed

