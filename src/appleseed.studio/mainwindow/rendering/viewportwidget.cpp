
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Gray Olson, The appleseedhq Organization
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
#include "viewportwidget.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/nativedrawing.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Qt headers.
#include <QColor>
#include <QDragEnterEvent>
#include <QDragMoveEvent>
#include <QDropEvent>
#include <QMimeData>
#include <QMutexLocker>
#include <QOpenGLFunctions_3_3_Core>
#include <QTimer>
#include <Qt>

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// ViewportWidget class implementation.
//

ViewportWidget::ViewportWidget(
    const renderer::Project&    project,
    const size_t                width,
    const size_t                height,
    OCIO::ConstConfigRcPtr      ocio_config,
    QWidget*                    parent)
  : QOpenGLWidget(parent)
  , m_project(project)
  , m_draw_light_paths(false)
  , m_active_base_layer(static_cast<BaseLayer>(0))
{
    setFocusPolicy(Qt::StrongFocus);
    setFixedWidth(static_cast<int>(width));
    setFixedHeight(static_cast<int>(height));
    setAutoFillBackground(false);
    setAttribute(Qt::WA_OpaquePaintEvent, true);

    create_render_layer(ocio_config);
    create_gl_scene_layer();
    create_light_paths_layer();

    resize(width, height);

    setAcceptDrops(true);
}

QString ViewportWidget::base_layer_string(BaseLayer layer)
{
    switch (layer)
    {
        case BaseLayer::FinalRender: return QString("Final Render");
        case BaseLayer::OpenGL: return QString("OpenGL");
    }
    assert(false);
    return QString("BaseLayer");
}

void ViewportWidget::create_render_layer(OCIO::ConstConfigRcPtr  ocio_config)
{
    m_render_layer =
        std::unique_ptr<RenderLayer>(new RenderLayer(
            m_width,
            m_height,
            ocio_config));
}

void ViewportWidget::create_gl_scene_layer()
{
    m_gl_scene_layer =
        std::unique_ptr<GLSceneLayer>(new GLSceneLayer(
            m_project,
            m_width,
            m_height));
}

void ViewportWidget::create_light_paths_layer()
{
    m_light_paths_layer =
        std::unique_ptr<LightPathsLayer>(new LightPathsLayer(
            m_project,
            m_width,
            m_height));
}

RenderLayer* ViewportWidget::get_render_layer()
{
    return m_render_layer.get();
}

LightPathsLayer* ViewportWidget::get_light_paths_layer()
{
    return m_light_paths_layer.get();
}

GLSceneLayer* ViewportWidget::get_gl_scene_layer()
{
    return m_gl_scene_layer.get();
}

QImage ViewportWidget::capture()
{
    return grabFramebuffer();
}

void ViewportWidget::initializeGL() {
    RENDERER_LOG_INFO("initializing opengl.");

    m_gl = QOpenGLContext::currentContext()->versionFunctions<QOpenGLFunctions_3_3_Core>();

    const auto qs_format = format();
    if (!m_gl->initializeOpenGLFunctions())
    {
        const int major_version = qs_format.majorVersion();
        const int minor_version = qs_format.minorVersion();
        RENDERER_LOG_ERROR(
            "opengl: could not load required gl functions. loaded version %d.%d, required version 3.3.",
            major_version,
            minor_version);
        return;
    }

    m_gl_scene_layer->set_gl_functions(m_gl);
    m_gl_scene_layer->init_gl(qs_format);
    m_light_paths_layer->set_gl_functions(m_gl);
    m_light_paths_layer->init_gl(qs_format);
}

void ViewportWidget::resize(
    const size_t width,
    const size_t height)
{
    m_render_layer->resize(width, height);
    m_light_paths_layer->resize(width, height);
}

void ViewportWidget::resizeGL(
    int width,
    int height)
{
    m_light_paths_layer->resize(width, height);
}

void ViewportWidget::set_draw_light_paths_enabled(const bool enabled)
{
    m_draw_light_paths = enabled;
    update();
}

void ViewportWidget::paintGL()
{
    if (m_active_base_layer == BaseLayer::FinalRender)
    {
        m_painter.begin(this);
        m_render_layer->paint(rect(), m_painter);
        m_painter.end();
    }

    m_gl->glViewport(0, 0, width(), height());

    if (m_active_base_layer == BaseLayer::OpenGL || m_draw_light_paths)
        m_gl_scene_layer->draw_depth_only();

    if (m_active_base_layer == BaseLayer::OpenGL)
        m_gl_scene_layer->draw();

    if (m_draw_light_paths)
    {
        if (m_active_base_layer == BaseLayer::FinalRender)
            m_light_paths_layer->draw_render_camera();
        else
            m_light_paths_layer->draw();
    }
}

void ViewportWidget::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasFormat("text/plain"))
        event->acceptProposedAction();
}

void ViewportWidget::dragMoveEvent(QDragMoveEvent* event)
{
    if (pos().x() <= event->pos().x() && pos().y() <= event->pos().y()
        && event->pos().x() < pos().x() + width() && event->pos().y() < pos().y() + height())
    {
        event->accept();
    }
    else
        event->ignore();
}

void ViewportWidget::dropEvent(QDropEvent* event)
{
    emit signal_material_dropped(
        Vector2d(
            static_cast<double>(event->pos().x()) / width(),
            static_cast<double>(event->pos().y()) / height()),
        event->mimeData()->text());
}

void ViewportWidget::slot_base_layer_changed(int index)
{
    assert(index < BaseLayer::BASE_LAYER_MAX_VALUE);
    m_active_base_layer = static_cast<BaseLayer>(index);
    update();
}

void ViewportWidget::slot_light_paths_toggled(bool checked)
{
    set_draw_light_paths_enabled(checked);
}

}   // namespace studio
}   // namespace appleseed
