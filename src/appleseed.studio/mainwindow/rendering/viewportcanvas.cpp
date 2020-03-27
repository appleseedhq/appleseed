
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
#include "viewportcanvas.h"

// appleseed.studio headers.
#include "utility/gl.h"

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
#include <QOpenGLContext>
#include <QOpenGLFunctions_4_1_Core>
#include <QSurfaceFormat>
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
// ViewportCanvas class implementation.
//

ViewportCanvas::ViewportCanvas(
    const renderer::Project&        project,
    const size_t                    width,
    const size_t                    height,
    OCIO::ConstConfigRcPtr          ocio_config,
    const LightPathsManager&        light_paths_manager,
    const ViewportCanvas::BaseLayer layer,
    QWidget*                        parent)
  : QOpenGLWidget(parent)
  , m_project(project)
  , m_draw_light_paths(false)
  , m_active_base_layer(layer)
  , m_resolve_program(0)
  , m_accum_loc(0)
  , m_accum_tex(0)
  , m_revealage_loc(0)
  , m_revealage_tex(0)
  , m_accum_revealage_fb(0)
{
    setFocusPolicy(Qt::StrongFocus);
    setFixedWidth(static_cast<int>(width));
    setFixedHeight(static_cast<int>(height));
    setAutoFillBackground(false);
    setAttribute(Qt::WA_OpaquePaintEvent, true);

    connect(
        &light_paths_manager, &LightPathsManager::signal_light_path_selection_changed,
        this, &ViewportCanvas::slot_light_path_selection_changed);

    create_render_layer(ocio_config, width, height);
    create_gl_scene_layer(width, height);
    create_light_paths_layer(light_paths_manager, width, height);

    resize(width, height);

    setAcceptDrops(true);
}

ViewportCanvas::~ViewportCanvas()
{
    m_gl->glDeleteProgram(m_resolve_program);
    m_gl->glDeleteTextures(1, &m_accum_tex);
    m_gl->glDeleteTextures(1, &m_revealage_tex);
    m_gl->glDeleteTextures(1, &m_color_tex);
    m_gl->glDeleteTextures(1, &m_depth_tex);
    m_gl->glDeleteFramebuffers(1, &m_accum_revealage_fb);
    m_gl->glDeleteFramebuffers(1, &m_main_fb);
}

void ViewportCanvas::create_render_layer(
    OCIO::ConstConfigRcPtr      ocio_config,
    const std::size_t           width,
    const std::size_t           height)
{
    m_render_layer =
        std::unique_ptr<RenderLayer>(new RenderLayer(
            width,
            height,
            ocio_config));
}

void ViewportCanvas::create_gl_scene_layer(
    const std::size_t           width,
    const std::size_t           height)
{
    m_gl_scene_layer =
        std::unique_ptr<GLSceneLayer>(new GLSceneLayer(
            m_project,
            width,
            height));
}

void ViewportCanvas::create_light_paths_layer(
    const LightPathsManager&    light_paths_manager,
    const std::size_t           width,
    const std::size_t           height)
{
    m_light_paths_layer =
        std::unique_ptr<LightPathsLayer>(new LightPathsLayer(
            m_project,
            light_paths_manager,
            width,
            height));
}

RenderLayer* ViewportCanvas::get_render_layer()
{
    return m_render_layer.get();
}

LightPathsLayer* ViewportCanvas::get_light_paths_layer()
{
    return m_light_paths_layer.get();
}

GLSceneLayer* ViewportCanvas::get_gl_scene_layer()
{
    return m_gl_scene_layer.get();
}

QImage ViewportCanvas::capture()
{
    return grabFramebuffer();
}

void ViewportCanvas::initializeGL()
{
    RENDERER_LOG_INFO("initializing opengl.");

    m_gl = QOpenGLContext::currentContext()->versionFunctions<QOpenGLFunctions_4_1_Core>();

    const QSurfaceFormat qs_format = format();
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

    m_gl->glGenVertexArrays(1, &m_empty_vao);
    m_gl->glBindVertexArray(m_empty_vao);
    m_gl->glGenBuffers(1, &m_empty_vbo);
    m_gl->glBindBuffer(GL_ARRAY_BUFFER, m_empty_vbo);
    float vals[3]{ 0.0f, 0.0f, 0.0f };
    m_gl->glBufferData(GL_ARRAY_BUFFER, sizeof(float) * 3, static_cast<const GLvoid*>(vals), GL_STATIC_DRAW);
    m_gl->glVertexAttribPointer(0, sizeof(float), GL_FLOAT, GL_FALSE, sizeof(float), static_cast<const GLvoid*>(0));
    m_gl->glEnableVertexAttribArray(0);

    auto vertex_shader = load_gl_shader("fullscreen_tri.vert");
    auto fragment_shader = load_gl_shader("oit_resolve.frag");

    m_resolve_program = create_shader_program(
        m_gl,
        &vertex_shader,
        &fragment_shader);

    m_accum_loc = m_gl->glGetUniformLocation(m_resolve_program, "u_accum_tex");
    m_revealage_loc = m_gl->glGetUniformLocation(m_resolve_program, "u_revealage_tex");

    GLuint temp_fbs[2];
    m_gl->glGenFramebuffers(2, temp_fbs);

    m_main_fb = temp_fbs[0];
    m_accum_revealage_fb = temp_fbs[1];

    GLuint temp_texs[4];
    m_gl->glGenTextures(4, temp_texs);

    m_color_tex = temp_texs[0];
    m_depth_tex = temp_texs[1];
    m_accum_tex = temp_texs[2];
    m_revealage_tex = temp_texs[3];

    resizeGL(width(), height());

    m_gl->glBindFramebuffer(GL_FRAMEBUFFER, m_main_fb);
    m_gl->glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_color_tex, 0);
    m_gl->glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, m_depth_tex, 0);

    m_gl->glBindFramebuffer(GL_FRAMEBUFFER, m_accum_revealage_fb);
    m_gl->glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_accum_tex, 0);
    m_gl->glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, m_revealage_tex, 0);
    m_gl->glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, m_depth_tex, 0);

    m_gl->glBindFramebuffer(GL_FRAMEBUFFER, 0);

    m_gl->glUseProgram(m_resolve_program);
    m_gl->glUniform1i(m_accum_loc, 0);
    m_gl->glUniform1i(m_revealage_loc, 1);

    m_render_layer->set_gl_functions(m_gl);
    m_render_layer->init_gl(qs_format);
    m_gl_scene_layer->set_gl_functions(m_gl);
    m_gl_scene_layer->init_gl(qs_format);
    m_light_paths_layer->set_gl_functions(m_gl);
    m_light_paths_layer->init_gl(qs_format);
}

void ViewportCanvas::resize(
    const size_t width,
    const size_t height)
{
    m_render_layer->resize(width, height);
    m_light_paths_layer->resize(width, height);
}

void ViewportCanvas::resizeGL(
    int width,
    int height)
{
    m_light_paths_layer->resize(width, height);

    m_gl->glBindTexture(GL_TEXTURE_2D, m_color_tex);
    m_gl->glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    m_gl->glBindTexture(GL_TEXTURE_2D, m_depth_tex);
    m_gl->glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, width, height, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, NULL);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    m_gl->glBindTexture(GL_TEXTURE_2D, m_accum_tex);
    m_gl->glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, width, height, 0, GL_RGBA, GL_HALF_FLOAT, NULL);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    m_gl->glBindTexture(GL_TEXTURE_2D, m_revealage_tex);
    m_gl->glTexImage2D(GL_TEXTURE_2D, 0, GL_R16F, width, height, 0, GL_RED, GL_HALF_FLOAT, NULL);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    m_gl->glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    m_gl->glBindTexture(GL_TEXTURE_2D, 0);
}

void ViewportCanvas::paintGL()
{
    double dpr = static_cast<double>(m_render_layer->image().devicePixelRatio());
    GLsizei w = static_cast<GLsizei>(width() * dpr);
    GLsizei h = static_cast<GLsizei>(height() * dpr);
    m_gl->glViewport(0, 0, w, h);

    // Clear the main framebuffers
    m_gl->glBindFramebuffer(GL_FRAMEBUFFER, m_main_fb);
    GLfloat main_clear[]{ 0.0, 0.0, 0.0, 0.0 };
    m_gl->glClearBufferfv(GL_COLOR, 0, main_clear);
    m_gl->glClearBufferfi(GL_DEPTH_STENCIL, 0, 1.0, 0);

    if (m_active_base_layer == BaseLayer::FinalRender)
        m_render_layer->draw(m_empty_vao, m_draw_light_paths);

    QOpenGLContext *ctx = const_cast<QOpenGLContext *>(QOpenGLContext::currentContext());

    if (m_active_base_layer == BaseLayer::OpenGL || m_draw_light_paths)
        m_gl_scene_layer->draw_depth_only();

    if (m_active_base_layer == BaseLayer::OpenGL)
        m_gl_scene_layer->draw();

    if (m_draw_light_paths)
    {
        // Bind accum/revealage framebuffer
        m_gl->glBindFramebuffer(GL_FRAMEBUFFER, m_accum_revealage_fb);

        // Set both attachments as active draw buffers
        const GLenum buffers[]{ GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1 };
        m_gl->glDrawBuffers(2, buffers);

        // Clear the buffers
        GLfloat accum_clear_col[]{ 0.0, 0.0, 0.0, 0.0 };
        m_gl->glClearBufferfv(GL_COLOR, 0, accum_clear_col);
        GLfloat revealage_clear_col[]{ 1.0, 0.0, 0.0, 0.0 };
        m_gl->glClearBufferfv(GL_COLOR, 1, revealage_clear_col);

        // Enable proper blending for each
        m_gl->glEnable(GL_BLEND);
        m_gl->glBlendFunci(0, GL_ONE, GL_ONE);
        m_gl->glBlendFunci(1, GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
        m_gl->glEnable(GL_DEPTH_TEST);
        m_gl->glDepthMask(GL_FALSE);
        m_gl->glDepthFunc(GL_LESS);

        if (m_active_base_layer == BaseLayer::FinalRender)
            m_light_paths_layer->draw_render_camera();
        else
            m_light_paths_layer->draw();

        m_gl->glUseProgram(m_resolve_program);

        // Set default framebuffer object
        m_gl->glBindFramebuffer(GL_FRAMEBUFFER, m_main_fb);
        m_gl->glDrawBuffer(GL_COLOR_ATTACHMENT0);

        m_gl->glBindVertexArray(m_empty_vao);

        m_gl->glActiveTexture(GL_TEXTURE0);
        m_gl->glBindTexture(GL_TEXTURE_2D, m_accum_tex);
        m_gl->glActiveTexture(GL_TEXTURE1);
        m_gl->glBindTexture(GL_TEXTURE_2D, m_revealage_tex);

        m_gl->glDisable(GL_DEPTH_TEST);
        m_gl->glDepthMask(GL_FALSE);
        m_gl->glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        m_gl->glDrawArrays(GL_TRIANGLES, 0, 3);

        m_gl->glActiveTexture(GL_TEXTURE1);
        m_gl->glBindTexture(GL_TEXTURE_2D, 0);
        m_gl->glActiveTexture(GL_TEXTURE0);
        m_gl->glBindTexture(GL_TEXTURE_2D, 0);
        m_gl->glDepthMask(GL_TRUE);
    }

    m_gl->glBindFramebuffer(GL_READ_FRAMEBUFFER, m_main_fb);
    m_gl->glBindFramebuffer(GL_DRAW_FRAMEBUFFER, ctx->defaultFramebufferObject());
    m_gl->glBlitFramebuffer(0, 0, w, h, 0, 0, w, h, GL_COLOR_BUFFER_BIT, GL_NEAREST);
}

void ViewportCanvas::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasFormat("text/plain"))
        event->acceptProposedAction();
}

void ViewportCanvas::dragMoveEvent(QDragMoveEvent* event)
{
    if (pos().x() <= event->pos().x() && pos().y() <= event->pos().y()
        && event->pos().x() < pos().x() + width() && event->pos().y() < pos().y() + height())
    {
        event->accept();
    }
    else
        event->ignore();
}

void ViewportCanvas::dropEvent(QDropEvent* event)
{
    emit signal_material_dropped(
        Vector2d(
            static_cast<double>(event->pos().x()) / width(),
            static_cast<double>(event->pos().y()) / height()),
        event->mimeData()->text());
}

void ViewportCanvas::slot_display_transform_changed(const QString& transform)
{
    m_render_layer->set_display_transform(transform);

    if (m_active_base_layer == BaseLayer::FinalRender)
    {
        update();
    }
}

void ViewportCanvas::slot_light_path_selection_changed(
    const bool      display_light_paths,
    const int       selected_light_path_index,
    const int       total_light_paths)
{
    m_draw_light_paths = display_light_paths && total_light_paths > 0;
    update();
}

}   // namespace studio
}   // namespace appleseed
