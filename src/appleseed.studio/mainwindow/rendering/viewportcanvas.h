
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

#pragma once

// appleseed.studio headers.
#include "mainwindow/rendering/lightpathslayer.h"
#include "mainwindow/rendering/lightpathsmanager.h"
#include "mainwindow/rendering/glscenelayer.h"
#include "mainwindow/rendering/renderclipboardhandler.h"
#include "mainwindow/rendering/renderlayer.h"


// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QImage>
#include <QMutex>
#include <QOpenGLWidget>
#include <QPainter>
#include <QWidget>

// OpenColorIO headers.
#include <OpenColorIO/OpenColorIO.h>
namespace OCIO = OCIO_NAMESPACE;

// Standard headers.
#include <cstddef>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class CanvasProperties; }
namespace renderer      { class Frame; }
class QDragEnterEvent;
class QDragMoveEvent;
class QDropEvent;
class QPaintEvent;
class QOpenGLFunctions_4_1_Core;

namespace appleseed {
namespace studio {

//
// A render widget based on QImage.
//

class ViewportCanvas
  : public QOpenGLWidget
  , public ICapturableWidget
{
    Q_OBJECT

  public:
    enum BaseLayer {
        FinalRender,
        OpenGL,
        BASE_LAYER_MAX_VALUE
    };

    // Constructor.
    ViewportCanvas(
        const renderer::Project&    project,
        const size_t                width,
        const size_t                height,
        OCIO::ConstConfigRcPtr      ocio_config,
        const LightPathsManager&    lith_paths_manager,
        const BaseLayer             layer,
        QWidget*                    parent = nullptr);

    ~ViewportCanvas();

    // Thread-safe.
    QImage capture() override;

    RenderLayer* get_render_layer();
    GLSceneLayer* get_gl_scene_layer();
    LightPathsLayer* get_light_paths_layer();

  signals:
    void signal_material_dropped(
        const foundation::Vector2d& drop_pos,
        const QString&              material_name);

  public slots:
    void slot_light_path_selection_changed(
        const bool                  display_light_paths,
        const int                   selected_light_path_index,
        const int                   total_light_paths);

  private:
    const renderer::Project&            m_project;

    QOpenGLFunctions_4_1_Core*          m_gl;
    QPainter                            m_painter;

    GLuint                              m_depth_tex;
    GLuint                              m_color_tex;
    GLuint                              m_accum_tex;
    GLuint                              m_revealage_tex;
    GLuint                              m_main_fb;
    GLuint                              m_accum_revealage_fb;
    GLuint                              m_empty_vao;
    GLuint                              m_empty_vbo;

    GLuint                              m_resolve_program;
    GLint                               m_accum_loc;
    GLint                               m_revealage_loc;

    std::unique_ptr<RenderLayer>        m_render_layer;
    std::unique_ptr<GLSceneLayer>       m_gl_scene_layer;
    std::unique_ptr<LightPathsLayer>    m_light_paths_layer;

    bool                                m_draw_light_paths;
    BaseLayer                           m_active_base_layer;

    void create_light_paths_layer(
        const LightPathsManager&                    light_paths_manager,
        const std::size_t                           width,
        const std::size_t                           height);
    void create_gl_scene_layer(
        const std::size_t                           width,
        const std::size_t                           height);
    void create_render_layer(
        OCIO::ConstConfigRcPtr                      ocio_config,
        const std::size_t                           width,
        const std::size_t                           height);

    void initializeGL() override;
    void resizeGL(int width, int height) override;
    void paintGL() override;
    void dragEnterEvent(QDragEnterEvent* event) override;
    void dragMoveEvent(QDragMoveEvent* event) override;
    void dropEvent(QDropEvent* event) override;
};

}   // namespace studio
}   // namespace appleseed
