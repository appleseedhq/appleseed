
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
#include "mainwindow/rendering/renderclipboardhandler.h"

// appleseed.renderer headers.
#include "renderer/api/lighting.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QOpenGLWidget>
#include <QObject>

// Standard headers.
#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class Camera; }
namespace renderer  { class Object; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class Project; }
class QKeyEvent;
class QImage;
class QOpenGLFunctions_4_1_Core;

namespace appleseed {
namespace studio {

//
// A widget providing an hardware-accelerated visualization of recorded light paths.
//

class LightPathsLayer: public QObject
{
    Q_OBJECT

  public:
    LightPathsLayer(
        const renderer::Project&            project,
        const size_t                        width,
        const size_t                        height);

    void resize(const size_t width, const size_t height);

    void update_render_camera_transform();

    void set_transform(
        const foundation::Transformd&       transform);

    void set_light_paths(
        const renderer::LightPathArray&     light_paths);

    void set_selected_light_path_index(
        const int                           selected_light_path_index);

    void set_gl_functions(
        QOpenGLFunctions_4_1_Core*          functions);

    void init_gl(QSurfaceFormat format);

    void draw() const;

    void draw_render_camera() const;

  signals:
    void signal_light_path_selection_changed(
        const int                           selected_light_path_index,
        const int                           total_light_paths) const;

  public slots:
    void slot_display_all_light_paths();
    void slot_display_previous_light_path();
    void slot_display_next_light_path();
    void slot_synchronize_camera();

  private:
    const renderer::Project&                m_project;
    renderer::Camera&                       m_camera;
    foundation::Matrix4d                    m_camera_matrix;
    foundation::Matrix4d                    m_render_camera_matrix;

    renderer::LightPathArray                m_light_paths;
    int                                     m_selected_light_path_index;    // -1 == display all paths
    float                                   m_max_luminance;
    float                                   m_max_thickness;
    float                                   m_min_thickness;
    size_t                                  m_width;
    size_t                                  m_height;

    QOpenGLFunctions_4_1_Core*              m_gl;

    GLuint                                  m_positions_vbo;
    GLuint                                  m_others_vbo;
    GLuint                                  m_indices_ebo;
    std::vector<GLsizei>                    m_index_offsets;
    GLuint                                  m_light_paths_vao;
    GLuint                                  m_shader_program;
    GLint                                   m_view_mat_loc;
    GLint                                   m_proj_mat_loc;
    GLint                                   m_res_loc;
    GLint                                   m_max_luminance_loc;
    GLint                                   m_min_thickness_loc;
    GLint                                   m_max_thickness_loc;
    foundation::Matrix4f                    m_gl_render_view_matrix;
    foundation::Matrix4f                    m_gl_view_matrix;
    foundation::Matrix4f                    m_gl_proj_matrix;
    bool                                    m_gl_initialized;

    void cleanup_gl_data();
    void load_light_paths_data();

    void render_scene(const GLfloat* gl_view_matrix) const;

    void dump_selected_light_path() const;
};

}   // namespace studio
}   // namespace appleseed
