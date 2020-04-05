
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

// appleseed.qtcommon headers.
#include "widgets/icapturablewidget.h"

// appleseed.renderer headers.
#include "renderer/api/lighting.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"

// Qt headers.
#include <QObject>
#include <QOpenGLWidget>

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
class QOpenGLFunctions_3_3_Core;

namespace appleseed {
namespace studio {

//
// A widget providing an hardware-accelerated visualization of recorded light paths.
//

class LightPathsWidget
  : public QOpenGLWidget
  , public qtcommon::ICapturableWidget
{
    Q_OBJECT

  public:
    LightPathsWidget(
        const renderer::Project&            project,
        const size_t                        width,
        const size_t                        height);

    QImage capture() override;

    void set_transform(
        const foundation::Transformd&       transform);

    void set_light_paths(
        const renderer::LightPathArray&     light_paths);

    void set_selected_light_path_index(
        const int                           selected_light_path_index);

  signals:
    void signal_light_path_selection_changed(
        const int                           selected_light_path_index,
        const int                           total_light_paths) const;

  public slots:
    void slot_display_all_light_paths();
    void slot_display_previous_light_path();
    void slot_display_next_light_path();
    void slot_toggle_backface_culling(const bool checked);
    void slot_synchronize_camera();

  private:
    const renderer::Project&                m_project;
    renderer::Camera&                       m_camera;
    foundation::Matrix4d                    m_camera_matrix;

    bool                                    m_backface_culling_enabled;

    renderer::LightPathArray                m_light_paths;
    int                                     m_selected_light_path_index;    // -1 == display all paths

    QOpenGLFunctions_3_3_Core*              m_gl;

    std::vector<GLuint>                     m_scene_object_data_vbos;
    std::vector<GLsizei>                    m_scene_object_data_index_counts;
    std::vector<GLuint>                     m_scene_object_instance_vbos;
    std::vector<GLsizei>                    m_scene_object_instance_counts;
    std::vector<GLsizei>                    m_scene_object_current_instances;
    std::vector<GLuint>                     m_scene_object_vaos;
    std::unordered_map<std::string, size_t> m_scene_object_index_map;
    GLuint                                  m_scene_shader_program;
    GLint                                   m_scene_view_mat_location;
    GLint                                   m_scene_proj_mat_location;
    GLuint                                  m_light_paths_vbo;
    std::vector<GLsizei>                    m_light_paths_index_offsets;
    GLuint                                  m_light_paths_vao;
    GLuint                                  m_light_paths_shader_program;
    GLint                                   m_light_paths_view_mat_location;
    GLint                                   m_light_paths_proj_mat_location;
    foundation::Matrix4f                    m_gl_view_matrix;
    foundation::Matrix4f                    m_gl_proj_matrix;
    bool                                    m_gl_initialized;

    void initializeGL() override;
    void resizeGL(int w, int h) override;
    void paintGL() override;
    void keyPressEvent(QKeyEvent* event) override;

    void cleanup_gl_data();
    void load_scene_data();
    void load_light_paths_data();

    void load_assembly_data(
        const renderer::Assembly&           object);

    void load_assembly_instance(
        const renderer::AssemblyInstance&   assembly_instance,
        const float                         time);

    void load_object_data(
        const renderer::Object&             object);

    void load_object_instance(
        const renderer::ObjectInstance&     object_instance,
        const foundation::Matrix4f&         assembly_transform_matrix);

    void render_geometry();
    void render_light_paths();

    void dump_selected_light_path() const;
};

}   // namespace studio
}   // namespace appleseed
