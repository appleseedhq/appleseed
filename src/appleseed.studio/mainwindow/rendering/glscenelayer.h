
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Gray Olson, The appleseedhq Organization
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
class QOpenGLFunctions_3_3_Core;
class QSurfaceFormat;

namespace appleseed {
namespace studio {

//
// A widget providing an hardware-accelerated visualization of the loaded scene.
//

class GLSceneLayer
  : public QObject
{
    Q_OBJECT

  public:
    GLSceneLayer(
        const renderer::Project&            project,
        const size_t                        width,
        const size_t                        height);

    void init_gl(
        QSurfaceFormat                      format);

    void set_transform(
        const foundation::Transformd&       transform);

    void set_gl_functions(
        QOpenGLFunctions_3_3_Core*          functions);

    void draw();
    void draw_depth_only();

  public slots:
    void slot_toggle_backface_culling(const bool checked);
    void slot_synchronize_camera();

  private:
    const renderer::Project&                m_project;
    renderer::Camera&                       m_camera;
    foundation::Matrix4d                    m_camera_matrix;
    foundation::Vector3f                    m_camera_position;

    bool                                    m_backface_culling_enabled;

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
    GLint                                   m_scene_camera_pos_location;
    GLuint                                  m_depthonly_shader_program;
    GLint                                   m_depthonly_view_mat_location;
    GLint                                   m_depthonly_proj_mat_location;
    GLint                                   m_depthonly_camera_pos_location;
    foundation::Matrix4f                    m_gl_view_matrix;
    foundation::Matrix4f                    m_gl_proj_matrix;
    bool                                    m_initialized;

    void render_scene();

    void cleanup_gl_data();
    void load_scene_data();

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
};

}   // namespace studio
}   // namespace appleseed
