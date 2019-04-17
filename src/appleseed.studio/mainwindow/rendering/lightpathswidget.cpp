
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
#include "lightpathswidget.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/entity.h"
#include "renderer/api/object.h"
#include "renderer/api/project.h"
#include "renderer/api/rasterization.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QKeyEvent>

// Standard headers.
#include <algorithm>
#include <cmath>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

LightPathsWidget::LightPathsWidget(
    const Project&          project,
    const size_t            width,
    const size_t            height)
  : m_project(project)
  , m_camera(*m_project.get_uncached_active_camera())
  , m_backface_culling_enabled(false)
  , m_selected_light_path_index(-1)
{
    setFocusPolicy(Qt::StrongFocus);
    setFixedWidth(static_cast<int>(width));
    setFixedHeight(static_cast<int>(height));

    const float time = m_camera.get_shutter_middle_time();
    m_camera_matrix = m_camera.transform_sequence().evaluate(time).get_parent_to_local();
}

QImage LightPathsWidget::capture()
{
    return grabFrameBuffer();
}

void LightPathsWidget::set_transform(const Transformd& transform)
{
    m_camera_matrix = transform.get_parent_to_local();
}

void LightPathsWidget::set_light_paths(const LightPathArray& light_paths)
{
    m_light_paths = light_paths;

    if (m_light_paths.size() > 1)
    {
        // Sort paths by descending radiance at the camera.
        const auto& light_path_recorder = m_project.get_light_path_recorder();
        sort(
            &m_light_paths[0],
            &m_light_paths[0] + m_light_paths.size(),
            [&light_path_recorder](const LightPath& lhs, const LightPath& rhs)
            {
                LightPathVertex lhs_v;
                light_path_recorder.get_light_path_vertex(lhs.m_vertex_end_index - 1, lhs_v);

                LightPathVertex rhs_v;
                light_path_recorder.get_light_path_vertex(rhs.m_vertex_end_index - 1, rhs_v);

                return
                    sum_value(Color3f::from_array(lhs_v.m_radiance)) >
                    sum_value(Color3f::from_array(rhs_v.m_radiance));
            });
    }

    // Display all paths by default.
    set_selected_light_path_index(-1);
}

void LightPathsWidget::set_selected_light_path_index(const int selected_light_path_index)
{
    m_selected_light_path_index = selected_light_path_index;

    dump_selected_light_path();
    update();

    emit signal_light_path_selection_changed(
        m_selected_light_path_index,
        static_cast<int>(m_light_paths.size()));
}

void LightPathsWidget::slot_display_all_light_paths()
{
    if (m_selected_light_path_index > -1)
        set_selected_light_path_index(-1);
}

void LightPathsWidget::slot_display_previous_light_path()
{
    if (m_selected_light_path_index > -1)
        set_selected_light_path_index(m_selected_light_path_index - 1);
}

void LightPathsWidget::slot_display_next_light_path()
{
    if (m_selected_light_path_index < static_cast<int>(m_light_paths.size()) - 1)
        set_selected_light_path_index(m_selected_light_path_index + 1);
}

void LightPathsWidget::slot_toggle_backface_culling(const bool checked)
{
    m_backface_culling_enabled = checked;
    update();
}

void LightPathsWidget::slot_synchronize_camera()
{
    m_camera.transform_sequence().clear();
    m_camera.transform_sequence().set_transform(0.0f,
        Transformd::from_local_to_parent(inverse(m_camera_matrix)));
}

void LightPathsWidget::initializeGL()
{
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_NORMALIZE);
    glShadeModel(GL_SMOOTH);

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

    static const GLfloat LightPosition[] = { 0.0f, 0.0f, 0.0f, 1.0f };
    static const GLfloat LightAmbient[] = { 0.0f, 0.0f, 0.0f, 1.0f };
    static const GLfloat LightDiffuse[] = { 0.8f, 0.8f, 0.8f, 1.0f };
    static const GLfloat LightSpecular[] = { 0.3f, 0.3f, 0.3f, 1.0f };

    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_POSITION, LightPosition);
    glLightfv(GL_LIGHT0, GL_AMBIENT, LightAmbient);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse);
    glLightfv(GL_LIGHT0, GL_SPECULAR, LightSpecular);
}

void LightPathsWidget::resizeGL(int w, int h)
{
    glViewport(0, 0, static_cast<GLsizei>(w), static_cast<GLsizei>(h));
}

namespace
{
    void glMultMatrixd(const Matrix4d& m)
    {
        const Matrix4d mt(transpose(m));
        ::glMultMatrixd(const_cast<GLdouble*>(&mt[0]));
    }

    void glColor(const Color3f& c)
    {
        glColor3f(c[0], c[1], c[2]);
    }

    struct OpenGLRasterizer
      : public ObjectRasterizer
    {
        void begin_object() override
        {
            glBegin(GL_TRIANGLES);
        }

        void end_object() override
        {
            glEnd();
        }

        void rasterize(const Triangle& triangle) override
        {
            glNormal3d(triangle.m_n0[0], triangle.m_n0[1], triangle.m_n0[2]);
            glVertex3d(triangle.m_v0[0], triangle.m_v0[1], triangle.m_v0[2]);

            glNormal3d(triangle.m_n1[0], triangle.m_n1[1], triangle.m_n1[2]);
            glVertex3d(triangle.m_v1[0], triangle.m_v1[1], triangle.m_v1[2]);

            glNormal3d(triangle.m_n2[0], triangle.m_n2[1], triangle.m_n2[2]);
            glVertex3d(triangle.m_v2[0], triangle.m_v2[1], triangle.m_v2[2]);
        }
    };

    void draw(const ObjectInstance& object_instance)
    {
        Object* object = object_instance.find_object();

        if (object == nullptr)
            return;

        static const GLfloat Ambient[] = { 0.0f, 0.0f, 0.0f, 1.0f };
        static const GLfloat Diffuse[] = { 0.4f, 0.4f, 0.4f, 1.0f };
        static const GLfloat Specular[] = { 0.15f, 0.15f, 0.15f, 1.0f };
        static const GLfloat Shininess = 20.0f;

        glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, Ambient);
        glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, Diffuse);
        glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, Specular);
        glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, Shininess);

        const Transformd& transform = object_instance.get_transform();

        glPushMatrix();
        glMultMatrixd(transform.get_local_to_parent());

        OpenGLRasterizer rasterizer;
        object->rasterize(rasterizer);

        glPopMatrix();
    }

    void draw(
        const AssemblyInstance& assembly_instance,
        const float             time)
    {
        const Assembly* assembly = assembly_instance.find_assembly();

        if (assembly == nullptr)
            return;

        const Transformd transform = assembly_instance.transform_sequence().evaluate(time);

        glPushMatrix();
        glMultMatrixd(transform.get_local_to_parent());

        for (const auto& object_instance : assembly->object_instances())
            draw(object_instance);

        for (const auto& child_assembly_instance : assembly->assembly_instances())
            draw(child_assembly_instance, time);

        glPopMatrix();
    }
}

void LightPathsWidget::paintGL()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    const auto& rc = m_camera.get_rasterization_camera();

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    const double ZNear = 0.01;
    const double ZFar = 1000.0;

    const double fy = tan(rc.m_hfov / rc.m_aspect_ratio * 0.5) * ZNear;
    const double fx = fy * rc.m_aspect_ratio;

    const double shift_x = rc.m_shift_x * 2.0 * fx;
    const double shift_y = rc.m_shift_y * 2.0 * fy;

    const double left   = -fx + shift_x;
    const double right  =  fx + shift_x;
    const double top    = -fy + shift_y;
    const double bottom =  fy + shift_y;

    glFrustum(left, right, top, bottom, ZNear, ZFar);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    if (m_backface_culling_enabled)
        glEnable(GL_CULL_FACE);
    else glDisable(GL_CULL_FACE);

    render_geometry();
    render_light_paths();
}

void LightPathsWidget::keyPressEvent(QKeyEvent* event)
{
    switch (event->key())
    {
      // Home key: display all paths.
      case Qt::Key_Home:
        slot_display_all_light_paths();
        break;

      // Left key: display previous path.
      case Qt::Key_Left:
        slot_display_previous_light_path();
        break;

      // Right key: display next path.
      case Qt::Key_Right:
        slot_display_next_light_path();
        break;

      default:
        QGLWidget::keyPressEvent(event);
        break;
    }
}

void LightPathsWidget::render_geometry() const
{
    glEnable(GL_LIGHTING);

    glMultMatrixd(m_camera_matrix);

    const float time = m_camera.get_shutter_middle_time();

    for (const auto& assembly_instance : m_project.get_scene()->assembly_instances())
        draw(assembly_instance, time);
}

void LightPathsWidget::render_light_paths() const
{
    glDisable(GL_LIGHTING);

    glBegin(GL_LINES);

    assert(m_selected_light_path_index >= -1);

    if (m_selected_light_path_index == -1)
    {
        for (size_t i = 0, e = m_light_paths.size(); i < e; ++i)
            render_light_path(i);
    }
    else
    {
        render_light_path(static_cast<size_t>(m_selected_light_path_index));
    }

    glEnd();
}

void LightPathsWidget::render_light_path(const size_t light_path_index) const
{
    const auto& path = m_light_paths[light_path_index];
    assert(path.m_vertex_end_index - path.m_vertex_begin_index >= 2);

    const auto& light_path_recorder = m_project.get_light_path_recorder();

    LightPathVertex v0;
    light_path_recorder.get_light_path_vertex(path.m_vertex_begin_index, v0);

    for (size_t i = path.m_vertex_begin_index + 1; i < path.m_vertex_end_index; ++i)
    {
        LightPathVertex v1;
        light_path_recorder.get_light_path_vertex(i, v1);

        auto radiance = Color3f::from_array(v1.m_radiance);
        radiance /= sum_value(radiance);
        radiance = linear_rgb_to_srgb(radiance);

        glColor(radiance);

        glVertex3f(v0.m_position[0], v0.m_position[1], v0.m_position[2]);
        glVertex3f(v1.m_position[0], v1.m_position[1], v1.m_position[2]);

        v0 = v1;
    }
}

void LightPathsWidget::dump_selected_light_path() const
{
    if (m_selected_light_path_index == -1)
    {
        if (m_light_paths.empty())
            RENDERER_LOG_INFO("no light path to display.");
        else
        {
            RENDERER_LOG_INFO("displaying all %s light path%s.",
                pretty_uint(m_light_paths.size()).c_str(),
                m_light_paths.size() > 1 ? "s" : "");
        }
    }
    else
    {
        RENDERER_LOG_INFO("displaying light path %s:",
            pretty_int(m_selected_light_path_index + 1).c_str());

        const auto& light_path_recorder = m_project.get_light_path_recorder();
        const auto& path = m_light_paths[m_selected_light_path_index];

        for (size_t i = path.m_vertex_begin_index; i < path.m_vertex_end_index; ++i)
        {
            LightPathVertex v;
            light_path_recorder.get_light_path_vertex(i, v);

            const string entity_name =
                v.m_entity != nullptr
                    ? foundation::format("\"{0}\"", v.m_entity->get_path().c_str())
                    : "n/a";

            RENDERER_LOG_INFO("  vertex " FMT_SIZE_T ": entity: %s - position: (%f, %f, %f) - radiance: (%f, %f, %f) - total radiance: %f",
                i - path.m_vertex_begin_index + 1,
                entity_name.c_str(),
                v.m_position[0], v.m_position[1], v.m_position[2],
                v.m_radiance[0], v.m_radiance[1], v.m_radiance[2],
                v.m_radiance[0] + v.m_radiance[1] + v.m_radiance[2]);
        }
    }
}

}   // namespace studio
}   // namespace appleseed
