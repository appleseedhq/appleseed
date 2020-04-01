
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
#include "glscenelayer.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// appleseed.studio headers.
#include "utility/gl.h"

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
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"

// Qt headers.
#include <QByteArray>
#include <QKeyEvent>
#include <QGLFormat>
#include <QOpenGLFunctions_4_1_Core>
#include <QString>
#include <QSurfaceFormat>

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    // Number of floats per OpenGL vertex for a piece of scene geometry.
    // Vector3 position and Vector3 normal.
    const std::size_t SceneVertexFloatStride = 6;

    // Number of bytes per OpenGL vertex for a piece of scene geometry.
    const std::size_t SceneVertexByteStride = SceneVertexFloatStride * sizeof(float);

    // Number of floats per triangle for a piece of scene geometry.
    const std::size_t SceneTriangleFloatStride = SceneVertexFloatStride * 3;

    // Number of floats per OpenGL vertex for a light path.
    // Vector3 position and Vector3 color.
    const std::size_t LightPathVertexFloatStride = 6;

    // Number of bytes per OpenGL vertex for a piece of scene geometry.
    const std::size_t LightPathVertexByteStride = LightPathVertexFloatStride * sizeof(float);

    // Number of floats per line for a light path.
    const std::size_t LightPathVertexLineFloatStride = LightPathVertexFloatStride * 2;

    // Number of floats per OpenGL transform matrix.
    const std::size_t TransformFloatStride = 16;

    // Number of bytes per OpenGL transform matrix.
    const std::size_t TransformByteStride = TransformFloatStride * sizeof(float);

    struct OpenGLRasterizer
      : public ObjectRasterizer
    {
        std::vector<float>   m_buffer;
        std::size_t          m_prim_count;

        void begin_object(const std::size_t triangle_count_hint) override
        {
            m_buffer.clear();
            m_buffer.reserve(triangle_count_hint * SceneTriangleFloatStride);
            m_prim_count = 0;
        }

        void end_object() override {}

        void rasterize(const Triangle& triangle) override
        {
            const float temp_store[SceneTriangleFloatStride] =
            {
                static_cast<float>(triangle.m_v0[0]), static_cast<float>(triangle.m_v0[1]), static_cast<float>(triangle.m_v0[2]),
                static_cast<float>(triangle.m_n0[0]), static_cast<float>(triangle.m_n0[1]), static_cast<float>(triangle.m_n0[2]),

                static_cast<float>(triangle.m_v1[0]), static_cast<float>(triangle.m_v1[1]), static_cast<float>(triangle.m_v1[2]),
                static_cast<float>(triangle.m_n1[0]), static_cast<float>(triangle.m_n1[1]), static_cast<float>(triangle.m_n1[2]),

                static_cast<float>(triangle.m_v2[0]), static_cast<float>(triangle.m_v2[1]), static_cast<float>(triangle.m_v2[2]),
                static_cast<float>(triangle.m_n2[0]), static_cast<float>(triangle.m_n2[1]), static_cast<float>(triangle.m_n2[2]),
            };

            m_buffer.reserve(m_buffer.size() + SceneTriangleFloatStride);
            m_buffer.insert(m_buffer.end(), temp_store, temp_store + SceneTriangleFloatStride);
            m_prim_count++;
        }
    };
}

GLSceneLayer::GLSceneLayer(
    const Project&                  project,
    const std::size_t               width,
    const std::size_t               height)
  : m_project(project)
  , m_camera(*m_project.get_uncached_active_camera())
  , m_backface_culling_enabled(false)
  , m_initialized(false)
{
    m_camera.transform_sequence().prepare();
    const float time = m_camera.get_shutter_middle_time();
    set_transform(m_camera.transform_sequence().evaluate(time));
}

GLSceneLayer::~GLSceneLayer()
{
    cleanup_gl_data();
}

void GLSceneLayer::set_gl_functions(QOpenGLFunctions_4_1_Core* functions)
{
    m_gl = functions;
}

void GLSceneLayer::set_transform(const Transformd& transform)
{
    m_camera_matrix = transform.get_parent_to_local();
    m_gl_view_matrix = transpose(m_camera_matrix);
}

void GLSceneLayer::slot_synchronize_camera()
{
    m_camera.transform_sequence().clear();
    m_camera.transform_sequence()
        .set_transform(
            0.0f,
            Transformd::from_local_to_parent(inverse(m_camera_matrix)));
}

void GLSceneLayer::load_object_instance(
    const ObjectInstance&   object_instance,
    const Matrix4f&         assembly_transform_matrix)
{
    const Object* object = object_instance.find_object();

    // This would already be logged in GLSceneLayer::load_scene_data
    if (object == nullptr)
        return;

    const Transformd& transform = object_instance.get_transform();
    const Matrix4f& object_transform_matrix(transform.get_local_to_parent());
    const Matrix4f model_matrix = assembly_transform_matrix * object_transform_matrix;

    // Object vertex buffer data has already been loaded, just add an instance.
    const std::string obj_name = std::string(object->get_name());
    const std::size_t buffer_index = m_scene_object_index_map.at(obj_name);

    const GLuint object_instances_vbo = m_scene_object_instance_vbos[buffer_index];
    const GLuint object_vao = m_scene_object_vaos[buffer_index];
    const GLsizei current_instance = m_scene_object_current_instances[buffer_index];
    m_scene_object_current_instances[buffer_index] += 1;

    m_gl->glBindVertexArray(object_vao);
    m_gl->glBindBuffer(GL_ARRAY_BUFFER, object_instances_vbo);

    const Matrix4f gl_matrix = transpose(model_matrix);
    m_gl->glBufferSubData(
        GL_ARRAY_BUFFER,
        current_instance * TransformByteStride,
        TransformByteStride,
        reinterpret_cast<const GLvoid*>(&gl_matrix[0]));
}

void GLSceneLayer::load_assembly_instance(
    const AssemblyInstance& assembly_instance,
    const float             time)
{
    const Assembly* assembly = assembly_instance.find_assembly();

    // This would already be logged in GLSceneLayer::load_scene_data
    if (assembly == nullptr)
        return;

    const Transformd transform = assembly_instance.transform_sequence().evaluate(time);
    const Matrix4f transform_matrix(transform.get_local_to_parent());

    for (const auto& object_instance : assembly->object_instances())
        load_object_instance(object_instance, transform_matrix);

    for (const auto& child_assembly_instance : assembly->assembly_instances())
        load_assembly_instance(child_assembly_instance, time);
}

void GLSceneLayer::load_object_data(const Object& object)
{
    const std::string obj_name = std::string(object.get_name());

    RENDERER_LOG_DEBUG("uploading OpenGL mesh data for object \"%s\"...", obj_name.c_str());

    if (m_scene_object_index_map.count(obj_name) == 0)
    {
        // Object vertex buffer data has not been loaded, load it.
        const std::size_t buffer_index = m_scene_object_data_vbos.size();

        GLuint object_vao;
        m_gl->glGenVertexArrays(1, &object_vao);

        GLuint object_data_vbo;
        m_gl->glGenBuffers(1, &object_data_vbo);
        m_gl->glBindVertexArray(object_vao);
        m_gl->glBindBuffer(GL_ARRAY_BUFFER, object_data_vbo);

        m_scene_object_vaos.push_back(object_vao);
        m_scene_object_data_vbos.push_back(object_data_vbo);
        m_scene_object_index_map[obj_name] = buffer_index;

        OpenGLRasterizer rasterizer;
        object.rasterize(rasterizer);
        m_scene_object_data_index_counts.push_back(static_cast<GLsizei>(rasterizer.m_prim_count * 3));

        m_gl->glBufferData(
            GL_ARRAY_BUFFER,
            rasterizer.m_buffer.size() * sizeof(float),
            reinterpret_cast<const GLvoid*>(&rasterizer.m_buffer[0]),
            GL_STATIC_DRAW);

        m_gl->glVertexAttribPointer(
            0,
            3,
            GL_FLOAT,
            GL_FALSE,
            SceneVertexByteStride,
            reinterpret_cast<const GLvoid*>(0));
        m_gl->glVertexAttribPointer(
            1,
            3,
            GL_FLOAT,
            GL_FALSE,
            SceneVertexByteStride,
            reinterpret_cast<const GLvoid*>(SceneVertexByteStride / 2));

        m_gl->glEnableVertexAttribArray(0);
        m_gl->glEnableVertexAttribArray(1);
    }
}

void GLSceneLayer::load_assembly_data(const Assembly& assembly)
{
    for (const Object& object : assembly.objects())
        load_object_data(object);

    for (const Assembly& child_assembly : assembly.assemblies())
        load_assembly_data(child_assembly);
}

void GLSceneLayer::load_scene_data()
{
    const float time = m_camera.get_shutter_middle_time();

    RENDERER_LOG_DEBUG("uploading OpenGL scene data...");

    // First, load all the unique object vertex buffer data into static VBOs
    for (const auto& assembly : m_project.get_scene()->assemblies())
        load_assembly_data(assembly);

    // Create space for per-instance data
    for (std::size_t i = 0; i < m_scene_object_index_map.size(); i++)
    {
        m_scene_object_instance_vbos.push_back(0);
        m_scene_object_instance_counts.push_back(0);
        m_scene_object_current_instances.push_back(0);
    }

    // Generate instance buffers for each object
    if (m_scene_object_instance_vbos.size() > 0)
        m_gl->glGenBuffers(
            static_cast<GLsizei>(m_scene_object_instance_vbos.size()),
            &m_scene_object_instance_vbos[0]);

    // Figure out how many instances of each mesh are required for all assembly instances
    for (const AssemblyInstance& assembly_instance : m_project.get_scene()->assembly_instances())
    {
        const Assembly* assembly = assembly_instance.find_assembly();

        if (assembly == nullptr)
        {
            RENDERER_LOG_ERROR(
                "assembly instance \"%s\" has null base assembly reference.",
                assembly_instance.get_name());
            continue;
        }

        for (const ObjectInstance& object_instance : assembly->object_instances())
        {
            const Object* object = object_instance.find_object();

            if (object == nullptr)
            {
                RENDERER_LOG_ERROR(
                    "object instance \"%s\" has null base object reference.",
                    object_instance.get_name());
                continue;
            }

            const std::string obj_name = std::string(object->get_name());
            const std::size_t buf_idx = m_scene_object_index_map[obj_name];
            m_scene_object_instance_counts[buf_idx] += 1;
        }
    }

    // Setup instance buffers by allocating a buffer big enough for the number
    // of required instances and setting up vertex attributes.
    for (std::size_t i = 0; i < m_scene_object_instance_vbos.size(); i++)
    {
        const GLuint object_vao = m_scene_object_vaos[i];
        const GLuint object_instance_vbo = m_scene_object_instance_vbos[i];
        const GLsizei object_instance_count = m_scene_object_instance_counts[i];

        m_gl->glBindVertexArray(object_vao);
        m_gl->glBindBuffer(GL_ARRAY_BUFFER, object_instance_vbo);
        m_gl->glBufferData(
            GL_ARRAY_BUFFER,
            object_instance_count * TransformByteStride,
            NULL,
            GL_DYNAMIC_DRAW);

        // Attributes for a 4x4 model matrix requires four separate attributes
        // to be setup, one for each column of the matrix.
        for (int i = 0; i < 4; i++)
        {
            m_gl->glVertexAttribPointer(
                2 + i,
                4,
                GL_FLOAT,
                GL_FALSE,
                TransformByteStride,
                reinterpret_cast<const GLvoid*>(sizeof(float) * 4 * i));
            m_gl->glEnableVertexAttribArray(2 + i);
            m_gl->glVertexAttribDivisor(2 + i, 1);
        }
    }

    // Actually load the transform data for each instance into the allocated instance buffers.
    for (const AssemblyInstance& assembly_instance : m_project.get_scene()->assembly_instances())
        load_assembly_instance(assembly_instance, time);
}


void GLSceneLayer::init_gl(QSurfaceFormat format)
{
    // If there was already previous data, clean up.
    cleanup_gl_data();

    const QByteArray vertex_shader = load_gl_shader("scene.vert");
    const QByteArray fragment_shader = load_gl_shader("scene.frag");

    m_scene_shader_program = create_shader_program(
        m_gl,
        &vertex_shader,
        &fragment_shader);

    m_scene_view_mat_location = m_gl->glGetUniformLocation(m_scene_shader_program, "u_view");
    m_scene_proj_mat_location = m_gl->glGetUniformLocation(m_scene_shader_program, "u_proj");

    m_depthonly_shader_program = create_shader_program(
        m_gl,
        &vertex_shader);

    m_depthonly_view_mat_location = m_gl->glGetUniformLocation(m_depthonly_shader_program, "u_view");
    m_depthonly_proj_mat_location = m_gl->glGetUniformLocation(m_depthonly_shader_program, "u_proj");

    const float z_near = 0.01f;
    const float z_far = 1000.0f;

    const RasterizationCamera& rc = m_camera.get_rasterization_camera();

    const float fy = std::tan(rc.m_hfov / rc.m_aspect_ratio * 0.5) * z_near;
    const float fx = fy * rc.m_aspect_ratio;

    const float shift_x = rc.m_shift_x * 2.0 * fx;
    const float shift_y = rc.m_shift_y * 2.0 * fy;

    const float left   = -fx + shift_x;
    const float right  =  fx + shift_x;
    const float top    = -fy + shift_y;
    const float bottom =  fy + shift_y;

    // Top and bottom are flipped because QOpenGLWidget draws to a framebuffer object and then blits
    // from the FBO to the default framebuffer, which flips the image.
    m_gl_proj_matrix = transpose(Matrix4f::make_frustum(top, bottom, left, right, z_near, z_far));

    load_scene_data();

    m_initialized = true;
}

void GLSceneLayer::cleanup_gl_data()
{
    if (!m_scene_object_vaos.empty())
    {
        m_gl->glDeleteVertexArrays(
            static_cast<GLsizei>(m_scene_object_vaos.size()),
            &m_scene_object_vaos[0]);
        m_scene_object_vaos.clear();
    }

    if (!m_scene_object_data_vbos.empty())
    {
        m_gl->glDeleteBuffers(
            static_cast<GLsizei>(m_scene_object_data_vbos.size()),
            &m_scene_object_data_vbos[0]);
        m_scene_object_data_vbos.clear();
    }

    if (!m_scene_object_instance_vbos.empty())
    {
        m_gl->glDeleteBuffers(
            static_cast<GLsizei>(m_scene_object_instance_vbos.size()),
            &m_scene_object_instance_vbos[0]);
        m_scene_object_instance_vbos.clear();
    }

    if (m_scene_shader_program != 0)
    {
        m_gl->glDeleteProgram(m_scene_shader_program);
    }

    if (m_depthonly_shader_program != 0)
    {
        m_gl->glDeleteProgram(m_depthonly_shader_program);
    }

    m_scene_object_index_map.clear();
    m_scene_object_data_index_counts.clear();
    m_scene_object_instance_counts.clear();
    m_scene_object_current_instances.clear();
}

void GLSceneLayer::toggle_backface_culling(const bool checked)
{
    m_backface_culling_enabled = checked;
}

void GLSceneLayer::draw()
{
    if (!m_initialized)
        return;

    if (m_backface_culling_enabled)
        glEnable(GL_CULL_FACE);
    else
        glDisable(GL_CULL_FACE);

    m_gl->glDepthMask(GL_FALSE);
    m_gl->glEnable(GL_DEPTH_TEST);
    m_gl->glDepthFunc(GL_LEQUAL);
    m_gl->glUseProgram(m_scene_shader_program);

    m_gl->glUniformMatrix4fv(
        m_scene_view_mat_location,
        1,
        false,
        const_cast<const GLfloat*>(&m_gl_view_matrix[0]));
    m_gl->glUniformMatrix4fv(
        m_scene_proj_mat_location,
        1,
        false,
        const_cast<const GLfloat*>(&m_gl_proj_matrix[0]));
    
    render_scene();

    m_gl->glDepthMask(GL_TRUE);

    glDisable(GL_CULL_FACE);
}

void GLSceneLayer::draw_depth_only()
{
    if (!m_initialized)
        return;

    if (m_backface_culling_enabled)
        glEnable(GL_CULL_FACE);
    else glDisable(GL_CULL_FACE);

    m_gl->glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    m_gl->glDepthMask(GL_TRUE);
    m_gl->glEnable(GL_DEPTH_TEST);
    m_gl->glDepthFunc(GL_LEQUAL);
    m_gl->glUseProgram(m_depthonly_shader_program);

    m_gl->glUniformMatrix4fv(
        m_depthonly_view_mat_location,
        1,
        false,
        const_cast<const GLfloat*>(&m_gl_view_matrix[0]));
    m_gl->glUniformMatrix4fv(
        m_depthonly_proj_mat_location,
        1,
        false,
        const_cast<const GLfloat*>(&m_gl_proj_matrix[0]));

    render_scene();

    m_gl->glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

    glDisable(GL_CULL_FACE);
}

void GLSceneLayer::render_scene()
{
    for (std::size_t i = 0; i < m_scene_object_data_vbos.size(); i++)
    {
        const GLuint vao = m_scene_object_vaos[i];
        const int index_count = m_scene_object_data_index_counts[i];
        const int instance_count = m_scene_object_instance_counts[i];

        m_gl->glBindVertexArray(vao);
        m_gl->glDrawArraysInstanced(
            GL_TRIANGLES,
            0,
            index_count,
            instance_count);
    }
}

}   // namespace studio
}   // namespace appleseed

