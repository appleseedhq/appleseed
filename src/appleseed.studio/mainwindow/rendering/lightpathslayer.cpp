
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
#include "lightpathslayer.h"

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
#include "renderer/kernel/lighting/lightpathrecorder.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/scalar.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"

// Qt headers.
#include <QKeyEvent>
#include <QOpenGLFunctions_4_1_Core>
#include <QString>
#include <QSurfaceFormat>

// Standard headers.
#include <algorithm>
#include <array>
#include <cmath>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

namespace
{
    // Byte stride of a vec3.
    constexpr std::size_t Vec3ByteStride = sizeof(float) * 3;

    // Struct of an element of the "others" vertex buffer
    struct OtherAttributes
    {
        GLint   v_bitmask;
        GLfloat v_color[3];
        GLfloat v_surface_normal[3];
    };
}

LightPathsLayer::LightPathsLayer(
    const Project&             project,
    const LightPathsManager&   light_paths_manager,
    const std::size_t          width,
    const std::size_t          height)
  : m_project(project)
  , m_light_paths_manager(light_paths_manager)
  , m_camera(*m_project.get_uncached_active_camera())
  , m_gl_initialized(false)
  , m_width(width)
  , m_height(height)
{
    const float time = m_camera.get_shutter_middle_time();
    set_transform(m_camera.transform_sequence().evaluate(time));

    connect(
        &m_light_paths_manager, &LightPathsManager::signal_light_path_selection_changed,
        this, &LightPathsLayer::slot_light_path_selection_changed);
}

void LightPathsLayer::resize(const std::size_t width, const std::size_t height)
{
    m_width = width,
    m_height = height;
}

void LightPathsLayer::set_gl_functions(QOpenGLFunctions_4_1_Core* functions)
{
    m_gl = functions;
}

void LightPathsLayer::update_render_camera_transform()
{
    const float time = m_camera.get_shutter_middle_time();
    m_render_camera_matrix = m_camera.transform_sequence().evaluate(time).get_parent_to_local();
    m_gl_render_view_matrix = transpose(m_render_camera_matrix);
}

void LightPathsLayer::set_transform(const Transformd& transform)
{
    m_camera_matrix = transform.get_parent_to_local();
    m_gl_view_matrix = transpose(m_camera_matrix);
}

void LightPathsLayer::slot_synchronize_camera()
{
    m_camera.transform_sequence().clear();
    m_camera.transform_sequence().set_transform(
        0.0f,
        Transformd::from_local_to_parent(inverse(m_camera_matrix)));
}

void LightPathsLayer::slot_light_path_selection_changed(
    const bool      display_light_paths,
    const int       selected_light_path_index,
    const int       total_light_paths)
{
    load_light_paths_data();
}

void LightPathsLayer::load_light_paths_data()
{
    m_path_terminator_vertex_indices.clear();

    const renderer::LightPathArray& light_paths = m_light_paths_manager.get_light_paths();

    if (!light_paths.empty())
    {
        m_path_terminator_vertex_indices.push_back(0);

        const auto& light_path_recorder = m_project.get_light_path_recorder();

        m_total_triangle_count = 0;

        // Vertex count * 4 since we will be adding two vertices for each point along the line and
        // will be adding each point twice for the beginning and end parts of each segment.
        // Add two because we need extra at the front and back for the extra 'previous' and 'next' attributes.
        const std::size_t total_gl_vertex_count = 2 * (light_path_recorder.get_vertex_count() + 2);

        std::vector<unsigned int> indices;
        std::vector<float> positions_buffer;
        // * 3 since we want 3 floats per position
        positions_buffer.reserve(total_gl_vertex_count * 3);
        indices.reserve(total_gl_vertex_count);

        // Add an empty vertex at the beginning to serve as first 'previous'.
        std::array<float, 6> empty_positions =
        {
            0.0, 0.0, 0.0,
            0.0, 0.0, 0.0,
        };
        positions_buffer.insert(
            positions_buffer.end(),
            empty_positions.begin(),
            empty_positions.end());
        
        std::vector<OtherAttributes> others_buffer;
        others_buffer.reserve(total_gl_vertex_count);
        std::array<OtherAttributes, 4> others;

        for (std::size_t light_path_idx = 0; light_path_idx < light_paths.size(); light_path_idx++)
        {
            const LightPath& path = light_paths[light_path_idx];
            assert(path.m_vertex_end_index - path.m_vertex_begin_index >= 2);

            LightPathVertex prev;
            light_path_recorder.get_light_path_vertex(path.m_vertex_begin_index, prev);

            for (std::size_t vertex_idx = path.m_vertex_begin_index + 1; vertex_idx < path.m_vertex_end_index; vertex_idx++)
            {
                LightPathVertex vert;
                light_path_recorder.get_light_path_vertex(vertex_idx, vert);

                const Color3f piece_radiance =
                    linear_rgb_to_srgb(
                        Color3f::from_array(vert.m_radiance));
                const float piece_luminance = luminance(piece_radiance);
                const Color3f normalized_piece_radiance = piece_radiance / piece_luminance;

                std::array<GLfloat, 12> positions_temp_store = {
                    prev.m_position[0], prev.m_position[1], prev.m_position[2],
                    prev.m_position[0], prev.m_position[1], prev.m_position[2],
                    vert.m_position[0], vert.m_position[1], vert.m_position[2],
                    vert.m_position[0], vert.m_position[1], vert.m_position[2],
                };
                positions_buffer.insert(
                    positions_buffer.end(),
                    positions_temp_store.begin(),
                    positions_temp_store.end());

                const auto start_index = static_cast<unsigned int>(others_buffer.size());
                
                others = {{
                    {
                        0,
                        {normalized_piece_radiance[0], normalized_piece_radiance[1], normalized_piece_radiance[2]},
                        {prev.m_surface_normal[0], prev.m_surface_normal[1], prev.m_surface_normal[2]}
                    },
                    {
                        1,
                        {normalized_piece_radiance[0], normalized_piece_radiance[1], normalized_piece_radiance[2]},
                        {prev.m_surface_normal[0], prev.m_surface_normal[1], prev.m_surface_normal[2]}
                    },
                    {
                        2,
                        {normalized_piece_radiance[0], normalized_piece_radiance[1], normalized_piece_radiance[2]},
                        {vert.m_surface_normal[0], vert.m_surface_normal[1], vert.m_surface_normal[2]}
                    },
                    {
                        3,
                        {normalized_piece_radiance[0], normalized_piece_radiance[1], normalized_piece_radiance[2]},
                        {vert.m_surface_normal[0], vert.m_surface_normal[1], vert.m_surface_normal[2]}
                    },
                }};
                others_buffer.insert(
                    others_buffer.end(),
                    others.begin(),
                    others.end());

                std::array<unsigned int, 6> indices_scratch = {
                    start_index, start_index + 1, start_index + 2,
                    start_index + 1, start_index + 2, start_index + 3,
                };
                indices.insert(
                    indices.end(),
                    indices_scratch.begin(),
                    indices_scratch.end());

                m_total_triangle_count += 6;
                prev = vert;
            }

            m_path_terminator_vertex_indices.push_back(static_cast<GLsizei>(others_buffer.size() - 1));
        }

        // Add an empty vertex at the end to serve as last 'next'.
        positions_buffer.insert(
            positions_buffer.end(),
            empty_positions.begin(),
            empty_positions.end());
        
        // Upload the data to the buffers.
        m_gl->glBindBuffer(GL_ARRAY_BUFFER, m_positions_vbo);
        m_gl->glBufferData(
            GL_ARRAY_BUFFER,
            positions_buffer.size() * sizeof(float),
            reinterpret_cast<const GLvoid*>(&positions_buffer[0]),
            GL_STATIC_DRAW);
        m_gl->glBindBuffer(GL_ARRAY_BUFFER, m_others_vbo);
        m_gl->glBufferData(
            GL_ARRAY_BUFFER,
            others_buffer.size() * sizeof(OtherAttributes),
            reinterpret_cast<const GLvoid*>(&others_buffer[0]),
            GL_STATIC_DRAW);
        m_gl->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_indices_ebo);
        m_gl->glBufferData(
            GL_ELEMENT_ARRAY_BUFFER,
            indices.size() * sizeof(unsigned int),
            reinterpret_cast<const GLvoid*>(&indices[0]),
            GL_STATIC_DRAW);
    }
}

void LightPathsLayer::init_gl(QSurfaceFormat format)
{
    // If there was already previous data, clean up.
    cleanup_gl_data();

    glEnable(GL_DEPTH_TEST);

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

    const QByteArray vertex_shader = load_gl_shader("lightpaths.vert");
    const QByteArray fragment_shader = load_gl_shader("lightpaths.frag");

    m_shader_program = create_shader_program(
        m_gl,
        &vertex_shader,
        &fragment_shader);

    m_view_mat_loc = m_gl->glGetUniformLocation(m_shader_program, "u_view");
    m_proj_mat_loc = m_gl->glGetUniformLocation(m_shader_program, "u_proj");
    m_res_loc = m_gl->glGetUniformLocation(m_shader_program, "u_res");
    m_first_selected_loc = m_gl->glGetUniformLocation(m_shader_program, "u_first_selected");
    m_last_selected_loc = m_gl->glGetUniformLocation(m_shader_program, "u_last_selected");

    const float z_near = 0.01f;
    const float z_far = 1000.0f;

    const RasterizationCamera& rc = m_camera.get_rasterization_camera();

    const float fy = tan(rc.m_hfov / rc.m_aspect_ratio * 0.5) * z_near;
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

    GLuint temp_light_paths_vao = 0;
    m_gl->glGenVertexArrays(1, &temp_light_paths_vao);
    m_light_paths_vao = temp_light_paths_vao;

    GLuint temp_vbos[3] = {0, 0, 0};
    m_gl->glGenBuffers(3, &temp_vbos[0]);
    m_positions_vbo = temp_vbos[0];
    m_others_vbo = temp_vbos[1];
    m_indices_ebo = temp_vbos[2];

    m_gl->glBindVertexArray(m_light_paths_vao);

    m_gl->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_indices_ebo);

    // v_previous, v_position, and v_next all reference offsets into the same vertex data buffer.
    // v_previous is at vertex offset 0, v_position is at vertex offset 2 and v_next is at vertex offset 4.
    // See http://codeflow.org/entries/2012/aug/05/webgl-rendering-of-solid-trails/ for reference.
    m_gl->glBindBuffer(GL_ARRAY_BUFFER, m_positions_vbo);

    // vec3 v_previous;
    GLint layout_attribute_location = 0;
    m_gl->glVertexAttribPointer(
        layout_attribute_location,
        3,
        GL_FLOAT,
        GL_FALSE,
        Vec3ByteStride,
        reinterpret_cast<const GLvoid*>(0));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    // vec3 v_position;
    layout_attribute_location = 1;
    m_gl->glVertexAttribPointer(
        layout_attribute_location,
        3,
        GL_FLOAT,
        GL_FALSE,
        Vec3ByteStride,
        reinterpret_cast<const GLvoid*>(Vec3ByteStride * 2));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    // vec3 v_next;
    layout_attribute_location = 2;
    m_gl->glVertexAttribPointer(
        layout_attribute_location,
        3,
        GL_FLOAT,
        GL_FALSE,
        Vec3ByteStride,
        reinterpret_cast<const GLvoid*>(Vec3ByteStride * 4));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    // The rest of the attributes are stored in a separate data buffer, interleaved.
    m_gl->glBindBuffer(GL_ARRAY_BUFFER, m_others_vbo);

    // This attribute stores 2 bools:
    // bit 0: normal direction which maps to -1.0 (false) or 1.0 (true)
    // bit 1: whether this is the end vertex of a new path
    // int v_bitmask;
    layout_attribute_location = 3;
    unsigned long long offset = 0;
    m_gl->glVertexAttribIPointer(
        layout_attribute_location,
        1,
        GL_INT,
        sizeof(OtherAttributes),
        reinterpret_cast<const GLvoid*>(offset));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    // vec3 v_color;
    layout_attribute_location = 4;
    offset += sizeof(GLint);
    m_gl->glVertexAttribPointer(
        layout_attribute_location,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(OtherAttributes),
        reinterpret_cast<const GLvoid*>(offset));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    // vec3 v_surface_normal;
    layout_attribute_location = 5;
    offset += 3 * sizeof(GLfloat);
    m_gl->glVertexAttribPointer(
        layout_attribute_location,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(OtherAttributes),
        reinterpret_cast<const GLvoid*>(offset));
    m_gl->glEnableVertexAttribArray(layout_attribute_location);

    m_gl->glBindVertexArray(0);
    m_gl->glBindBuffer(GL_ARRAY_BUFFER, 0);

    load_light_paths_data();

    m_gl_initialized = true;
}

void LightPathsLayer::cleanup_gl_data()
{
    if (m_shader_program != 0)
        m_gl->glDeleteProgram(m_shader_program);
}

void LightPathsLayer::draw_render_camera() const
{
    const auto gl_view_matrix = const_cast<const GLfloat*>(&m_gl_render_view_matrix[0]);
    render_scene(gl_view_matrix);
}

void LightPathsLayer::draw() const
{
    const auto gl_view_matrix = const_cast<const GLfloat*>(&m_gl_view_matrix[0]);
    render_scene(gl_view_matrix);
}

void LightPathsLayer::render_scene(const GLfloat* gl_view_matrix) const
{
    if (!m_gl_initialized)
        return;

    if (m_total_triangle_count > 1)
    {
        const int selected_light_path_index = m_light_paths_manager.get_selected_light_path_index();

        GLint first_selected, last_selected;

        if (selected_light_path_index == -1)
        {
            first_selected = 0;
            last_selected = static_cast<GLint>(m_path_terminator_vertex_indices[m_path_terminator_vertex_indices.size() - 1]);
        }
        else
        {
            assert(m_path_terminator_vertex_indices.size() > selected_light_path_index + 1);
            first_selected = static_cast<GLint>(m_path_terminator_vertex_indices[selected_light_path_index]);
            last_selected = static_cast<GLint>(m_path_terminator_vertex_indices[selected_light_path_index + 1]);
        }

        m_gl->glUseProgram(m_shader_program);

        m_gl->glUniformMatrix4fv(
            m_view_mat_loc,
            1,
            false,
            gl_view_matrix);
        m_gl->glUniformMatrix4fv(
            m_proj_mat_loc,
            1,
            false,
            const_cast<const GLfloat*>(&m_gl_proj_matrix[0]));
        m_gl->glUniform2f(
            m_res_loc,
            (GLfloat)m_width,
            (GLfloat)m_height);
        m_gl->glUniform1i(
            m_first_selected_loc,
            first_selected);
        m_gl->glUniform1i(
            m_last_selected_loc,
            last_selected);

        m_gl->glBindVertexArray(m_light_paths_vao);

        m_gl->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_indices_ebo);
        m_gl->glDrawElements(GL_TRIANGLES, m_total_triangle_count, GL_UNSIGNED_INT, static_cast<const GLvoid*>(0));
        m_gl->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    }
}

}   // namespace studio
}   // namespace appleseed
