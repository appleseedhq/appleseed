
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

// Qt headers.
#include <QOpenGLWidget>
#include <QOpenGLFunctions_4_1_Core>

// standard headers.
#include <string>

// Forward declarations.
class QByteArray;
class QString;

namespace appleseed {
namespace studio {

// Get a string from an OpenGL shader kind value.
const std::string shader_kind_to_string(const GLint shader_kind);

// Compile a GL shader.
void compile_shader(
    QOpenGLFunctions_4_1_Core* f,
    const GLuint               shader,
    const GLsizei              count,
    const GLchar**             src_string,
    const GLint*               length);

// Link a GL shader program.
void link_shader_program(
    QOpenGLFunctions_4_1_Core*  f,
    const GLuint                program,
    const GLuint                vert,
    const GLuint                frag);

// Create a GL shader program with a vertex and optional fragment shader.
void create_shader_program(
    QOpenGLFunctions_4_1_Core*  f,
    GLuint&                     program,
    const QByteArray*           vert_source,
    const QByteArray*           frag_source);

// Load a GLSL shader from file into a QByteArray.
QByteArray load_gl_shader(const QString& base_name);

}   // namespace studio
}   // namespace appleseed

