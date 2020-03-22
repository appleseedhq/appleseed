
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

#include "gl.h"

// appleseed.studio headers.
#include "renderer/api/utility.h"

// appleseed.qtcommon headers.
#include "utility/miscellaneous.h"

// Qt headers.
#include <QByteArray>
#include <QFile>
#include <QString>

using namespace appleseed::qtcommon;
using namespace std;
using namespace renderer;

namespace appleseed {
namespace studio {

const string shader_kind_to_string(const GLint shader_kind)
{
    switch (shader_kind)
    {
        case GL_VERTEX_SHADER:      return "Vertex";
        case GL_FRAGMENT_SHADER:    return "Fragment";
        default:                    return "Unknown Kind";
    }
}

void compile_shader(
    QOpenGLFunctions_4_1_Core* f,
    const GLuint               shader,
    const GLsizei              count,
    const GLchar**             src_string,
    const GLint*               length)
{
    f->glShaderSource(shader, count, src_string, length);
    f->glCompileShader(shader);
    GLint success;
    f->glGetShaderiv(shader, GL_COMPILE_STATUS, &success);

    if (!success)
    {
        char info_log[1024];
        f->glGetShaderInfoLog(shader, 1024, NULL, info_log);

        GLint shader_kind;
        f->glGetShaderiv(shader, GL_SHADER_TYPE, &shader_kind);
        string shader_kind_string = shader_kind_to_string(shader_kind);

        RENDERER_LOG_ERROR("opengl: %s shader compilation failed:\n%s", shader_kind_string.c_str(), info_log);
    }
}

void link_shader_program(
    QOpenGLFunctions_4_1_Core*  f,
    const GLuint                program,
    const GLuint                vert,
    const GLuint                frag)
{
    f->glAttachShader(program, vert);

    if (frag != 0)
        f->glAttachShader(program, frag);

    f->glLinkProgram(program);

    GLint success;
    f->glGetProgramiv(program, GL_LINK_STATUS, &success);

    if (!success)
    {
        char info_log[1024];
        f->glGetProgramInfoLog(program, 1024, NULL, info_log);
        RENDERER_LOG_ERROR("opengl: shader program linking failed:\n%s", info_log);
    }
}

GLuint create_shader_program(
    QOpenGLFunctions_4_1_Core*  f,
    const QByteArray*           vert_source,
    const QByteArray*           frag_source)
{
    assert(vert_source != 0);
    const bool has_frag_shader = frag_source != 0;

    GLuint vert = f->glCreateShader(GL_VERTEX_SHADER);
    GLuint frag = has_frag_shader ? f->glCreateShader(GL_FRAGMENT_SHADER) : 0;

    auto gl_vert_source = static_cast<const GLchar*>(vert_source->constData());
    auto gl_vert_source_length = static_cast<const GLint>(vert_source->size());

    compile_shader(f, vert, 1, &gl_vert_source, &gl_vert_source_length);

    if (has_frag_shader)
    {
        auto gl_frag_source = static_cast<const GLchar*>(frag_source->constData());
        auto gl_frag_source_length = static_cast<const GLint>(frag_source->size());
        compile_shader(f, frag, 1, &gl_frag_source, &gl_frag_source_length);
    }

    const GLuint program = f->glCreateProgram();
    link_shader_program(f, program, vert, frag);

    f->glDeleteShader(vert);
    if (has_frag_shader)
        f->glDeleteShader(frag);

    return program;
}

QByteArray load_gl_shader(const QString& base_name)
{
    const QString resource_path(QString(":/shaders/%1").arg(base_name));

    QFile file(resource_path);
    file.open(QFile::ReadOnly);

    return file.readAll();
}

}   // namespace studio
}   // namespace appleseed
