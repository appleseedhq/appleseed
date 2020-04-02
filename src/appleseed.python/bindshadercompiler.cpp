
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/platform/python.h"
#include "foundation/utility/api/apistring.h"

// appleseed.renderer headers.
#include "renderer/api/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/python.h"

// Standard headers.
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    auto_release_ptr<ShaderCompiler> create_shader_compiler(const char* stdosl_path)
    {
        return ShaderCompilerFactory::create(stdosl_path);
    }

    bpy::object compile_buffer(const ShaderCompiler* compiler, const std::string& buffer)
    {
        APIString result;

        if (compiler->compile_buffer(buffer.c_str(), result))
            return bpy::str(result.c_str());

        // Return None if compilation failed.
        return bpy::object();
    }
}

void bind_shader_compiler()
{
    bpy::class_<ShaderCompiler, auto_release_ptr<ShaderCompiler>, boost::noncopyable>("ShaderCompiler", bpy::no_init)
        .def("__init__", bpy::make_constructor(create_shader_compiler))
        .def("clear_options", &ShaderCompiler::clear_options)
        .def("add_option", &ShaderCompiler::add_option)
        .def("compile_buffer", compile_buffer);
}
