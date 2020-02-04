
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "shadercompiler.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/oiioerrorhandler.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslcomp.h"
#include "foundation/platform/_endoslheaders.h"

// Standard headers.
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// ShaderCompiler class implementation.
//

struct ShaderCompiler::Impl
{
    std::string                         m_stdosl_path;
    std::unique_ptr<OIIOErrorHandler>   m_error_handler;
    std::vector<std::string>            m_options;

    explicit Impl(const char* stdosl_path)
      : m_stdosl_path(stdosl_path)
    {
        m_error_handler.reset(new OIIOErrorHandler());
    #ifndef NDEBUG
        m_error_handler->verbosity(OIIO::ErrorHandler::VERBOSE);
    #endif
    }
};

ShaderCompiler::ShaderCompiler(const char* stdosl_path)
  : impl(new Impl(stdosl_path))
{
}

ShaderCompiler::~ShaderCompiler()
{
    delete impl;
}

void ShaderCompiler::release()
{
    delete this;
}

void ShaderCompiler::clear_options()
{
    impl->m_options.clear();
}

void ShaderCompiler::add_option(const char* option)
{
    impl->m_options.push_back(option);
}

bool ShaderCompiler::compile_buffer(
    const char* source_code,
    APIString&  result) const
{

    OSL::OSLCompiler compiler(impl->m_error_handler.get());

    std::string buffer;
    const bool ok =
        compiler.compile_buffer(
            source_code,
            buffer,
            impl->m_options,
            impl->m_stdosl_path.c_str());
    if (ok)
        result = APIString(buffer.c_str());

    return ok;
}


//
// ShaderCompilerFactory class implementation.
//

auto_release_ptr<ShaderCompiler> ShaderCompilerFactory::create(
    const char* stdosl_path)
{
    return auto_release_ptr<ShaderCompiler>(new ShaderCompiler(stdosl_path));
}

}   // namespace renderer
