
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERCOMPILER_H
#define APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERCOMPILER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace foundation    { class APIString; }

namespace renderer
{

//
// Simple wrapper around OSL's OSLCompiler.
//

class ShaderCompiler
  : public foundation::NonCopyable
{
  public:
    // Delete this instance.
    void release();

    bool compile_buffer(
        const char*             source_code,
        foundation::APIString&  result);

  private:
    friend class ShaderCompilerFactory;

    struct Impl;
    Impl* impl;

    explicit ShaderCompiler(const char* stdosl_path);
    ~ShaderCompiler();
};


//
// ShaderCompiler factory.
//

class ShaderCompilerFactory
{
  public:
    // Create a new shader compiler.
    static foundation::auto_release_ptr<ShaderCompiler> create(
        const char* stdosl_path);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_OSLSHADERCOMPILER_H
