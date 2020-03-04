
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class SearchPaths; }

namespace renderer
{

//
// OSL Shader Query.
//
// A class that can open compiled OSL shaders (oso files) and
// return useful information about the shader, its parameters and
// shader metadata. A thin wrapper around OSLQuery.
//

class APPLESEED_DLLSYMBOL ShaderQuery
  : public foundation::NonCopyable
{
  public:
    // Delete this instance.
    void release();

    // Open a compiled shader.
    bool open(const char* shader_name);

    // Open a bytecode memory compiled shader.
    bool open_bytecode(const char* shader_code);

    // Return the shader name.
    const char* get_shader_name() const;

    // Return the shader type.
    const char* get_shader_type() const;

    // Return number of shader parameters.
    size_t get_param_count() const;

    // Return shader parameter information.
    const foundation::Dictionary& get_param_info(const size_t param_index) const;

    // Return shader metadata.
    const foundation::Dictionary& get_metadata() const;

  private:
    friend class ShaderQueryFactory;

    struct Impl;
    Impl* impl;

    ShaderQuery();
    explicit ShaderQuery(const char* search_path);
    explicit ShaderQuery(const foundation::SearchPaths& search_paths);

    ~ShaderQuery();
};


//
// ShaderQuery factory.
//

class APPLESEED_DLLSYMBOL ShaderQueryFactory
{
  public:
    // Create a new shader query without a search path.
    // Shaders can only be queried using absolute paths.
    static foundation::auto_release_ptr<ShaderQuery> create();

    // Create a new shader query with a single search path.
    static foundation::auto_release_ptr<ShaderQuery> create(const char* search_path);

    // Create a new shader query with multiple search paths.
    static foundation::auto_release_ptr<ShaderQuery> create(
        const foundation::SearchPaths& search_paths);
};

}   // namespace renderer
