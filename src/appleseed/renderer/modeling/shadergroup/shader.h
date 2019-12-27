
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/shadergroup/shaderparam.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Assembly; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShaderCompiler; }
namespace renderer      { class ShaderGroup; }

namespace renderer
{

//
// OSL shader.
//

class APPLESEED_DLLSYMBOL Shader
  : public Entity
{
  public:
    // Delete this instance.
    void release() override;

    const char* get_type() const;
    const char* get_shader() const;
    const char* get_layer() const;

    const char* get_source_code() const;

    const ShaderParamContainer& shader_params() const;

  private:
    friend class ShaderGroup;

    struct Impl;
    Impl* impl;

    // Constructor.
    Shader(
        const char*         type,
        const char*         shader,
        const char*         layer,
        const ParamArray&   params);

    // Constructor.
    Shader(
        const char*         type,
        const char*         shader,
        const char*         layer,
        const char*         source,
        const ParamArray&   params);

    // Destructor.
    ~Shader() override;

    bool compile_shader(const ShaderCompiler* compiler);

    bool add(OSLShadingSystem& shading_system);
};

}   // namespace renderer
