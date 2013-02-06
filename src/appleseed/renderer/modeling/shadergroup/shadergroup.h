
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Esteban Tovagliari, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
#define APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// OSL headers.
#include <OSL/oslexec.h>

namespace renderer
{

//
// OSL Shadergroup.
//

class DLLSYMBOL ShaderGroup
  : public Entity
{
  public:
    virtual void release() OVERRIDE;

    // Return a string identifying the model of this shadergroup.
    const char* get_model() const;

    void add_int_parameter(const char*name, int value);
    void add_float_parameter(const char*name, float value);
    void add_vector_parameter(const char* name, float vx, float vy, float vz);
    void add_point_parameter(const char* name, float vx, float vy, float vz);
    void add_color_parameter(const char* name, float vx, float vy, float vz);
    void add_string_parameter(const char* name, const char* value);

    void add_shader(const char* type, const char* name, const char* layer);
    void add_shader(const char* name, const char* layer);

    void add_connection(const char* src_layer, const char* src_param,
                        const char* dst_layer, const char* dst_param);

  private:
    friend class ShaderGroupFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    ShaderGroup(
        const char*             name,
        const ParamArray&       params);

    // Destructor
    ~ShaderGroup();
};

//
// ShaderGroup factory.
//

class DLLSYMBOL ShaderGroupFactory
{
  public:
    // Return a string identifying this ShaderGroup model.
    static const char* get_model();

    // Create a new ShaderGroup.
    static foundation::auto_release_ptr<ShaderGroup> create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
