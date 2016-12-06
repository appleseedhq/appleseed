
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAM_H
#define APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAM_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES

// Forward declarations.
namespace renderer  { class Shader; }

namespace renderer
{

//
// Enumeration of all supported OSL parameter types.
//

enum OSLParamType
{
    OSLParamTypeColor,
    OSLParamTypeColorArray,
    OSLParamTypeFloat,
    OSLParamTypeFloatArray,
    OSLParamTypeInt,
    OSLParamTypeIntArray,
    OSLParamTypeMatrix,
    OSLParamTypeMatrixArray,
    OSLParamTypeNormal,
    OSLParamTypeNormalArray,
    OSLParamTypePoint,
    OSLParamTypePointArray,
    OSLParamTypeString,
    OSLParamTypeVector,
    OSLParamTypeVectorArray
};


//
// A parameter in an OSL shader.
//

class APPLESEED_DLLSYMBOL ShaderParam
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // TODO: STL classes cannot be used in DLL-exported classes.
    std::string get_value_as_string() const;

  private:
    friend class Shader;

    struct Impl;
    Impl* impl;

    // Constructor.
    explicit ShaderParam(const char* name);

    // Destructor.
    ~ShaderParam();

    // Create an int param.
    static foundation::auto_release_ptr<ShaderParam> create_int_param(
        const char*         name,
        const int           value);

    // Create an int array param.
    static foundation::auto_release_ptr<ShaderParam> create_int_array_param(
        const char*         name,
        std::vector<int>&   value);

    // Create a float param.
    static foundation::auto_release_ptr<ShaderParam> create_float_param(
        const char*         name,
        const float         value);

    // Create a float array param.
    static foundation::auto_release_ptr<ShaderParam> create_float_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a vector param.
    static foundation::auto_release_ptr<ShaderParam> create_vector_param(
        const char*         name,
        const float         vx,
        const float         vy,
        const float         vz);

    // Create a vector array param.
    static foundation::auto_release_ptr<ShaderParam> create_vector_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a normal param.
    static foundation::auto_release_ptr<ShaderParam> create_normal_param(
        const char*         name,
        const float         nx,
        const float         ny,
        const float         nz);

    // Create a normal array param.
    static foundation::auto_release_ptr<ShaderParam> create_normal_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a point param.
    static foundation::auto_release_ptr<ShaderParam> create_point_param(
        const char*         name,
        const float         vx,
        const float         vy,
        const float         vz);

    // Create a point array param.
    static foundation::auto_release_ptr<ShaderParam> create_point_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a color param.
    static foundation::auto_release_ptr<ShaderParam> create_color_param(
        const char*         name,
        const float         vx,
        const float         vy,
        const float         vz);

    // Create a color array param.
    static foundation::auto_release_ptr<ShaderParam> create_color_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a matrix param.
    static foundation::auto_release_ptr<ShaderParam> create_matrix_param(
        const char*         name,
        const float*        values);

    // Create a matrix array param.
    static foundation::auto_release_ptr<ShaderParam> create_matrix_array_param(
        const char*         name,
        std::vector<float>& value);

    // Create a string param.
    static foundation::auto_release_ptr<ShaderParam> create_string_param(
        const char*         name,
        const char*         value);

    // Return a const void pointer to this param value.
    const void* get_value() const;

    // Add this param to OSL's shading system.
    bool add(OSL::ShadingSystem& shading_system);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAM_H
