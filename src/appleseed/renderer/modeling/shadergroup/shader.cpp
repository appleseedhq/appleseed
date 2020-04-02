
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

// Interface header.
#include "shader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/modeling/shadergroup/shadercompiler.h"
#include "renderer/modeling/shadergroup/shaderparamparser.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;

namespace renderer
{

//
// Shader class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct Shader::Impl
{
    std::string             m_type;
    std::string             m_shader;
    ShaderParamContainer    m_params;

    std::string             m_source_code;
    std::string             m_byte_code;

    Impl(
        const char*         type,
        const char*         shader,
        const char*         layer,
        const ParamArray&   params)
      : m_type(type)
      , m_shader(shader)
    {
        for (const_each<StringDictionary> i = params.strings(); i; ++i)
        {
            try
            {
                ShaderParamParser parser(i.it().value());

                switch (parser.param_type())
                {
                  case OSLParamTypeColor:
                    {
                        float r, g, b;
                        parser.parse_three_values<float>(r, g, b, true);
                        m_params.insert(ShaderParam::create_color_param(i.it().key(), r, g, b));
                    }
                    break;

                case OSLParamTypeColorArray:
                  {
                      std::vector<float> values;
                      parser.parse_float3_array(values);
                      m_params.insert(ShaderParam::create_color_array_param(i.it().key(), values));
                  }
                  break;

                  case OSLParamTypeFloat:
                    {
                        const float val = parser.parse_one_value<float>();
                        m_params.insert(ShaderParam::create_float_param(i.it().key(), val));
                    }
                    break;

                case OSLParamTypeFloatArray:
                  {
                      std::vector<float> values;
                      parser.parse_float_array(values);
                      m_params.insert(ShaderParam::create_float_array_param(i.it().key(), values));
                  }
                  break;

                  case OSLParamTypeInt:
                    {
                        const int val = parser.parse_one_value<int>();
                        m_params.insert(ShaderParam::create_int_param(i.it().key(), val));
                    }
                    break;

                case OSLParamTypeIntArray:
                  {
                      std::vector<int> values;
                      parser.parse_int_array(values);
                      m_params.insert(ShaderParam::create_int_array_param(i.it().key(), values));
                  }
                  break;

                  case OSLParamTypeMatrix:
                    {
                        float val[16];
                        parser.parse_n_values(16, val);
                        m_params.insert(ShaderParam::create_matrix_param(i.it().key(), val));
                    }
                    break;

                  case OSLParamTypeMatrixArray:
                    {
                        std::vector<float> values;
                        parser.parse_matrix_array(values);
                        m_params.insert(ShaderParam::create_matrix_array_param(i.it().key(), values));
                    }
                    break;

                  case OSLParamTypeNormal:
                    {
                        float x, y, z;
                        parser.parse_three_values<float>(x, y, z);
                        m_params.insert(ShaderParam::create_normal_param(i.it().key(), x, y, z));
                    }
                    break;

                case OSLParamTypeNormalArray:
                  {
                      std::vector<float> values;
                      parser.parse_float3_array(values);
                      m_params.insert(ShaderParam::create_normal_array_param(i.it().key(), values));
                  }
                  break;

                  case OSLParamTypePoint:
                    {
                        float x, y, z;
                        parser.parse_three_values<float>(x, y, z);
                        m_params.insert(ShaderParam::create_point_param(i.it().key(), x, y, z));
                    }
                    break;

                case OSLParamTypePointArray:
                  {
                      std::vector<float> values;
                      parser.parse_float3_array(values);
                      m_params.insert(ShaderParam::create_point_array_param(i.it().key(), values));
                  }
                  break;

                  case OSLParamTypeString:
                    {
                        m_params.insert(
                            ShaderParam::create_string_param(
                                i.it().key(),
                                parser.parse_string_value().c_str()));
                    }
                    break;

                  case OSLParamTypeVector:
                    {
                        float x, y, z;
                        parser.parse_three_values<float>(x, y, z);
                        m_params.insert(ShaderParam::create_vector_param(i.it().key(), x, y, z));
                    }
                    break;

                case OSLParamTypeVectorArray:
                  {
                      std::vector<float> values;
                      parser.parse_float3_array(values);
                      m_params.insert(ShaderParam::create_vector_array_param(i.it().key(), values));
                  }
                  break;

                  default:
                    RENDERER_LOG_ERROR(
                        "error adding OSL param %s, of unknown type %s; will use the default value.",
                        i.it().key(),
                        i.it().value());
                    break;
                }
            }
            catch (const ExceptionOSLParamParseError&)
            {
                RENDERER_LOG_ERROR(
                    "error parsing OSL param value, param = %s, value = %s; will use the default value.",
                    i.it().key(),
                    i.it().value());
            }
        }
    }
};

Shader::Shader(
    const char*          type,
    const char*          shader,
    const char*          layer,
    const ParamArray&    params)
  : Entity(g_class_uid, params)
  , impl(new Impl(type, shader, layer, params))
{
    // We use the layer name as the entity name, as it's unique.
    set_name(layer);
}

Shader::Shader(
    const char*          type,
    const char*          shader,
    const char*          layer,
    const char*          source,
    const ParamArray&    params)
  : Entity(g_class_uid, params)
  , impl(new Impl(type, shader, layer, params))
{
    // We use the layer name as the entity name, as it's unique.
    set_name(layer);

    impl->m_source_code = source;
}

Shader::~Shader()
{
    delete impl;
}

void Shader::release()
{
    delete this;
}

const char* Shader::get_type() const
{
    return impl->m_type.c_str();
}

const char* Shader::get_shader() const
{
    return impl->m_shader.c_str();
}

const char* Shader::get_layer() const
{
    return get_name();
}

const char* Shader::get_source_code() const
{
    return impl->m_source_code.empty() ? nullptr : impl->m_source_code.c_str();
}

const ShaderParamContainer& Shader::shader_params() const
{
    return impl->m_params;
}

bool Shader::compile_shader(const ShaderCompiler* compiler)
{
    // Skip already compiled shaders.
    if (!impl->m_byte_code.empty())
        return true;

    if (!impl->m_source_code.empty())
    {
        if (!compiler)
        {
            RENDERER_LOG_ERROR(
                "OSL source shader found but shader compiler is not available.");
            return false;
        }

        APIString buffer;
        const bool success = compiler->compile_buffer(impl->m_source_code.c_str(), buffer);

        if (success)
            impl->m_byte_code = buffer.c_str();

        return success;
    }

    return true;
}

bool Shader::add(OSLShadingSystem& shading_system)
{
    for (ShaderParam& param : impl->m_params)
    {
        if (!param.add(shading_system))
            return false;
    }

    if (!impl->m_byte_code.empty())
    {
        if (!shading_system.LoadMemoryCompiledShader(impl->m_shader, impl->m_byte_code))
        {
            RENDERER_LOG_ERROR("error loading memory compiled shader %s, %s.", get_shader(), get_layer());
            return false;
        }
    }

    if (!shading_system.Shader("surface", get_shader(), get_layer()))
    {
        RENDERER_LOG_ERROR("error adding shader \"%s\" for layer \"%s\".", get_shader(), get_layer());
        return false;
    }

    return true;
}

}   // namespace renderer
