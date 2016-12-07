
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

// Interface header.
#include "shader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/shadergroup/shaderparamparser.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;
using namespace std;

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
                      vector<float> values;
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
                      vector<float> values;
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
                      vector<int> values;
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
                        vector<float> values;
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
                      vector<float> values;
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
                      vector<float> values;
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
                      vector<float> values;
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

    string                  m_type;
    string                  m_shader;
    ShaderParamContainer    m_params;
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

const ShaderParamContainer& Shader::shader_params() const
{
    return impl->m_params;
}

bool Shader::add(OSL::ShadingSystem& shading_system)
{
    for (each<ShaderParamContainer> i = impl->m_params; i; ++i)
    {
        if (!i->add(shading_system))
            return false;
    }

    if (!shading_system.Shader("surface", get_shader(), get_layer()))
    {
        RENDERER_LOG_ERROR("error adding shader %s, %s.", get_shader(), get_layer());
        return false;
    }

    return true;
}

}   // namespace renderer
