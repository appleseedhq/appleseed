
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace std;
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
    Impl(
        const char*         type, 
        const char*         shader,
        const char*         layer,
        const ParamArray&   params)
    {
        for (const_each<StringDictionary> i = params.strings(); i; ++i)
        {
            try
            {
                ShaderParamParser parser(i.it().value());
                
                switch (parser.param_type())
                {
                  case OSLParamTypeColor:
                    assert(false);
                  break;

                  case OSLParamTypeFloat:
                    {
                        float val = parser.float_value();
                        m_params.insert(ShaderParam::create_float_param(i.it().name(), val));
                    }
                  break;
                    
                  case OSLParamTypeInt:
                    {
                        int val = parser.int_value();
                        m_params.insert(ShaderParam::create_int_param(i.it().name(), val));
                    }
                  break;

                  case OSLParamTypeNormal:
                  case OSLParamTypePoint:
                  case OSLParamTypeString:
                  case OSLParamTypeVector:
                    assert(false);
                  break;

                  default:
                    RENDERER_LOG_FATAL(
                        "error adding osl param %s, of unknown type %s",
                        i.it().name(),
                        i.it().value());
                }
            }
            catch(const ExceptionOSLParamParseError&)
            {
                RENDERER_LOG_FATAL(
                    "error parsing osl param value. param = %s, value = %s",
                    i.it().name(),
                    i.it().value());
            }

            /*
            vector<string> tokens;
            tokenize(string(i.it().value()), Blanks, tokens);
            vector<string>::const_iterator tok_it(tokens.begin());
            vector<string>::const_iterator tok_end(tokens.end());
            const string tok(*tok_it);

            if (tok == "float")
            {
            }
            else if (tok == "int")
            {
            }
            else if (tok == "color")
            {
                ++tok_it;
                float r, g, b;
                parse_param3<float>(i.it().name(), i.it().value(), tok_it, tok_end, true, r, g, b);
                m_params.insert(ShaderParam::create_color_param(i.it().name(), r, g, b));
            }
            else if (tok == "normal")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(i.it().name(), i.it().value(), tok_it, tok_end, false, x, y, z);
                m_params.insert(ShaderParam::create_normal_param(i.it().name(), x, y, z));
            }
            else if (tok == "point")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(i.it().name(), i.it().value(), tok_it, tok_end, false, x, y, z);
                m_params.insert(ShaderParam::create_point_param(i.it().name(), x, y, z));
            }
            else if (tok == "vector")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(i.it().name(), i.it().value(), tok_it, tok_end, false, x, y, z);
                m_params.insert(ShaderParam::create_vector_param(i.it().name(), x, y, z));
            }
            else if (tok == "string")
            {
                ++tok_it;
                m_params.insert(ShaderParam::create_string_param(i.it().name(), tok_it->c_str()));
            }
            else
            {
            }
            */

            RENDERER_LOG_DEBUG("added osl param %s", i.it().name());
        }
    }

    string                  m_type;
    string                  m_shader;
    ShaderParamContainer    m_params;

  private:
    /*
    template<class T>
    T parse_one_value(
        const char*                             param_name,
        const char*                             param_value_string,
        vector<string>::const_iterator&         it,
        const vector<string>::const_iterator&   end)
    {
        if (it == end)
            report_param_value_parse_error(param_name, param_value_string);

        string s(*it);
        s = trim_both(s);

        if (s[0] == '\0')
            s.erase(0, 1);

        T value = from_string<T>(s);
        ++it;
        return value;
    }

    template<class T>
    T parse_param1(
        const char*                             param_name,
        const char*                             param_value_string,
        vector<string>::const_iterator&         it,
        const vector<string>::const_iterator&   end)
    {
        T val = parse_one_value<T>(param_name, param_value_string, it, end);

        if (it != end)
            report_param_value_parse_error(param_name, param_value_string);

        return val;
    }

    template<class T>
    void parse_param3(
        const char*                             param_name,
        const char*                             param_value_string,
        vector<string>::const_iterator&         it,
        const vector<string>::const_iterator&   end,
        bool                                    parse_as_color,
        T&                                      a,
        T&                                      b,
        T&                                      c)
    {
        a = parse_one_value<T>(param_name, param_value_string, it, end);

        if (it == end)
        {
            if (parse_as_color)
            {
                b = c = a;
                return;
            }

            report_param_value_parse_error(param_name, param_value_string);
        }

        b = parse_one_value<T>(param_name, param_value_string, it, end);

        if (it == end)
            report_param_value_parse_error(param_name, param_value_string);

        c = parse_one_value<T>(param_name, param_value_string, it, end);

        if (it != end)
            report_param_value_parse_error(param_name, param_value_string);
    }
    */
};

Shader::Shader(
    const char*          type,
    const char*          shader,
    const char*          layer,
    const ParamArray&    params)
  : Entity(g_class_uid, params)
  , impl(new Impl(type, shader, layer, params))
{
    // We use the layer name as the Entity name, as it's unique.
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

const char *Shader::get_type() const
{
    return impl->m_type.c_str();
}

const char *Shader::get_shader() const
{
    return impl->m_shader.c_str();
}

const char *Shader::get_layer() const
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

    if (!shading_system.Shader(get_type(), get_shader(), get_layer()))
    {
        RENDERER_LOG_ERROR("error adding shader %s, %s", get_shader(), get_layer());
        return false;
    }

    return true;
}

}   // namespace renderer
