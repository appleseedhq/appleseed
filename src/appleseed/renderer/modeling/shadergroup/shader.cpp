
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
        const char* type, 
        const char* shader,
        const char* layer,
        const ParamArray& params)
    {
        for (StringDictionary::const_iterator it(params.strings().begin()), e(params.strings().end()); it != e; ++it)
        {
            vector<string> tokens;
            tokenize(string(it.value()), Blanks, tokens);
            vector<string>::const_iterator tok_it(tokens.begin());
            vector<string>::const_iterator tok_end(tokens.end());
                    
            if (tok_it == tok_end)
                assert(false);

            string tok(*tok_it);
            if (tok == "float")
            {
                ++tok_it;
                float val = parse_param1<float>(tok_it, tok_end);
                m_params.insert(ShaderParam::create_float_param(it.name(), val));
            }
            else if (tok == "int")
            {
                ++tok_it;
                int val = parse_param1<int>(tok_it, tok_end);
                m_params.insert(ShaderParam::create_int_param(it.name(), val));
            }
            else if (tok == "color")
            {
                ++tok_it;
                assert(tok_it != tokens.end());

                string s = *tok_it;
                s = trim_both(s);
                if (s[0] == '\0')
                    s.erase(0, 1);
                float rval = from_string<float>(s);
                float gval = rval;
                float bval = rval;
                ++tok_it;

                if (tok_it != tok_end)
                {
                    s = *tok_it;
                    s = trim_both(s);
                    if (s[0] == '\0')
                        s.erase(0, 1);
                    gval = from_string<float>(s);
                    ++tok_it;
                    assert(tok_it != tok_end);

                    s = *tok_it;
                    s = trim_both(s);
                    if (s[0] == '\0')
                        s.erase(0, 1);
                    bval = from_string<float>(s);
                    ++tok_it;
                    assert(tok_it == tok_end);
                }

                m_params.insert(ShaderParam::create_color_param(it.name(), rval, gval, bval));
            }
            else if (tok == "normal")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(tok_it, tok_end, x, y, z);
                m_params.insert(ShaderParam::create_normal_param(it.name(), x, y, z));
            }
            else if (tok == "point")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(tok_it, tok_end, x, y, z);
                m_params.insert(ShaderParam::create_point_param(it.name(), x, y, z));
            }
            else if (tok == "vector")
            {
                ++tok_it;
                float x, y, z;
                parse_param3<float>(tok_it, tok_end, x, y, z);
                m_params.insert(ShaderParam::create_vector_param(it.name(), x, y, z));
            }
            else if (tok == "string")
            {
                ++tok_it;
                m_params.insert(ShaderParam::create_string_param(it.name(), tok_it->c_str()));
            }
            else
            {
                RENDERER_LOG_ERROR("error adding osl param %s, with unknown type %s", it.name(), tok.c_str());
                assert(false);
            }

            RENDERER_LOG_INFO("added osl param %s", it.name());
        }
    }

    string                  m_type;
    string                  m_shader;
    ShaderParamContainer    m_params;

  private:
    template<class T>
    T parse_param1(
        vector<string>::const_iterator& it,
        vector<string>::const_iterator& end)
    {
        assert(it != end);
        string s(*it);
        s = trim_both(s);

        if (s[0] == '\0')
            s.erase(0, 1);

        T val = from_string<T>(s);
        ++it;
        assert(it == end);
        return val;
    }

    template<class T>
    void parse_param3(
        vector<string>::const_iterator& it,
        vector<string>::const_iterator& end,
        T& a,
        T& b,
        T&c)
    {
        string s(*it);
        s = trim_both(s);
        if (s[0] == '\0')
            s.erase(0, 1);
        a = from_string<T>(s);
        ++it;
        assert(it != end);

        s = *it;
        s = trim_both(s);
        if (s[0] == '\0')
            s.erase(0, 1);
        b = from_string<T>(s);
        ++it;
        assert(it != end);

        s = *it;
        s = trim_both(s);
        if (s[0] == '\0')
            s.erase(0, 1);
        c = from_string<T>(s);
        ++it;
        assert(it == end);
    }
};

Shader::Shader(const char* type,
               const char* shader,
               const char* layer,
               const ParamArray& params)
    : Entity(g_class_uid, params)
    , impl(new Impl(type, shader, layer, params))
{
    // We use the layer name as the Entity name, as it's unique.
    set_name(layer);
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
    for (ShaderParamContainer::iterator it(impl->m_params.begin()), e(impl->m_params.end()); it != e; ++it)
    {
        if (!it->add(shading_system))
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
