
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
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
#include "shadergroup.h"

// boost headers.
#include <boost/bind.hpp>
#include <boost/range/algorithm/for_each.hpp>

// Standard headers.
#include <cassert>
#include <cstring>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    class Parameter
    {
      public:
        static Parameter int_parameter(const char* name, int value)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypeInt;
            p.m_int_value = value;
            return p;
        }

        static Parameter float_parameter(const char* name, float value)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypeFloat;
            p.m_float_value[0] = value;
            return p;
        }

        static Parameter vector_parameter(const char* name, float vx, float vy, float vz)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypeVector;
            p.m_float_value[0] = vx;
            p.m_float_value[1] = vy;
            p.m_float_value[2] = vz;
            return p;
        }

        static Parameter point_parameter(const char* name, float vx, float vy, float vz)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypePoint;
            p.m_float_value[0] = vx;
            p.m_float_value[1] = vy;
            p.m_float_value[2] = vz;
            return p;
        }

        static Parameter color_parameter(const char* name, float vx, float vy, float vz)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypeColor;
            p.m_float_value[0] = vx;
            p.m_float_value[1] = vy;
            p.m_float_value[2] = vz;
            return p;
        }

        static Parameter string_parameter(const char* name, const char* value)
        {
            Parameter p;
            p.m_name = name;
            p.m_type_desc = OSL::TypeDesc::TypeString;
            p.m_string_value = value;
            return p;
        }

        void add(OSL::ShadingSystem& shading_system)
        {
            shading_system.Parameter("name", m_type_desc, value());
        }

        void* value()
        {
            if (m_type_desc == OSL::TypeDesc::TypeInt)
            {
                return &m_int_value;
            }

            if (m_type_desc == OSL::TypeDesc::TypeFloat)
            {
                return &m_float_value;
            }

            if (m_type_desc == OSL::TypeDesc::TypeVector)
            {
                return &m_float_value;
            }

            if (m_type_desc == OSL::TypeDesc::TypePoint)
            {
                return &m_float_value;
            }

            if (m_type_desc == OSL::TypeDesc::TypeColor)
            {
                return &m_float_value;
            }

            if (m_type_desc == OSL::TypeDesc::TypeString)
            {
                return const_cast<char*>(m_string_value.c_str());
            }

            assert(!"Invalid parameter type.");
            return 0;
        }

        string              m_name;
        OSL::TypeDesc       m_type_desc;
        int                 m_int_value;
        float               m_float_value[3];
        string              m_string_value;

      private:
        Parameter() {}
    };

    struct Shader
    {
        vector<Parameter>       m_params;
        const string            m_type;
        const string            m_name;
        const string            m_layer;

        Shader(
            const char*         type,
            const char*         name,
            const char*         layer,
            vector<Parameter>&  params)
          : m_type(type)
          , m_name(name)
          , m_layer(layer)
          , m_params()
        {
            m_params.swap(params);
        }

        void add(OSL::ShadingSystem& shading_system)
        {
            boost::range::for_each(m_params, boost::bind(&Parameter::add, _1, boost::ref(shading_system)));
            shading_system.Shader(m_type.c_str(), m_name.c_str(), m_layer.c_str());
        }
    };

    struct Connection
    {
        const string    m_src_layer;
        const string    m_src_param;
        const string    m_dst_layer;
        const string    m_dst_param;

        Connection(
            const char* src_layer,
            const char* src_param,
            const char* dst_layer,
            const char* dst_param)
          : m_src_layer(src_layer)
          , m_src_param(src_param)
          , m_dst_layer(dst_layer)
          , m_dst_param(dst_param)
        {
        }

        void add(OSL::ShadingSystem& shading_system)
        {
            shading_system.ConnectShaders(
                m_src_layer.c_str(),
                m_src_param.c_str(),
                m_dst_layer.c_str(),
                m_dst_param.c_str());
        }
    };
}


//
// ShaderGroup class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct ShaderGroup::Impl
{
    void clear()
    {
        m_params_to_assign.clear();
        m_shaders.clear();
        m_connections.clear();
    }

    void add_shader(const char* type, const char* name, const char* layer)
    {
        assert(m_params_to_assign.empty());

        m_shaders.push_back(Shader(type, name, layer, m_params_to_assign));
    }

    vector<Parameter>   m_params_to_assign;
    vector<Shader>      m_shaders;
    vector<Connection>  m_connections;
};

ShaderGroup::ShaderGroup(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);
}

ShaderGroup::~ShaderGroup()
{
    delete impl;
}

void ShaderGroup::release()
{
    delete this;
}

const char* ShaderGroup::get_model() const
{
    return ShaderGroupFactory::get_model();
}

void ShaderGroup::add_int_parameter(const char*name, int value)
{
    impl->m_params_to_assign.push_back(Parameter::int_parameter(name, value));
}

void ShaderGroup::add_float_parameter(const char*name, float value)
{
    impl->m_params_to_assign.push_back(Parameter::float_parameter(name, value));
}

void ShaderGroup::add_vector_parameter(const char* name, float vx, float vy, float vz)
{
    impl->m_params_to_assign.push_back(Parameter::vector_parameter(name, vx, vy, vz));
}

void ShaderGroup::add_point_parameter(const char* name, float vx, float vy, float vz)
{
    impl->m_params_to_assign.push_back(Parameter::point_parameter(name, vx, vy, vz));
}

void ShaderGroup::add_color_parameter(const char* name, float vx, float vy, float vz)
{
    impl->m_params_to_assign.push_back(Parameter::color_parameter(name, vx, vy, vz));
}

void ShaderGroup::add_string_parameter(const char* name, const char* value)
{
    impl->m_params_to_assign.push_back(Parameter::string_parameter(name, value));
}

void ShaderGroup::add_shader(const char* type, const char* name, const char* layer)
{
    impl->add_shader(type, name, layer);
}

void ShaderGroup::add_shader(const char* name, const char* layer)
{
    add_shader("surface", name, layer);
}

void ShaderGroup::add_connection(
    const char*         src_layer,
    const char*         src_param,
    const char*         dst_layer,
    const char*         dst_param)
{
    impl->m_connections.push_back(Connection(src_layer, src_param, dst_layer, dst_param));
}


//
// ShaderGroupFactory class implementation.
//

const char* ShaderGroupFactory::get_model()
{
    return "osl_shadergroup";
}

auto_release_ptr<ShaderGroup> ShaderGroupFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<ShaderGroup>(new ShaderGroup(name, params));
}

}   // namespace renderer
