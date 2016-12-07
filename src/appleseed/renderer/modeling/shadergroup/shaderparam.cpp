
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
#include "shaderparam.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <sstream>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShaderParam class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct ShaderParam::Impl
{
    OSL::TypeDesc   m_type_desc;
    int             m_int_value;
    float           m_float_value[16];
    string          m_string_storage;
    const char*     m_string_value;
    vector<float>   m_float_array_value;
    vector<int>     m_int_array_value;
};

ShaderParam::ShaderParam(const char* name)
  : Entity(g_class_uid)
  , impl(new Impl)
{
    set_name(name);
}

ShaderParam::~ShaderParam()
{
    delete impl;
}

void ShaderParam::release()
{
    delete this;
}

const void* ShaderParam::get_value() const
{
    if (!impl->m_float_array_value.empty())
        return &impl->m_float_array_value.front();
    if (!impl->m_int_array_value.empty())
        return &impl->m_int_array_value.front();
    if (impl->m_type_desc == OSL::TypeDesc::TypeInt)
        return &impl->m_int_value;
    if (impl->m_type_desc == OSL::TypeDesc::TypeString)
        return &impl->m_string_value;

    // All the other param types use the float storage.
    return &impl->m_float_value;
}

string ShaderParam::get_value_as_string() const
{
    stringstream ss;

    if (!impl->m_float_array_value.empty())
    {
        if (impl->m_type_desc.aggregate == OSL::TypeDesc::SCALAR)
            ss << "float[] ";
        else if (impl->m_type_desc.aggregate == OSL::TypeDesc::MATRIX44)
            ss << "matrix[] ";
        else if (impl->m_type_desc.vecsemantics == OSL::TypeDesc::COLOR)
            ss << "color[] ";
        else if (impl->m_type_desc.vecsemantics == OSL::TypeDesc::POINT)
            ss << "point[] ";
        else if (impl->m_type_desc.vecsemantics == OSL::TypeDesc::VECTOR)
            ss << "vector[] ";
        else if (impl->m_type_desc.vecsemantics == OSL::TypeDesc::NORMAL)
            ss << "normal[] ";

        for (size_t i = 0, e = impl->m_float_array_value.size(); i < e; ++i)
            ss << impl->m_float_array_value[i] << " ";
    }
    else if (!impl->m_int_array_value.empty())
    {
        ss << "int[] ";
        for (size_t i = 0, e = impl->m_int_array_value.size(); i < e; ++i)
            ss << impl->m_int_array_value[i] << " ";
    }
    else
    {
        if (impl->m_type_desc == OSL::TypeDesc::TypeInt)
            ss << "int " << impl->m_int_value;
        else if (impl->m_type_desc == OSL::TypeDesc::TypeFloat)
            ss << "float "  << impl->m_float_value[0];
        else if (impl->m_type_desc == OSL::TypeDesc::TypeVector)
            ss << "vector " << impl->m_float_value[0] << " " << impl->m_float_value[1] << " " << impl->m_float_value[2];
        else if (impl->m_type_desc == OSL::TypeDesc::TypeNormal)
            ss << "normal " << impl->m_float_value[0] << " " << impl->m_float_value[1] << " " << impl->m_float_value[2];
        else if (impl->m_type_desc == OSL::TypeDesc::TypePoint)
            ss << "point "  << impl->m_float_value[0] << " " << impl->m_float_value[1] << " " << impl->m_float_value[2];
        else if (impl->m_type_desc == OSL::TypeDesc::TypeColor)
            ss << "color "  << impl->m_float_value[0] << " " << impl->m_float_value[1] << " " << impl->m_float_value[2];
        else if (impl->m_type_desc == OSL::TypeDesc::TypeString)
            ss << "string " << impl->m_string_value;
        else if (impl->m_type_desc == OSL::TypeDesc::TypeMatrix)
        {
            ss << "matrix";
            for (size_t i = 0; i < 16; ++i)
                ss << ' ' << impl->m_float_value[i];
        }
        else
            assert(!"Invalid parameter type.");
    }

    return ss.str();
}

auto_release_ptr<ShaderParam> ShaderParam::create_int_param(
    const char*         name,
    const int           value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeInt;
    p->impl->m_int_value = value;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_int_array_param(
    const char*         name,
    std::vector<int>&   value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc(OSL::TypeDesc::INT, static_cast<int>(value.size()));
    p->impl->m_int_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_float_param(
    const char*         name,
    const float         value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeFloat;
    p->impl->m_float_value[0] = value;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_float_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc(OSL::TypeDesc::FLOAT, static_cast<int>(value.size()));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_vector_param(
    const char*         name,
    const float         vx,
    const float         vy,
    const float         vz)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeVector;
    p->impl->m_float_value[0] = vx;
    p->impl->m_float_value[1] = vy;
    p->impl->m_float_value[2] = vz;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_vector_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc =
        OSL::TypeDesc(
            OSL::TypeDesc::FLOAT,
            OSL::TypeDesc::VEC3,
            OSL::TypeDesc::VECTOR,
            static_cast<int>(value.size() / 3));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_normal_param(
    const char*         name,
    const float         nx,
    const float         ny,
    const float         nz)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeNormal;
    p->impl->m_float_value[0] = nx;
    p->impl->m_float_value[1] = ny;
    p->impl->m_float_value[2] = nz;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_normal_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc =
        OSL::TypeDesc(
            OSL::TypeDesc::FLOAT,
            OSL::TypeDesc::VEC3,
            OSL::TypeDesc::NORMAL,
            static_cast<int>(value.size() / 3));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_point_param(
    const char*         name,
    const float         vx,
    const float         vy,
    const float         vz)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypePoint;
    p->impl->m_float_value[0] = vx;
    p->impl->m_float_value[1] = vy;
    p->impl->m_float_value[2] = vz;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_point_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc =
        OSL::TypeDesc(
            OSL::TypeDesc::FLOAT,
            OSL::TypeDesc::VEC3,
            OSL::TypeDesc::POINT,
            static_cast<int>(value.size() / 3));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_color_param(
    const char*         name,
    const float         vx,
    const float         vy,
    const float         vz)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeColor;
    p->impl->m_float_value[0] = vx;
    p->impl->m_float_value[1] = vy;
    p->impl->m_float_value[2] = vz;
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_color_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc =
        OSL::TypeDesc(
            OSL::TypeDesc::FLOAT,
            OSL::TypeDesc::VEC3,
            OSL::TypeDesc::COLOR,
            static_cast<int>(value.size() / 3));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_matrix_param(
    const char*         name,
    const float*        values)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeMatrix;
    memcpy(p->impl->m_float_value, values, sizeof(float) * 16);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_matrix_array_param(
    const char*         name,
    std::vector<float>& value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc =
        OSL::TypeDesc(
            OSL::TypeDesc::FLOAT,
            OSL::TypeDesc::MATRIX44,
            OSL::TypeDesc::NOSEMANTICS,
            static_cast<int>(value.size() / 16));
    p->impl->m_float_array_value.swap(value);
    return p;
}

auto_release_ptr<ShaderParam> ShaderParam::create_string_param(
    const char*         name,
    const char*         value)
{
    auto_release_ptr<ShaderParam> p(new ShaderParam(name));
    p->impl->m_type_desc = OSL::TypeDesc::TypeString;
    p->impl->m_string_storage = value;
    p->impl->m_string_value = p->impl->m_string_storage.c_str();
    return p;
}

bool ShaderParam::add(OSL::ShadingSystem& shading_system)
{
    if (!shading_system.Parameter(get_name(), impl->m_type_desc, get_value()))
    {
        RENDERER_LOG_ERROR("error adding parameter %s.", get_path().c_str());
        return false;
    }

    return true;
}

}   // namespace renderer
