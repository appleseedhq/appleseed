
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
#include "shaderconnection.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/oslshadingsystem.h"

// appleseed.foundation headers.
#include "foundation/utility/uid.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// ShaderConnection class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct ShaderConnection::Impl
{
    std::string m_src_layer;
    std::string m_src_param;
    std::string m_dst_layer;
    std::string m_dst_param;
};

ShaderConnection::ShaderConnection(
    const char* src_layer,
    const char* src_param,
    const char* dst_layer,
    const char* dst_param)
  : Entity(g_class_uid, ParamArray())
  , impl(new Impl)
{
    impl->m_src_layer = src_layer;
    impl->m_src_param = src_param;
    impl->m_dst_layer = dst_layer;
    impl->m_dst_param = dst_param;

    const std::string entity_name =
        std::string(get_src_layer()) + ":" + get_src_param() + "->" + get_dst_layer() + ":" + get_dst_param();

    set_name(entity_name.c_str());
}

ShaderConnection::~ShaderConnection()
{
    delete impl;
}

void ShaderConnection::release()
{
    delete this;
}

const char *ShaderConnection::get_src_layer() const
{
    return impl->m_src_layer.c_str();
}

const char *ShaderConnection::get_src_param() const
{
    return impl->m_src_param.c_str();
}

const char *ShaderConnection::get_dst_layer() const
{
    return impl->m_dst_layer.c_str();
}

const char *ShaderConnection::get_dst_param() const
{
    return impl->m_dst_param.c_str();
}

bool ShaderConnection::add(OSLShadingSystem& shading_system)
{
    if (!shading_system.ConnectShaders(
            get_src_layer(),
            get_src_param(),
            get_dst_layer(),
            get_dst_param()))
    {
        RENDERER_LOG_ERROR(
            "error connecting shaders %s::%s -> %s::%s.",
            get_src_layer(),
            get_src_param(),
            get_dst_layer(),
            get_dst_param());
        return false;
    }

    return true;
}

}   // namespace renderer
