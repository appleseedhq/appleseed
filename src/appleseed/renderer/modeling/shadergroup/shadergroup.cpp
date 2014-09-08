
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
#include "shadergroup.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/shadergroup/shader.h"
#include "renderer/modeling/shadergroup/shaderconnection.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <exception>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShaderGroup class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

struct ShaderGroup::Impl
{
    ShaderContainer                 m_shaders;
    ShaderConnectionContainer       m_connections;
    mutable OSL::ShaderGroupRef     m_shader_group_ref;
};

ShaderGroup::ShaderGroup(const char* name)
  : ConnectableEntity(g_class_uid, ParamArray())
  , impl(new Impl())
  , m_has_emission(false)
  , m_has_transparency(false)
  , m_has_holdout(false)
  , m_has_debug(false)
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

void ShaderGroup::add_shader(
    const char*         type,
    const char*         name,
    const char*         layer,
    const ParamArray&   params)
{
    auto_release_ptr<Shader> shader(
        new Shader(
            type,
            name,
            layer,
            params));

    RENDERER_LOG_DEBUG("created shader %s, layer = %s.", name, layer);
    impl->m_shaders.insert(shader);
}

void ShaderGroup::add_connection(
    const char*         src_layer,
    const char*         src_param,
    const char*         dst_layer,
    const char*         dst_param)
{
    auto_release_ptr<ShaderConnection> connection(
        new ShaderConnection(
            src_layer,
            src_param,
            dst_layer,
            dst_param));

    impl->m_connections.insert(connection);

    RENDERER_LOG_DEBUG("created shader connection: src = %s, src_param = %s, dst = %s, dst_param = %s.",
        src_layer,
        src_param,
        dst_layer,
        dst_param);
}

bool ShaderGroup::create_osl_shader_group(
    OSL::ShadingSystem& shading_system,
    AbortSwitch*        abort_switch)
{
    assert(impl->m_shader_group_ref.get() == 0);

    RENDERER_LOG_DEBUG("setup shader group %s.", get_name());

    try
    {
        impl->m_shader_group_ref = shading_system.ShaderGroupBegin(get_name());

        if (!valid())
        {
            RENDERER_LOG_ERROR("shader group begin error: shader = %s.", get_name());
            return false;
        }

        bool success = true;

        for (each<ShaderContainer> i = impl->m_shaders; i; ++i)
        {
            if (is_aborted(abort_switch))
                return success;

            success = success && i->add(shading_system);
        }

        for (each<ShaderConnectionContainer> i = impl->m_connections; i; ++i)
        {
            if (is_aborted(abort_switch))
                return success;

            success = success && i->add(shading_system);
        }

        if (success)
        {
            if (!shading_system.ShaderGroupEnd())
            {
                RENDERER_LOG_ERROR("shader group end error: shader = %s.", get_name());
                return false;
            }
        }

        get_shader_group_info(shading_system);
        report_has_closures("emission", m_has_emission);
        report_has_closures("transparent", m_has_transparency);
        report_has_closures("holdout", m_has_holdout);
        report_has_closures("debug", m_has_debug);

        return success;
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR("shader group exception: %s.", e.what());
        return false;
    }
}

void ShaderGroup::release_osl_shader_group()
{
    impl->m_shader_group_ref.reset();
}

const ShaderContainer& ShaderGroup::shaders() const
{
    return impl->m_shaders;
}

const ShaderConnectionContainer& ShaderGroup::shader_connections() const
{
    return impl->m_connections;
}

bool ShaderGroup::valid() const
{
    return impl->m_shader_group_ref.get() != 0;
}

OSL::ShaderGroupRef& ShaderGroup::shader_group_ref() const
{
    return impl->m_shader_group_ref;
}

void ShaderGroup::report_has_closures(const char* closure_name, bool has_closures) const
{
    if (has_closures)
    {
        RENDERER_LOG_INFO(
            "shader group %s has %s closures.",
            get_name(),
            closure_name);
    }
    else
    {
        RENDERER_LOG_INFO(
            "shader group %s does not have %s closures.",
            get_name(),
            closure_name);
    }
}

void ShaderGroup::get_shader_group_info(OSL::ShadingSystem& shading_system)
{
    m_has_emission = true;
    m_has_transparency = true;
    m_has_holdout = true;
    m_has_debug = true;

    int num_unknown_closures = 0;
    if (!shading_system.getattribute(
            impl->m_shader_group_ref.get(),
            "unknown_closures_needed",
            num_unknown_closures))
    {
        RENDERER_LOG_WARNING(
            "getattribute call failed for shader group %s; "
            "assuming shader group has all kinds of closures.",
            get_name());

        return;
    }

    if (num_unknown_closures)
    {
        RENDERER_LOG_WARNING(
            "shader group %s has unknown closures; "
            "assuming shader group has all kinds of closures.",
            get_name());

        return;
    }

    int num_closures = 0;
    if (!shading_system.getattribute(
            impl->m_shader_group_ref.get(),
            "num_closures_needed",
            num_closures))
    {
        RENDERER_LOG_WARNING(
            "getattribute call failed for shader group %s; "
            "assuming shader group has all kinds of closures.",
            get_name());

        return;
    }

    if (num_closures)
    {
        OIIO::ustring *closures = 0;
        if (!shading_system.getattribute(
                impl->m_shader_group_ref.get(),
                "closures_needed",
                OIIO::TypeDesc::PTR,
                &closures))
        {
            RENDERER_LOG_WARNING(
                "getattribute call failed for shader group %s; "
                "assuming shader group has all kinds of closures.",
                get_name());

            return;
        }

        m_has_emission = false;
        m_has_transparency = false;
        m_has_holdout = false;
        m_has_debug = false;

        for (int i = 0; i < num_closures; ++i)
        {
            if (closures[i] == "emission")
                m_has_emission = true;

            if (closures[i] == "transparent")
                m_has_transparency = true;

            if (closures[i] == "holdout")
                m_has_holdout = true;

            if (closures[i] == "debug")
                m_has_debug = true;
        }
    }
}


//
// ShaderGroupFactory class implementation.
//

const char* ShaderGroupFactory::get_model()
{
    return "shadergroup";
}

auto_release_ptr<ShaderGroup> ShaderGroupFactory::create(const char* name)
{
    return
        auto_release_ptr<ShaderGroup>(new ShaderGroup(name));
}

}   // namespace renderer
