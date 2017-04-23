
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/uid.h"

// Boost headers.
#include "boost/unordered/unordered_map.hpp"

// Standard headers.
#include <exception>
#include <utility>

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

    const OIIO::ustring g_emission_str("emission");
    const OIIO::ustring g_transparent_str("transparent");
    const OIIO::ustring g_subsurface_str("as_subsurface");
    const OIIO::ustring g_holdout_str("holdout");
    const OIIO::ustring g_debug_str("debug");
    const OIIO::ustring g_dPdtime_str("dPdtime");
}

struct ShaderGroup::Impl
{
    typedef pair<const AssemblyInstance*, const ObjectInstance*> SurfaceAreaKey;
    typedef boost::unordered_map<SurfaceAreaKey, float>          SurfaceAreaMap;

    ShaderContainer             m_shaders;
    ShaderConnectionContainer   m_connections;
    mutable OSL::ShaderGroupRef m_shader_group_ref;
    mutable SurfaceAreaMap      m_surface_areas;
    const OSL::ShaderSymbol*    m_surface_shader_color_sym;
    const OSL::ShaderSymbol*    m_surface_shader_alpha_sym;
};

ShaderGroup::ShaderGroup(const char* name)
  : ConnectableEntity(g_class_uid, ParamArray())
  , impl(new Impl())
{
    set_name(name);
    clear();
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

void ShaderGroup::clear()
{
    impl->m_shaders.clear();
    impl->m_connections.clear();
    impl->m_shader_group_ref.reset();
    m_flags = 0;
    impl->m_surface_shader_color_sym = 0;
    impl->m_surface_shader_alpha_sym = 0;
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

    impl->m_shaders.insert(shader);

    RENDERER_LOG_DEBUG("created shader %s, layer = %s.", name, layer);
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

    RENDERER_LOG_DEBUG(
        "created shader connection: src_layer = %s, src_param = %s, dst_layer = %s, dst_param = %s.",
            src_layer,
            src_param,
            dst_layer,
            dst_param);
}

bool ShaderGroup::create_optimized_osl_shader_group(
    OSL::ShadingSystem& shading_system,
    IAbortSwitch*       abort_switch)
{
    if (is_valid())
        return true;

    RENDERER_LOG_DEBUG("setting up shader group \"%s\"...", get_path().c_str());

    try
    {
        OSL::ShaderGroupRef shader_group_ref = shading_system.ShaderGroupBegin(get_name());

        if (shader_group_ref.get() == 0)
        {
            RENDERER_LOG_ERROR("failed to setup shader group \"%s\": ShaderGroupBegin() call failed.", get_path().c_str());
            return false;
        }

        for (each<ShaderContainer> i = impl->m_shaders; i; ++i)
        {
            if (is_aborted(abort_switch))
            {
                shading_system.ShaderGroupEnd();
                return true;
            }

            if (!i->add(shading_system))
                return false;
        }

        for (each<ShaderConnectionContainer> i = impl->m_connections; i; ++i)
        {
            if (is_aborted(abort_switch))
            {
                shading_system.ShaderGroupEnd();
                return true;
            }

            if (!i->add(shading_system))
                return false;
        }

        if (!shading_system.ShaderGroupEnd())
        {
            RENDERER_LOG_ERROR("failed to setup shader group \"%s\": ShaderGroupEnd() call failed.", get_path().c_str());
            return false;
        }

        impl->m_shader_group_ref = shader_group_ref;

        // Set the shadergroup outputs and get symbols if needed.
        for (each<ShaderContainer> i = impl->m_shaders; i; ++i)
        {
            if (strcmp(i->get_type(), "surface_shader") == 0)
            {
                // Renderer outputs.
                const char* renderer_outputs[] =
                {
                    "Cout",
                    "Aout",
                };

                if (shading_system.attribute(
                        impl->m_shader_group_ref.get(),
                        "renderer_outputs",
                        OIIO::TypeDesc(OIIO::TypeDesc::STRING, 2),
                        &renderer_outputs[0]))
                {
                    OSL::ShadingContext *ctx = shading_system.get_context();
                    OSL::ShaderGlobals sg;
                    memset(&sg, 0, sizeof(OSL::ShaderGlobals));

                    if (shading_system.execute(
                            ctx,
                            *impl->m_shader_group_ref,
                            sg,
                            false))
                    {
                        impl->m_surface_shader_color_sym =
                            shading_system.find_symbol(
                                *impl->m_shader_group_ref,
                                OIIO::ustring("Cout"));

                        impl->m_surface_shader_alpha_sym =
                            shading_system.find_symbol(
                                *impl->m_shader_group_ref,
                                OIIO::ustring("Aout"));
                    }
                }
                break;
            }
        }

        get_shadergroup_closures_info(shading_system);
        report_has_closure("bsdf", HasBSDFs);
        report_has_closure("emission", HasEmission);
        report_has_closure("transparent", HasTransparency);
        report_has_closure("subsurface", HasSubsurface);
        report_has_closure("holdout", HasHoldout);
        report_has_closure("debug", HasDebug);

        get_shadergroup_globals_info(shading_system);
        report_uses_global("dPdtime", UsesdPdTime);

        return true;
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR("failed to setup shader group \"%s\": %s.", get_path().c_str(), e.what());
        return false;
    }
}

void ShaderGroup::release_optimized_osl_shader_group()
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

bool ShaderGroup::is_valid() const
{
    return impl->m_shader_group_ref.get() != 0;
}

float ShaderGroup::get_surface_area(const AssemblyInstance* ass, const ObjectInstance* obj) const
{
    assert(has_emission());
    return impl->m_surface_areas[Impl::SurfaceAreaKey(ass, obj)];
}

OSL::ShaderGroupRef& ShaderGroup::shader_group_ref() const
{
    return impl->m_shader_group_ref;
}

const OSL::ShaderSymbol* ShaderGroup::surface_shader_color_symbol() const
{
    return impl->m_surface_shader_color_sym;
}

const OSL::ShaderSymbol* ShaderGroup::surface_shader_alpha_symbol() const
{
    return impl->m_surface_shader_alpha_sym;
}

void ShaderGroup::get_shadergroup_closures_info(OSL::ShadingSystem& shading_system)
{
    // Assume the shader group has all closure types.
    m_flags |= HasAllClosures;

    int num_unknown_closures = 0;
    if (!shading_system.getattribute(
            impl->m_shader_group_ref.get(),
            "unknown_closures_needed",
            num_unknown_closures))
    {
        RENDERER_LOG_WARNING(
            "getattribute: unknown_closures_needed call failed for shader group \"%s\"; "
            "assuming shader group has all kinds of closures.",
            get_path().c_str());
        return;
    }

    if (num_unknown_closures != 0)
    {
        RENDERER_LOG_WARNING(
            "shader group \"%s\" has unknown closures; "
            "assuming shader group has all kinds of closures.",
            get_path().c_str());
        return;
    }

    int num_closures = 0;
    if (!shading_system.getattribute(
            impl->m_shader_group_ref.get(),
            "num_closures_needed",
            num_closures))
    {
        RENDERER_LOG_WARNING(
            "getattribute: num_closures_needed call failed for shader group \"%s\"; "
            "assuming shader group has all kinds of closures.",
            get_path().c_str());
    }

    if (num_closures != 0)
    {
        OIIO::ustring* closures = 0;
        if (!shading_system.getattribute(
                impl->m_shader_group_ref.get(),
                "closures_needed",
                OIIO::TypeDesc::PTR,
                &closures))
        {
            RENDERER_LOG_WARNING(
                "getattribute: closures_needed call failed for shader group \"%s\"; "
                "assuming shader group has all kinds of closures.",
                get_path().c_str());
            return;
        }

        // Clear all closure flags.
        m_flags &= ~HasAllClosures;

        // Set the closure flags.
        for (int i = 0; i < num_closures; ++i)
        {
            if (closures[i] == g_emission_str)
                m_flags |= HasEmission;
            else if (closures[i] == g_transparent_str)
                m_flags |= HasTransparency;
            else if (closures[i] == g_subsurface_str)
                m_flags |= HasSubsurface;
            else if (closures[i] == g_holdout_str)
                m_flags |= HasHoldout;
            else if (closures[i] == g_debug_str)
                m_flags |= HasDebug;
            else
                m_flags |= HasBSDFs;
        }
    }
    else
    {
        // Shader group uses no closures.
        m_flags &= ~HasAllClosures;
    }
}

void ShaderGroup::report_has_closure(const char* closure_name, const Flags flag) const
{
    if (m_flags & flag)
    {
        RENDERER_LOG_DEBUG(
            "shader group \"%s\" has %s closures.",
            get_path().c_str(),
            closure_name);
    }
    else
    {
        RENDERER_LOG_DEBUG(
            "shader group \"%s\" does not have %s closures.",
            get_path().c_str(),
            closure_name);
    }
}

void ShaderGroup::get_shadergroup_globals_info(OSL::ShadingSystem& shading_system)
{
    // Assume the shader group uses all globals.
    m_flags |= UsesAllGlobals;

    int num_globals = 0;
    if (!shading_system.getattribute(
            impl->m_shader_group_ref.get(),
            "num_globals_needed",
            num_globals))
    {
        RENDERER_LOG_WARNING(
            "getattribute: num_globals_needed call failed for shader group \"%s\"; "
            "assuming shader group uses all globals.",
            get_path().c_str());
        return;
    }

    if (num_globals != 0)
    {
        OIIO::ustring* globals = 0;
        if (!shading_system.getattribute(
                impl->m_shader_group_ref.get(),
                "globals_needed",
                OIIO::TypeDesc::PTR,
                &globals))
        {
            RENDERER_LOG_WARNING(
                "getattribute: globals_needed call failed for shader group \"%s\"; "
                "assuming shader group uses all globals.",
                get_path().c_str());
            return;
        }

        // Clear all globals flags.
        m_flags &= ~UsesAllGlobals;

        // Set the globals flags.
        for (int i = 0; i < num_globals; ++i)
        {
            if (globals[i] == g_dPdtime_str)
                m_flags |= UsesdPdTime;
        }
    }
    else
    {
        // The shader group uses no globals.
        m_flags &= ~UsesAllGlobals;
    }
}

void ShaderGroup::report_uses_global(const char* global_name, const Flags flag) const
{
    if (m_flags & flag)
    {
        RENDERER_LOG_DEBUG(
            "shader group \"%s\" uses the %s global.",
            get_path().c_str(),
            global_name);
    }
    else
    {
        RENDERER_LOG_DEBUG(
            "shader group \"%s\" does not use the %s global.",
            get_path().c_str(),
            global_name);
    }
}

void ShaderGroup::set_surface_area(
    const AssemblyInstance* ass,
    const ObjectInstance*   obj,
    const float             area) const
{
    assert(has_emission());
    impl->m_surface_areas[Impl::SurfaceAreaKey(ass, obj)] = area;
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
    return auto_release_ptr<ShaderGroup>(new ShaderGroup(name));
}

}   // namespace renderer
