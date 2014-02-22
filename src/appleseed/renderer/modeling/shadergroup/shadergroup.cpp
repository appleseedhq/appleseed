
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

ShaderGroup::ShaderGroup(const char* name)
  : ConnectableEntity(g_class_uid, ParamArray())
{
    set_name(name);
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

    m_shaders.insert(shader);

    RENDERER_LOG_DEBUG("created osl shader %s, layer = %s.", name, layer);
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

    m_connections.insert(connection);

    RENDERER_LOG_DEBUG("created osl connection: src = %s, src_param = %s, dst = %s, dst_param = %s.",
        src_layer,
        src_param,
        dst_layer,
        dst_param);
}

bool ShaderGroup::on_frame_begin(
    const Project&      project,
    const Assembly&     assembly,
    OSL::ShadingSystem* shading_system,
    AbortSwitch*        abort_switch)
{
    RENDERER_LOG_DEBUG("setup shader group %s.", get_name());

    try
    {
        m_shadergroup_ref = shading_system->ShaderGroupBegin(get_name());

        if (!valid())
        {
            RENDERER_LOG_ERROR("shader group begin error: shader = %s.", get_name());
            return false;
        }

        bool success = true;

        for (each<ShaderContainer> i = m_shaders; i; ++i)
        {
            if (is_aborted(abort_switch))
                return success;

            success = success && i->add(*shading_system);
        }

        for (each<ShaderConnectionContainer> i = m_connections; i; ++i)
        {
            if (is_aborted(abort_switch))
                return success;

            success = success && i->add(*shading_system);
        }

        if (!shading_system->ShaderGroupEnd())
        {
            RENDERER_LOG_ERROR("shader group end error: shader = %s.", get_name());
            return false;
        }

        return success;
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR("shader group exception: what = %s.", e.what());
        return false;
    }
}

void ShaderGroup::on_frame_end(
    const Project&      project,
    const Assembly&     assembly)
{
    m_shadergroup_ref.reset();
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
