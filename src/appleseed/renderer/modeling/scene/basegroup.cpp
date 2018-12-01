
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "basegroup.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"

using namespace foundation;

namespace renderer
{

struct BaseGroup::Impl
{
    ColorContainer              m_colors;
    TextureContainer            m_textures;
    TextureInstanceContainer    m_texture_instances;
    ShaderGroupContainer        m_shader_groups;
    AssemblyContainer           m_assemblies;
    AssemblyInstanceContainer   m_assembly_instances;

    explicit Impl(Entity* parent)
      : m_colors(parent)
      , m_textures(parent)
      , m_texture_instances(parent)
      , m_shader_groups(parent)
      , m_assemblies(parent)
      , m_assembly_instances(parent)
    {
    }
};

BaseGroup::BaseGroup(Entity* parent)
  : impl(new Impl(parent))
{
}

BaseGroup::~BaseGroup()
{
    delete impl;
}

ColorContainer& BaseGroup::colors() const
{
    return impl->m_colors;
}

TextureContainer& BaseGroup::textures() const
{
    return impl->m_textures;
}

TextureInstanceContainer& BaseGroup::texture_instances() const
{
    return impl->m_texture_instances;
}

ShaderGroupContainer& BaseGroup::shader_groups() const
{
    return impl->m_shader_groups;
}

void BaseGroup::clear()
{
    impl->m_colors.clear();
    impl->m_textures.clear();
    impl->m_texture_instances.clear();
    impl->m_shader_groups.clear();
    impl->m_assemblies.clear();
    impl->m_assembly_instances.clear();
}

bool BaseGroup::create_optimized_osl_shader_groups(
    OSLShadingSystem&           shading_system,
    const ShaderCompiler*       shader_compiler,
    IAbortSwitch*               abort_switch)
{
    bool success = true;

    for (each<AssemblyContainer> i = assemblies(); i; ++i)
    {
        if (is_aborted(abort_switch))
            return true;

        success = success && i->create_optimized_osl_shader_groups(
            shading_system,
            shader_compiler,
            abort_switch);
    }

    for (each<ShaderGroupContainer> i = shader_groups(); i; ++i)
    {
        if (is_aborted(abort_switch))
            return true;

        success = success && i->create_optimized_osl_shader_group(
            shading_system,
            shader_compiler,
            abort_switch);
    }

    return success;
}

void BaseGroup::release_optimized_osl_shader_groups()
{
    for (each<AssemblyContainer> i = assemblies(); i; ++i)
        i->release_optimized_osl_shader_groups();

    for (each<ShaderGroupContainer> i = shader_groups(); i; ++i)
        i->release_optimized_osl_shader_group();
}

AssemblyContainer& BaseGroup::assemblies() const
{
    return impl->m_assemblies;
}

AssemblyInstanceContainer& BaseGroup::assembly_instances() const
{
    return impl->m_assembly_instances;
}

void BaseGroup::collect_asset_paths(StringArray& paths) const
{
    invoke_collect_asset_paths(colors(), paths);
    invoke_collect_asset_paths(textures(), paths);
    invoke_collect_asset_paths(texture_instances(), paths);
    invoke_collect_asset_paths(shader_groups(), paths);
    invoke_collect_asset_paths(assemblies(), paths);
    invoke_collect_asset_paths(assembly_instances(), paths);
}

void BaseGroup::update_asset_paths(const StringDictionary& mappings)
{
    invoke_update_asset_paths(colors(), mappings);
    invoke_update_asset_paths(textures(), mappings);
    invoke_update_asset_paths(texture_instances(), mappings);
    invoke_update_asset_paths(shader_groups(), mappings);
    invoke_update_asset_paths(assemblies(), mappings);
    invoke_update_asset_paths(assembly_instances(), mappings);
}

bool BaseGroup::on_render_begin(
    const Project&              project,
    const BaseGroup*            parent,
    OnRenderBeginRecorder&      recorder,
    IAbortSwitch*               abort_switch)
{
    bool success = true;
    success = success && invoke_on_render_begin(colors(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(textures(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(texture_instances(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(shader_groups(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(assemblies(), project, this, recorder, abort_switch);
    success = success && invoke_on_render_begin(assembly_instances(), project, this, recorder, abort_switch);
    return success;
}

bool BaseGroup::on_frame_begin(
    const Project&              project,
    const BaseGroup*            parent,
    OnFrameBeginRecorder&       recorder,
    IAbortSwitch*               abort_switch)
{
    bool success = true;
    success = success && invoke_on_frame_begin(colors(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(textures(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(texture_instances(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(shader_groups(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(assemblies(), project, this, recorder, abort_switch);
    success = success && invoke_on_frame_begin(assembly_instances(), project, this, recorder, abort_switch);
    return success;
}

}   // namespace renderer
