
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

bool BaseGroup::create_optimized_osl_shader_groups(
    OSLShadingSystem&   shading_system,
    IAbortSwitch*       abort_switch)
{
    bool success = true;

    for (each<AssemblyContainer> i = assemblies(); i; ++i)
    {
        if (is_aborted(abort_switch))
            return true;

        success = success && i->create_optimized_osl_shader_groups(
            shading_system,
            abort_switch);
    }

    for (each<ShaderGroupContainer> i = shader_groups(); i; ++i)
    {
        if (is_aborted(abort_switch))
            return true;

        success = success && i->create_optimized_osl_shader_group(
            shading_system,
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

namespace
{
    template <typename EntityCollection>
    void do_collect_asset_paths(
        StringArray&            paths,
        const EntityCollection& entities)
    {
        for (const_each<EntityCollection> i = entities; i; ++i)
            i->collect_asset_paths(paths);
    }

    template <typename EntityCollection>
    void do_update_asset_paths(
        const StringDictionary& mappings,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->update_asset_paths(mappings);
    }
}

void BaseGroup::collect_asset_paths(StringArray& paths) const
{
    do_collect_asset_paths(paths, colors());
    do_collect_asset_paths(paths, textures());
    do_collect_asset_paths(paths, texture_instances());
    do_collect_asset_paths(paths, shader_groups());
    do_collect_asset_paths(paths, assemblies());
    do_collect_asset_paths(paths, assembly_instances());
}

void BaseGroup::update_asset_paths(const StringDictionary& mappings)
{
    do_update_asset_paths(mappings, colors());
    do_update_asset_paths(mappings, textures());
    do_update_asset_paths(mappings, texture_instances());
    do_update_asset_paths(mappings, shader_groups());
    do_update_asset_paths(mappings, assemblies());
    do_update_asset_paths(mappings, assembly_instances());
}

}   // namespace renderer
