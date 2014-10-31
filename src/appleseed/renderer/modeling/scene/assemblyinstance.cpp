
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "assemblyinstance.h"

// appleseed.renderer headers.
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/basegroup.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AssemblyInstance class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID AssemblyInstance::get_class_uid()
{
    return g_class_uid;
}

struct AssemblyInstance::Impl
{
    string m_assembly_name;
    string m_front_material_override;
    string m_back_material_override;
};

AssemblyInstance::AssemblyInstance(
    const char*         name,
    const ParamArray&   params,
    const char*         assembly_name,
    const char*         front_material_override,
    const char*         back_material_override)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_assembly_name = assembly_name;

    if (front_material_override)
        impl->m_front_material_override = front_material_override;

    if (back_material_override)
        impl->m_back_material_override = back_material_override;

    m_assembly = 0;
    m_front_material_override = 0;
    m_back_material_override = 0;
}

AssemblyInstance::~AssemblyInstance()
{
    delete impl;
}

void AssemblyInstance::release()
{
    delete this;
}

const char* AssemblyInstance::get_assembly_name() const
{
    return impl->m_assembly_name.c_str();
}

Assembly* AssemblyInstance::find_assembly() const
{
    const Entity* parent = get_parent();

    while (parent)
    {
        const BaseGroup* parent_base_group = dynamic_cast<const BaseGroup*>(parent);
        assert(parent_base_group);

        Assembly* assembly =
            parent_base_group->assemblies().get_by_name(impl->m_assembly_name.c_str());

        if (assembly)
            return assembly;

        parent = parent->get_parent();
    }

    return 0;
}

GAABB3 AssemblyInstance::compute_parent_bbox() const
{
    // In many places, we need the parent-space bounding box of an assembly instance
    // before input binding is performed, i.e. before the instantiated assembly is
    // bound to the instance. Therefore we manually look the assembly up through the
    // assembly hierarchy instead of simply using m_assembly.

    const Assembly* assembly = find_assembly();

    return
        assembly
            ? m_transform_sequence.to_parent(assembly->compute_local_bbox())
            : GAABB3::invalid();
}

void AssemblyInstance::unbind_assembly()
{
    m_assembly = 0;
}

void AssemblyInstance::bind_assembly(const AssemblyContainer& assemblies)
{
    if (m_assembly == 0)
        m_assembly = assemblies.get_by_name(impl->m_assembly_name.c_str());
}

void AssemblyInstance::check_assembly() const
{
    if (m_assembly == 0)
        throw ExceptionUnknownEntity(impl->m_assembly_name.c_str(), this);
}

void AssemblyInstance::unbind_material_overrides()
{
    m_front_material_override = 0;
    m_back_material_override = 0;
}

void AssemblyInstance::bind_material_overrides(const MaterialContainer& materials)
{
    if (!impl->m_front_material_override.empty())
    {
        if (m_front_material_override == 0)
            m_front_material_override = materials.get_by_name(impl->m_front_material_override.c_str());
    }

    if (!impl->m_back_material_override.empty())
    {
        if (m_back_material_override == 0)
            m_back_material_override = materials.get_by_name(impl->m_back_material_override.c_str());
    }
}

void AssemblyInstance::check_material_overrides()
{
    if (!impl->m_front_material_override.empty() && m_front_material_override == 0)
    {
        RENDERER_LOG_WARNING(
            "assembly instance \"%s\" front override material \"%s\" not found.",
            get_name(),
            impl->m_front_material_override.c_str());
    }

    if (!impl->m_back_material_override.empty() && m_back_material_override == 0)
    {
        RENDERER_LOG_WARNING(
            "assembly instance \"%s\" back override material \"%s\" not found.",
            get_name(),
            impl->m_back_material_override.c_str());
    }

    if (m_front_material_override && m_front_material_override->has_alpha_map())
    {
        RENDERER_LOG_WARNING(
            "assembly instance \"%s\" alpha maps can give unexpected results in override material \"%s\".",
            get_name(),
            impl->m_front_material_override.c_str());
    }

    if (m_back_material_override && m_back_material_override->has_alpha_map())
    {
        RENDERER_LOG_WARNING(
            "assembly instance \"%s\" alpha maps can give unexpected results in override material \"%s\".",
            get_name(),
            impl->m_back_material_override.c_str());
    }
}

const char* AssemblyInstance::get_front_material_override_name() const
{
    return impl->m_front_material_override.empty() ? 0 : impl->m_front_material_override.c_str();
}

const char* AssemblyInstance::get_back_material_override_name() const
{
    return impl->m_back_material_override.empty() ? 0 : impl->m_back_material_override.c_str();
}

bool AssemblyInstance::on_frame_begin(
    const Project&      project,
    AbortSwitch*        abort_switch)
{
    m_transform_sequence.optimize();

    if (!m_transform_sequence.prepare())
        RENDERER_LOG_WARNING("assembly instance \"%s\" has one or more invalid transforms.", get_name());

    return true;
}

void AssemblyInstance::on_frame_end(const Project& project)
{
}


//
// AssemblyInstanceFactory class implementation.
//

auto_release_ptr<AssemblyInstance> AssemblyInstanceFactory::create(
    const char*         name,
    const ParamArray&   params,
    const char*         assembly_name,
    const char*         front_material_override,
    const char*         back_material_override)
{
    return
        auto_release_ptr<AssemblyInstance>(
            new AssemblyInstance(
                name,
                params,
                assembly_name,
                front_material_override,
                back_material_override));
}

}   // namespace renderer
