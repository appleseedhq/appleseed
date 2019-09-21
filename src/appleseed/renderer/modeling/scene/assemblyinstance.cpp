
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
#include "assemblyinstance.h"

// appleseed.renderer headers.
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/utility/api/apistring.h"

// Standard headers.
#include <string>

using namespace foundation;

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
    std::string m_assembly_name;
};

AssemblyInstance::AssemblyInstance(
    const char*             name,
    const ParamArray&       params,
    const char*             assembly_name)
  : Entity(g_class_uid,     params)
  , impl(new Impl())
{
    set_name(name);

    impl->m_assembly_name = assembly_name;

    const EntityDefMessageContext context("assembly instance", this);

    // Retrieve visibility flags.
    m_vis_flags = VisibilityFlags::parse(params.child("visibility"), context);

    // No bound assembly yet.
    m_assembly = nullptr;
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

    return nullptr;
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
    m_assembly = nullptr;
}

void AssemblyInstance::bind_assembly(const AssemblyContainer& assemblies)
{
    if (m_assembly == nullptr)
        m_assembly = assemblies.get_by_name(impl->m_assembly_name.c_str());
}

void AssemblyInstance::check_assembly() const
{
    if (m_assembly == nullptr)
        throw ExceptionUnknownEntity(impl->m_assembly_name.c_str(), this);
}

bool AssemblyInstance::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_transform_sequence.optimize();

    if (!m_transform_sequence.prepare())
        RENDERER_LOG_WARNING("assembly instance \"%s\" has one or more invalid transforms.", get_path().c_str());

    return true;
}


//
// AssemblyInstanceFactory class implementation.
//

auto_release_ptr<AssemblyInstance> AssemblyInstanceFactory::create(
    const char*             name,
    const ParamArray&       params,
    const char*             assembly_name)
{
    return
        auto_release_ptr<AssemblyInstance>(
            new AssemblyInstance(
                name,
                params,
                assembly_name));
}

}   // namespace renderer
