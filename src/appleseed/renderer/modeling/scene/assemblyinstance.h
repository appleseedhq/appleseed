
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstdint>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An instance of an assembly.
//

class APPLESEED_DLLSYMBOL AssemblyInstance
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    void release() override;

    // Return the name of the instantiated assembly.
    const char* get_assembly_name() const;

    // Access the transform sequence of the instance.
    TransformSequence& transform_sequence();
    const TransformSequence& transform_sequence() const;

    // Return the visibility flags of this instance.
    std::uint32_t get_vis_flags() const;

    // Find the assembly bound to this instance.
    Assembly* find_assembly() const;

    // Compute the parent space bounding box of the instance.
    GAABB3 compute_parent_bbox() const;

    // Assembly binding.
    void unbind_assembly();
    void bind_assembly(const AssemblyContainer& assemblies);
    void check_assembly() const;

    // Return the assembly bound to this instance.
    Assembly& get_assembly() const;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

  private:
    friend class AssemblyInstanceFactory;

    struct Impl;
    Impl* impl;

    std::uint32_t       m_vis_flags;
    Assembly*           m_assembly;
    TransformSequence   m_transform_sequence;

    // Constructor.
    AssemblyInstance(
        const char*                 name,
        const ParamArray&           params,
        const char*                 assembly_name);

    // Destructor.
    ~AssemblyInstance() override;
};


//
// Assembly instance factory.
//

class APPLESEED_DLLSYMBOL AssemblyInstanceFactory
{
  public:
    // Create a new assembly instance.
    static foundation::auto_release_ptr<AssemblyInstance> create(
        const char*                 name,
        const ParamArray&           params,
        const char*                 assembly_name);
};


//
// AssemblyInstance class implementation.
//

inline std::uint32_t AssemblyInstance::get_vis_flags() const
{
    return m_vis_flags;
}

inline TransformSequence& AssemblyInstance::transform_sequence()
{
    return m_transform_sequence;
}

inline const TransformSequence& AssemblyInstance::transform_sequence() const
{
    return m_transform_sequence;
}

inline Assembly& AssemblyInstance::get_assembly() const
{
    assert(m_assembly);
    return *m_assembly;
}

}   // namespace renderer
