
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLYINSTANCE_H
#define APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLYINSTANCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Material; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An instance of an assembly.
//

class DLLSYMBOL AssemblyInstance
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    virtual void release() OVERRIDE;

    // Return the name of the instantiated assembly.
    const char* get_assembly_name() const;

    // Access the transform sequence of the instance.
    TransformSequence& transform_sequence();
    const TransformSequence& transform_sequence() const;

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

    // Material override binding.
    void unbind_material_overrides();
    void bind_material_overrides(const MaterialContainer& materials);
    void check_material_overrides();

    // Returns true if the assembly instance has material overrides.
    bool has_material_overrides() const;

    // Return the material overrides of this assembly
    // instance or null if not present.
    const Material* get_front_material_override() const;
    const Material* get_back_material_override() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    bool on_frame_begin(
        const Project&              project,
        foundation::AbortSwitch*    abort_switch);

    // This method is called once after rendering each frame.
    void on_frame_end(const Project& project);

  private:
    friend class AssemblyInstanceFactory;

    struct Impl;
    Impl* impl;

    Assembly*                       m_assembly;
    TransformSequence               m_transform_sequence;

    const Material*                 m_front_material_override;
    const Material*                 m_back_material_override;

    // Constructor.
    AssemblyInstance(
        const char*                 name,
        const ParamArray&           params,
        const char*                 assembly_name,
        const char*                 front_material_override = 0,
        const char*                 back_material_override = 0);

    // Destructor.
    ~AssemblyInstance();
};


//
// Assembly instance factory.
//

class DLLSYMBOL AssemblyInstanceFactory
{
  public:
    // Create a new assembly instance.
    static foundation::auto_release_ptr<AssemblyInstance> create(
        const char*                 name,
        const ParamArray&           params,
        const char*                 assembly_name,
        const char*                 front_material_override = 0,
        const char*                 back_material_override = 0);
};


//
// AssemblyInstance class implementation.
//

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

inline bool AssemblyInstance::has_material_overrides() const
{
    return m_front_material_override != 0 || m_back_material_override != 0;
}

inline const Material* AssemblyInstance::get_front_material_override() const
{
    return m_front_material_override;
}

inline const Material* AssemblyInstance::get_back_material_override() const
{
    return m_back_material_override;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLYINSTANCE_H
