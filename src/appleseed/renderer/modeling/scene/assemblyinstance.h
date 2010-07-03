
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"

// Forward declarations.
namespace renderer  { class Assembly; }

namespace renderer
{

//
// An instance of an assembly.
//

class RENDERERDLL AssemblyInstance
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return the name of this instance.
    virtual const char* get_name() const;

    // Return the instantiated assembly.
    const Assembly& get_assembly() const;

    // Return the unique ID of the instantiated assembly.
    foundation::UniqueID get_assembly_uid() const;

    // Return the transform of the instance.
    const foundation::Transformd& get_transform() const;

    // Return the parent space bounding box of the instance.
    const GAABB3& get_parent_bbox() const;

  private:
    friend class AssemblyInstanceFactory;

    // Private implementation.
    struct Impl;
    Impl* impl;

    // Derogate to the private implementation rule, for performance reasons.
    const Assembly&         m_assembly;
    foundation::UniqueID    m_assembly_uid;

    // Constructor.
    AssemblyInstance(
        const char*                     name,
        const Assembly&                 assembly,
        const foundation::Transformd&   transform);

    // Destructor.
    ~AssemblyInstance();
};


//
// Assembly instance factory.
//

class RENDERERDLL AssemblyInstanceFactory
{
  public:
    // Create a new assembly instance.
    static foundation::auto_release_ptr<AssemblyInstance> create(
        const char*                     name,
        const Assembly&                 assembly,
        const foundation::Transformd&   transform);
};


//
// AssemblyInstance class implementation.
//

// Return the instantiated assembly.
inline const Assembly& AssemblyInstance::get_assembly() const
{
    return m_assembly;
}

// Return the unique ID of the instantiated assembly.
inline foundation::UniqueID AssemblyInstance::get_assembly_uid() const
{
    return m_assembly_uid;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLYINSTANCE_H
