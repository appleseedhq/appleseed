
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLY_H
#define APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLY_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/basegroup.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An assembly is either entirely self-contained, or it references colors,
// textures and texture instances defined in the parent scene or assembly.
//

class DLLSYMBOL Assembly
  : public Entity
  , public BaseGroup
{
  public:
    // Delete this instance.
    virtual void release();

    // Access the BSDFs.
    BSDFContainer& bsdfs() const;

    // Access the EDFs.
    EDFContainer& edfs() const;

    // Access the surface shaders.
    SurfaceShaderContainer& surface_shaders() const;

    // Access the materials.
    MaterialContainer& materials() const;

    // Access the lights.
    LightContainer& lights() const;

    // Access the objects.
    ObjectContainer& objects() const;

    // Access the object instances.
    ObjectInstanceContainer& object_instances() const;

    // Return true if this assembly is tagged as flushable.
    bool is_flushable() const;

    // Perform pre-frame rendering actions.
    // Returns true on success, false otherwise.
    bool on_frame_begin(const Project& project);

    // Perform post-frame rendering actions.
    void on_frame_end(const Project& project);

  private:
    friend class AssemblyFactory;

    struct Impl;
    Impl* impl;

    // Derogate to the private implementation rule, for performance reasons.
    bool m_flushable;

    // Constructor.
    Assembly(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~Assembly();
};


//
// Assembly factory.
//

class DLLSYMBOL AssemblyFactory
{
  public:
    // Create a new assembly.
    static foundation::auto_release_ptr<Assembly> create(
        const char*         name,
        const ParamArray&   params);
};


//
// Assembly class implementation.
//

inline bool Assembly::is_flushable() const
{
    return m_flushable;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_ASSEMBLY_H
