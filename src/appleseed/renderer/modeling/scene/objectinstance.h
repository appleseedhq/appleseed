
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_OBJECTINSTANCE_H
#define APPLESEED_RENDERER_MODELING_SCENE_OBJECTINSTANCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/array.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Material; }
namespace renderer  { class Object; }
namespace renderer  { class ParamArray; }

namespace renderer
{

//
// An array of materials.
//

DECLARE_ARRAY(MaterialArray, const Material*);


//
// An instance of an object.
//

class DLLSYMBOL ObjectInstance
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release() override;

    // Return the name of the instantiated object.
    const char* get_object_name() const;

    // Return the transform of the instance.
    const foundation::Transformd& get_transform() const;

    // Find the object bound to this instance.
    Object* find_object() const;

    // Compute the parent space bounding box of the instance.
    GAABB3 compute_parent_bbox() const;

    enum Side
    {
        FrontSide = 0,
        BackSide
    };

    // Clear all material assignments.
    void clear_front_materials();
    void clear_back_materials();

    // Assign a material to a given slot.
    void assign_material(
        const size_t    slot,
        const Side      side,
        const char*     material_name);

    // Return the names of the materials referenced by this instance.
    const foundation::StringArray& get_front_material_names() const;
    const foundation::StringArray& get_back_material_names() const;

    // Object binding.
    void unbind_object();
    void bind_object(const ObjectContainer& objects);
    void check_object() const;

    // Material binding.
    void unbind_materials();
    void bind_materials(const MaterialContainer& materials);
    void check_materials() const;

    // Return the object bound to this instance.
    Object& get_object() const;

    // Return the materials bound to this instance.
    const MaterialArray& get_front_materials() const;
    const MaterialArray& get_back_materials() const;

  private:
    friend class ObjectInstanceFactory;

    struct Impl;
    Impl* impl;

    Object*             m_object;
    MaterialArray       m_front_materials;
    MaterialArray       m_back_materials;

    // Constructor.
    ObjectInstance(
        const char*                     name,
        const ParamArray&               params,
        const char*                     object_name,
        const foundation::Transformd&   transform,
        const foundation::StringArray&  front_materials,
        const foundation::StringArray&  back_materials);

    // Destructor.
    ~ObjectInstance();
};


//
// Object instance factory.
//

class DLLSYMBOL ObjectInstanceFactory
{
  public:
    // Create a new object instance.
    static foundation::auto_release_ptr<ObjectInstance> create(
        const char*                     name,
        const ParamArray&               params,
        const char*                     object_name,
        const foundation::Transformd&   transform,
        const foundation::StringArray&  front_materials,
        const foundation::StringArray&  back_materials = foundation::StringArray());
};


//
// ObjectInstance class implementation.
//

inline Object& ObjectInstance::get_object() const
{
    assert(m_object);

    return *m_object;
}

inline const MaterialArray& ObjectInstance::get_front_materials() const
{
    return m_front_materials;
}

inline const MaterialArray& ObjectInstance::get_back_materials() const
{
    return m_back_materials;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_OBJECTINSTANCE_H
