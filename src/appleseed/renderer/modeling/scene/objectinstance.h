
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/utility/containers/array.h"

// Forward declarations.
namespace foundation    { class StringArray; }
namespace renderer      { class Material; }
namespace renderer      { class Object; }

namespace renderer
{

//
// An array of materials.
//

DECLARE_ARRAY(MaterialArray, const Material*);


//
// An instance of an object.
//

class RENDERERDLL ObjectInstance
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return the instantiated object.
    Object& get_object() const;

    // Return the transform of the instance.
    const foundation::Transformd& get_transform() const;

    // Return the parent space bounding box of the instance.
    const GAABB3& get_parent_bbox() const;

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

    // Perform entity binding.
    void bind_entities(const MaterialContainer& materials);

    // Return the materials referenced by this instance.
    const MaterialArray& get_front_materials() const;
    const MaterialArray& get_back_materials() const;

  private:
    friend class ObjectInstanceFactory;

    // Private implementation.
    struct Impl;
    Impl* impl;

    // Derogate to the private implementation rule, for performance reasons.
    MaterialArray   m_front_materials;
    MaterialArray   m_back_materials;

    // Constructor.
    ObjectInstance(
        const char*                     name,
        const ParamArray&               params,
        Object&                         object,
        const foundation::Transformd&   transform,
        const foundation::StringArray&  front_materials,
        const foundation::StringArray&  back_materials);

    // Destructor.
    ~ObjectInstance();
};


//
// Object instance factory.
//

class RENDERERDLL ObjectInstanceFactory
{
  public:
    // Create a new object instance.
    static foundation::auto_release_ptr<ObjectInstance> create(
        const char*                     name,
        const ParamArray&               params,
        Object&                         object,
        const foundation::Transformd&   transform,
        const foundation::StringArray&  front_materials,
        const foundation::StringArray&  back_materials = foundation::StringArray());
};


//
// ObjectInstance class implementation.
//

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
