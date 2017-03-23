
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_OBJECTINSTANCE_H
#define APPLESEED_RENDERER_MODELING_SCENE_OBJECTINSTANCE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/math/transform.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apiarray.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class Assembly; }
namespace renderer      { class Material; }
namespace renderer      { class Object; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// An array of materials.
//

APPLESEED_DECLARE_APIARRAY(MaterialArray, const Material*);

// Return true if at least one material in the array emits light.
bool has_emitting_materials(const MaterialArray& materials);

// Return true if at least one material in the array has an alpha map set.
bool uses_alpha_mapping(const MaterialArray& materials);


//
// An instance of an object.
//

class APPLESEED_DLLSYMBOL ObjectInstance
  : public Entity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Compute and return the unique signature of this instance.
    virtual foundation::uint64 compute_signature() const;

    // Return the name of the instantiated object.
    const char* get_object_name() const;

    // Return the transform of this instance.
    const foundation::Transformd& get_transform() const;

    // Return true if the transform of this instance swaps handedness.
    bool transform_swaps_handedness() const;

    // Return true if the normals of this instance must be flipped.
    bool flip_normals() const;

    // Return the visibility flags of this instance.
    foundation::uint32 get_vis_flags() const;

    // Return the medium priority of this instance.
    foundation::uint8 get_medium_priority() const;

    enum RayBiasMethod
    {
        RayBiasMethodNone,                  // no ray bias for this object instance
        RayBiasMethodNormal,                // shift the ray's origin along the surface's geometric normal
        RayBiasMethodIncomingDirection,     // shift the ray's origin along the incoming ray's direction
        RayBiasMethodOutgoingDirection      // shift the ray's origin along the outgoing ray's direction
    };

    // Return the ray bias settings. The bias distance is expressed in world space.
    RayBiasMethod get_ray_bias_method() const;
    double get_ray_bias_distance() const;

    // Find the object bound to this instance.
    Object* find_object() const;

    // Compute the parent space bounding box of the instance.
    GAABB3 compute_parent_bbox() const;

    // Sides of this object instance's surface.
    enum Side
    {
        FrontSide = 1 << 0,
        BackSide  = 1 << 1,
        BothSides = FrontSide | BackSide
    };

    // Clear all material assignments.
    void clear_front_materials();
    void clear_back_materials();

    // Assign a material to a given slot.
    void assign_material(
        const char*     slot,
        const Side      side,
        const char*     name);

    // Unassign a material from a given slot.
    void unassign_material(
        const char*     slot,
        const Side      side);

    // Return the slot-to-material mappings of this instance.
    foundation::StringDictionary& get_front_material_mappings() const;
    foundation::StringDictionary& get_back_material_mappings() const;

    // Get the name of the material bound to a given primitive of this instance.
    const char* get_material_name(const size_t pa_index, const Side side) const;

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

    // Check if this object instance is in the same sss set as the given one
    bool is_in_same_sss_set(const ObjectInstance& other) const;

    // Return the materials bound to this instance.
    const MaterialArray& get_front_materials() const;
    const MaterialArray& get_back_materials() const;

    // Return true if at least one of the material referenced by this instance has an alpha map set.
    bool uses_alpha_mapping() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = 0) APPLESEED_OVERRIDE;

  private:
    friend class ObjectInstanceFactory;

    struct Impl;
    Impl* impl;

    foundation::uint32  m_vis_flags;
    foundation::uint8   m_medium_priority;
    RayBiasMethod       m_ray_bias_method;
    double              m_ray_bias_distance;
    bool                m_transform_swaps_handedness;
    bool                m_flip_normals;

    Object*             m_object;
    MaterialArray       m_front_materials;
    MaterialArray       m_back_materials;

    // Constructor.
    ObjectInstance(
        const char*                         name,
        const ParamArray&                   params,
        const char*                         object_name,
        const foundation::Transformd&       transform,
        const foundation::StringDictionary& front_material_mappings,
        const foundation::StringDictionary& back_material_mappings);

    // Destructor.
    ~ObjectInstance();
};


//
// Object instance factory.
//

class APPLESEED_DLLSYMBOL ObjectInstanceFactory
{
  public:
    // Return a set of input metadata for object instance entities.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new object instance.
    static foundation::auto_release_ptr<ObjectInstance> create(
        const char*                         name,
        const ParamArray&                   params,
        const char*                         object_name,
        const foundation::Transformd&       transform,
        const foundation::StringDictionary& front_material_mappings,
        const foundation::StringDictionary& back_material_mappings = foundation::StringDictionary());
};


//
// ObjectInstance class implementation.
//

inline bool ObjectInstance::transform_swaps_handedness() const
{
    return m_transform_swaps_handedness;
}

inline bool ObjectInstance::flip_normals() const
{
    return m_flip_normals;
}

inline foundation::uint32 ObjectInstance::get_vis_flags() const
{
    return m_vis_flags;
}

inline foundation::uint8 ObjectInstance::get_medium_priority() const
{
    return m_medium_priority;
}

inline ObjectInstance::RayBiasMethod ObjectInstance::get_ray_bias_method() const
{
    return m_ray_bias_method;
}

inline double ObjectInstance::get_ray_bias_distance() const
{
    return m_ray_bias_distance;
}

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
