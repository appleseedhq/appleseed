
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
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/containers/hashtable.h"
#include "foundation/hash/hash.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <vector>

// Forward declarations.
namespace renderer  { class Intersector; }
namespace renderer  { class Light; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{
//
// A key to uniquely identify a light-emitting shape in a hash table.
//

class EmittingShapeKey
{
  public:
    foundation::UniqueID            m_assembly_instance_uid;
    std::uint32_t                   m_object_instance_index;
    std::uint32_t                   m_primitive_index;

    EmittingShapeKey();
    EmittingShapeKey(
        const foundation::UniqueID  assembly_instance_uid,
        const size_t                object_instance_index,
        const size_t                primitive_index);

    bool operator==(const EmittingShapeKey& rhs) const;
};


//
// A hash table of light-emitting shapes.
//

struct EmittingShapeKeyHasher
{
    size_t operator()(const EmittingShapeKey& key) const;
};

typedef foundation::HashTable<
    EmittingShapeKey,
    EmittingShapeKeyHasher,
    const EmittingShape*
> EmittingShapeHashTable;


//
// Light sample: the result of sampling sets of non-physical lights and
// light-emitting shapes.
//

class LightSample
{
  public:
    LightSample();

    // Data for a light-emitting shape sample.
    const EmittingShape*        m_shape;
    foundation::Vector2f        m_param_coords;                 // parametric coordinates of the sample
    foundation::Vector3d        m_point;                        // world space position of the sample
    foundation::Vector3d        m_shading_normal;               // world space shading normal at the sample, unit-length
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal at the sample, unit-length

    // Data for a non-physical light sample.
    const Light*                m_light;
    foundation::Transformd      m_light_transform;              // light space to world space transform

    // Data common to all sample types.
    float                       m_probability;                  // probability density of this sample

    // Construct a shading point out of this light sample and a given direction.
    void make_shading_point(
        ShadingPoint&                   shading_point,
        const foundation::Vector3d&     direction,
        const Intersector&              intersector) const;
};


//
// EmittingShapeKey class implementation.
//

inline EmittingShapeKey::EmittingShapeKey()
{
}

inline EmittingShapeKey::EmittingShapeKey(
    const foundation::UniqueID              assembly_instance_uid,
    const size_t                            object_instance_index,
    const size_t                            primitive_index)
  : m_assembly_instance_uid(static_cast<std::uint32_t>(assembly_instance_uid))
  , m_object_instance_index(static_cast<std::uint32_t>(object_instance_index))
  , m_primitive_index(static_cast<std::uint32_t>(primitive_index))
{
}

inline bool EmittingShapeKey::operator==(const EmittingShapeKey& rhs) const
{
    return
        m_primitive_index == rhs.m_primitive_index &&
        m_object_instance_index == rhs.m_object_instance_index &&
        m_assembly_instance_uid == rhs.m_assembly_instance_uid;
}


//
// EmittingShapeKeyHasher class implementation.
//

inline size_t EmittingShapeKeyHasher::operator()(const EmittingShapeKey& key) const
{
    return
        foundation::mix_uint32(
            static_cast<std::uint32_t>(key.m_assembly_instance_uid),
            key.m_object_instance_index,
            key.m_primitive_index);
}


//
// LightSample class implementation.
//

inline LightSample::LightSample()
  : m_shape(nullptr)
  , m_light(nullptr)
{
}

inline void LightSample::make_shading_point(
    ShadingPoint&               shading_point,
    const foundation::Vector3d& direction,
    const Intersector&          intersector) const
{
    assert(m_shape && !m_light);

    m_shape->make_shading_point(
        shading_point,
        m_point,
        direction,
        m_param_coords,
        intersector);
}

}   // namespace renderer
