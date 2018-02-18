
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLE_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lighttree.h"
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/math/hash.h"
#include "foundation/utility/containers/hashtable.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class Intersector; }
namespace renderer  { class Light; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{
//
// A key to uniquely identify a light-emitting triangle in a hash table.
//

class EmittingTriangleKey
{
  public:
    foundation::UniqueID            m_assembly_instance_uid;
    foundation::uint32              m_object_instance_index;
    foundation::uint32              m_region_index;
    foundation::uint32              m_triangle_index;

    EmittingTriangleKey();
    EmittingTriangleKey(
        const foundation::UniqueID  assembly_instance_uid,
        const size_t                object_instance_index,
        const size_t                region_index,
        const size_t                triangle_index);

    bool operator==(const EmittingTriangleKey& rhs) const;
};


//
// A hash table of light-emitting triangles.
//

struct EmittingTriangleKeyHasher
{
    size_t operator()(const EmittingTriangleKey& key) const;
};

typedef foundation::HashTable<
    EmittingTriangleKey,
    EmittingTriangleKeyHasher,
    const EmittingTriangle*
> EmittingTriangleHashTable;


//
// Light sample: the result of sampling sets of non-physical lights and
// light-emitting triangles.
//

class LightSample
{
  public:
    LightSample();

    // Data for a light-emitting triangle sample.
    const EmittingTriangle*     m_triangle;
    foundation::Vector2f        m_bary;                         // barycentric coordinates of the sample
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
// EmittingTriangleKey class implementation.
//

inline EmittingTriangleKey::EmittingTriangleKey()
{
}

inline EmittingTriangleKey::EmittingTriangleKey(
    const foundation::UniqueID              assembly_instance_uid,
    const size_t                            object_instance_index,
    const size_t                            region_index,
    const size_t                            triangle_index)
  : m_assembly_instance_uid(static_cast<foundation::uint32>(assembly_instance_uid))
  , m_object_instance_index(static_cast<foundation::uint32>(object_instance_index))
  , m_region_index(static_cast<foundation::uint32>(region_index))
  , m_triangle_index(static_cast<foundation::uint32>(triangle_index))
{
}

inline bool EmittingTriangleKey::operator==(const EmittingTriangleKey& rhs) const
{
    return
        m_triangle_index == rhs.m_triangle_index &&
        m_object_instance_index == rhs.m_object_instance_index &&
        m_assembly_instance_uid == rhs.m_assembly_instance_uid &&
        m_region_index == rhs.m_region_index;
}


//
// EmittingTriangleKeyHasher class implementation.
//

inline size_t EmittingTriangleKeyHasher::operator()(const EmittingTriangleKey& key) const
{
    return
        foundation::mix_uint32(
            static_cast<foundation::uint32>(key.m_assembly_instance_uid),
            key.m_object_instance_index,
            key.m_region_index,
            key.m_triangle_index);
}


//
// LightSample class implementation.
//

inline LightSample::LightSample()
  : m_triangle(nullptr)
  , m_light(nullptr)
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLE_H
