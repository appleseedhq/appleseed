
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/cdf.h"
#include "foundation/math/hash.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/hashtable.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class Intersector; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }
namespace renderer  { class MaterialArray; }
namespace renderer  { class ObjectInstance; }
namespace renderer  { class ParamArray; }
namespace renderer  { class Scene; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Non-physical light.
//

class NonPhysicalLightInfo
{
  public:
    TransformSequence           m_transform_sequence;           // assembly instance (parent of the light) space to world space
    const Light*                m_light;
};


//
// Light-emitting triangle.
//

class EmittingTriangle
{
  public:
    const AssemblyInstance*     m_assembly_instance;
    size_t                      m_object_instance_index;
    size_t                      m_region_index;
    size_t                      m_triangle_index;
    foundation::Vector3d        m_v0, m_v1, m_v2;               // world space vertices of the triangle
    foundation::Vector3d        m_n0, m_n1, m_n2;               // world space vertex normals
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    TriangleSupportPlaneType    m_triangle_support_plane;       // support plane of the triangle in assembly space
    float                       m_area;                         // world space triangle area 
    float                       m_rcp_area;                     // world space triangle area reciprocal
    float                       m_triangle_prob;                // probability density of this triangle
    const Material*             m_material;
};


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
// Light sample: the result of sampling the sets of non-physical lights and light-emitting triangles.
//

class LightSample
{
  public:
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
// The light sampler collects all the light-emitting entities (non-physical lights, mesh lights)
// and allows to sample them.
//

class LightSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    LightSampler(
        const Scene&                        scene,
        const ParamArray&                   params = ParamArray());

    // Return the number of non-physical lights in the scene.
    size_t get_non_physical_light_count() const;

    // Return the number of emitting triangles in the scene.
    size_t get_emitting_triangle_count() const;

    // Return true if the scene contains at least one light or emitting triangle.
    bool has_lights_or_emitting_triangles() const;

    // Sample the set of non-physical lights.
    void sample_non_physical_lights(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        LightSample&                        light_sample) const;

    // Sample a single given non-physical light.
    void sample_non_physical_light(
        const ShadingRay::Time&             time,
        const size_t                        light_index,
        LightSample&                        light_sample) const;

    // Sample the set of emitting triangles.
    void sample_emitting_triangles(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        LightSample&                        light_sample) const;

    // Sample the sets of non-physical lights and emitting triangles.
    void sample(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        LightSample&                        light_sample) const;

    // Compute the probability density in area measure of a given light sample.
    float evaluate_pdf(const ShadingPoint& shading_point) const;

  private:
    struct Parameters
    {
        const bool m_importance_sampling;

        explicit Parameters(const ParamArray& params);
    };

    typedef std::vector<NonPhysicalLightInfo> NonPhysicalLightVector;
    typedef std::vector<EmittingTriangle> EmittingTriangleVector;
    typedef foundation::CDF<size_t, float> EmitterCDF;

    const Parameters            m_params;

    NonPhysicalLightVector      m_non_physical_lights;
    size_t                      m_non_physical_light_count;

    EmittingTriangleVector      m_emitting_triangles;

    EmitterCDF                  m_non_physical_lights_cdf;
    EmitterCDF                  m_emitting_triangles_cdf;

    EmittingTriangleKeyHasher   m_triangle_key_hasher;
    EmittingTriangleHashTable   m_emitting_triangle_hash_table;

    // Recursively collect non-physical lights from a given set of assembly instances.
    void collect_non_physical_lights(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq);

    // Collect non-physical lights from a given assembly.
    void collect_non_physical_lights(
        const Assembly&                     assembly,
        const TransformSequence&            transform_sequence);

    // Recursively collect emitting triangles from a given set of assembly instances.
    void collect_emitting_triangles(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq);

    // Collect emitting triangles from a given assembly.
    void collect_emitting_triangles(
        const Assembly&                     assembly,
        const AssemblyInstance&             assembly_instance,
        const TransformSequence&            transform_sequence);

    // Build a hash table that allows to find the emitting triangle at a given shading point.
    void build_emitting_triangle_hash_table();

    // Sample a given non-physical light.
    void sample_non_physical_light(
        const ShadingRay::Time&             time,
        const size_t                        light_index,
        const float                         light_prob,
        LightSample&                        sample) const;

    // Sample a given emitting triangle.
    void sample_emitting_triangle(
        const ShadingRay::Time&             time,
        const foundation::Vector2f&         s,
        const size_t                        triangle_index,
        const float                         triangle_prob,
        LightSample&                        sample) const;

    void store_object_area_in_shadergroups(
        const AssemblyInstance*             assembly_instance,
        const ObjectInstance*               object_instance,
        const float                         object_area,
        const MaterialArray&                materials);
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
// LightSampler class implementation.
//

inline size_t LightSampler::get_non_physical_light_count() const
{
    return m_non_physical_light_count;
}

inline size_t LightSampler::get_emitting_triangle_count() const
{
    return m_emitting_triangles.size();
}

inline bool LightSampler::has_lights_or_emitting_triangles() const
{
    return m_non_physical_lights_cdf.valid() || m_emitting_triangles_cdf.valid();
}

inline void LightSampler::sample_non_physical_light(
    const ShadingRay::Time&                 time,
    const size_t                            light_index,
    LightSample&                            sample) const
{
    sample_non_physical_light(time, light_index, 1.0, sample);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTSAMPLER_H
