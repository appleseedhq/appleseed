
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_BACKWARDLIGHTSAMPLER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_BACKWARDLIGHTSAMPLER_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/lighting/lightsample.h"
#include "renderer/kernel/lighting/lighttree.h"
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/hash.h"
#include "foundation/utility/containers/hashtable.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <vector>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class Assembly; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class MaterialArray; }
namespace renderer      { class ObjectInstance; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Scene; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// The backward light sampler is intended to be used with backward tracing techniques
// such as unidirectional path tracing.
//

class BackwardLightSampler
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    BackwardLightSampler(
        const Scene&                        scene,
        const ParamArray&                   params = ParamArray());

    // Return true if the scene contains at least one non-physical light or emitting triangle.
    bool has_lights() const;

    // Return true if the scene contains at least one light that can be hit by a ray.
    bool has_hittable_lights() const;

    // Return true if the light set is not empty.
    bool has_lightset() const;

    // Sample the light set.
    void sample_lightset(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        const ShadingPoint&                 shading_point,
        LightSample&                        light_sample) const;

    // Return the number of non-physical lights in the scene.
    size_t get_non_physical_light_count() const;

    // Sample a single given non-physical light.
    void sample_non_physical_light(
        const ShadingRay::Time&             time,
        const size_t                        light_index,
        LightSample&                        light_sample) const;

    // Compute the probability density in area measure of a given light sample.
    float evaluate_pdf(const ShadingPoint& shading_point) const;

    // Return the metadata of the light sampler parameters.
    static foundation::Dictionary get_params_metadata();

  private:
    struct Parameters
    {
        const bool m_importance_sampling;

        explicit Parameters(const ParamArray& params);
    };

    typedef std::vector<NonPhysicalLightInfo>   NonPhysicalLightVector;
    typedef std::vector<EmittingTriangle>       EmittingTriangleVector;
    typedef foundation::CDF<size_t, float>      EmitterCDF;

    const Parameters            m_params;

    NonPhysicalLightVector      m_light_tree_lights;
    NonPhysicalLightVector      m_non_physical_lights;
    size_t                      m_non_physical_light_count;

    EmittingTriangleVector      m_emitting_triangles;

    EmitterCDF                  m_non_physical_lights_cdf;
    EmitterCDF                  m_emitting_triangles_cdf;

    EmittingTriangleKeyHasher   m_triangle_key_hasher;
    EmittingTriangleHashTable   m_emitting_triangle_hash_table;

    std::unique_ptr<LightTree>  m_light_tree;

    bool                        m_use_light_tree;

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

    void sample_light_tree(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        const ShadingPoint&                 shading_point,
        LightSample&                        light_sample) const;

    void sample_emitting_triangles(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        LightSample&                        light_sample) const;

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
// BackwardLightSampler class implementation.
//

inline bool BackwardLightSampler::has_lights() const
{
    return
        m_non_physical_lights_cdf.valid() ||
        m_emitting_triangles.size() > 0 ||
        m_light_tree_lights.size() > 0;
}

inline bool BackwardLightSampler::has_hittable_lights() const
{
    return !m_emitting_triangles.empty();
}

inline bool BackwardLightSampler::has_lightset() const
{
    return m_light_tree_lights.size() > 0 || m_emitting_triangles.size() > 0;
}

inline size_t BackwardLightSampler::get_non_physical_light_count() const
{
    return m_non_physical_light_count;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_BACKWARDLIGHTSAMPLER_H
