
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "renderer/kernel/lighting/lightsample.h"
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/cdf.h"

// Standard headers.
#include <functional>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class Assembly; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class Material; }
namespace renderer      { class MaterialArray; }

namespace renderer
{

//
// LightSamplerBase class implementation.
//
// The LightSamplerBase contains function used by both BackwardLightSampler and
// ForwardLightSampler classes.
//

class LightSamplerBase
  : public foundation::NonCopyable
{
  public:
    // Return the number of non-physical lights in the scene.
    size_t get_non_physical_light_count() const;

    // Sample a single given non-physical light.
    void sample_non_physical_light(
        const ShadingRay::Time&             time,
        const size_t                        light_index,
        LightSample&                        light_sample,
        const float                         light_prob = 1.0f) const;

  protected:
    struct Parameters
    {
        const bool m_importance_sampling;

        explicit Parameters(const ParamArray& params);
    };

    typedef std::vector<NonPhysicalLightInfo> NonPhysicalLightVector;
    typedef std::vector<EmittingShape> EmittingShapeVector;
    typedef foundation::CDF<size_t, float> EmitterCDF;

    typedef std::function<void (const NonPhysicalLightInfo&)> LightHandlingFunction;
    typedef std::function<bool (const Material*, const float, const size_t)> ShapeHandlingFunction;

    const Parameters                        m_params;

    NonPhysicalLightVector                  m_non_physical_lights;
    EmittingShapeVector                     m_emitting_shapes;

    size_t                                  m_non_physical_light_count;

    EmitterCDF                              m_non_physical_lights_cdf;
    EmitterCDF                              m_emitting_shapes_cdf;

    EmittingShapeKeyHasher                  m_shape_key_hasher;
    EmittingShapeHashTable                  m_emitting_shape_hash_table;

    // Return metadata for parameters common to all light samplers.
    static foundation::Dictionary get_params_metadata();

    // Constructor.
    explicit LightSamplerBase(const ParamArray& params);

    // Build a hash table that allows to find the emitting shape at a given shading point.
    void build_emitting_shape_hash_table();

    // Recursively collect emitting shapes from a given set of assembly instances.
    void collect_emitting_shapes(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq,
        const ShapeHandlingFunction&        shape_handling);

    // Collect emitting shapes from a given assembly.
    void collect_emitting_shapes(
        const Assembly&                     assembly,
        const AssemblyInstance&             assembly_instance,
        const TransformSequence&            transform_sequence,
        const ShapeHandlingFunction&        shape_handling);

    // Recursively collect non-physical lights from a given set of assembly instances.
    void collect_non_physical_lights(
        const AssemblyInstanceContainer&    assembly_instances,
        const TransformSequence&            parent_transform_seq,
        const LightHandlingFunction&        light_handling);

    // Collect non-physical lights from a given assembly.
    void collect_non_physical_lights(
        const Assembly&                     assembly,
        const TransformSequence&            transform_sequence,
        const LightHandlingFunction&        light_handling);

    void store_object_area_in_shadergroups(
        const AssemblyInstance*             assembly_instance,
        const ObjectInstance*               object_instance,
        const float                         object_area,
        const MaterialArray&                materials);

    // Sample a given emitting shape.
    void sample_emitting_shape(
        const ShadingRay::Time&             time,
        const foundation::Vector2f&         s,
        const size_t                        shape_index,
        const float                         shape_prob,
        LightSample&                        light_sample) const;

    // Sample the set of emitting shapes.
    void sample_emitting_shapes(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        LightSample&                        light_sample) const;
};


//
// LightSamplerBase class implementation.
//

inline size_t LightSamplerBase::get_non_physical_light_count() const
{
    return m_non_physical_light_count;
}

}   // namespace renderer
