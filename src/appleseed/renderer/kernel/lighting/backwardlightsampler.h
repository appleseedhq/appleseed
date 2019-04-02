
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
#include "renderer/kernel/lighting/lightsamplerbase.h"
#include "renderer/kernel/lighting/lighttree.h"
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>
#include <memory>
#include <vector>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class LightSample; }
namespace renderer      { class Scene; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// The backward light sampler is intended to be used with backward tracing techniques
// such as unidirectional path tracing.
//

class BackwardLightSampler
  : public LightSamplerBase
{
  public:
    // Return parameters metadata.
    static foundation::Dictionary get_params_metadata();

    // Constructor.
    BackwardLightSampler(
        const Scene&                        scene,
        const ParamArray&                   params = ParamArray());

    // Return true if the scene contains at least one non-physical light or emitting shape.
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

    // Compute the probability density in area measure of a given light sample.
    // Shading points are located on the light (emitting shape) hit by the
    // path tracer, and the surface actually being illuminated, respectively.
    float evaluate_pdf(
        const ShadingPoint&                 light_shading_point,
        const ShadingPoint&                 surface_shading_point) const;

  private:
    bool                                    m_use_light_tree;
    NonPhysicalLightVector                  m_light_tree_lights;
    std::unique_ptr<LightTree>              m_light_tree;

    void sample_light_tree(
        const ShadingRay::Time&             time,
        const foundation::Vector3f&         s,
        const ShadingPoint&                 shading_point,
        LightSample&                        light_sample) const;
};


//
// BackwardLightSampler class implementation.
//

inline bool BackwardLightSampler::has_lights() const
{
    return
        m_non_physical_lights_cdf.valid() ||
        !m_emitting_shapes.empty() ||
        !m_light_tree_lights.empty();
}

inline bool BackwardLightSampler::has_hittable_lights() const
{
    return !m_emitting_shapes.empty();
}

inline bool BackwardLightSampler::has_lightset() const
{
    return !m_emitting_shapes.empty() || !m_light_tree_lights.empty();
}

}   // namespace renderer
