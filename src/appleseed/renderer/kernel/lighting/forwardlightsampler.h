
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
#include "renderer/kernel/lighting/lighttypes.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class LightSample; }
namespace renderer  { class Scene; }

namespace renderer
{

//
// The forward light sampler is intended to be used with forward tracing techniques
// such as light tracing, SPPM, etc.
//

class ForwardLightSampler
  : public LightSamplerBase
{
  public:
    // Constructor.
    ForwardLightSampler(
        const Scene&                    scene,
        const ParamArray&               params = ParamArray());

    // Return true if the scene contains at least one light or emitting shape.
    bool has_lights() const;

    // Sample the sets of non-physical lights and emitting shapes.
    void sample(
        const ShadingRay::Time&         time,
        const foundation::Vector3f&     s,
        LightSample&                    light_sample) const;

    float evaluate_pdf(
        const ShadingPoint&             light_shading_point) const;

  private:
    // Sample the set of non-physical lights.
    void sample_non_physical_lights(
        const ShadingRay::Time&         time,
        const foundation::Vector3f&     s,
        LightSample&                    light_sample) const;
};


//
// ForwardLightSampler class implementation.
//

inline bool ForwardLightSampler::has_lights() const
{
    return m_non_physical_lights_cdf.valid() || m_emitting_shapes_cdf.valid();
}

}   // namespace renderer
