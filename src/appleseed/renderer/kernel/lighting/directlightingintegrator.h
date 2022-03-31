
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/materialsamplers.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/mis.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class BackwardLightSampler; }
namespace renderer  { class DirectShadingComponents; }
namespace renderer  { class LightPathStream; }
namespace renderer  { class LightSample; }
namespace renderer  { class ShadingContext; }

namespace renderer
{

//
// The direct lighting integrator allows to estimate direct lighting at a given point in the scene.
//
// Note about the methods ending with *_low_variance():
//
//   The sample_lights() and sample_material_and_lights() methods have "low variance" counterparts
//   respectively called sample_lights_low_variance() and sample_material_and_lights_low_variance().
//   These methods treat non-physical light sources (such as point lights) and light-emitting
//   shapes differently: every light source of the scene is sampled individually, while the
//   set of light-emitting shapes is sampled as a whole.
//
//   The number of shadow rays cast by these functions may be as high as the number of light
//   samples passed to the constructor plus the number of non-physical lights in the scene.
//

class DirectLightingIntegrator
{
  public:
    // Constructor.
    DirectLightingIntegrator(
        const ShadingContext&           shading_context,
        const BackwardLightSampler&     light_sampler,
        const IMaterialSampler&         material_sampler,
        const ShadingRay::Time&         time,
        const int                       light_sampling_modes,
        const size_t                    material_sample_count,        // number of samples in material sampling
        const size_t                    light_sample_count,           // number of samples in light sampling
        const float                     low_light_threshold,          // light contribution threshold to disable shadow rays
        const bool                      indirect);                    // are we computing indirect lighting?

    // Compute outgoing radiance due to direct lighting via combined BSDF and light sampling.
    void compute_outgoing_radiance_combined_sampling_low_variance(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        DirectShadingComponents&        radiance,
        Spectrum&                       unshaded_radiance,
        Spectrum&                       shaded_radiance,
        LightPathStream*                light_path_stream) const;

    // Compute outgoing radiance due to direct lighting via BSDF sampling only.
    void compute_outgoing_radiance_material_sampling(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        DirectShadingComponents&        radiance) const;

    // Compute outgoing radiance due to direct lighting via light sampling only.
    void compute_outgoing_radiance_light_sampling_low_variance(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        DirectShadingComponents&        radiance,
        Spectrum&                       unshaded_radiance,
        Spectrum&                       shaded_radiance,
        LightPathStream*                light_path_stream) const;

  private:
    friend class VolumeLightingIntegrator;

    const ShadingContext&               m_shading_context;
    const BackwardLightSampler&         m_light_sampler;
    const ShadingRay::Time&             m_time;
    const IMaterialSampler&             m_material_sampler;
    const int                           m_light_sampling_modes;
    const float                         m_low_light_threshold;
    const size_t                        m_material_sample_count;
    const size_t                        m_light_sample_count;
    const bool                          m_indirect;

    void take_single_material_sample(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,
        DirectShadingComponents&        radiance) const;

    void add_emitting_shape_sample_contribution(
        SamplingContext&                sampling_context,
        const LightSample&              sample,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,
        DirectShadingComponents&        radiance,
        Spectrum&                       unshaded_radiance,
        Spectrum&                       shaded_radiance,
        LightPathStream*                light_path_stream) const;

    void add_non_physical_light_sample_contribution(
        SamplingContext&                sampling_context,
        const LightSample&              sample,
        const foundation::Dual3d&       outgoing,
        DirectShadingComponents&        radiance,
        Spectrum&                       unshaded_radiance,
        Spectrum&                       shaded_radiance,
        LightPathStream*                light_path_stream) const;
};

}   // namespace renderer
