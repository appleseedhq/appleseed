
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/mis.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class BackwardLightSampler; }
namespace renderer  { class DirectShadingComponents; }
namespace renderer  { class LightSample; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class ShadingRay; }

namespace renderer
{

//
// The volume lighting integrator allows to integrate in-scattered direct lighting
// along a ray in participating medium.
//
// Combines exponential importance sampling (based on Beer's law) and
// equiangular sampling (based on proximity to light sources).
//
// For more information:
//
//   Importance Sampling Techniques for Path Tracing in Participating Media
//   Christopher Kulla, Marcos Fajardo
//   Journal Computer Graphics Forum Vol. 31 Issue 4, pp. 1519-1528, June 2012.
//   https://www.solidangle.com/research/egsr2012_volume.pdf
//

class VolumeLightingIntegrator
{
  public:
    // Constructor.
    VolumeLightingIntegrator(
        const ShadingContext&           shading_context,
        const BackwardLightSampler&     light_sampler,
        const Volume&                   volume,
        const ShadingRay&               volume_ray,
        const void*                     volume_data,
        const ShadingPoint&             shading_point,
        const int                       scattering_modes,
        const size_t                    distance_sample_count,
        const size_t                    light_sample_count,
        const float                     low_light_threshold,
        const bool                      indirect);

    // Integrate in-scattered radiance over the given ray
    // using both equiangular and exponential sampling.
    void compute_radiance_combined_sampling(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        DirectShadingComponents&        radiance) const;

    // Integrate in-scattered radiance over the given ray
    // using only exponential sampling.
    void compute_radiance_exponential_sampling(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        DirectShadingComponents&        radiance) const;

  private:
    const ShadingContext&               m_shading_context;
    const BackwardLightSampler&         m_light_sampler;
    const ShadingRay::Time&             m_time;
    const Volume&                       m_volume;
    const ShadingRay&                   m_volume_ray;
    const void*                         m_volume_data;
    const ShadingPoint&                 m_shading_point;
    const int                           m_scattering_modes;
    const size_t                        m_distance_sample_count;
    const float                         m_rcp_distance_sample_count;
    const size_t                        m_light_sample_count;
    const float                         m_low_light_threshold;
    const bool                          m_indirect;

    // Sample distance and integrate in-scattered lighting at this distance.
    void add_single_distance_sample_contribution(
        const LightSample*              light_sample,
        const Spectrum&                 extinction_coef,
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        DirectShadingComponents&        radiance,
        const bool                      sample_phase_function) const;

    // Sample distance and integrate in-scattered lighting at this distance.
    // This is the optimized version that uses only exponential sampling.
    void add_single_distance_sample_contribution_exponential_only(
        const LightSample*              light_sample,
        const Spectrum&                 extinction_coef,
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        DirectShadingComponents&        radiance,
        const bool                      sample_phasefunction) const;

    float draw_exponential_sample(
        SamplingContext&                sampling_context,
        const ShadingRay&               volume_ray,
        const float                     extinction) const;

    float evaluate_exponential_sample(
        const float                     distance,
        const ShadingRay&               volume_ray,
        const float                     extinction) const;

    // Add single light sample contribution for the specified distance sample.
    // You can pass light sample that is used as a center for equiangular sampling.
    // Note that the same light sample is used for shading only if it is non-physical light,
    // otherwise (if nullptr or area light sample) the lights are sampled again.
    void take_single_direction_sample(
        const bool                      sample_phasefunction,
        SamplingContext&                sampling_context,
        const LightSample*              light_sample,
        const float                     distance_sample,
        const foundation::MISHeuristic  mis_heuristic,
        DirectShadingComponents&        radiance) const;
};

}   // namespace renderer
