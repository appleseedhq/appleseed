
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/mis.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class BSDF; }
namespace renderer  { class LightSample; }
namespace renderer  { class LightSampler; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// The direct lighting integrator allows to estimate direct lighting at a given point in the scene.
//
// Note about the methods ending with *_low_variance():
//
//   The sample_lights() and sample_bsdf_and_lights() methods have "low variance" counterparts
//   respectively called sample_lights_low_variance() and sample_bsdf_and_lights_low_variance().
//   These methods treat non-physical light sources (such as point lights) and light-emitting
//   triangles differently: every light source of the scene is sampled individually, while the
//   set of light-emitting triangles is sampled as a whole.
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
        const LightSampler&             light_sampler,
        const ShadingPoint&             shading_point,              // point at which to integrate direct lighting
        const BSDF&                     bsdf,                       // BSDF at 'shading_point'
        const void*                     bsdf_data,                  // input data of the BSDF
        const int                       bsdf_sampling_modes,        // permitted scattering modes during BSDF sampling
        const int                       light_sampling_modes,       // permitted scattering modes during environment sampling
        const size_t                    bsdf_sample_count,          // number of samples in BSDF sampling
        const size_t                    light_sample_count,         // number of samples in light sampling
        const float                     low_light_threshold,        // light contribution threshold to disable shadow rays
        const bool                      indirect);                  // are we computing indirect lighting?

    // Compute outgoing radiance due to direct lighting via combined BSDF and light sampling.
    void compute_outgoing_radiance_combined_sampling(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        Spectrum&                       radiance) const;
    void compute_outgoing_radiance_combined_sampling_low_variance(
        SamplingContext&                sampling_context,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        Spectrum&                       radiance) const;

    // Compute outgoing radiance due to direct lighting via BSDF sampling only.
    void compute_outgoing_radiance_bsdf_sampling(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        Spectrum&                       radiance) const;

    // Compute outgoing radiance due to direct lighting via light sampling only.
    void compute_outgoing_radiance_light_sampling(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        Spectrum&                       radiance) const;
    void compute_outgoing_radiance_light_sampling_low_variance(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        Spectrum&                       radiance) const;

    // Evaluate incoming radiance.
    bool compute_incoming_radiance(
        SamplingContext&                sampling_context,
        foundation::Vector3d&           incoming,
        float&                          incoming_prob,
        Spectrum&                       radiance) const;

  private:
    const ShadingContext&               m_shading_context;
    const LightSampler&                 m_light_sampler;
    const ShadingPoint&                 m_shading_point;
    const foundation::Vector3d&         m_point;
    const foundation::Vector3d&         m_geometric_normal;
    const foundation::Basis3d&          m_shading_basis;
    const ShadingRay::Time&             m_time;
    const BSDF&                         m_bsdf;
    const void*                         m_bsdf_data;
    const int                           m_bsdf_sampling_modes;
    const int                           m_light_sampling_modes;
    const float                         m_low_light_threshold;
    const size_t                        m_bsdf_sample_count;
    const size_t                        m_light_sample_count;
    const bool                          m_indirect;

    void take_single_bsdf_sample(
        SamplingContext&                sampling_context,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,
        Spectrum&                       radiance) const;

    void add_emitting_triangle_sample_contribution(
        const LightSample&              sample,
        const foundation::MISHeuristic  mis_heuristic,
        const foundation::Dual3d&       outgoing,
        Spectrum&                       radiance) const;

    void add_non_physical_light_sample_contribution(
        const LightSample&              sample,
        const foundation::Dual3d&       outgoing,
        Spectrum&                       radiance) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H
