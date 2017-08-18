
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_VOLUMELIGHTINGINTEGRATOR_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_VOLUMELIGHTINGINTEGRATOR_H

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
namespace renderer  { class LightSample; }
namespace renderer  { class ShadingComponents; }
namespace renderer  { class ShadingContext; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

    //
    // The volume lighting integrator allows to integrate in-scattered direct lighting
    // along a ray in participating medium.
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
            const int                       light_sampling_modes,
            const bool                      samle_phasefunction,
            const size_t                    equiangular_sample_count,
            const size_t                    exponential_sample_count,
            const float                     low_light_threshold,
            const bool                      indirect);

        void compute_radiance(
            SamplingContext&                sampling_context,
            const foundation::MISHeuristic  mis_heuristic,
            ShadingComponents&              radiance) const;

      private:
        const ShadingContext&               m_shading_context;
        const BackwardLightSampler&         m_light_sampler;
        const ShadingRay::Time&             m_time;
        const Volume&                       m_volume;
        const ShadingRay&                   m_volume_ray;
        const void*                         m_volume_data;
        const ShadingPoint&                 m_shading_point;
        size_t                              m_channel_count;
        Spectrum                            m_precomputed_mis_weights;
        const int                           m_light_sampling_modes;
        const float                         m_low_light_threshold;
        const size_t                        m_equiangular_sample_count;
        const size_t                        m_exponential_sample_count;
        const bool                          m_sample_phasefunction;
        const bool                          m_indirect;

        void precompute_mis_weights();

        void add_single_equiangular_sample_contribution(
            const LightSample&              light_sample,
            const Spectrum&                 extinction_coef,
            const SamplingContext&          sampling_context,
            const foundation::MISHeuristic  mis_heuristic,
            ShadingComponents&              radiance,
            const bool                      sample_phasefunction,
            const float                     weight = 1.0f) const;

        void add_single_exponential_sample_contribution(
            const LightSample&              light_sample,
            const Spectrum&                 extinction_coef,
            const SamplingContext&          sampling_context,
            const foundation::MISHeuristic  mis_heuristic,
            ShadingComponents&              radiance,
            const bool                      sample_phasefunction,
            const float                     weight = 1.0f) const;

        float draw_exponential_sample(
            SamplingContext&        sampling_context,
            const ShadingRay&       volume_ray,
            const float             extinction) const;

        float evaluate_exponential_sample(
            const float             distance,
            const ShadingRay&       volume_ray,
            const float             extinction) const;

        void take_single_light_sample(
            SamplingContext&                sampling_context,
            const LightSample&              light_sample,
            const float                     distance_sample,
            const foundation::MISHeuristic  mis_heuristic,
            ShadingComponents&              radiance) const;

        void take_single_phasefunction_sample(
            SamplingContext&                sampling_context,
            const float                     distance_sample,
            const foundation::MISHeuristic  mis_heuristic,
            ShadingComponents&              radiance) const;
    };
}

#endif // APPLESEED_RENDERER_KERNEL_LIGHTING_VOLUMELIGHTINGINTEGRATOR_H
