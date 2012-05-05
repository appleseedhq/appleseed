
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTING_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTING_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/tracer.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class AOVCollection; }
namespace renderer      { class BSDF; }
namespace renderer      { class LightSample; }
namespace renderer      { class LightSampler; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// The direct lighting integrator allows to estimate direct lighting at a given point in the scene.
//

class DirectLightingIntegrator
{
  public:
    DirectLightingIntegrator(
        const ShadingContext&           shading_context,
        const LightSampler&             light_sampler,
        const foundation::Vector3d&     point,              // world space point
        const foundation::Vector3d&     geometric_normal,   // world space geometric normal, unit-length
        const foundation::Basis3d&      shading_basis,      // world space orthonormal basis around shading normal
        const double                    time,
        const foundation::Vector3d&     outgoing,           // world space outgoing direction, unit-length
        const BSDF&                     bsdf,
        const void*                     bsdf_data,
        const size_t                    bsdf_sample_count,
        const size_t                    light_sample_count,
        const ShadingPoint*             parent_shading_point = 0);

    void sample_bsdf(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    void sample_lights(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    void sample_bsdf_and_lights(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

  private:
    const ShadingContext&               m_shading_context;
    const LightSampler&                 m_light_sampler;
    const foundation::Vector3d&         m_point;
    const foundation::Vector3d&         m_geometric_normal;
    const foundation::Basis3d&          m_shading_basis;
    const double                        m_time;
    const foundation::Vector3d&         m_outgoing;
    const BSDF&                         m_bsdf;
    const void*                         m_bsdf_data;
    const size_t                        m_bsdf_sample_count;
    const size_t                        m_light_sample_count;
    const ShadingPoint*                 m_parent_shading_point;
    Tracer                              m_tracer;

    void take_single_bsdf_or_light_sample(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    template <typename WeightingFunction>
    void take_single_bsdf_sample(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    template <typename WeightingFunction>
    void take_single_light_sample(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    template <typename WeightingFunction>
    void add_emitting_triangle_sample_contribution(
        SamplingContext&                sampling_context,
        const LightSample&              sample,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    void add_light_sample_contribution(
        SamplingContext&                sampling_context,
        const LightSample&              sample,
        Spectrum&                       radiance,
        AOVCollection&                  aovs);

    bool check_visibility(
        SamplingContext&                sampling_context,
        const LightSample&              sample,
        double&                         transmission);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTING_H
