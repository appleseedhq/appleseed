
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

// Interface header.
#include "directlightingintegrator.h"

// appleseed.renderer headers.
#include "renderer/modeling/light/light.h"

using namespace foundation;
using namespace std;

namespace renderer
{

DirectLightingIntegrator::DirectLightingIntegrator(
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const double                time,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const int                   bsdf_modes,
    const size_t                bsdf_sample_count,
    const size_t                light_sample_count,
    const ShadingPoint*         parent_shading_point)
  : m_shading_context(shading_context)
  , m_light_sampler(light_sampler)
  , m_point(point)
  , m_geometric_normal(geometric_normal)
  , m_shading_basis(shading_basis)
  , m_time(time)
  , m_outgoing(outgoing)
  , m_bsdf(bsdf)
  , m_bsdf_data(bsdf_data)
  , m_bsdf_modes(bsdf_modes)
  , m_bsdf_sample_count(bsdf_sample_count)
  , m_light_sample_count(light_sample_count)
  , m_parent_shading_point(parent_shading_point)
{
    assert(is_normalized(geometric_normal));
    assert(is_normalized(outgoing));
}

void DirectLightingIntegrator::sample_bsdf_and_lights(
    SamplingContext&            sampling_context,
    Spectrum&                   radiance,
    SpectrumStack&              aovs)
{
    if (m_bsdf_sample_count + m_light_sample_count == 0)
        take_single_bsdf_or_light_sample(sampling_context, radiance, aovs);
    else
    {
        Spectrum radiance_light_sampling;
        sample_bsdf(sampling_context, DirectLightingIntegrator::mis_power2, radiance, aovs);
        sample_lights(sampling_context, DirectLightingIntegrator::mis_power2, radiance_light_sampling, aovs);
        radiance += radiance_light_sampling;
    }
}

void DirectLightingIntegrator::sample_bsdf_and_lights_low_variance(
    SamplingContext&            sampling_context,
    Spectrum&                   radiance,
    SpectrumStack&              aovs)
{
    if (m_bsdf_sample_count + m_light_sample_count == 0)
        take_single_bsdf_or_light_sample(sampling_context, radiance, aovs);
    else
    {
        Spectrum radiance_light_sampling;
        sample_bsdf(sampling_context, DirectLightingIntegrator::mis_power2, radiance, aovs);
        sample_lights_low_variance(sampling_context, DirectLightingIntegrator::mis_power2, radiance_light_sampling, aovs);
        radiance += radiance_light_sampling;
    }
}

void DirectLightingIntegrator::take_single_bsdf_or_light_sample(
    SamplingContext&            sampling_context,
    Spectrum&                   radiance,
    SpectrumStack&              aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    sampling_context.split_in_place(1, 1);

    if (sampling_context.next_double2() < 0.5)
    {
        sampling_context.split_in_place(3, m_light_sample_count);

        take_single_light_sample(
            sampling_context,
            DirectLightingIntegrator::mis_balance,
            radiance,
            aovs);
    }
    else
    {
        take_single_bsdf_sample(
            sampling_context,
            DirectLightingIntegrator::mis_balance,
            radiance,
            aovs);
    }
}

void DirectLightingIntegrator::add_non_physical_light_sample_contribution(
    const LightSample&          sample,
    Spectrum&                   radiance,
    SpectrumStack&              aovs)
{
    // Evaluate the light.
    InputEvaluator input_evaluator(m_shading_context.get_texture_cache());
    Vector3d sample_position, emission_direction;
    Spectrum light_value;
    sample.m_light->evaluate(
        input_evaluator,
        sample.m_light_transform.point_to_local(m_point),
        sample_position,
        emission_direction,
        light_value);

    // Transform the sample position and emission direction from assembly space to world space.
    sample_position = sample.m_light_transform.point_to_parent(sample_position);
    emission_direction = normalize(sample.m_light_transform.vector_to_parent(emission_direction));

    // Compute the incoming direction in world space.
    const Vector3d incoming = -emission_direction;

    // Cull light samples behind the shading surface.
    double cos_in = dot(incoming, m_shading_basis.get_normal());
    if (m_bsdf.get_type() == BSDF::Transmissive)
        cos_in = -cos_in;
    if (cos_in <= 0.0)
        return;

    // Compute the transmission factor between the light sample and the shading point.
    const double transmission =
        m_shading_context.get_tracer().trace_between(
            m_point,
            sample_position,
            m_time,
            m_parent_shading_point);

    // Discard occluded samples.
    if (transmission == 0.0)
        return;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    const double bsdf_prob =
        m_bsdf.evaluate(
            m_bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            m_geometric_normal,
            m_shading_basis,
            m_outgoing,
            incoming,
            m_bsdf_modes,
            bsdf_value);
    if (bsdf_prob == 0.0)
        return;

    // Add the contribution of this sample to the illumination.
    const double attenuation = sample.m_light->compute_distance_attenuation(m_point, sample_position);
    const double weight = (transmission * attenuation) / sample.m_probability;
    light_value *= static_cast<float>(weight);
    light_value *= bsdf_value;
    radiance += light_value;
    aovs.add(sample.m_light->get_render_layer_index(), radiance);
}

}   // namespace renderer
