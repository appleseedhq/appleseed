
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

// Interface header.
#include "directlightingintegrator.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/backwardlightsampler.h"
#include "renderer/kernel/lighting/lightpathstream.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/rr.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// DirectLightingIntegrator class implementation.
//
// Call graph:
//
//   compute_outgoing_radiance_material_sampling
//       take_single_bsdf_sample
//
//   compute_outgoing_radiance_light_sampling_low_variance
//       add_emitting_triangle_sample_contribution
//       add_non_physical_light_sample_contribution
//
//   compute_outgoing_radiance_combined_sampling_low_variance
//       compute_outgoing_radiance_material_sampling
//       compute_outgoing_radiance_light_sampling_low_variance
//

DirectLightingIntegrator::DirectLightingIntegrator(
    const ShadingContext&           shading_context,
    const BackwardLightSampler&     light_sampler,
    const ShadingPoint&             shading_point,
    const BSDF&                     bsdf,
    const void*                     bsdf_data,
    const int                       bsdf_sampling_modes,
    const int                       light_sampling_modes,
    const size_t                    bdsf_sample_count,
    const size_t                    light_sample_count,
    const float                     low_light_threshold,
    const bool                      indirect)
  : m_shading_context(shading_context)
  , m_light_sampler(light_sampler)
  , m_shading_point(shading_point)
  , m_point(shading_point.get_point())
  , m_geometric_normal(shading_point.get_geometric_normal())
  , m_shading_basis(shading_point.get_shading_basis())
  , m_time(shading_point.get_time())
  , m_bsdf(bsdf)
  , m_bsdf_data(bsdf_data)
  , m_bsdf_sampling_modes(bsdf_sampling_modes)
  , m_light_sampling_modes(light_sampling_modes)
  , m_bsdf_sample_count(bdsf_sample_count)
  , m_light_sample_count(light_sample_count)
  , m_low_light_threshold(low_light_threshold)
  , m_indirect(indirect)
{
}

void DirectLightingIntegrator::compute_outgoing_radiance_material_sampling(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance) const
{
    radiance.set(0.0f);

    // No hittable light in the scene.
    if (!m_light_sampler.has_hittable_lights())
        return;

    for (size_t i = 0, e = m_bsdf_sample_count; i < e; ++i)
    {
        take_single_bsdf_sample(
            sampling_context,
            mis_heuristic,
            outgoing,
            radiance);
    }

    if (m_bsdf_sample_count > 1)
        radiance /= static_cast<float>(m_bsdf_sample_count);
}

void DirectLightingIntegrator::compute_outgoing_radiance_light_sampling_low_variance(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance,
    LightPathStream*            light_path_stream) const
{
    radiance.set(0.0f);

    // No light source in the scene.
    if (!m_light_sampler.has_lights())
        return;

    // There cannot be any contribution for purely specular BSDFs.
    if (m_bsdf.is_purely_specular())
        return;

    if (m_light_sample_count > 0)
    {
        // Add contributions from non-physical light sources that don't belong to the lightset.
        for (size_t i = 0, e = m_light_sampler.get_non_physical_light_count(); i < e; ++i)
        {
            // Sample the set of non-physical lights.
            LightSample sample;
            m_light_sampler.sample_non_physical_light(m_time, i, sample);

            // Add the contribution of the chosen light.
            add_non_physical_light_sample_contribution(
                sampling_context,
                sample,
                outgoing,
                radiance,
                light_path_stream);
        }
    }

    // Add contributions from the light set.
    if (m_light_sampler.has_lightset())
    {
        DirectShadingComponents lightset_radiance;

        sampling_context.split_in_place(3, m_light_sample_count);

        for (size_t i = 0, e = m_light_sample_count; i < e; ++i)
        {
            // Sample the light set.
            LightSample sample;
            m_light_sampler.sample_lightset(
                m_time,
                sampling_context.next2<Vector3f>(),
                m_shading_point,
                sample);

            // Add the contribution of the chosen light.
            if (sample.m_triangle)
            {
                add_emitting_triangle_sample_contribution(
                    sampling_context,
                    sample,
                    mis_heuristic,
                    outgoing,
                    lightset_radiance,
                    light_path_stream);
            }
            else
            {
                add_non_physical_light_sample_contribution(
                    sampling_context,
                    sample,
                    outgoing,
                    lightset_radiance,
                    light_path_stream);
            }
        }

        if (m_light_sample_count > 1)
            lightset_radiance /= static_cast<float>(m_light_sample_count);

        radiance += lightset_radiance;
    }
}

void DirectLightingIntegrator::compute_outgoing_radiance_combined_sampling_low_variance(
    SamplingContext&            sampling_context,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance,
    LightPathStream*            light_path_stream) const
{
    compute_outgoing_radiance_material_sampling(
        sampling_context,
        MISPower2,
        outgoing,
        radiance);

    DirectShadingComponents radiance_light_sampling;
    compute_outgoing_radiance_light_sampling_low_variance(
        sampling_context,
        MISPower2,
        outgoing,
        radiance_light_sampling,
        light_path_stream);

    radiance += radiance_light_sampling;
}

void DirectLightingIntegrator::take_single_bsdf_sample(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance) const
{
    assert(m_light_sampler.has_hittable_lights());

    // Sample the BSDF.
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    m_bsdf.sample(
        sampling_context,
        m_bsdf_data,
        false,                  // not adjoint
        true,                   // multiply by |cos(incoming, normal)|
        m_bsdf_sampling_modes,
        sample);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & sample.get_mode()))
        return;

    // Trace a ray in the direction of the reflection.
    Spectrum weight;
    ShadingRay shadow_ray(
        m_point,
        Vector3d(sample.m_incoming.get_value()),
        m_time,
        VisibilityFlags::ShadowRay,
        m_shading_point.get_ray().m_depth + 1);
    shadow_ray.m_media = m_shading_point.get_ray().m_media;
    const ShadingPoint& light_shading_point =
        m_shading_context.get_tracer().trace_full(
            m_shading_context,
            sampling_context,
            m_shading_point,
            shadow_ray,
            weight);

    // todo: wouldn't it be more efficient to look the environment up at this point?
    if (!light_shading_point.hit_surface())
        return;

    // Retrieve the material at the intersection point.
    const Material* material = light_shading_point.get_material();
    if (material == nullptr)
        return;
    const Material::RenderData& material_data = material->get_render_data();

    // Retrieve the EDF at the intersection point.
    const EDF* edf = material_data.m_edf;
    if (edf == nullptr)
        return;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Cull the samples on the back side of the lights' shading surface.
    const float cos_on = dot(
        -sample.m_incoming.get_value(),
        Vector3f(light_shading_point.get_shading_normal()));
    if (cos_on <= 0.0f)
        return;

    if (material_data.m_surface_shader_group)
    {
        m_shading_context.execute_osl_emission(
            *material_data.m_surface_shader_group,
            light_shading_point);
    }

    // Evaluate emitted radiance.
    Spectrum edf_value(Spectrum::Illuminance);
    float edf_prob;
    edf->evaluate(
        edf->evaluate_inputs(m_shading_context, light_shading_point),
        Vector3f(light_shading_point.get_geometric_normal()),
        Basis3f(light_shading_point.get_shading_basis()),
        -sample.m_incoming.get_value(),
        edf_value,
        edf_prob);
    if (edf_prob == 0.0f)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double square_distance = square(light_shading_point.get_distance());

    // Don't use this sample if we're closer than the light near start value.
    if (square_distance < square(edf->get_light_near_start()))
        return;

    if (sample.get_probability() != BSDF::DiracDelta)
    {
        if (mis_heuristic != MISNone && square_distance > 0.0)
        {
            // Transform material_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
            const float bsdf_prob_area = sample.get_probability() * cos_on / static_cast<float>(square_distance);

            // Compute the probability density wrt. surface area measure of the light sample.
            const float light_prob_area = m_light_sampler.evaluate_pdf(
                light_shading_point, m_shading_point);

            // Apply the weighting function.
            weight *=
                mis(
                    mis_heuristic,
                    m_bsdf_sample_count * bsdf_prob_area,
                    m_light_sample_count * light_prob_area);
        }

        edf_value *= weight / sample.get_probability();
    }
    else
    {
        edf_value *= weight;
    }

    // Add the contribution of this sample to the illumination.
    madd(radiance, sample.m_value, edf_value);
}

void DirectLightingIntegrator::add_emitting_triangle_sample_contribution(
    SamplingContext&            sampling_context,
    const LightSample&          sample,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance,
    LightPathStream*            light_path_stream) const
{
    const Material* material = sample.m_triangle->m_material;
    const Material::RenderData& material_data = material->get_render_data();
    const EDF* edf = material_data.m_edf;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Compute the incoming direction in world space.
    Vector3d incoming = sample.m_point - m_point;

    // No contribution if the shading point is behind the light.
    double cos_on = dot(-incoming, sample.m_shading_normal);
    if (cos_on <= 0.0)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double square_distance = square_norm(incoming);

    // Don't use this sample if we're closer than the light near start value.
    if (square_distance < square(edf->get_light_near_start()))
        return;

    const double rcp_sample_square_distance = 1.0 / square_distance;
    const double rcp_sample_distance = sqrt(rcp_sample_square_distance);

    // Normalize the incoming direction.
    cos_on *= rcp_sample_distance;
    incoming *= rcp_sample_distance;

    // Probabilistically skip light samples with low maximum contribution.
    float contribution_prob = 1.0f;
    if (m_low_light_threshold > 0.0f)
    {
        // Compute the approximate maximum contribution of this light sample.
        const float max_contribution =
            static_cast<float>(
                cos_on *
                rcp_sample_square_distance *
                edf->get_max_contribution() *
                sample.m_triangle->m_area);

        // Use Russian Roulette to skip this sample if its maximum contribution is low.
        if (max_contribution < m_low_light_threshold)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();

            // Compute the probability of taking the sample's contribution into account.
            contribution_prob = max_contribution / m_low_light_threshold;

            // Russian Roulette.
            if (!pass_rr(contribution_prob, s))
                return;
        }
    }

    // Compute the transmission factor between the light sample and the shading point.
    Spectrum transmission;
    m_shading_context.get_tracer().trace_between_simple(
        m_shading_context,
        sampling_context,
        m_shading_point,
        sample.m_point,
        VisibilityFlags::ShadowRay,
        transmission,
        &m_shading_point.get_ray().m_media);

    // Discard occluded samples.
    if (is_zero(transmission))
        return;

    // Evaluate the BSDF.
    DirectShadingComponents bsdf_value;
    const float bsdf_probability =
        m_bsdf.evaluate(
            m_bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            Vector3f(m_geometric_normal),
            Basis3f(m_shading_basis),
            Vector3f(outgoing.get_value()),
            Vector3f(incoming),
            m_light_sampling_modes,
            bsdf_value);
    assert(bsdf_probability >= 0.0f);
    if (bsdf_probability == 0.0f)
        return;

    // Build a shading point on the light source.
    ShadingPoint light_shading_point;
    sample.make_shading_point(
        light_shading_point,
        sample.m_shading_normal,
        m_shading_context.get_intersector());

    if (material_data.m_surface_shader_group)
    {
        m_shading_context.execute_osl_emission(
            *material_data.m_surface_shader_group,
            light_shading_point);
    }

    // Evaluate the EDF.
    Spectrum edf_value(Spectrum::Illuminance);
    edf->evaluate(
        edf->evaluate_inputs(m_shading_context, light_shading_point),
        Vector3f(sample.m_geometric_normal),
        Basis3f(Vector3f(sample.m_shading_normal)),
        -Vector3f(incoming),
        edf_value);

    const float g = static_cast<float>(cos_on * rcp_sample_square_distance);

    // Apply MIS weighting.
    const float mis_weight =
        mis(
            mis_heuristic,
            m_light_sample_count * sample.m_probability,
            m_bsdf_sample_count * bsdf_probability * g);

    // Add the contribution of this sample to the illumination.
    edf_value *= transmission;
    edf_value *= (mis_weight * g) / (sample.m_probability * contribution_prob);
    madd(radiance, bsdf_value, edf_value);

    // Record light path event.
    if (light_path_stream)
    {
        light_path_stream->sampled_emitting_triangle(
            sample.m_triangle,
            sample.m_point,
            bsdf_value.m_beauty,
            edf_value);
    }
}

void DirectLightingIntegrator::add_non_physical_light_sample_contribution(
    SamplingContext&            sampling_context,
    const LightSample&          sample,
    const Dual3d&               outgoing,
    DirectShadingComponents&    radiance,
    LightPathStream*            light_path_stream) const
{
    const Light* light = sample.m_light;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(light->get_flags() & Light::CastIndirectLight))
        return;

    // Generate a uniform sample in [0,1).
    SamplingContext child_sampling_context = sampling_context.split(2, 1);
    const Vector2d s = child_sampling_context.next2<Vector2d>();

    // Evaluate the light.
    Vector3d emission_position, emission_direction;
    Spectrum light_value(Spectrum::Illuminance);
    float probability;
    light->sample(
        m_shading_context,
        sample.m_light_transform,
        m_point,
        s,
        emission_position,
        emission_direction,
        light_value,
        probability);

    // Compute the incoming direction in world space.
    const Vector3d incoming = -emission_direction;

    // Compute the transmission factor between the light sample and the shading point.
    Spectrum transmission;
    m_shading_context.get_tracer().trace_between_simple(
        m_shading_context,
        sampling_context,
        m_shading_point,
        emission_position,
        VisibilityFlags::ShadowRay,
        transmission,
        &m_shading_point.get_ray().m_media);

    // Discard occluded samples.
    if (is_zero(transmission))
        return;

    // Evaluate the BSDF (or volume).
    DirectShadingComponents bsdf_value;
    const float bsdf_probability =
        m_bsdf.evaluate(
            m_bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            Vector3f(m_geometric_normal),
            Basis3f(m_shading_basis),
            Vector3f(outgoing.get_value()),
            Vector3f(incoming),
            m_light_sampling_modes,
            bsdf_value);
    assert(bsdf_probability >= 0.0f);
    if (bsdf_probability == 0.0f)
        return;

    // Add the contribution of this sample to the illumination.
    const float attenuation = light->compute_distance_attenuation(m_point, emission_position);
    light_value *= transmission;
    light_value *= attenuation / (sample.m_probability * probability);
    madd(radiance, bsdf_value, light_value);

    // Record light path event.
    if (light_path_stream)
    {
        light_path_stream->sampled_non_physical_light(
            light,
            emission_position,
            bsdf_value.m_beauty,
            light_value);
    }
}

}   // namespace renderer
