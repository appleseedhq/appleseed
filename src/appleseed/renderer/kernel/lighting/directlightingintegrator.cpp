
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

// Interface header.
#include "directlightingintegrator.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
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
//   compute_outgoing_radiance_bsdf_sampling
//       take_single_bsdf_sample
//
//   compute_outgoing_radiance_light_sampling
//       add_emitting_triangle_sample_contribution
//       add_non_physical_light_sample_contribution
//
//   compute_outgoing_radiance_light_sampling_low_variance
//       add_emitting_triangle_sample_contribution
//       add_non_physical_light_sample_contribution
//
//   compute_outgoing_radiance_combined_sampling
//       compute_outgoing_radiance_bsdf_sampling
//       compute_outgoing_radiance_light_sampling
//
//   compute_outgoing_radiance_combined_sampling_low_variance
//       compute_outgoing_radiance_bsdf_sampling
//       compute_outgoing_radiance_light_sampling_low_variance
//
//   compute_incoming_radiance
//

DirectLightingIntegrator::DirectLightingIntegrator(
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const ShadingPoint&         shading_point,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const int                   bsdf_sampling_modes,
    const int                   light_sampling_modes,
    const size_t                bsdf_sample_count,
    const size_t                light_sample_count,
    const float                 low_light_threshold,
    const bool                  indirect)
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
  , m_bsdf_sample_count(bsdf_sample_count)
  , m_light_sample_count(light_sample_count)
  , m_low_light_threshold(low_light_threshold)
  , m_indirect(indirect)
{
}

void DirectLightingIntegrator::compute_outgoing_radiance_bsdf_sampling(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    radiance.set(0.0f);

    // No emitting triangles in the scene.
    if (m_light_sampler.get_emitting_triangle_count() == 0)
        return;

    for (size_t i = 0; i < m_bsdf_sample_count; ++i)
    {
        take_single_bsdf_sample(
            sampling_context,
            mis_heuristic,
            outgoing,
            radiance);
    }

    if (m_bsdf_sample_count > 1)
    {
        const float rcp_bsdf_sample_count = 1.0f / m_bsdf_sample_count;
        radiance *= rcp_bsdf_sample_count;
    }
}

void DirectLightingIntegrator::compute_outgoing_radiance_light_sampling(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    radiance.set(0.0f);

    // No light source in the scene.
    if (!m_light_sampler.has_lights_or_emitting_triangles())
        return;

    // There cannot be any contribution for purely specular BSDFs.
    if (m_bsdf.is_purely_specular())
        return;

    sampling_context.split_in_place(3, m_light_sample_count);

    // Add contributions from both emitting triangles and non-physical light sources.
    for (size_t i = 0; i < m_light_sample_count; ++i)
    {
        // Sample both emitting triangles and non-physical light sources.
        LightSample sample;
        m_light_sampler.sample(
            m_time,
            sampling_context.next2<Vector3f>(),
            sample);

        if (sample.m_triangle)
        {
            add_emitting_triangle_sample_contribution(
                sample,
                mis_heuristic,
                outgoing,
                radiance);
        }
        else
        {
            add_non_physical_light_sample_contribution(
                sample,
                outgoing,
                radiance);
        }
    }

    if (m_light_sample_count > 1)
    {
        const float rcp_light_sample_count = 1.0f / m_light_sample_count;
        radiance *= rcp_light_sample_count;
    }
}

void DirectLightingIntegrator::compute_outgoing_radiance_light_sampling_low_variance(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    radiance.set(0.0f);

    // No light source in the scene.
    if (!m_light_sampler.has_lights_or_emitting_triangles())
        return;

    // There cannot be any contribution for purely specular BSDFs.
    if (m_bsdf.is_purely_specular())
        return;

    // Add contributions from emitting triangles only.
    if (m_light_sampler.get_emitting_triangle_count() > 0)
    {
        sampling_context.split_in_place(3, m_light_sample_count);

        for (size_t i = 0; i < m_light_sample_count; ++i)
        {
            // Sample emitting triangles only.
            LightSample sample;
            m_light_sampler.sample_emitting_triangles(
                m_time,
                sampling_context.next2<Vector3f>(),
                sample);

            add_emitting_triangle_sample_contribution(
                sample,
                mis_heuristic,
                outgoing,
                radiance);
        }

        if (m_light_sample_count > 1)
        {
            const float rcp_light_sample_count = 1.0f / m_light_sample_count;
            radiance *= rcp_light_sample_count;
        }
    }

    // Add contributions from non-physical light sources only.
    for (size_t i = 0, e = m_light_sampler.get_non_physical_light_count(); i < e; ++i)
    {
        LightSample sample;
        m_light_sampler.sample_non_physical_light(m_time, i, sample);

        add_non_physical_light_sample_contribution(
            sample,
            outgoing,
            radiance);
    }
}

void DirectLightingIntegrator::compute_outgoing_radiance_combined_sampling(
    SamplingContext&            sampling_context,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    compute_outgoing_radiance_bsdf_sampling(
        sampling_context,
        MISPower2,
        outgoing,
        radiance);

    Spectrum radiance_light_sampling(Spectrum::Illuminance);

    compute_outgoing_radiance_light_sampling(
        sampling_context,
        MISPower2,
        outgoing,
        radiance_light_sampling);

    radiance += radiance_light_sampling;
}

void DirectLightingIntegrator::compute_outgoing_radiance_combined_sampling_low_variance(
    SamplingContext&            sampling_context,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    compute_outgoing_radiance_bsdf_sampling(
        sampling_context,
        MISPower2,
        outgoing,
        radiance);

    Spectrum radiance_light_sampling(Spectrum::Illuminance);

    compute_outgoing_radiance_light_sampling_low_variance(
        sampling_context,
        MISPower2,
        outgoing,
        radiance_light_sampling);

    radiance += radiance_light_sampling;
}

bool DirectLightingIntegrator::compute_incoming_radiance(
    SamplingContext&            sampling_context,
    Vector3d&                   incoming,
    float&                      incoming_prob,
    Spectrum&                   radiance) const
{
    radiance.set(0.0f);

    // No light source in the scene.
    if (!m_light_sampler.has_lights_or_emitting_triangles())
        return false;

    sampling_context.split_in_place(3, 1);

    // Sample both emitting triangles and non-physical light sources.
    LightSample sample;
    m_light_sampler.sample(
        m_time,
        sampling_context.next2<Vector3f>(),
        sample);

    if (sample.m_triangle)
    {
        const Material* material = sample.m_triangle->m_material;
        const Material::RenderData& material_data = material->get_render_data();
        const EDF* edf = material_data.m_edf;

        // No contribution if we are computing indirect lighting but this light does not cast indirect light.
        if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
            return false;

        // Compute the incoming direction in world space.
        incoming = sample.m_point - m_point;

        // No contribution if the shading point is behind the light.
        double cos_on_light = dot(-incoming, sample.m_shading_normal);
        if (cos_on_light <= 0.0)
            return false;

        // Compute the transmission factor between the light sample and the shading point.
        const float transmission =
            m_shading_context.get_tracer().trace_between(
                m_shading_point,
                sample.m_point,
                VisibilityFlags::ShadowRay);

        // Discard occluded samples.
        if (transmission == 0.0f)
            return false;

        // Don't use this sample if we're closer than the light near start value.
        const double square_distance = square_norm(incoming);
        if (square_distance < square(edf->get_light_near_start()))
            return false;

        // Normalize the incoming direction.
        const double rcp_square_distance = 1.0 / square_distance;
        const double rcp_distance = sqrt(rcp_square_distance);
        incoming *= rcp_distance;
        cos_on_light *= rcp_distance;

        // Build a shading point on the light source.
        ShadingPoint light_shading_point;
        sample.make_shading_point(
            light_shading_point,
            sample.m_shading_normal,
            m_shading_context.get_intersector());

        if (material_data.m_shader_group)
        {
            m_shading_context.execute_osl_emission(
                *material_data.m_shader_group,
                light_shading_point);
        }

        // Evaluate the EDF.
        edf->evaluate(
            edf->evaluate_inputs(m_shading_context, light_shading_point),
            Vector3f(sample.m_geometric_normal),
            Basis3f(Vector3f(sample.m_shading_normal)),
            -Vector3f(incoming),
            radiance);

        // Compute probability with respect to solid angle of incoming direction.
        const float g = static_cast<float>(cos_on_light * rcp_square_distance);
        incoming_prob = sample.m_probability / g;

        // Compute and return the incoming radiance.
        radiance *= transmission * g / sample.m_probability;
    }
    else
    {
        const Light* light = sample.m_light;

        // No contribution if we are computing indirect lighting but this light does not cast indirect light.
        if (m_indirect && !(light->get_flags() & Light::CastIndirectLight))
            return false;

        // Evaluate the light.
        Vector3d emission_position, emission_direction;
        light->evaluate(
            m_shading_context,
            sample.m_light_transform,
            m_point,
            emission_position,
            emission_direction,
            radiance);

        // Compute the transmission factor between the light sample and the shading point.
        const float transmission =
            m_shading_context.get_tracer().trace_between(
                m_shading_point,
                emission_position,
                VisibilityFlags::ShadowRay);

        // Discard occluded samples.
        if (transmission == 0.0f)
            return false;

        // Compute the incoming direction in world space.
        incoming = -emission_direction;
        incoming_prob = BSDF::DiracDelta;

        // Compute and return the incoming radiance.
        const float attenuation = light->compute_distance_attenuation(m_point, emission_position);
        radiance *= transmission * attenuation / sample.m_probability;
    }

    return true;
}

void DirectLightingIntegrator::take_single_bsdf_sample(
    SamplingContext&            sampling_context,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    assert(m_light_sampler.get_emitting_triangle_count() > 0);

    // Sample the BSDF.
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    m_bsdf.sample(
        sampling_context,
        m_bsdf_data,
        false,                  // not adjoint
        true,                   // multiply by |cos(incoming, normal)|
        sample);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & sample.m_mode))
        return;

    // Trace a ray in the direction of the reflection.
    float weight;
    const ShadingPoint& light_shading_point =
        m_shading_context.get_tracer().trace(
            m_shading_point,
            Vector3d(sample.m_incoming.get_value()),
            VisibilityFlags::ShadowRay,
            weight);

    // todo: wouldn't it be more efficient to look the environment up at this point?
    if (!light_shading_point.hit())
        return;

    // Retrieve the material at the intersection point.
    const Material* material = light_shading_point.get_material();
    if (material == 0)
        return;
    const Material::RenderData& material_data = material->get_render_data();

    // Retrieve the EDF at the intersection point.
    const EDF* edf = material_data.m_edf;
    if (edf == 0)
        return;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Cull the samples on the back side of the lights' shading surface.
    const float cos_on = dot(-sample.m_incoming.get_value(), Vector3f(light_shading_point.get_shading_normal()));
    if (cos_on <= 0.0f)
        return;

    if (material_data.m_shader_group)
    {
        m_shading_context.execute_osl_emission(
            *material_data.m_shader_group,
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

    if (sample.m_probability != BSDF::DiracDelta)
    {
        if (mis_heuristic != MISNone && square_distance > 0.0)
        {
            // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
            const float bsdf_prob_area = sample.m_probability * cos_on / static_cast<float>(square_distance);

            // Compute the probability density wrt. surface area mesure of the light sample.
            const float light_prob_area = m_light_sampler.evaluate_pdf(light_shading_point);

            // Apply the weighting function.
            weight *=
                mis(
                    mis_heuristic,
                    m_bsdf_sample_count * bsdf_prob_area,
                    m_light_sample_count * light_prob_area);
        }

        edf_value *= weight / sample.m_probability;
    }
    else
    {
        edf_value *= weight;
    }

    // Add the contribution of this sample to the illumination.
    edf_value *= sample.m_value;
    radiance += edf_value;
}

void DirectLightingIntegrator::add_emitting_triangle_sample_contribution(
    const LightSample&          sample,
    const MISHeuristic          mis_heuristic,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    const Material* material = sample.m_triangle->m_material;
    const Material::RenderData& material_data = material->get_render_data();
    const EDF* edf = material_data.m_edf;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Compute the incoming direction in world space.
    Vector3d incoming = sample.m_point - m_point;

    // Cull light samples behind the shading surface if the BSDF is either reflective or transmissive,
    // but not both.
    if (m_bsdf.get_type() != BSDF::AllBSDFTypes)
    {
        double cos_in = dot(incoming, m_shading_basis.get_normal());
        if (m_bsdf.get_type() == BSDF::Transmissive)
            cos_in = -cos_in;
        if (cos_in <= 0.0)
            return;
    }

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
    
    // Compute the approximate contribution.
    const double approximate_contribution = 
        cos_on * 
        rcp_sample_square_distance * 
        edf->get_max_contribution() * 
        sample.m_triangle->m_area;

    // Don't use this sample if the approximate contribution is low.
    if (approximate_contribution < m_low_light_threshold)
        return;

    // Compute the transmission factor between the light sample and the shading point.
    const float transmission =
        m_shading_context.get_tracer().trace_between(
            m_shading_point,
            sample.m_point,
            VisibilityFlags::ShadowRay);

    // Discard occluded samples.
    if (transmission == 0.0f)
        return;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    const float bsdf_prob =
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
    if (bsdf_prob == 0.0f)
        return;

    // Build a shading point on the light source.
    ShadingPoint light_shading_point;
    sample.make_shading_point(
        light_shading_point,
        sample.m_shading_normal,
        m_shading_context.get_intersector());

    if (material_data.m_shader_group)
    {
        m_shading_context.execute_osl_emission(
            *material_data.m_shader_group,
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
    float weight = transmission * g / sample.m_probability;

    // Apply MIS weighting.
    weight *=
        mis(
            mis_heuristic,
            m_light_sample_count * sample.m_probability,
            m_bsdf_sample_count * bsdf_prob * g);

    // Add the contribution of this sample to the illumination.
    edf_value *= weight;
    edf_value *= bsdf_value;
    radiance += edf_value;
}

void DirectLightingIntegrator::add_non_physical_light_sample_contribution(
    const LightSample&          sample,
    const Dual3d&               outgoing,
    Spectrum&                   radiance) const
{
    const Light* light = sample.m_light;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(light->get_flags() & Light::CastIndirectLight))
        return;

    // Evaluate the light.
    Vector3d emission_position, emission_direction;
    Spectrum light_value(Spectrum::Illuminance);
    light->evaluate(
        m_shading_context,
        sample.m_light_transform,
        m_point,
        emission_position,
        emission_direction,
        light_value);

    // Compute the incoming direction in world space.
    const Vector3d incoming = -emission_direction;

    // Cull light samples behind the shading surface if the BSDF is either reflective or transmissive,
    // but not both.
    if (m_bsdf.get_type() != BSDF::AllBSDFTypes)
    {
        double cos_in = dot(incoming, m_shading_basis.get_normal());
        if (m_bsdf.get_type() == BSDF::Transmissive)
            cos_in = -cos_in;
        if (cos_in <= 0.0)
            return;
    }

    // Compute the transmission factor between the light sample and the shading point.
    const float transmission =
        m_shading_context.get_tracer().trace_between(
            m_shading_point,
            emission_position,
            VisibilityFlags::ShadowRay);

    // Discard occluded samples.
    if (transmission == 0.0f)
        return;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    const float bsdf_prob =
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
    if (bsdf_prob == 0.0f)
        return;

    // Add the contribution of this sample to the illumination.
    const float attenuation = light->compute_distance_attenuation(m_point, emission_position);
    const float weight = transmission * attenuation / sample.m_probability;
    light_value *= weight;
    light_value *= bsdf_value;
    radiance += light_value;
}

}   // namespace renderer
