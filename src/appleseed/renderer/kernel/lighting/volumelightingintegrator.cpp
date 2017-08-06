
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

// Interface header.
#include "renderer/kernel/lighting/volumelightingintegrator.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/backwardlightsampler.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/volume/volume.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cmath>

using namespace foundation;

namespace renderer
{
    //
    // VolumeLightingIntegrator class implementation.
    //
    // Call graph:
    //
    //   compute_outgoing_radiance_material_sampling
    //       take_single_material_sample
    //
    //   compute_outgoing_radiance_light_sampling_low_variance
    //       add_emitting_triangle_sample_contribution
    //       add_non_physical_light_sample_contribution
    //
    //   compute_outgoing_radiance_combined_sampling_low_variance
    //       compute_outgoing_radiance_material_sampling
    //       compute_outgoing_radiance_light_sampling_low_variance
    //

    VolumeLightingIntegrator::VolumeLightingIntegrator(
        const ShadingContext&           shading_context,
        const BackwardLightSampler&     light_sampler,
        const Volume&                   volume,
        const ShadingRay&               volume_ray,
        const void*                     volume_data,
        const ShadingRay::Time&         time,
        const int                       light_sampling_modes,
        const size_t                    phasefunction_sample_count,
        const size_t                    equiangular_sample_count,
        const size_t                    exponential_sample_count,
        const float                     low_light_threshold,
        const bool                      indirect)
        : m_shading_context(shading_context)
        , m_light_sampler(light_sampler)
        , m_volume(volume)
        , m_volume_ray(volume_ray)
        , m_volume_data(volume_data)
        , m_time(time)
        , m_light_sampling_modes(light_sampling_modes)
        , m_phasefunction_sample_count(phasefunction_sample_count)
        , m_equiangular_sample_count(equiangular_sample_count)
        , m_exponential_sample_count(exponential_sample_count)
        , m_low_light_threshold(low_light_threshold)
        , m_indirect(indirect)
    {
        precompute_mis_weights();
    }

    size_t VolumeLightingIntegrator::get_effective_equiangular_sample_count() const
    {
        return
            m_equiangular_sample_count +
            m_light_sampler.get_non_physical_light_count();
    }

    void VolumeLightingIntegrator::precompute_mis_weights()
    {
        // Get full ray transmission.
        Spectrum ray_transmission;
        m_volume.evaluate_transmission(
            m_volume_data, m_volume_ray, ray_transmission);
        m_channel_count = ray_transmission.size();
        if (ray_transmission.size() != m_channel_count)
            Spectrum::upgrade(ray_transmission, ray_transmission);

        const Spectrum& scattering_coef = m_volume.scattering_coefficient(
            m_volume_data, m_volume_ray);

        Spectrum extinction_coef = m_volume.extinction_coefficient(
            m_volume_data, m_volume_ray);
        if (extinction_coef.size() != m_channel_count)
            Spectrum::upgrade(extinction_coef, extinction_coef);

        // Precompute MIS weights.
        // MIS terms are:
        //  - scattering albedo,
        //  - one minus total ray transmittance (because we sample distance on segment),
        //  - throughput of the entire path up to the sampled point.
        // Reference: "Practical and Controllable Subsurface Scattering
        // for Production Path Tracing", p. 1 [ACM 2016 Article].
        if (m_channel_count == scattering_coef.size())
            m_precomputed_mis_weights = scattering_coef;
        else
            Spectrum::upgrade(scattering_coef, m_precomputed_mis_weights);
        for (size_t i = 0; i < m_channel_count; ++i)
        {
            const float density = 1.0f - ray_transmission[i];
            m_precomputed_mis_weights[i] *= density;
            if (extinction_coef[i] > 1.0e-6f)
                m_precomputed_mis_weights[i] /= extinction_coef[i];
        }
    }

    struct EquiangularSampler
    {
      public:
        EquiangularSampler(
            const LightSample&          light_sample,
            const ShadingRay&           volume_ray)
        {
            const Vector3d light_dir = light_sample.m_point - volume_ray.m_org;

            m_origin_to_center = dot(light_dir, volume_ray.m_dir);

            m_center_to_light =
                sqrt(square_norm(light_dir) - m_origin_to_center * m_origin_to_center);

            m_near_angle = std::atan2(-m_origin_to_center, m_center_to_light);
            m_far_angle =
                (volume_ray.m_tmax == std::numeric_limits<double>::max()) ? HalfPi<double>() :
                std::atan2(volume_ray.get_length() - m_origin_to_center, m_center_to_light);
        }

        float sample(SamplingContext& sampling_context) const
        {
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next2<double>();

            return static_cast<float>(m_origin_to_center +
                sample_equiangular_distribution(s, m_near_angle, m_far_angle, m_center_to_light));
        }

        float evaluate(const float distance_sample) const
        {
            return static_cast<float>(equiangular_distribution_pdf(
                static_cast<double>(distance_sample), m_near_angle, m_far_angle, m_center_to_light));
        }

      private:
        double          m_origin_to_center;
        double          m_center_to_light;
        double          m_near_angle;
        double          m_far_angle;
    };

    float VolumeLightingIntegrator::take_exponential_sample(
        SamplingContext&    sampling_context,
        const ShadingRay&   volume_ray,
        const float         extinction) const
    {
        sampling_context.split_in_place(1, 1);

        if (volume_ray.m_tmax == std::numeric_limits<ShadingRay::ValueType>().max())
            return sample_exponential_distribution(
                sampling_context.next2<float>(), extinction);
        else
        {
            const float ray_length = static_cast<float>(volume_ray.get_length());
            return sample_exponential_distribution_on_segment(
                sampling_context.next2<float>(), extinction, 0.0f, ray_length);
        }
    }

    float VolumeLightingIntegrator::evaluate_exponential_sample(
        const float         distance,
        const ShadingRay&   volume_ray,
        const float         extinction) const
    {
        if (volume_ray.m_tmax == std::numeric_limits<ShadingRay::ValueType>().max())
            return exponential_distribution_pdf(distance, extinction);
        else
        {
            const float ray_length = static_cast<float>(volume_ray.get_length());
            return exponential_distribution_on_segment_pdf(
                distance, extinction, 0.0f, ray_length);
        }
    }

    void VolumeLightingIntegrator::add_single_light_sample_contribution_equiangular(
        const LightSample&          light_sample,
        const Spectrum&             extinction_coef,
        SamplingContext&            sampling_context,
        const MISHeuristic          mis_heuristic,
        Spectrum&                   radiance) const
    {
        // Take one equiangular sample and evaluate its pdf value.
        const EquiangularSampler equiangular_distance_sampler(light_sample, m_volume_ray);
        const float equiangular_sample = equiangular_distance_sampler.sample(sampling_context);
        const float equiangular_prob = equiangular_distance_sampler.evaluate(equiangular_sample);

        // Evaluate pdf using exponential estimator as well (for MIS).
        // Since we sample spectral channels uniformly at random,
        // the pdf value of this estimator is the mean value of per-channel pdfs.
        float exponential_prob = 0.0f;
        for (size_t i = 0; i < extinction_coef.size(); ++i)
        {
            exponential_prob += evaluate_exponential_sample(
                equiangular_sample, m_volume_ray, extinction_coef[i]);
        }
        exponential_prob /= extinction_coef.size();

        // Calculate MIS weight for distance sampling.
        const float mis_weight = mis(
            mis_heuristic,
            get_effective_equiangular_sample_count() * equiangular_prob,
            m_exponential_sample_count * exponential_prob);

        if (light_sample.m_triangle)
        {
            add_emitting_triangle_sample_contribution(
                sampling_context,
                light_sample,
                equiangular_sample,
                mis_heuristic,
                radiance,
                mis_weight);
        }
        else
        {
            add_non_physical_light_sample_contribution(
                sampling_context,
                light_sample,
                equiangular_sample,
                radiance,
                mis_weight);
        }
    }

    void VolumeLightingIntegrator::add_single_light_sample_contribution_exponential(
        const LightSample&          light_sample,
        const Spectrum&             extinction_coef,
        SamplingContext&            sampling_context,
        const MISHeuristic          mis_heuristic,
        Spectrum&                   radiance) const
    {
        // Sample channel uniformly at random.
        sampling_context.split_in_place(1, 1);
        const float s = sampling_context.next2<float>();
        const size_t channel = truncate<size_t>(s * m_channel_count);
        if (m_precomputed_mis_weights[channel] == 0.0f)
            return;

        // Take one exponential sample and evaluate its pdf value.
        const EquiangularSampler equiangular_distance_sampler(light_sample, m_volume_ray);
        const float exponential_sample =
            take_exponential_sample(sampling_context, m_volume_ray, extinction_coef[channel]);
        const float exponential_prob =
            evaluate_exponential_sample(exponential_sample, m_volume_ray, extinction_coef[channel]);

        // Evaluate pdf using equiangular estimator as well (for MIS).
        const float equiangular_prob = equiangular_distance_sampler.evaluate(exponential_sample);

        // Calculate MIS weight for spectral channel sampling (balance heuristic).
        // One-sample estimator is used (Veach: 9.2.4 eq. 9.15).
        Spectrum transmission;
        m_volume.evaluate_transmission(
            m_volume_data, m_volume_ray, exponential_sample, transmission);
        if (transmission.size() != m_channel_count)
            Spectrum::upgrade(transmission, transmission);
        float mis_weights_sum = 0.0f;
        for (size_t i = 0; i < m_channel_count; ++i)
            mis_weights_sum += m_precomputed_mis_weights[i] * transmission[i];
        const float mis_weight_channel =
            m_channel_count * m_precomputed_mis_weights[channel] *
            transmission[channel] / mis_weights_sum;

        // Calculate MIS weight for distance sampling.
        const float mis_weight_distance = mis(
            mis_heuristic,
            m_exponential_sample_count * exponential_prob,
            get_effective_equiangular_sample_count() * equiangular_prob);

        if (light_sample.m_triangle)
        {
            add_emitting_triangle_sample_contribution(
                sampling_context,
                light_sample,
                exponential_sample,
                mis_heuristic,
                radiance,
                mis_weight_distance * mis_weight_channel);
        }
        else
        {
            add_non_physical_light_sample_contribution(
                sampling_context,
                light_sample,
                exponential_sample,
                radiance,
                mis_weight_distance * mis_weight_channel);
        }
    }

    void VolumeLightingIntegrator::compute_radiance(
        SamplingContext&            sampling_context,
        const MISHeuristic          mis_heuristic,
        Spectrum&                   radiance) const
    {
        radiance.set(0.0f);

        // No light source in the scene.
        if (!m_light_sampler.has_lights())
            return;

        Spectrum extinction_coef = m_volume.extinction_coefficient(
            m_volume_data, m_volume_ray);
        if (extinction_coef.size() != m_channel_count)
            Spectrum::upgrade(extinction_coef, extinction_coef);

        if (m_equiangular_sample_count > 0)
        {
            // Add contributions from non-physical light sources that don't belong to the lightset.
            for (size_t i = 0, e = m_light_sampler.get_non_physical_light_count(); i < e; ++i)
            {
                LightSample light_sample;
                m_light_sampler.sample_non_physical_light(m_time, i, light_sample);
                add_single_light_sample_contribution_equiangular(
                    light_sample,
                    extinction_coef,
                    sampling_context,
                    MISPower2,
                    radiance);
            }
        }

        // Add contributions from the light set.
        if (m_light_sampler.has_lightset())
        {
            Spectrum lightset_radiance(Spectrum::Illuminance);

            const size_t total_sample_count =
                m_equiangular_sample_count + m_exponential_sample_count;

            sampling_context.split_in_place(3, total_sample_count);

            for (size_t i = 0, e = m_equiangular_sample_count; i < e; ++i)
            {
                // Sample the light set.
                LightSample light_sample;
                ShadingPoint fake_point;
                fake_point.clear();
                m_light_sampler.sample_lightset(
                    m_time,
                    sampling_context.next2<Vector3f>(),
                    fake_point,
                    light_sample);

                // Add the contribution of the chosen light.
                add_single_light_sample_contribution_equiangular(
                    light_sample,
                    extinction_coef,
                    sampling_context,
                    MISPower2,
                    radiance);
            }

            for (size_t i = 0, e = m_exponential_sample_count; i < e; ++i)
            {
                // Sample the light set.
                LightSample light_sample;
                ShadingPoint fake_point;
                fake_point.clear();
                m_light_sampler.sample_lightset(
                    m_time,
                    sampling_context.next2<Vector3f>(),
                    fake_point,
                    light_sample);

                // Add the contribution of the chosen light.
                add_single_light_sample_contribution_exponential(
                    light_sample,
                    extinction_coef,
                    sampling_context,
                    MISPower2,
                    radiance);
            }

            if (total_sample_count > 1)
                lightset_radiance /= static_cast<float>(total_sample_count);

            radiance += lightset_radiance;
        }
    }

    void VolumeLightingIntegrator::add_non_physical_light_sample_contribution(
        SamplingContext&            sampling_context,
        const LightSample&          light_sample,
        const float                 distance_sample,
        Spectrum&                   radiance,
        const float                 weight) const
    {
        const Light* light = light_sample.m_light;

        const Vector3d volume_point = m_volume_ray.point_at(distance_sample);

        // No contribution if we are computing indirect lighting but this light does not cast indirect light.
        if (m_indirect && !(light->get_flags() & Light::CastIndirectLight))
            return;

        // Generate a uniform sample in [0,1).
        sampling_context.split_in_place(2, 1);
        const Vector2d s = sampling_context.next2<Vector2d>();

        // Evaluate the light.
        Vector3d emission_position, emission_direction;
        Spectrum light_value(Spectrum::Illuminance);
        float probability;
        light->sample(
            m_shading_context,
            light_sample.m_light_transform,
            volume_point,
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
            volume_point,
            emission_position,
            m_volume_ray,
            VisibilityFlags::ShadowRay,
            transmission);

        // Discard occluded samples.
        if (max_value(transmission) == 0.0f)
            return;

        // Evaluate the phase function.
        const float phase_function_pdf =
            m_volume.evaluate(
                m_volume_data,
                m_volume_ray,
                distance_sample,
                Vector3f(incoming));
        if (phase_function_pdf == 0.0f)
            return;

        Spectrum value;
        m_volume.scattering_coefficient(
            m_volume_data, m_volume_ray, distance_sample, value);
        value *= phase_function_pdf;

        // Add the contribution of this sample to the illumination.
        const float attenuation = light->compute_distance_attenuation(
            m_volume_ray.point_at(distance_sample), emission_position);
        light_value *= transmission;
        light_value *= attenuation / (light_sample.m_probability * probability);
        value *= light_value;
        radiance += value;
    }

    void VolumeLightingIntegrator::add_emitting_triangle_sample_contribution(
        SamplingContext&            sampling_context,
        const LightSample&          light_sample,
        const float                 distance_sample,
        const MISHeuristic          mis_heuristic,
        Spectrum&                   radiance,
        const float                 weight) const
    {
        const Vector3d volume_point = m_volume_ray.point_at(distance_sample);

        const Material* material = light_sample.m_triangle->m_material;
        const Material::RenderData& material_data = material->get_render_data();
        const EDF* edf = material_data.m_edf;

        // No contribution if we are computing indirect lighting but this light does not cast indirect light.
        if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
            return;

        // Compute the incoming direction in world space.
        Vector3d incoming = light_sample.m_point - volume_point;

        // No contribution if the shading point is behind the light.
        double cos_on = dot(-incoming, light_sample.m_shading_normal);
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
                    light_sample.m_triangle->m_area);

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
            volume_point,
            light_sample.m_point,
            m_volume_ray,
            VisibilityFlags::ShadowRay,
            transmission);

        // Discard occluded samples.
        if (max_value(transmission) == 0.0f)
            return;

        // Evaluate the BSDF (or volume).
        const float phase_function_pdf =
            m_volume.evaluate(
                m_volume_data,
                m_volume_ray,
                distance_sample,
                Vector3f(incoming));
        if (phase_function_pdf == 0.0f)
            return;

        Spectrum value;
        m_volume.scattering_coefficient(
            m_volume_data, m_volume_ray, distance_sample, value);
        value *= phase_function_pdf;

        // Build a shading point on the light source.
        ShadingPoint light_shading_point;
        light_sample.make_shading_point(
            light_shading_point,
            light_sample.m_shading_normal,
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
            Vector3f(light_sample.m_geometric_normal),
            Basis3f(Vector3f(light_sample.m_shading_normal)),
            -Vector3f(incoming),
            edf_value);

        const float g = static_cast<float>(cos_on * rcp_sample_square_distance);

        // Apply MIS weighting.
        const size_t total_sample_count = m_equiangular_sample_count + m_exponential_sample_count;
        const float mis_weight =
            mis(
                mis_heuristic,
                total_sample_count * light_sample.m_probability,
                m_phasefunction_sample_count * phase_function_pdf * g);

        // Add the contribution of this sample to the illumination.
        edf_value *= transmission;
        edf_value *= (mis_weight * g) / (light_sample.m_probability * contribution_prob);
        value *= edf_value;
        radiance += value;
    }
}   // namespace renderer
