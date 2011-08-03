
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "directlighting.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/math/mis.h"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace std;

namespace renderer
{

void compute_direct_lighting_bsdf_sampling(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const size_t                bsdf_sample_count,
    const size_t                light_sample_count,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    radiance.set(0.0f);

    for (size_t i = 0; i < bsdf_sample_count; ++i)
    {
        // Sample the BSDF.
        Vector3d incoming;
        Spectrum bsdf_value;
        double bsdf_prob;
        BSDF::Mode bsdf_mode;
        bsdf.sample(
            sampling_context,
            bsdf_data,
            false,              // adjoint
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            bsdf_value,
            bsdf_prob,
            bsdf_mode);

        // Handle absorption.
        if (bsdf_mode == BSDF::None)
            continue;

        // Ignore specular components: they must be handled by the parent.
        // See Physically Based Rendering vol. 1 page 732.
        if (bsdf_mode == BSDF::Specular)
            continue;

        // Trace a ray in the direction of the reflection.
        Tracer tracer(
            shading_context.get_intersector(),
            shading_context.get_texture_cache(),
            sampling_context);
        double weight;
        const ShadingPoint& light_shading_point =
            tracer.trace(
                point,
                incoming,
                weight,
                parent_shading_point);

        // todo: wouldn't it be more efficient to look the environment up at this point?
        if (!light_shading_point.hit())
            continue;

        // Retrieve the material at the intersection point.
        const Material* material = light_shading_point.get_material();
        if (material == 0)
            continue;

        // Retrieve the EDF at the intersection point.
        const EDF* edf = material->get_edf();
        if (edf == 0)
            continue;

        // Evaluate the input values of the EDF.
        InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
        const void* edf_data =
            edf_input_evaluator.evaluate(
                edf->get_inputs(),
                light_shading_point.get_input_params());

        // Evaluate emitted radiance.
        Spectrum edf_value;
        double edf_prob;
        edf->evaluate(
            edf_data,
            light_shading_point.get_geometric_normal(),
            light_shading_point.get_shading_basis(),
            -incoming,
            edf_value,
            edf_prob);

        if (edf_prob == 0.0)
            continue;

        // Multiple importance sampling.
        const double square_distance = square(light_shading_point.get_distance());
        if (square_distance > 0.0)
        {
            // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
            const double bsdf_point_prob =
                  bsdf_prob
                * dot(-incoming, light_shading_point.get_shading_normal())
                / square_distance;

            // Compute the probability density wrt. surface area of choosing this point
            // by sampling the light sources.
            const double light_point_prob = light_sampler.evaluate_pdf(light_shading_point);

            // Apply MIS.
            weight *=
                mis_power2(
                    bsdf_sample_count * bsdf_point_prob,
                    light_sample_count * light_point_prob);
        }

        // Add the contribution of this sample to the illumination.
        edf_value *= static_cast<float>(weight);
        edf_value *= bsdf_value;
        radiance += edf_value;
    }

    if (bsdf_sample_count > 1)
        radiance /= static_cast<float>(bsdf_sample_count);
}

void compute_direct_lighting_light_sampling(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const size_t                bsdf_sample_count,
    const size_t                light_sample_count,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    radiance.set(0.0f);

    sampling_context = sampling_context.split(3, light_sample_count);

    for (size_t i = 0; i < light_sample_count; ++i)
    {
        // Generate a uniform sample in [0,1)^3.
        const Vector3d s = sampling_context.next_vector2<3>();

        // Sample the light sources.
        LightSample sample;
        if (!light_sampler.sample(s, sample))
            continue;
        assert(sample.m_edf);

        // Compute the incoming direction in world space.
        Vector3d incoming = sample.m_input_params.m_point - point;

        // Cull light samples behind the shading surface.
        double cos_in = dot(incoming, shading_basis.get_normal());
        if (cos_in <= 0.0)
            continue;

        // Cull samples on lights emitting in the wrong direction.
        double cos_on = dot(-incoming, sample.m_input_params.m_shading_normal);
        if (cos_on <= 0.0)
            continue;

        // Compute the square distance between the light sample and the shading point.
        const double sample_square_distance = square_norm(incoming);
        const double rcp_sample_square_distance = 1.0 / sample_square_distance;
        const double rcp_sample_distance = sqrt(rcp_sample_square_distance);

        // Normalize the incoming direction.
        incoming *= rcp_sample_distance;
        cos_in *= rcp_sample_distance;
        cos_on *= rcp_sample_distance;

        // Compute the transmission factor between the light sample and the shading point.
        Tracer tracer(
            shading_context.get_intersector(),
            shading_context.get_texture_cache(),
            sampling_context);
        double transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                point,
                sample.m_input_params.m_point,
                transmission,
                parent_shading_point);

        // Discard occluded samples.
        if (shading_point.hit())
            continue;

        // Evaluate the BSDF.
        Spectrum bsdf_value;
        double bsdf_prob;
        const bool bsdf_defined =
            bsdf.evaluate(
                bsdf_data,
                false,              // adjoint
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                bsdf_value,
                &bsdf_prob);
        if (!bsdf_defined)
            continue;

        // Evaluate the input values of the EDF.
        InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
        const void* edf_data =
            edf_input_evaluator.evaluate(
                sample.m_edf->get_inputs(),
                sample.m_input_params);

        // Evaluate the EDF.
        Spectrum edf_value;
        double edf_prob;
        sample.m_edf->evaluate(
            edf_data,
            sample.m_input_params.m_geometric_normal,
            Basis3d(sample.m_input_params.m_shading_normal),
            -incoming,
            edf_value,
            edf_prob);

        // Compute the geometric term. To keep the estimator unbiased, we don't
        // clamp the geometric term g if it is too small, and in particular we
        // allow it to be exactly zero, which will result in a variance spike.
        const double g = cos_on * rcp_sample_square_distance;
        assert(g >= 0.0);

        // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
        const double bsdf_point_prob = bsdf_prob * cos_on * rcp_sample_square_distance;

        // Compute MIS weight.
        const double mis_weight =
            mis_power2(
                light_sample_count * sample.m_probability,
                bsdf_sample_count * bsdf_point_prob);

        // Compute the contribution of this sample to the illumination.
        assert(sample.m_probability > 0.0);
        const double weight = transmission * g / sample.m_probability * mis_weight;
        edf_value *= static_cast<float>(weight);
        edf_value *= bsdf_value;

        // Add the contribution of this sample to the illumination.
        radiance += edf_value;
    }

    if (light_sample_count > 1)
        radiance /= static_cast<float>(light_sample_count);
}

void compute_direct_lighting_single_sample(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    radiance.set(0.0f);

    // Generate a uniform sample in [0,1).
    sampling_context = sampling_context.split(1, 1);
    const double s = sampling_context.next_double2();

    if (s < 0.5)
    {
        // Sample the BSDF.
        Vector3d incoming;
        Spectrum bsdf_value;
        double bsdf_prob;
        BSDF::Mode bsdf_mode;
        bsdf.sample(
            sampling_context,
            bsdf_data,
            false,              // adjoint
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            bsdf_value,
            bsdf_prob,
            bsdf_mode);

        // Handle absorption.
        if (bsdf_mode == BSDF::None)
            return;

        // Ignore specular components: they must be handled by the parent.
        // See Physically Based Rendering vol. 1 page 732.
        if (bsdf_mode == BSDF::Specular)
            return;

        // Trace a ray in the direction of the reflection.
        Tracer tracer(
            shading_context.get_intersector(),
            shading_context.get_texture_cache(),
            sampling_context);
        double weight;
        const ShadingPoint& light_shading_point =
            tracer.trace(
                point,
                incoming,
                weight,
                parent_shading_point);

        // todo: wouldn't it be more efficient to look the environment up at this point?
        if (!light_shading_point.hit())
            return;

        // Retrieve the material at the intersection point.
        const Material* material = light_shading_point.get_material();
        if (material == 0)
            return;

        // Retrieve the EDF at the intersection point.
        const EDF* edf = material->get_edf();
        if (edf == 0)
            return;

        // Evaluate the input values of the EDF.
        InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
        const void* edf_data =
            edf_input_evaluator.evaluate(
                edf->get_inputs(),
                light_shading_point.get_input_params());

        // Evaluate emitted radiance.
        Spectrum edf_value;
        double edf_prob;
        edf->evaluate(
            edf_data,
            light_shading_point.get_geometric_normal(),
            light_shading_point.get_shading_basis(),
            -incoming,
            edf_value,
            edf_prob);

        if (edf_prob == 0)
            return;

        // Multiple importance sampling.
        const double square_distance = square(light_shading_point.get_distance());
        if (square_distance > 0.0)
        {
            // Compute the probability density wrt. solid angle of choosing
            // this point by sampling the light sources.
            const double light_dir_prob =
                  light_sampler.evaluate_pdf(light_shading_point)
                / dot(-incoming, light_shading_point.get_shading_normal())
                * square_distance;

            // Apply the MIS balance heuristic.
            weight *= bsdf_prob;                            // cancel division by PDF in BSDF::sample()
            weight /= 0.5 * (bsdf_prob + light_dir_prob);   // divide by PDF computed using MIS
        }

        // Add the contribution of this sample to the illumination.
        edf_value *= static_cast<float>(weight);
        edf_value *= bsdf_value;
        radiance += edf_value;
    }
    else
    {
        // Generate a uniform sample in [0,1)^3.
        sampling_context = sampling_context.split(3, 1);
        const Vector3d s = sampling_context.next_vector2<3>();

        // Sample the light sources.
        LightSample sample;
        if (!light_sampler.sample(s, sample))
            return;
        assert(sample.m_edf);

        // Compute the incoming direction in world space.
        Vector3d incoming = sample.m_input_params.m_point - point;

        // Cull light samples behind the shading surface.
        double cos_in = dot(incoming, shading_basis.get_normal());
        if (cos_in <= 0.0)
            return;

        // Cull samples on lights emitting in the wrong direction.
        double cos_on = dot(-incoming, sample.m_input_params.m_shading_normal);
        if (cos_on <= 0.0)
            return;

        // Compute the square distance between the light sample and the shading point.
        const double sample_square_distance = square_norm(incoming);
        const double rcp_sample_square_distance = 1.0 / sample_square_distance;
        const double rcp_sample_distance = sqrt(rcp_sample_square_distance);

        // Normalize the incoming direction.
        incoming *= rcp_sample_distance;
        cos_in *= rcp_sample_distance;
        cos_on *= rcp_sample_distance;

        // Compute the transmission factor between the light sample and the shading point.
        Tracer tracer(
            shading_context.get_intersector(),
            shading_context.get_texture_cache(),
            sampling_context);
        double transmission;
        const ShadingPoint& shading_point =
            tracer.trace_between(
                point,
                sample.m_input_params.m_point,
                transmission,
                parent_shading_point);

        // Discard occluded samples.
        if (shading_point.hit())
            return;

        // Evaluate the BSDF.
        Spectrum bsdf_value;
        double bsdf_prob;
        const bool bsdf_defined =
            bsdf.evaluate(
                bsdf_data,
                false,              // adjoint
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                bsdf_value,
                &bsdf_prob);
        if (!bsdf_defined)
            return;

        // Evaluate the input values of the EDF.
        InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
        const void* edf_data =
            edf_input_evaluator.evaluate(
                sample.m_edf->get_inputs(),
                sample.m_input_params);

        // Evaluate the EDF.
        Spectrum edf_value;
        double edf_prob;
        sample.m_edf->evaluate(
            edf_data,
            sample.m_input_params.m_geometric_normal,
            Basis3d(sample.m_input_params.m_shading_normal),
            -incoming,
            edf_value,
            edf_prob);

        // Compute the geometric term. To keep the estimator unbiased, we don't
        // clamp the geometric term g if it is too small, and in particular we
        // allow it to be exactly zero, which will result in a variance spike.
        const double g = cos_on * rcp_sample_square_distance;
        assert(g >= 0.0);

        // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
        const double bsdf_point_prob = bsdf_prob * cos_on * rcp_sample_square_distance;

        // Compute the combined PDF using MIS balance heuristic.
        const double mis_pdf = 0.5 * (sample.m_probability + bsdf_point_prob);

        // Add the contribution of this sample to the illumination.
        edf_value *= static_cast<float>(transmission * g / mis_pdf);
        edf_value *= bsdf_value;
        radiance += edf_value;
    }
}

void compute_direct_lighting(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const LightSampler&         light_sampler,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const size_t                bsdf_sample_count,
    const size_t                light_sample_count,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(geometric_normal));
    assert(is_normalized(outgoing));

    if (bsdf_sample_count == 0 && light_sample_count == 0)
    {
        // Compute direct lighting using a single BSDF/light sample and MIS.
        compute_direct_lighting_single_sample(
            sampling_context,
            shading_context,
            light_sampler,
            point,
            geometric_normal,
            shading_basis,
            outgoing,
            bsdf,
            bsdf_data,
            radiance,
            parent_shading_point);
    }
    else
    {
        // Compute direct lighting by sampling the BSDF.
        compute_direct_lighting_bsdf_sampling(
            sampling_context,
            shading_context,
            light_sampler,
            point,
            geometric_normal,
            shading_basis,
            outgoing,
            bsdf,
            bsdf_data,
            bsdf_sample_count,
            light_sample_count,
            radiance,
            parent_shading_point);

        // Compute direct lighting by sampling the lights.
        Spectrum radiance_light_sampling;
        compute_direct_lighting_light_sampling(
            sampling_context,
            shading_context,
            light_sampler,
            point,
            geometric_normal,
            shading_basis,
            outgoing,
            bsdf,
            bsdf_data,
            bsdf_sample_count,
            light_sample_count,
            radiance_light_sampling,
            parent_shading_point);
        radiance += radiance_light_sampling;
    }
}

}   // namespace renderer
