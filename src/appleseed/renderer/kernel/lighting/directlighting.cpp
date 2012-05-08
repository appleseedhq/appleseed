
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

// Interface header.
#include "directlighting.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/aov/aovcollection.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/math/mis.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

namespace
{
    double mis_balance_wrapper(
        const size_t    n1,
        const size_t    n2,
        const double    q1,
        const double    q2)
    {
        return q1 / (0.5 * (q1 + q2));
    }

    double mis_power2_wrapper(
        const size_t    n1,
        const size_t    n2,
        const double    q1,
        const double    q2)
    {
        return mis_power2(n1 * q1, n2 * q2);
    }
}

DirectLightingIntegrator::DirectLightingIntegrator(
    const ShadingContext&   shading_context,
    const LightSampler&     light_sampler,
    const Vector3d&         point,
    const Vector3d&         geometric_normal,
    const Basis3d&          shading_basis,
    const double            time,
    const Vector3d&         outgoing,
    const BSDF&             bsdf,
    const void*             bsdf_data,
    const size_t            bsdf_sample_count,
    const size_t            light_sample_count,
    const ShadingPoint*     parent_shading_point)
  : m_shading_context(shading_context)
  , m_light_sampler(light_sampler)
  , m_point(point)
  , m_geometric_normal(geometric_normal)
  , m_shading_basis(shading_basis)
  , m_time(time)
  , m_outgoing(outgoing)
  , m_bsdf(bsdf)
  , m_bsdf_data(bsdf_data)
  , m_bsdf_sample_count(bsdf_sample_count)
  , m_light_sample_count(light_sample_count)
  , m_parent_shading_point(parent_shading_point)
  , m_tracer(
        shading_context.get_intersector(),
        shading_context.get_texture_cache())
{
    assert(is_normalized(geometric_normal));
    assert(is_normalized(outgoing));
}

void DirectLightingIntegrator::sample_bsdf(
    SamplingContext&        sampling_context,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    for (size_t i = 0; i < m_bsdf_sample_count; ++i)
    {
        take_single_bsdf_sample(
            sampling_context,
            mis_power2_wrapper,
            radiance,
            aovs);
    }

    if (m_bsdf_sample_count > 1)
    {
        const float rcp_bsdf_sample_count = 1.0f / m_bsdf_sample_count;
        radiance *= rcp_bsdf_sample_count;
        aovs *= rcp_bsdf_sample_count;
    }
}

void DirectLightingIntegrator::sample_lights(
    SamplingContext&        sampling_context,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    sampling_context.split_in_place(3, m_light_sample_count);

    for (size_t i = 0; i < m_light_sample_count; ++i)
    {
        take_single_light_sample(
            sampling_context,
            mis_power2_wrapper,
            radiance,
            aovs);
    }

    if (m_light_sample_count > 1)
    {
        const float rcp_light_sample_count = 1.0f / m_light_sample_count;
        radiance *= rcp_light_sample_count;
        aovs *= rcp_light_sample_count;
    }
}

void DirectLightingIntegrator::sample_bsdf_and_lights(
    SamplingContext&        sampling_context,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    if (m_bsdf_sample_count + m_light_sample_count == 0)
        take_single_bsdf_or_light_sample(sampling_context, radiance, aovs);
    else
    {
        Spectrum radiance_light_sampling;
        sample_bsdf(sampling_context, radiance, aovs);
        sample_lights(sampling_context, radiance_light_sampling, aovs);
        radiance += radiance_light_sampling;
    }
}

void DirectLightingIntegrator::take_single_bsdf_or_light_sample(
    SamplingContext&        sampling_context,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    sampling_context.split_in_place(1, 1);

    if (sampling_context.next_double2() < 0.5)
    {
        sampling_context.split_in_place(3, m_light_sample_count);

        take_single_light_sample(
            sampling_context,
            mis_balance_wrapper,
            radiance,
            aovs);
    }
    else
    {
        take_single_bsdf_sample(
            sampling_context,
            mis_balance_wrapper,
            radiance,
            aovs);
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::take_single_bsdf_sample(
    SamplingContext&        sampling_context,
    WeightingFunction&      weighting_function,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    // Sample the BSDF.
    Vector3d incoming;
    Spectrum bsdf_value;
    double bsdf_prob;
    BSDF::Mode bsdf_mode;
    m_bsdf.sample(
        sampling_context,
        m_bsdf_data,
        false,              // not adjoint
        true,               // multiply by |cos(incoming, normal)|
        m_geometric_normal,
        m_shading_basis,
        m_outgoing,
        incoming,
        bsdf_value,
        bsdf_prob,
        bsdf_mode);

    // Ignore glossy/specular components: they must be handled by the parent.
    // See Physically Based Rendering vol. 1 page 732.
    if (bsdf_mode != BSDF::Diffuse)
        return;

    if (bsdf_prob > 0.0)
        bsdf_value /= static_cast<float>(bsdf_prob);

    // Trace a ray in the direction of the reflection.
    double weight;
    const ShadingPoint& light_shading_point =
        m_tracer.trace(
            sampling_context,
            m_point,
            incoming,
            m_time,
            weight,
            m_parent_shading_point);

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
    InputEvaluator edf_input_evaluator(m_shading_context.get_texture_cache());
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
        return;

    const double square_distance = square(light_shading_point.get_distance());

    if (square_distance > 0.0)
    {
        // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
        const double cos_on = dot(-incoming, light_shading_point.get_shading_normal());
        const double bsdf_point_prob = bsdf_prob * cos_on / square_distance;

        // Compute the probability density wrt. surface area mesure of the light sample.
        const double light_point_prob = m_light_sampler.evaluate_pdf(light_shading_point);

        // Apply multiply importance sampling.
        weight *=
            weighting_function(
                m_bsdf_sample_count,
                m_light_sample_count,
                bsdf_point_prob,
                light_point_prob);
    }

    // Add the contribution of this sample to the illumination.
    edf_value *= static_cast<float>(weight);
    edf_value *= bsdf_value;
    radiance += edf_value;
    aovs.add(edf->get_render_layer_index(), radiance);
}

template <typename WeightingFunction>
void DirectLightingIntegrator::take_single_light_sample(
    SamplingContext&        sampling_context,
    WeightingFunction&      weighting_function,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    const Vector3d s = sampling_context.next_vector2<3>();

    LightSample sample;
    if (!m_light_sampler.sample(s, sample))
        return;

    SamplingContext child_sampling_context(sampling_context);

    if (sample.m_triangle)
    {
        add_emitting_triangle_sample_contribution(
            child_sampling_context,
            sample,
            weighting_function,
            radiance,
            aovs);
    }
    else
    {
        add_light_sample_contribution(
            child_sampling_context,
            sample,
            radiance,
            aovs);
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::add_emitting_triangle_sample_contribution(
    SamplingContext&        sampling_context,
    const LightSample&      sample,
    WeightingFunction&      weighting_function,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    // Compute the incoming direction in world space.
    Vector3d incoming = sample.m_point - m_point;

    // Cull light samples behind the shading surface.
    double cos_in = dot(incoming, m_shading_basis.get_normal());
    if (cos_in <= 0.0)
        return;

    // Cull samples on lights emitting in the wrong direction.
    double cos_on = dot(-incoming, sample.m_shading_normal);
    if (cos_on <= 0.0)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double rcp_sample_square_distance = 1.0 / square_norm(incoming);
    const double rcp_sample_distance = sqrt(rcp_sample_square_distance);

    // Normalize the incoming direction.
    incoming *= rcp_sample_distance;
    cos_in *= rcp_sample_distance;
    cos_on *= rcp_sample_distance;

    // Compute the transmission factor between the light sample and the shading point.
    double transmission;
    if (!check_visibility(sampling_context, sample, transmission))
        return;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    double bsdf_prob;
    if (!m_bsdf.evaluate(
            m_bsdf_data,
            false,          // not adjoint
            true,           // multiply by |cos(incoming, normal)|
            m_geometric_normal,
            m_shading_basis,
            m_outgoing,
            incoming,
            bsdf_value,
            &bsdf_prob))
        return;

    const EDF* edf = sample.m_triangle->m_edf;

    // Evaluate the input values of the EDF.
    InputEvaluator edf_input_evaluator(m_shading_context.get_texture_cache());
    const void* edf_data =
        edf_input_evaluator.evaluate(
            edf->get_inputs(),
            sample.m_input_params);

    // Evaluate the EDF.
    Spectrum edf_value;
    edf->evaluate(
        edf_data,
        sample.m_geometric_normal,
        Basis3d(sample.m_shading_normal),
        -incoming,
        edf_value);

    // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
    const double bsdf_point_prob = bsdf_prob * cos_on * rcp_sample_square_distance;

    // Compute multiple importance sampling weight.
    const double mis_weight =
        weighting_function(
            m_light_sample_count,
            m_bsdf_sample_count,
            sample.m_probability,
            bsdf_point_prob);

    // Add the contribution of this sample to the illumination.
    const double weight = mis_weight * transmission * cos_on * rcp_sample_square_distance / sample.m_probability;
    edf_value *= static_cast<float>(weight);
    edf_value *= bsdf_value;
    radiance += edf_value;
    aovs.add(edf->get_render_layer_index(), radiance);
}

void DirectLightingIntegrator::add_light_sample_contribution(
    SamplingContext&        sampling_context,
    const LightSample&      sample,
    Spectrum&               radiance,
    AOVCollection&          aovs)
{
    // Compute the incoming direction in world space.
    Vector3d incoming = sample.m_point - m_point;

    // Cull light samples behind the shading surface.
    double cos_in = dot(incoming, m_shading_basis.get_normal());
    if (cos_in <= 0.0)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double rcp_sample_square_distance = 1.0 / square_norm(incoming);
    const double rcp_sample_distance = sqrt(rcp_sample_square_distance);

    // Normalize the incoming direction.
    incoming *= rcp_sample_distance;
    cos_in *= rcp_sample_distance;

    // Compute the transmission factor between the light sample and the shading point.
    double transmission;
    if (!check_visibility(sampling_context, sample, transmission))
        return;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    double bsdf_prob;
    if (!m_bsdf.evaluate(
            m_bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            m_geometric_normal,
            m_shading_basis,
            m_outgoing,
            incoming,
            bsdf_value,
            &bsdf_prob))
        return;

    const Light* light = sample.m_light;

    // Evaluate the input values of the light.
    InputEvaluator light_input_evaluator(m_shading_context.get_texture_cache());
    const void* light_data =
        light_input_evaluator.evaluate(
            light->get_inputs(),
            sample.m_input_params);

    // Evaluate the light.
    Spectrum light_value;
    light->evaluate(light_data, -incoming, light_value);

    // Add the contribution of this sample to the illumination.
    const double weight = transmission * rcp_sample_square_distance / sample.m_probability;
    light_value *= static_cast<float>(weight);
    light_value *= bsdf_value;
    radiance += light_value;
    aovs.add(light->get_render_layer_index(), radiance);
}

bool DirectLightingIntegrator::check_visibility(
    SamplingContext&        sampling_context,
    const LightSample&      sample,
    double&                 transmission)
{
    const ShadingPoint& shading_point =
        m_tracer.trace_between(
            sampling_context,
            m_point,
            sample.m_point,
            m_time,
            transmission,
            m_parent_shading_point);

    return !shading_point.hit();
}

}   // namespace renderer
