
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "imagebasedlighting.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/mis.h"
#include "foundation/math/sampling/mappings.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

void compute_ibl(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const ShadingPoint&     shading_point,
    const Dual3d&           outgoing,
    const BSDF&             bsdf,
    const void*             bsdf_data,
    const int               bsdf_sampling_modes,
    const int               env_sampling_modes,
    const size_t            bsdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    // Compute IBL by sampling the BSDF.
    compute_ibl_bsdf_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        shading_point,
        outgoing,
        bsdf,
        bsdf_data,
        bsdf_sampling_modes,
        bsdf_sample_count,
        env_sample_count,
        radiance);

    // Compute IBL by sampling the environment.
    Spectrum radiance_env_sampling;
    compute_ibl_environment_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        shading_point,
        outgoing,
        bsdf,
        bsdf_data,
        env_sampling_modes,
        bsdf_sample_count,
        env_sample_count,
        radiance_env_sampling);
    radiance += radiance_env_sampling;
}

void compute_ibl(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const BSSRDF&           bssrdf,
    const void*             bssrdf_data,
    const ShadingPoint&     incoming_point,
    const ShadingPoint&     outgoing_point,
    const Dual3d&           outgoing,
    const size_t            bssrdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    // Compute IBL by sampling the BSSRDF.
    compute_ibl_bssrdf_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        bssrdf,
        bssrdf_data,
        incoming_point,
        outgoing_point,
        outgoing,
        bssrdf_sample_count,
        env_sample_count,
        radiance);

    // Compute IBL by sampling the environment.
    Spectrum radiance_env_sampling;
    compute_ibl_environment_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        bssrdf,
        bssrdf_data,
        incoming_point,
        outgoing_point,
        outgoing,
        bssrdf_sample_count,
        env_sample_count,
        radiance_env_sampling);
    radiance += radiance_env_sampling;
}

void compute_ibl_bsdf_sampling(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const ShadingPoint&     shading_point,
    const Dual3d&           outgoing,
    const BSDF&             bsdf,
    const void*             bsdf_data,
    const int               bsdf_sampling_modes,
    const size_t            bsdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    radiance.set(0.0f);

    for (size_t i = 0; i < bsdf_sample_count; ++i)
    {
        // Sample the BSDF.
        // todo: rendering will be incorrect if the BSDF value returned by the sample() method
        // includes the contribution of a specular component since these are explicitly rejected
        // afterward. We need a mechanism to indicate that we want the contribution of some of
        // the components only.
        BSDFSample sample(
            shading_point,
            outgoing);
        bsdf.sample(
            sampling_context,
            bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            sample);

        // Filter scattering modes.
        if (!(bsdf_sampling_modes & sample.m_mode))
            continue;

        // Discard occluded samples.
        const double transmission =
            shading_context.get_tracer().trace(
                shading_point,
                sample.m_incoming.get_value(),
                VisibilityFlags::ShadowRay);
        if (transmission == 0.0)
            continue;

        // Evaluate the environment's EDF.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        Spectrum env_value;
        double env_prob;
        environment_edf.evaluate(
            shading_context,
            input_evaluator,
            sample.m_incoming.get_value(),
            env_value,
            env_prob);

        // Apply all weights, including MIS weight.
        if (sample.m_mode == ScatteringMode::Specular)
            env_value *= static_cast<float>(transmission);
        else
        {
            const double mis_weight =
                mis_power2(
                    bsdf_sample_count * sample.m_probability,
                    env_sample_count * env_prob);
            env_value *= static_cast<float>(transmission / sample.m_probability * mis_weight);
        }

        // Add the contribution of this sample to the illumination.
        env_value *= sample.m_value;
        radiance += env_value;
    }

    if (bsdf_sample_count > 1)
        radiance /= static_cast<float>(bsdf_sample_count);
}

void compute_ibl_bssrdf_sampling(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const BSSRDF&           bssrdf,
    const void*             bssrdf_data,
    const ShadingPoint&     incoming_point,
    const ShadingPoint&     outgoing_point,
    const Dual3d&           outgoing,
    const size_t            bssrdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    radiance.set(0.0f);

    sampling_context.split_in_place(2, bssrdf_sample_count);

    for (size_t i = 0; i < bssrdf_sample_count; ++i)
    {
        // Generate a uniform sample in [0,1)^2.
        const Vector2d s = sampling_context.next_vector2<2>();

        // Sample the BSSRDF (hemisphere cosine).
        Vector3d incoming = sample_hemisphere_cosine(s);
        const double cos_in = incoming.y;
        const double bssrdf_prob = cos_in * RcpPi;
        incoming = incoming_point.get_shading_basis().transform_to_parent(incoming);
        if (incoming_point.get_side() == ObjectInstance::BackSide)
            incoming = -incoming;
        assert(is_normalized(incoming));

        // Discard occluded samples.
        const double transmission =
            shading_context.get_tracer().trace(
                incoming_point,
                incoming,
                VisibilityFlags::ShadowRay);
        if (transmission == 0.0)
            continue;

        // Evaluate the BSSRDF.
        Spectrum bssrdf_value;
        bssrdf.evaluate(
            bssrdf_data,
            outgoing_point,
            outgoing.get_value(),
            incoming_point,
            incoming,
            bssrdf_value);

        // Evaluate the environment's EDF.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        Spectrum env_value;
        double env_prob;
        environment_edf.evaluate(
            shading_context,
            input_evaluator,
            incoming,
            env_value,
            env_prob);

        // Compute MIS weight.
        const double mis_weight =
            mis_power2(
                bssrdf_sample_count * bssrdf_prob,
                env_sample_count * env_prob);

        // Add the contribution of this sample to the illumination.
        env_value *= static_cast<float>(transmission * cos_in / bssrdf_prob * mis_weight);
        env_value *= bssrdf_value;
        radiance += env_value;
    }

    if (bssrdf_sample_count > 1)
        radiance /= static_cast<float>(bssrdf_sample_count);
}

void compute_ibl_environment_sampling(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const ShadingPoint&     shading_point,
    const Dual3d&           outgoing,
    const BSDF&             bsdf,
    const void*             bsdf_data,
    const int               env_sampling_modes,
    const size_t            bsdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    const Vector3d& geometric_normal = shading_point.get_geometric_normal();
    const Basis3d& shading_basis = shading_point.get_shading_basis();

    radiance.set(0.0f);

    // todo: if we had a way to know that a BSDF is purely specular, we could
    // immediately return black here since there will be no contribution from
    // such a BSDF.

    sampling_context.split_in_place(2, env_sample_count);

    for (size_t i = 0; i < env_sample_count; ++i)
    {
        // Generate a uniform sample in [0,1)^2.
        const Vector2d s = sampling_context.next_vector2<2>();

        // Sample the environment.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        Vector3d incoming;
        Spectrum env_value;
        double env_prob;
        environment_edf.sample(
            shading_context,
            input_evaluator,
            s,
            incoming,
            env_value,
            env_prob);

        // Cull samples behind the shading surface.
        assert(is_normalized(incoming));
        const double cos_in = dot(incoming, shading_basis.get_normal());
        if (cos_in < 0.0)
            continue;

        // Discard occluded samples.
        const double transmission =
            shading_context.get_tracer().trace(
                shading_point,
                incoming,
                VisibilityFlags::ShadowRay);
        if (transmission == 0.0)
            continue;

        // Evaluate the BSDF.
        Spectrum bsdf_value;
        const double bsdf_prob =
            bsdf.evaluate(
                bsdf_data,
                false,                          // not adjoint
                true,                           // multiply by |cos(incoming, normal)|
                geometric_normal,
                shading_basis,
                outgoing.get_value(),
                incoming,
                env_sampling_modes,
                bsdf_value);
        if (bsdf_prob == 0.0)
            continue;

        // Compute MIS weight.
        const double mis_weight =
            mis_power2(
                env_sample_count * env_prob,
                bsdf_sample_count * bsdf_prob);

        // Add the contribution of this sample to the illumination.
        env_value *= static_cast<float>(transmission / env_prob * mis_weight);
        env_value *= bsdf_value;
        radiance += env_value;
    }

    if (env_sample_count > 1)
        radiance /= static_cast<float>(env_sample_count);
}

void compute_ibl_environment_sampling(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const EnvironmentEDF&   environment_edf,
    const BSSRDF&           bssrdf,
    const void*             bssrdf_data,
    const ShadingPoint&     incoming_point,
    const ShadingPoint&     outgoing_point,
    const Dual3d&           outgoing,
    const size_t            bssrdf_sample_count,
    const size_t            env_sample_count,
    Spectrum&               radiance)
{
    assert(is_normalized(outgoing.get_value()));

    const Basis3d& shading_basis = incoming_point.get_shading_basis();

    radiance.set(0.0f);

    sampling_context.split_in_place(2, env_sample_count);

    for (size_t i = 0; i < env_sample_count; ++i)
    {
        // Generate a uniform sample in [0,1)^2.
        const Vector2d s = sampling_context.next_vector2<2>();

        // Sample the environment.
        InputEvaluator input_evaluator(shading_context.get_texture_cache());
        Vector3d incoming;
        Spectrum env_value;
        double env_prob;
        environment_edf.sample(
            shading_context,
            input_evaluator,
            s,
            incoming,
            env_value,
            env_prob);

        // Cull samples behind the shading surface.
        assert(is_normalized(incoming));
        const double cos_in = dot(incoming, shading_basis.get_normal());
        if (cos_in <= 0.0)
            continue;

        // Discard occluded samples.
        const double transmission =
            shading_context.get_tracer().trace(
                incoming_point,
                incoming,
                VisibilityFlags::ShadowRay);
        if (transmission == 0.0)
            continue;

        // Evaluate the BSSRDF.
        Spectrum bssrdf_value;
        bssrdf.evaluate(
            bssrdf_data,
            outgoing_point,
            outgoing.get_value(),
            incoming_point,
            incoming,
            bssrdf_value);

        // Compute MIS weight.
        const double bssrdf_prob = cos_in * RcpPi;
        const double mis_weight =
            mis_power2(
                env_sample_count * env_prob,
                bssrdf_sample_count * bssrdf_prob);

        // Add the contribution of this sample to the illumination.
        env_value *= static_cast<float>(transmission * cos_in / env_prob * mis_weight);
        env_value *= bssrdf_value;
        radiance += env_value;
    }

    if (env_sample_count > 1)
        radiance /= static_cast<float>(env_sample_count);
}

}   // namespace renderer
