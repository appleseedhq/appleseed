
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
#include "imagebasedlighting.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/lightpathstream.h"
#include "renderer/kernel/lighting/materialsamplers.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
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

void compute_ibl_combined_sampling(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const EnvironmentEDF&       environment_edf,
    const Dual3d&               outgoing,
    const IMaterialSampler&     material_sampler,
    const int                   env_sampling_modes,
    const size_t                material_sample_count,
    const size_t                env_sample_count,
    DirectShadingComponents&    radiance,
    Spectrum&                   unshaded_radiance,
    Spectrum&                   shaded_radiance,
    LightPathStream*            light_path_stream)
{
    assert(is_normalized(outgoing.get_value()));

    // Compute IBL by sampling the BSDF.
    compute_ibl_material_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        outgoing,
        material_sampler,
        material_sample_count,
        env_sample_count,
        radiance);

    // Compute IBL by sampling the environment.
    DirectShadingComponents radiance_env_sampling;
    compute_ibl_environment_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        outgoing,
        material_sampler,
        env_sampling_modes,
        material_sample_count,
        env_sample_count,
        radiance_env_sampling,
        unshaded_radiance,
        shaded_radiance,
        light_path_stream);
    radiance += radiance_env_sampling;
}

void compute_ibl_material_sampling(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const EnvironmentEDF&       environment_edf,
    const Dual3d&               outgoing,
    const IMaterialSampler&     material_sampler,
    const size_t                bsdf_sample_count,
    const size_t                env_sample_count,
    DirectShadingComponents&    radiance)
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
        Dual3f incoming;
        DirectShadingComponents material_value;
        float material_prob;
        if (!material_sampler.sample(
                sampling_context,
                Dual3d(outgoing),
                incoming,
                material_value,
                material_prob))
            continue;

        // Compute the transmission factor between the environment and the shading point.
        Spectrum transmission;
        if (environment_edf.get_flags() & EnvironmentEDF::CastShadows)
        {
            material_sampler.trace_simple(shading_context, incoming.get_value(), transmission);

            // Discard occluded samples.
            if (is_zero(transmission))
                continue;
        }
        else transmission.set(1.0f);

        // Evaluate the environment EDF.
        Spectrum env_value(Spectrum::Illuminance);
        float env_prob;
        environment_edf.evaluate(
            shading_context,
            incoming.get_value(),
            env_value,
            env_prob);

        // Apply all weights, including MIS weight.
        if (material_prob == BSDF::DiracDelta)
            env_value *= transmission;
        else
        {
            const float mis_weight =
                mis_power2(
                    bsdf_sample_count * material_prob,
                    env_sample_count * env_prob);
            env_value *= transmission;
            env_value *= mis_weight / material_prob;
        }

        // Add the contribution of this sample to the illumination.
        madd(radiance, material_value, env_value);
    }

    if (bsdf_sample_count > 1)
        radiance /= static_cast<float>(bsdf_sample_count);
}

void compute_ibl_environment_sampling(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const EnvironmentEDF&       environment_edf,
    const Dual3d&               outgoing,
    const IMaterialSampler&     material_sampler,
    const int                   env_sampling_modes,
    const size_t                material_sample_count,
    const size_t                env_sample_count,
    DirectShadingComponents&    radiance,
    Spectrum&                   unshaded_radiance,
    Spectrum&                   shaded_radiance,
    LightPathStream*            light_path_stream)
{
    assert(is_normalized(outgoing.get_value()));

    radiance.set(0.0f);

    // Check if PDF of the sampler is Dirac delta and therefore cannot contribute to the light sampling.
    if (!material_sampler.contributes_to_light_sampling())
        return;

    sampling_context.split_in_place(2, env_sample_count);

    for (size_t i = 0; i < env_sample_count; ++i)
    {
        // Generate a uniform sample in [0,1)^2.
        const Vector2f s = sampling_context.next2<Vector2f>();

        // Sample the environment.
        Vector3f incoming;
        Spectrum env_value(Spectrum::Illuminance);
        float env_prob;
        environment_edf.sample(
            shading_context,
            s,
            incoming,
            env_value,
            env_prob);
        assert(is_normalized(incoming));

        // Add unshaded light contribution for the shadow catcher
        unshaded_radiance += env_value;

        // Compute the transmission factor between the environment and the shading point.
        Spectrum transmission;
        if (environment_edf.get_flags() & EnvironmentEDF::CastShadows)
        {
            material_sampler.trace_simple(shading_context, incoming, transmission);

            // Discard occluded samples.
            if (is_zero(transmission))
                continue;
        }
        else transmission.set(1.0f);

        // Evaluate the BSDF.
        DirectShadingComponents material_value;
        const float material_prob =
            material_sampler.evaluate(
                Vector3f(outgoing.get_value()),
                incoming,
                env_sampling_modes,
                material_value);
        if (material_prob == 0.0f)
            continue;

        // Compute MIS weight.
        const float mis_weight =
            mis_power2(
                env_sample_count * env_prob,
                material_sample_count * material_prob);

        // Add the contribution of this sample to the illumination.
        env_value *= transmission;
        shaded_radiance += env_value;
        env_value *= mis_weight / env_prob;
        madd(radiance, material_value, env_value);

        // Record light path event.
        if (light_path_stream)
        {
            light_path_stream->sampled_environment(
                environment_edf,
                incoming,
                material_value.m_beauty,
                env_value);
        }
    }

    if (env_sample_count > 1)
    {
        radiance /= static_cast<float>(env_sample_count);
        unshaded_radiance /= static_cast<float>(env_sample_count);
        shaded_radiance /= static_cast<float>(env_sample_count);
    }
}

}   // namespace renderer
