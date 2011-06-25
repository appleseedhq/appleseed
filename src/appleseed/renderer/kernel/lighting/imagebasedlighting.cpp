
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
#include "imagebasedlighting.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/transmission.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"

// appleseed.foundation headers.
#include "foundation/math/mis.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Compute image-based lighting via BSDF sampling.
    //

    void compute_ibl_bsdf_sampling(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const EnvironmentEDF&       environment_edf,
        const Vector3d&             point,
        const Vector3d&             geometric_normal,
        const Basis3d&              shading_basis,
        const Vector3d&             outgoing,
        const BSDF&                 bsdf,
        const void*                 bsdf_data,
        const size_t                bsdf_sample_count,
        const size_t                env_sample_count,
        Spectrum&                   radiance,
        const ShadingPoint*         parent_shading_point)
    {
        radiance.set(0.0f);

        sampling_context = sampling_context.split(3, bsdf_sample_count);

        for (size_t i = 0; i < bsdf_sample_count; ++i)
        {
            // Generate a uniform sample in [0,1)^3.
            const Vector3d s = sampling_context.next_vector2<3>();

            // Sample the BSDF.
            Vector3d incoming;
            Spectrum bsdf_value;
            double bsdf_prob;
            BSDF::Mode mode;
            bsdf.sample(
                bsdf_data,
                false,              // adjoint
                geometric_normal,
                shading_basis,
                s,
                outgoing,
                incoming,
                bsdf_value,
                bsdf_prob,
                mode);

            // Handle absorption.
            if (mode == BSDF::None)
                continue;

            // todo: deliberately ignore specular mode to avoid double contribution?

            // Cull samples behind the shading surface.
            assert(is_normalized(incoming));
            const double cos_in = dot(incoming, shading_basis.get_normal());
            if (cos_in < 0.0)
                continue;

            // Compute the transmission factor toward the incoming direction.
            const double transmission =
                compute_transmission(
                    sampling_context,
                    shading_context,
                    point,
                    incoming,
                    parent_shading_point);

            // Discard occluded samples.
            if (transmission == 0.0)
                continue;

            // Evaluate the environment's EDF.
            InputEvaluator input_evaluator(shading_context.get_texture_cache());
            Spectrum env_value;
            double env_prob;
            environment_edf.evaluate(
                input_evaluator,
                incoming,
                env_value,
                env_prob);

            // Compute MIS weight.
            const double mis_weight =
                bsdf_prob < 0.0
                    ? 1.0
                    : mis_power2(
                          bsdf_sample_count * bsdf_prob,
                          env_sample_count * env_prob);

            // Add the contribution of this sample to the illumination.
            env_value *= static_cast<float>(transmission * mis_weight);
            env_value *= bsdf_value;
            radiance += env_value;
        }

        if (bsdf_sample_count > 1)
            radiance /= static_cast<float>(bsdf_sample_count);
    }


    //
    // Compute image-based lighting via environment sampling.
    //

    void compute_ibl_environment_sampling(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const EnvironmentEDF&       environment_edf,
        const Vector3d&             point,
        const Vector3d&             geometric_normal,
        const Basis3d&              shading_basis,
        const Vector3d&             outgoing,
        const BSDF&                 bsdf,
        const void*                 bsdf_data,
        const size_t                bsdf_sample_count,
        const size_t                env_sample_count,
        Spectrum&                   radiance,
        const ShadingPoint*         parent_shading_point)
    {
        radiance.set(0.0f);

        sampling_context = sampling_context.split(2, env_sample_count);

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

            // Compute the transmission factor toward the incoming direction.
            const double transmission =
                compute_transmission(
                    sampling_context,
                    shading_context,
                    point,
                    incoming,
                    parent_shading_point);

            // Discard occluded samples.
            if (transmission == 0.0)
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
}


//
// Compute image-based lighting at a given point in space.
//

void compute_image_based_lighting(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const EnvironmentEDF&       environment_edf,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const size_t                bsdf_sample_count,
    const size_t                env_sample_count,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(geometric_normal));
    assert(is_normalized(outgoing));

    // Compute IBL by sampling the BSDF.
    compute_ibl_bsdf_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        point,
        geometric_normal,
        shading_basis,
        outgoing,
        bsdf,
        bsdf_data,
        bsdf_sample_count,
        env_sample_count,
        radiance,
        parent_shading_point);

    // Compute IBL by sampling the environment.
    Spectrum radiance_env_sampling;
    compute_ibl_environment_sampling(
        sampling_context,
        shading_context,
        environment_edf,
        point,
        geometric_normal,
        shading_basis,
        outgoing,
        bsdf,
        bsdf_data,
        bsdf_sample_count,
        env_sample_count,
        radiance_env_sampling,
        parent_shading_point);
    radiance += radiance_env_sampling;
}

}   // namespace renderer
