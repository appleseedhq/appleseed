
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/transmission.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"

// appleseed.foundation headers.
#include "foundation/math/mis.h"

using namespace foundation;

namespace renderer
{

//
// Compute the direct lighting at a given point in space.
//

void compute_direct_lighting(
    const ShadingContext&       shading_context,
    const Vector3d&             point,
    const Vector3d&             geometric_normal,
    const Basis3d&              shading_basis,
    const Vector3d&             outgoing,
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const LightSampleVector&    samples,
    Spectrum&                   radiance,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(geometric_normal));
    assert(is_normalized(outgoing));

    // Initialize radiance.
    radiance.set(0.0f);

    // Retrieve the shading normal at the intersection point.
    const Vector3d& shading_normal = shading_basis.get_normal();

    // Compute direct illumination from this set of light samples.
    const size_t sample_count = samples.size();
    for (size_t i = 0; i < sample_count; ++i)
    {
        // Fetch the light sample.
        const LightSample& sample = samples[i];
        assert(sample.m_edf);

        // Compute the incoming direction in world space.
        const Vector3d incoming = -sample.m_outgoing;

        // Cull light samples behind the shading surface.
        const double cos_in = dot(incoming, shading_normal);
        if (cos_in <= 0.0)
            continue;

        // Compute the transmission factor between the light sample and the shading point.
        const double transmission =
            compute_transmission_between(
                shading_context,
                point,
                sample.m_input_params.m_point,
                parent_shading_point);

        // Discard occluded samples.
        if (transmission == 0.0)
            continue;

        // Evaluate the BSDF.
        Spectrum bsdf_value;
        bsdf.evaluate(
            bsdf_data,
            geometric_normal,
            shading_basis,
            outgoing,
            incoming,
            bsdf_value);

        // Evaluate the input values of the EDF.
        InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
        const void* edf_data =
            edf_input_evaluator.evaluate(
                sample.m_edf->get_inputs(),
                sample.m_input_params);

        // Evaluate the EDF.
        Spectrum edf_value;
        sample.m_edf->evaluate(
            edf_data,
            sample.m_input_params.m_geometric_normal,
            Basis3d(sample.m_input_params.m_shading_normal),
            sample.m_outgoing,
            edf_value);

        // Compute the geometric term. To keep the estimator unbiased, we don't
        // clamp the geometric term g if it is too small, and in particular we
        // allow it to be exactly zero, which will result in a variance spike.
        const double cos_ln = dot(sample.m_outgoing, sample.m_input_params.m_shading_normal);
        const double g = (cos_in * cos_ln) / sample.m_square_distance;
        assert(g >= 0.0);

        // Compute the probability density with respect to surface area
        // of choosing this light sample through sampling of the BSDF.
        double py =
            bsdf.evaluate_pdf(
                bsdf_data,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming);
        py *= dot(sample.m_outgoing, sample.m_input_params.m_geometric_normal);
        py /= sample.m_square_distance;

        // Compute the MIS weight.
        const double mis_weight = mis_power2(sample.m_probability, py);

        // Compute the contribution of this sample to the illumination.
        assert(sample.m_probability > 0.0);
        const double weight = transmission * mis_weight * g / sample.m_probability;
        edf_value *= static_cast<float>(weight);
        edf_value *= bsdf_value;

        // Add the contribution of this sample to the illumination.
        radiance += edf_value;
    }

    // Normalize the radiance.
    if (sample_count > 1)
        radiance /= static_cast<float>(sample_count);
}

}   // namespace renderer
