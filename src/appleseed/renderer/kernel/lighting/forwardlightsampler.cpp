
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
#include "forwardlightsampler.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/lightsample.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/scene.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;

namespace renderer
{

//
// ForwardLightSampler class implementation.
//

ForwardLightSampler::ForwardLightSampler(const Scene& scene, const ParamArray& params)
  : LightSamplerBase(params)
{
    RENDERER_LOG_INFO("collecting light emitters...");

    // Collect all non-physical lights.
    collect_non_physical_lights(
        scene.assembly_instances(),
        TransformSequence(),
        [&](const NonPhysicalLightInfo& light_info)
        {
            // Insert into non-physical lights to be evaluated using a CDF.
            const size_t light_index = m_non_physical_lights.size();
            m_non_physical_lights.push_back(light_info);

            // Insert the light into the CDF.
            // todo: compute importance.
            float importance = 1.0f;
            importance *= light_info.m_light->get_uncached_importance_multiplier();
            m_non_physical_lights_cdf.insert(light_index, importance);
        });
    m_non_physical_light_count = m_non_physical_lights.size();

    // Collect all light-emitting shapes.
    collect_emitting_shapes(
        scene.assembly_instances(),
        TransformSequence(),
        [&](
            const Material* material,
            const float     area,
            const size_t    emitting_shape_index)
        {
            // Retrieve the EDF and get the importance multiplier.
            float importance_multiplier = 1.0f;
            if (const EDF* edf = material->get_uncached_edf())
                importance_multiplier = edf->get_uncached_importance_multiplier();

            // Compute the probability density of this shape.
            const float shape_importance = m_params.m_importance_sampling ? area : 1.0f;
            const float shape_prob = shape_importance * importance_multiplier;

            // Insert the light-emitting shape into the CDF.
            m_emitting_shapes_cdf.insert(emitting_shape_index, shape_prob);

            // Accept this shape.
            return true;
        });

    // Build the hash table of emitting shapes.
    build_emitting_shape_hash_table();

    // Prepare the CDFs for sampling.
    if (m_non_physical_lights_cdf.valid())
        m_non_physical_lights_cdf.prepare();
    if (m_emitting_shapes_cdf.valid())
        m_emitting_shapes_cdf.prepare();

    // Store the shape probability densities into the emitting shapes.
    for (size_t i = 0, e = m_emitting_shapes.size(); i < e; ++i)
        m_emitting_shapes[i].set_shape_prob(m_emitting_shapes_cdf[i].second);

   RENDERER_LOG_INFO(
        "found %s %s, %s emitting %s.",
        pretty_int(m_non_physical_light_count).c_str(),
        plural(m_non_physical_light_count, "non-physical light").c_str(),
        pretty_int(m_emitting_shapes.size()).c_str(),
        plural(m_emitting_shapes.size(), "shape").c_str());
}

void ForwardLightSampler::sample(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    LightSample&                        light_sample) const
{
    assert(m_non_physical_lights_cdf.valid() || m_emitting_shapes_cdf.valid());

    if (m_non_physical_lights_cdf.valid())
    {
        if (m_emitting_shapes_cdf.valid())
        {
            if (s[0] < 0.5f)
            {
                sample_non_physical_lights(
                    time,
                    Vector3f(s[0] * 2.0f, s[1], s[2]),
                    light_sample);
            }
            else
            {
                sample_emitting_shapes(
                    time,
                    Vector3f((s[0] - 0.5f) * 2.0f, s[1], s[2]),
                    light_sample);
            }

            light_sample.m_probability *= 0.5f;
        }
        else sample_non_physical_lights(time, s, light_sample);
    }
    else sample_emitting_shapes(time, s, light_sample);
}

float ForwardLightSampler::evaluate_pdf(const ShadingPoint& light_shading_point) const
{
    assert(light_shading_point.is_triangle_primitive());
    const EmittingShapeKey shape_key(
        light_shading_point.get_assembly_instance().get_uid(),
        light_shading_point.get_object_instance_index(),
        light_shading_point.get_primitive_index());

    const auto* shape_ptr = m_emitting_shape_hash_table.get(shape_key);

    if (shape_ptr == nullptr)
        return 0.0f;

    const EmittingShape* shape = *shape_ptr;
    return shape->evaluate_pdf_uniform();
}

void ForwardLightSampler::sample_non_physical_lights(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    LightSample&                        light_sample) const
{
    assert(m_non_physical_lights_cdf.valid());

    const EmitterCDF::ItemWeightPair result = m_non_physical_lights_cdf.sample(s[0]);
    const size_t light_index = result.first;
    const float light_prob = result.second;

    light_sample.m_shape = nullptr;
    sample_non_physical_light(
        time,
        light_index,
        light_sample,
        light_prob);

    assert(light_sample.m_light);
    assert(light_sample.m_probability > 0.0f);
}

}   // namespace renderer
