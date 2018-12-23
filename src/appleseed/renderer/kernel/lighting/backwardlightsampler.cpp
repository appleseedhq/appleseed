
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
#include "backwardlightsampler.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/lightsample.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BackwardLightSampler class implementation.
//

BackwardLightSampler::BackwardLightSampler(
    const Scene&                        scene,
    const ParamArray&                   params)
  : LightSamplerBase(params)
{
    // Read which sampling algorithm should be used.
    m_use_light_tree = params.get_optional<string>("algorithm", "cdf") == "lighttree";

    RENDERER_LOG_INFO("collecting light emitters...");

    // Collect all non-physical lights and separate them according to their
    // compatibility with the LightTree.
    collect_non_physical_lights(
        scene.assembly_instances(),
        TransformSequence(),
        [&](const NonPhysicalLightInfo& light_info)
        {
            if (m_use_light_tree
                && ((light_info.m_light->get_flags() & Light::LightTreeCompatible) != 0))
            {
                // Insert into light tree compatible lights.
                m_light_tree_lights.push_back(light_info);
            }
            else
            {
                // Insert into non-physical lights to be evaluated using a CDF.
                const size_t light_index = m_non_physical_lights.size();
                m_non_physical_lights.push_back(light_info);

                // Insert the light into the CDF.
                // todo: compute importance.
                float importance = 1.0f;
                importance *= light_info.m_light->get_uncached_importance_multiplier();
                m_non_physical_lights_cdf.insert(light_index, importance);
            }
        });
    m_non_physical_light_count = m_non_physical_lights.size();

    // Collect all light-emitting triangles.
    collect_emitting_triangles(
        scene.assembly_instances(),
        TransformSequence(),
        [&](
            const Material* material,
            const float     area,
            const size_t    emitting_triangle_index)
        {
            if (m_use_light_tree)
            {
                // Only accept this triangle if its material has an EDF.
                // This excludes triangles with light-emitting OSL materials
                // since these are not handled by the light tree yet.
                return material->get_uncached_edf() != nullptr;
            }
            else
            {
                // Retrieve the EDF and get the importance multiplier.
                float importance_multiplier = 1.0f;
                if (const EDF* edf = material->get_uncached_edf())
                    importance_multiplier = edf->get_uncached_importance_multiplier();

                // Compute the probability density of this triangle.
                const float triangle_importance = m_params.m_importance_sampling ? static_cast<float>(area) : 1.0f;
                const float triangle_prob = triangle_importance * importance_multiplier;

                // Insert the light-emitting triangle into the CDF.
                m_emitting_triangles_cdf.insert(emitting_triangle_index, triangle_prob);

                // Accept this triangle.
                return true;
            }
        });

    // Build the hash table of emitting triangles.
    build_emitting_triangle_hash_table();

    // Prepare the non-physical lights CDF for sampling.
    if (m_non_physical_lights_cdf.valid())
        m_non_physical_lights_cdf.prepare();

    if (m_use_light_tree)
    {
        // Initialize the LightTree only after the lights are collected.
        m_light_tree.reset(new LightTree(m_light_tree_lights, m_emitting_triangles));

        // Build the light tree.
        const vector<size_t> tri_index_to_node_index = m_light_tree->build();
        assert(tri_index_to_node_index.size() == m_emitting_triangles.size());

        // Associate light tree nodes to emitting triangles.
        for (size_t i = 0, e = m_emitting_triangles.size(); i < e; ++i)
            m_emitting_triangles[i].m_light_tree_node_index = tri_index_to_node_index[i];
    }
    else
    {
        // Prepare the light-emitting triangles CDF for smapling.
        if (m_emitting_triangles_cdf.valid())
            m_emitting_triangles_cdf.prepare();

        // Store the triangle probability densities into the emitting triangles.
        for (size_t i = 0, e = m_emitting_triangles.size(); i < e; ++i)
            m_emitting_triangles[i].m_triangle_prob = m_emitting_triangles_cdf[i].second;
    }

    RENDERER_LOG_INFO(
        "found %s %s, %s %s, %s emitting %s.",
        pretty_int(m_non_physical_light_count).c_str(),
        plural(m_non_physical_light_count, "non-physical light").c_str(),
        pretty_int(m_light_tree_lights.size() + m_emitting_triangles.size()).c_str(),
        plural(m_light_tree_lights.size() + m_emitting_triangles.size(), "light-tree compatible light").c_str(),
        pretty_int(m_emitting_triangles.size()).c_str(),
        plural(m_emitting_triangles.size(), "triangle").c_str());
}

void BackwardLightSampler::sample_lightset(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    const ShadingPoint&                 shading_point,
    LightSample&                        light_sample) const
{
    if (m_use_light_tree)
    {
        // Light tree sampling.
        sample_light_tree(
            time,
            s,
            shading_point,
            light_sample);
    }
    else
    {
        // CDF-based sampling.
        sample_emitting_triangles(
            time,
            s,
            light_sample);
    }
}

float BackwardLightSampler::evaluate_pdf(
    const ShadingPoint&                 light_shading_point,
    const ShadingPoint&                 surface_shading_point) const
{
    assert(light_shading_point.is_triangle_primitive());

    const EmittingTriangleKey triangle_key(
        light_shading_point.get_assembly_instance().get_uid(),
        light_shading_point.get_object_instance_index(),
        light_shading_point.get_primitive_index());

    const auto* triangle_ptr = m_emitting_triangle_hash_table.get(triangle_key);

    if (triangle_ptr == nullptr)
        return 0.0f;

    const EmittingTriangle* triangle = *triangle_ptr;

    const float triangle_probability =
        m_use_light_tree
            ? m_light_tree->evaluate_node_pdf(
                surface_shading_point,
                triangle->m_light_tree_node_index)
            : triangle->m_triangle_prob;

    const float pdf = triangle_probability * triangle->m_rcp_area;
    assert(pdf >= 0.0f);

    return pdf;
}

Dictionary BackwardLightSampler::get_params_metadata()
{
    Dictionary metadata;

    metadata.insert(
        "algorithm",
        Dictionary()
            .insert("type", "enum")
            .insert("values", "cdf|lighttree")
            .insert("default", "cdf")
            .insert("label", "Light Sampler")
            .insert("help", "Light sampling algoritm")
            .insert(
                "options",
                Dictionary()
                    .insert(
                        "cdf",
                        Dictionary()
                            .insert("label", "CDF")
                            .insert("help", "Cumulative Distribution Function"))
                    .insert(
                        "lighttree",
                        Dictionary()
                            .insert("label", "Light Tree")
                            .insert("help", "Lights organized in a BVH"))));

    return metadata;
}

void BackwardLightSampler::sample_light_tree(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    const ShadingPoint&                 shading_point,
    LightSample&                        light_sample) const
{
    assert(has_lightset());

    LightType light_type;
    size_t light_index;
    float light_prob;
    m_light_tree->sample(
        shading_point,
        s[0],
        light_type,
        light_index,
        light_prob);

    if (light_type == NonPhysicalLightType)
    {
        // Fetch the light.
        const NonPhysicalLightInfo& light_info = m_light_tree_lights[light_index];
        light_sample.m_light = light_info.m_light;

        // Evaluate and store the transform of the light.
        light_sample.m_light_transform =
              light_info.m_light->get_transform()
            * light_info.m_transform_sequence.evaluate(time.m_absolute);

        // Store the probability density of this light.
        light_sample.m_probability = light_prob;
    }
    else
    {
        assert(light_type == EmittingTriangleType);
        sample_emitting_triangle(
            time,
            Vector2f(s[1], s[2]),
            light_index,
            light_prob,
            light_sample);
    }

    assert(light_sample.m_light || light_sample.m_triangle);
    assert(light_sample.m_probability > 0.0f);
}

}   // namespace renderer
