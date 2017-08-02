
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
#include "backwardlightsampler.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"

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
    // Read which sampling algorithm should the sampler use.
    m_use_light_tree = params.get_optional<string>("algorithm", "cdf") == "lighttree";

    RENDERER_LOG_INFO("collecting light emitters...");

    // Collect all non-physical lights and separate them according to their
    // compatibility with the LightTree.
    collect_non_physical_lights(scene.assembly_instances(), TransformSequence());
    m_non_physical_light_count = m_non_physical_lights.size();

    // Collect all light-emitting triangles.
    collect_emitting_triangles(
        scene.assembly_instances(),
        TransformSequence());

    // Build the hash table of emitting triangles.
    build_emitting_triangle_hash_table();

    // Prepare the CDFs for sampling.
    if (m_non_physical_lights_cdf.valid())
        m_non_physical_lights_cdf.prepare();

    if (m_use_light_tree)
    {
        // Initialize the LightTree only after the lights are collected.
        m_light_tree.reset(new LightTree(m_light_tree_lights, m_emitting_triangles));
        
        const vector<size_t> tri_index_to_node_index = m_light_tree->build();
        if (has_hittable_lights())
        {
            // Update information about emitting triangle position within the light tree.
            for (size_t i = 0, e = tri_index_to_node_index.size(); i < e; ++i)
                m_emitting_triangles[i].m_light_tree_node_index = tri_index_to_node_index[i];
        }
    }
    else
    {
        if (m_emitting_triangles_cdf.valid())
            m_emitting_triangles_cdf.prepare();

        // Store the triangle probability densities into the emitting triangles.
        const size_t emitting_triangle_count = m_emitting_triangles.size();
        for (size_t i = 0; i < emitting_triangle_count; ++i)
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

void BackwardLightSampler::sample_non_physical_light(
    const ShadingRay::Time&             time,
    const size_t                        light_index,
    LightSample&                        light_sample) const
{
    // Fetch the light.
    const NonPhysicalLightInfo& light_info = m_non_physical_lights[light_index];
    light_sample.m_light = light_info.m_light;

    // Evaluate and store the transform of the light.
    light_sample.m_light_transform =
          light_info.m_light->get_transform()
        * light_info.m_transform_sequence.evaluate(time.m_absolute);

    // Store the probability density of this light.
    light_sample.m_probability = 1.0f;
}

float BackwardLightSampler::evaluate_pdf(const ShadingPoint& shading_point) const
{
    assert(shading_point.is_triangle_primitive());

    const EmittingTriangleKey triangle_key(
        shading_point.get_assembly_instance().get_uid(),
        shading_point.get_object_instance_index(),
        shading_point.get_region_index(),
        shading_point.get_primitive_index());

    const EmittingTriangle* triangle = m_emitting_triangle_hash_table.get(triangle_key);

    if (m_use_light_tree)
    {
        const float triangle_probability =
            m_light_tree->evaluate_node_pdf(
                shading_point.get_ray().m_org,
                triangle->m_light_tree_node_index);

        return triangle_probability * triangle->m_rcp_area;
    }
    else
        return triangle->m_triangle_prob * triangle->m_rcp_area;
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
        shading_point.get_point(),
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

void BackwardLightSampler::sample_emitting_triangles(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    LightSample&                        light_sample) const
{
    assert(m_emitting_triangles_cdf.valid());

    const EmitterCDF::ItemWeightPair result = m_emitting_triangles_cdf.sample(s[0]);
    const size_t emitter_index = result.first;
    const float emitter_prob = result.second;

    light_sample.m_light = 0;
    sample_emitting_triangle(
        time,
        Vector2f(s[1], s[2]),
        emitter_index,
        emitter_prob,
        light_sample);

    assert(light_sample.m_triangle);
    assert(light_sample.m_probability > 0.0f);
}

void BackwardLightSampler::sample_emitting_triangle(
    const ShadingRay::Time&             time,
    const Vector2f&                     s,
    const size_t                        triangle_index,
    const float                         triangle_prob,
    LightSample&                        light_sample) const
{
    // Fetch the emitting triangle.
    const EmittingTriangle& emitting_triangle = m_emitting_triangles[triangle_index];

    // Store a pointer to the emitting triangle.
    light_sample.m_triangle = &emitting_triangle;

    // Uniformly sample the surface of the triangle.
    const Vector3d bary = sample_triangle_uniform(Vector2d(s));

    // Set the barycentric coordinates.
    light_sample.m_bary[0] = static_cast<float>(bary[0]);
    light_sample.m_bary[1] = static_cast<float>(bary[1]);

    // Compute the world space position of the sample.
    light_sample.m_point =
          bary[0] * emitting_triangle.m_v0
        + bary[1] * emitting_triangle.m_v1
        + bary[2] * emitting_triangle.m_v2;

    // Compute the world space shading normal at the position of the sample.
    light_sample.m_shading_normal =
          bary[0] * emitting_triangle.m_n0
        + bary[1] * emitting_triangle.m_n1
        + bary[2] * emitting_triangle.m_n2;
    light_sample.m_shading_normal = normalize(light_sample.m_shading_normal);

    // Set the world space geometric normal.
    light_sample.m_geometric_normal = emitting_triangle.m_geometric_normal;

    // Compute the probability density of this sample.
    light_sample.m_probability = triangle_prob * emitting_triangle.m_rcp_area;
}

}   // namespace renderer
