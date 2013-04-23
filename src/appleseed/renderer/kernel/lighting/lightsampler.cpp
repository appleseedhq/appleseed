
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "lightsampler.h"

// appleseed.renderer headers.
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/area.h"
#include "foundation/math/sampling.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <map>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// LightSampler class implementation.
//

namespace
{
    // Return true if a given assembly references at least one light-emitting material.
    bool has_emitting_materials(const Assembly& assembly)
    {
        for (const_each<MaterialContainer> i = assembly.materials(); i; ++i)
        {
            if (i->get_uncached_edf())
                return true;
        }

        return false;
    }

    // Return true if at least one material emits light.
    bool has_emitting_materials(const MaterialArray& materials)
    {
        for (size_t i = 0; i < materials.size(); ++i)
        {
            if (materials[i] && materials[i]->get_uncached_edf())
                return true;
        }

        return false;
    }
}

LightSampler::LightSampler(const Scene& scene, const ParamArray& params)
  : m_params(params)
  , m_total_emissive_area(0.0)
{
    RENDERER_LOG_INFO("collecting light emitters...");

    // Collect all non-physical lights.
    collect_non_physical_lights(scene.assembly_instances(), TransformSequence());

    // Collect all light-emitting triangles.
    // todo: update to support nested assemblies.
    for (const_each<AssemblyInstanceContainer> i = scene.assembly_instances(); i; ++i)
    {
        const AssemblyInstance& assembly_instance = *i;
        const Assembly& assembly = assembly_instance.get_assembly();

        if (has_emitting_materials(assembly))
            collect_emitting_triangles(assembly, assembly_instance);
    }

    // Precompute some values.
    m_non_physical_light_count = m_non_physical_lights.size();
    m_rcp_total_emissive_area = 1.0 / m_total_emissive_area;
    m_rcp_emitting_triangle_count = 1.0 / m_emitting_triangles.size();

    // Prepare the CDFs for sampling.
    if (m_non_physical_lights_cdf.valid())
        m_non_physical_lights_cdf.prepare();
    if (m_emitting_triangles_cdf.valid())
        m_emitting_triangles_cdf.prepare();

    RENDERER_LOG_INFO(
        "found %s %s, %s emitting %s.",
        pretty_int(m_non_physical_light_count).c_str(),
        plural(m_non_physical_light_count, "non-physical light").c_str(),
        pretty_int(m_emitting_triangles.size()).c_str(),
        plural(m_emitting_triangles.size(), "triangle").c_str());
}

void LightSampler::collect_non_physical_lights(
    const AssemblyInstanceContainer&    assembly_instances,
    const TransformSequence&            parent_transform_seq)
{
    for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Compute the cumulated transform sequence of this assembly instance.
        TransformSequence cumulated_transform_seq =
            assembly_instance.transform_sequence() * parent_transform_seq;
        cumulated_transform_seq.prepare();

        // Recurse into child assembly instances.
        collect_non_physical_lights(
            assembly.assembly_instances(),
            cumulated_transform_seq);

        // Collect lights from this assembly instance.
        collect_non_physical_lights(
            assembly,
            cumulated_transform_seq);
    }
}

void LightSampler::collect_non_physical_lights(
    const Assembly&                     assembly,
    const TransformSequence&            transform_sequence)
{
    for (const_each<LightContainer> i = assembly.lights(); i; ++i)
    {
        // Retrieve the light.
        const Light& light = *i;

        // Copy the light into the light vector.
        const size_t light_index = m_non_physical_lights.size();
        NonPhysicalLightInfo light_info;
        light_info.m_transform_sequence = transform_sequence;
        light_info.m_light = &light;
        m_non_physical_lights.push_back(light_info);

        // Insert the light into the CDF.
        // todo: compute importance.
        const double importance = 1.0;
        m_non_physical_lights_cdf.insert(light_index, importance);
    }
}

void LightSampler::collect_emitting_triangles(
    const Assembly&                     assembly,
    const AssemblyInstance&             assembly_instance)
{
    // Loop over the object instances of the assembly.
    const size_t object_instance_count = assembly.object_instances().size();
    for (size_t object_instance_index = 0; object_instance_index < object_instance_count; ++object_instance_index)
    {
        // Retrieve the object instance.
        const ObjectInstance* object_instance = assembly.object_instances().get_by_index(object_instance_index);

        // Retrieve the materials of the object instance.
        const MaterialArray& front_materials = object_instance->get_front_materials();
        const MaterialArray& back_materials = object_instance->get_back_materials();

        // Skip object instances without light-emitting materials.
        if (!has_emitting_materials(front_materials) && !has_emitting_materials(back_materials))
            continue;

        // Compute the object space to world space transformation.
        // todo: add support for moving light-emitters.
        const Transformd& object_instance_transform = object_instance->get_transform();
        const Transformd& assembly_instance_transform =
            assembly_instance.transform_sequence().get_earliest_transform();
        const Transformd global_transform = assembly_instance_transform * object_instance_transform;

        // Retrieve the object.
        Object& object = object_instance->get_object();

        // Retrieve the region kit of the object.
        Access<RegionKit> region_kit(&object.get_region_kit());

        // Loop over the regions of the object.
        const size_t region_count = region_kit->size();
        for (size_t region_index = 0; region_index < region_count; ++region_index)
        {
            // Retrieve the region.
            const IRegion* region = (*region_kit)[region_index];

            // Retrieve the tessellation of the region.
            Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

            // Loop over the triangles of the region.
            const size_t triangle_count = tess->m_primitives.size();
            for (size_t triangle_index = 0; triangle_index < triangle_count; ++triangle_index)
            {
                // Fetch the triangle.
                const Triangle& triangle = tess->m_primitives[triangle_index];

                // Skip triangles without a material.
                if (triangle.m_pa == Triangle::None)
                    continue;

                // Fetch the materials assigned to this triangle.
                const size_t pa_index = static_cast<size_t>(triangle.m_pa);
                const Material* front_material =
                    pa_index < front_materials.size() ? front_materials[pa_index] : 0;
                const Material* back_material =
                    pa_index < back_materials.size() ? back_materials[pa_index] : 0;

                // Skip triangles that don't emit light.
                if ((front_material == 0 || front_material->get_uncached_edf() == 0) &&
                    (back_material == 0 || back_material->get_uncached_edf() == 0))
                    continue;

                // Retrieve object instance space vertices of the triangle.
                const GVector3& v0_os = tess->m_vertices[triangle.m_v0];
                const GVector3& v1_os = tess->m_vertices[triangle.m_v1];
                const GVector3& v2_os = tess->m_vertices[triangle.m_v2];

                // Transform triangle vertices to assembly space.
                const GVector3 v0_as = object_instance_transform.point_to_parent(v0_os);
                const GVector3 v1_as = object_instance_transform.point_to_parent(v1_os);
                const GVector3 v2_as = object_instance_transform.point_to_parent(v2_os);

                // Compute the support plane of the hit triangle in assembly space.
                const GTriangleType triangle_geometry(v0_as, v1_as, v2_as);
                TriangleSupportPlaneType triangle_support_plane;
                triangle_support_plane.initialize(TriangleType(triangle_geometry));

                // Transform triangle vertices to world space.
                const Vector3d v0(assembly_instance_transform.point_to_parent(v0_as));
                const Vector3d v1(assembly_instance_transform.point_to_parent(v1_as));
                const Vector3d v2(assembly_instance_transform.point_to_parent(v2_as));

                // Compute the geometric normal to the triangle and the area of the triangle.
                Vector3d geometric_normal = cross(v1 - v0, v2 - v0);
                const double geometric_normal_norm = norm(geometric_normal);
                if (geometric_normal_norm == 0.0)
                    continue;
                const double rcp_geometric_normal_norm = 1.0 / geometric_normal_norm;
                const double rcp_area = 2.0 * rcp_geometric_normal_norm;
                const double area = 0.5 * geometric_normal_norm;
                geometric_normal *= rcp_geometric_normal_norm;
                assert(is_normalized(geometric_normal));

                // Retrieve object instance space vertex normals.
                const GVector3& n0_os = tess->m_vertex_normals[triangle.m_n0];
                const GVector3& n1_os = tess->m_vertex_normals[triangle.m_n1];
                const GVector3& n2_os = tess->m_vertex_normals[triangle.m_n2];

                // Transform vertex normals to world space.
                const Vector3d n0(normalize(global_transform.normal_to_parent(n0_os)));
                const Vector3d n1(normalize(global_transform.normal_to_parent(n1_os)));
                const Vector3d n2(normalize(global_transform.normal_to_parent(n2_os)));

                for (size_t side = 0; side < 2; ++side)
                {
                    const Material* material = side == 0 ? front_material : back_material;
                    const Vector3d side_geometric_normal = side == 0 ? geometric_normal : -geometric_normal;
                    const Vector3d side_n0 = side == 0 ? n0 : -n0;
                    const Vector3d side_n1 = side == 0 ? n1 : -n1;
                    const Vector3d side_n2 = side == 0 ? n2 : -n2;

                    // Skip sides without a material.
                    if (material == 0)
                        continue;

                    const EDF* edf = material->get_uncached_edf();

                    // Skip sides without a light-emitting material.
                    if (edf == 0)
                        continue;

                    // Create a light-emitting triangle.
                    EmittingTriangle emitting_triangle;
                    emitting_triangle.m_assembly_instance = &assembly_instance;
                    emitting_triangle.m_object_instance_index = object_instance_index;
                    emitting_triangle.m_region_index = region_index;
                    emitting_triangle.m_triangle_index = triangle_index;
                    emitting_triangle.m_v0 = v0;
                    emitting_triangle.m_v1 = v1;
                    emitting_triangle.m_v2 = v2;
                    emitting_triangle.m_n0 = side_n0;
                    emitting_triangle.m_n1 = side_n1;
                    emitting_triangle.m_n2 = side_n2;
                    emitting_triangle.m_geometric_normal = side_geometric_normal;
                    emitting_triangle.m_triangle_support_plane = triangle_support_plane;
                    emitting_triangle.m_rcp_area = rcp_area;
                    emitting_triangle.m_edf = edf;

                    // Store the light-emitting triangle.
                    const size_t emitting_triangle_index = m_emitting_triangles.size();
                    m_emitting_triangles.push_back(emitting_triangle);

                    // Insert the light-emitting triangle into the CDF.
                    const double importance = m_params.m_importance_sampling ? area : 1.0;
                    m_emitting_triangles_cdf.insert(emitting_triangle_index, importance);

                    // Keep track of the total area of the light-emitting triangles.
                    m_total_emissive_area += area;
                }
            }
        }
    }
}

void LightSampler::sample_non_physical_lights(
    const double                        time,
    const Vector3d&                     s,
    LightSample&                        sample) const
{
    assert(m_non_physical_lights_cdf.valid());

    const EmitterCDF::ItemWeightPair result = m_non_physical_lights_cdf.sample(s[0]);
    const size_t light_index = result.first;
    const double light_prob = result.second;

    sample.m_triangle = 0;
    sample_non_physical_light(
        time,
        Vector2d(s[1], s[2]),
        light_index,
        light_prob,
        sample);

    assert(sample.m_light);
    assert(sample.m_probability > 0.0);
}

void LightSampler::sample_emitting_triangles(
    const double                        time,
    const Vector3d&                     s,
    LightSample&                        sample) const
{
    assert(m_emitting_triangles_cdf.valid());

    const EmitterCDF::ItemWeightPair result = m_emitting_triangles_cdf.sample(s[0]);
    const size_t emitter_index = result.first;
    const double emitter_prob = result.second;

    sample.m_light = 0;
    sample_emitting_triangle(
        time,
        Vector2d(s[1], s[2]),
        emitter_index,
        emitter_prob,
        sample);

    assert(sample.m_triangle);
    assert(sample.m_triangle->m_edf);
    assert(sample.m_probability > 0.0);
}

void LightSampler::sample(
    const double                        time,
    const Vector3d&                     s,
    LightSample&                        sample) const
{
    assert(m_non_physical_lights_cdf.valid() || m_emitting_triangles_cdf.valid());

    if (m_non_physical_lights_cdf.valid())
    {
        if (m_emitting_triangles_cdf.valid())
        {
            if (s[0] < 0.5)
            {
                sample_non_physical_lights(
                    time,
                    Vector3d(s[0] * 2.0, s[1], s[2]),
                    sample);
            }
            else
            {
                sample_emitting_triangles(
                    time,
                    Vector3d((s[0] - 0.5) * 2.0, s[1], s[2]),
                    sample);
            }

            sample.m_probability *= 0.5;
        }
        else sample_non_physical_lights(time, s, sample);
    }
    else sample_emitting_triangles(time, s, sample);
}

void LightSampler::sample_non_physical_light(
    const double                        time,
    const Vector2d&                     s,
    const size_t                        light_index,
    const double                        light_prob,
    LightSample&                        sample) const
{
    // Fetch the light.
    const NonPhysicalLightInfo& light_info = m_non_physical_lights[light_index];
    sample.m_light = light_info.m_light;

    // Evaluate and store the transform of the light.
    sample.m_light_transform = light_info.m_transform_sequence.evaluate(time);

    // Store the probability of choosing this light.
    sample.m_probability = light_prob;
}

void LightSampler::sample_emitting_triangle(
    const double                        time,
    const Vector2d&                     s,
    const size_t                        triangle_index,
    const double                        triangle_prob,
    LightSample&                        sample) const
{
    // Fetch and set the emitting triangle.
    const EmittingTriangle& triangle = m_emitting_triangles[triangle_index];
    sample.m_triangle = &triangle;

    // Uniformly sample the surface of the triangle.
    const Vector3d bary = sample_triangle_uniform(s);

    // Set the barycentric coordinates.
    sample.m_bary[0] = bary[0];
    sample.m_bary[1] = bary[1];

    // Compute the world space position of the sample.
    sample.m_point =
          bary[0] * triangle.m_v0
        + bary[1] * triangle.m_v1
        + bary[2] * triangle.m_v2;

    // Compute the world space shading normal at the position of the sample.
    sample.m_shading_normal =
          bary[0] * triangle.m_n0
        + bary[1] * triangle.m_n1
        + bary[2] * triangle.m_n2;
    sample.m_shading_normal = normalize(sample.m_shading_normal);

    // Set the world space geometric normal.
    sample.m_geometric_normal = triangle.m_geometric_normal;

    // Compute the probability of choosing this sample.
    sample.m_probability = triangle_prob * triangle.m_rcp_area;
}


//
// LightSampler::Parameters class implementation.
//

LightSampler::Parameters::Parameters(const ParamArray& params)
  : m_importance_sampling(params.get_optional<bool>("enable_importance_sampling", false))
{
}

}   // namespace renderer
