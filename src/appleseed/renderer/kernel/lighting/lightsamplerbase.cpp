
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "lightsamplerbase.h"

// appleseed.renderer headers
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/utility/triangle.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// LightSamplerBase class implementation.
//

LightSamplerBase::LightSamplerBase(const ParamArray& params)
  : m_params(params)
  , m_emitting_triangle_hash_table(m_triangle_key_hasher)
  {
  }

void LightSamplerBase::sample_non_physical_light(
    const ShadingRay::Time&             time,
    const size_t                        light_index,
    LightSample&                        light_sample,
    const float                         light_prob) const
{
    // Fetch the light.
    const NonPhysicalLightInfo& light_info = m_non_physical_lights[light_index];
    light_sample.m_light = light_info.m_light;

    // Evaluate and store the transform of the light.
    light_sample.m_light_transform =
          light_info.m_light->get_transform()
        * light_info.m_transform_sequence.evaluate(time.m_absolute);

    // Store the probability density of this light.
    light_sample.m_probability = light_prob;
    assert(light_sample.m_probability > 0.0f);
}

void LightSamplerBase::build_emitting_triangle_hash_table()
{
    const size_t emitting_triangle_count = m_emitting_triangles.size();

    m_emitting_triangle_hash_table.resize(
        emitting_triangle_count > 0 ? next_pow2(emitting_triangle_count) : 0);

    for (size_t i = 0; i < emitting_triangle_count; ++i)
    {
        const EmittingTriangle& emitting_triangle = m_emitting_triangles[i];

        const EmittingTriangleKey emitting_triangle_key(
            emitting_triangle.m_assembly_instance->get_uid(),
            emitting_triangle.m_object_instance_index,
            emitting_triangle.m_triangle_index);

        m_emitting_triangle_hash_table.insert(emitting_triangle_key, &emitting_triangle);
    }
}

void LightSamplerBase::collect_emitting_triangles(
    const AssemblyInstanceContainer&    assembly_instances,
    const TransformSequence&            parent_transform_seq,
    const TriangleHandlingFunction&     triangle_handling)
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
        collect_emitting_triangles(
            assembly.assembly_instances(),
            cumulated_transform_seq,
            triangle_handling);

        // Collect emitting triangles from this assembly instance.
        collect_emitting_triangles(
            assembly,
            assembly_instance,
            cumulated_transform_seq,
            triangle_handling);
    }
}

void LightSamplerBase::collect_emitting_triangles(
    const Assembly&                     assembly,
    const AssemblyInstance&             assembly_instance,
    const TransformSequence&            transform_sequence,
    const TriangleHandlingFunction&     triangle_handling)
{
    // Loop over the object instances of the assembly.
    const size_t object_instance_count = assembly.object_instances().size();
    for (size_t object_instance_index = 0; object_instance_index < object_instance_count; ++object_instance_index)
    {
        // Retrieve the object instance.
        const ObjectInstance* object_instance = assembly.object_instances().get_by_index(object_instance_index);

        // Retrieve the object.
        Object& object = object_instance->get_object();

        // Only consider mesh objects.
        if (strcmp(object.get_model(), MeshObjectFactory().get_model()) != 0)
            continue;

        const MeshObject& mesh = static_cast<const MeshObject&>(object);
        const StaticTriangleTess& tess = mesh.get_static_triangle_tess();

        // Retrieve the materials of the object instance.
        const MaterialArray& front_materials = object_instance->get_front_materials();
        const MaterialArray& back_materials = object_instance->get_back_materials();

        // Skip object instances without light-emitting materials.
        if (!has_emitting_materials(front_materials) && !has_emitting_materials(back_materials))
            continue;

        // Compute the object space to world space transformation.
        // todo: add support for moving light-emitters.
        const Transformd& object_instance_transform = object_instance->get_transform();
        const Transformd& assembly_instance_transform = transform_sequence.get_earliest_transform();
        const Transformd global_transform = assembly_instance_transform * object_instance_transform;

        // Loop over the triangles of the region.
        double object_area = 0.0;
        for (size_t triangle_index = 0, triangle_count = tess.m_primitives.size();
             triangle_index < triangle_count; ++triangle_index)
        {
            // Fetch the triangle.
            const Triangle& triangle = tess.m_primitives[triangle_index];

            // Skip triangles without a material.
            if (triangle.m_pa == Triangle::None)
                continue;

            // Fetch the materials assigned to this triangle.
            const size_t pa_index = static_cast<size_t>(triangle.m_pa);
            const Material* front_material =
                pa_index < front_materials.size() ? front_materials[pa_index] : nullptr;
            const Material* back_material =
                pa_index < back_materials.size() ? back_materials[pa_index] : nullptr;

            // Skip triangles that don't emit light.
            if ((front_material == nullptr || !front_material->has_emission()) &&
                (back_material == nullptr || !back_material->has_emission()))
                continue;

            // Retrieve object instance space vertices of the triangle.
            const GVector3& v0_os = tess.m_vertices[triangle.m_v0];
            const GVector3& v1_os = tess.m_vertices[triangle.m_v1];
            const GVector3& v2_os = tess.m_vertices[triangle.m_v2];

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
            Vector3d geometric_normal = compute_triangle_normal(v0, v1, v2);
            const double geometric_normal_norm = norm(geometric_normal);
            if (geometric_normal_norm == 0.0)
                continue;
            const double rcp_geometric_normal_norm = 1.0 / geometric_normal_norm;
            const double rcp_area = 2.0 * rcp_geometric_normal_norm;
            const double area = 0.5 * geometric_normal_norm;
            geometric_normal *= rcp_geometric_normal_norm;
            assert(is_normalized(geometric_normal));

            // Flip the geometric normal if the object instance requests so.
            if (object_instance->must_flip_normals())
                geometric_normal = -geometric_normal;

            Vector3d n0, n1, n2;

            if (triangle.m_n0 != Triangle::None &&
                triangle.m_n1 != Triangle::None &&
                triangle.m_n2 != Triangle::None)
            {
                // Retrieve object instance space vertex normals.
                const Vector3d n0_os = Vector3d(tess.m_vertex_normals[triangle.m_n0]);
                const Vector3d n1_os = Vector3d(tess.m_vertex_normals[triangle.m_n1]);
                const Vector3d n2_os = Vector3d(tess.m_vertex_normals[triangle.m_n2]);

                // Transform vertex normals to world space.
                n0 = normalize(global_transform.normal_to_parent(n0_os));
                n1 = normalize(global_transform.normal_to_parent(n1_os));
                n2 = normalize(global_transform.normal_to_parent(n2_os));

                // Flip normals if the object instance requests so.
                if (object_instance->must_flip_normals())
                {
                    n0 = -n0;
                    n1 = -n1;
                    n2 = -n2;
                }
            }
            else
            {
                n0 = n1 = n2 = geometric_normal;
            }

            for (size_t side = 0; side < 2; ++side)
            {
                // Retrieve the material; skip sides without a material or without emission.
                const Material* material = side == 0 ? front_material : back_material;
                if (material == nullptr || !material->has_emission())
                    continue;

                // Invoke the triangle handling function.
                const bool accept_triangle =
                    triangle_handling(
                        material,
                        static_cast<float>(area),
                        m_emitting_triangles.size());

                if (accept_triangle)
                {
                    // Create a light-emitting triangle.
                    EmittingTriangle emitting_triangle;
                    emitting_triangle.m_assembly_instance = &assembly_instance;
                    emitting_triangle.m_object_instance_index = object_instance_index;
                    emitting_triangle.m_triangle_index = triangle_index;
                    emitting_triangle.m_v0 = v0;
                    emitting_triangle.m_v1 = v1;
                    emitting_triangle.m_v2 = v2;
                    emitting_triangle.m_n0 = side == 0 ? n0 : -n0;
                    emitting_triangle.m_n1 = side == 0 ? n1 : -n1;
                    emitting_triangle.m_n2 = side == 0 ? n2 : -n2;
                    emitting_triangle.m_geometric_normal = side == 0 ? geometric_normal : -geometric_normal;
                    emitting_triangle.m_triangle_support_plane = triangle_support_plane;
                    emitting_triangle.m_area = static_cast<float>(area);
                    emitting_triangle.m_rcp_area = static_cast<float>(rcp_area);
                    emitting_triangle.m_triangle_prob = 0.0f;   // will be initialized once the emitting triangle CDF is built
                    emitting_triangle.m_material = material;

                    // Store the light-emitting triangle.
                    m_emitting_triangles.push_back(emitting_triangle);

                    // Accumulate the object area for OSL shaders.
                    object_area += area;
                }
            }
        }

        store_object_area_in_shadergroups(
            &assembly_instance,
            object_instance,
            static_cast<float>(object_area),
            front_materials);

        store_object_area_in_shadergroups(
            &assembly_instance,
            object_instance,
            static_cast<float>(object_area),
            back_materials);
    }
}

void LightSamplerBase::collect_non_physical_lights(
    const AssemblyInstanceContainer&    assembly_instances,
    const TransformSequence&            parent_transform_seq,
    const LightHandlingFunction&        light_handling)
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
            cumulated_transform_seq,
            light_handling);

        // Collect lights from this assembly.
        collect_non_physical_lights(
            assembly,
            cumulated_transform_seq,
            light_handling);
    }
}

void LightSamplerBase::collect_non_physical_lights(
    const Assembly&                     assembly,
    const TransformSequence&            transform_sequence,
    const LightHandlingFunction&        light_handling)
{
    for (const_each<LightContainer> i = assembly.lights(); i; ++i)
    {
        // Retrieve the light.
        const Light& light = *i;

        NonPhysicalLightInfo light_info;
        light_info.m_transform_sequence = transform_sequence;
        light_info.m_light = &light;

        light_handling(light_info);
    }
}

void LightSamplerBase::store_object_area_in_shadergroups(
    const AssemblyInstance*             assembly_instance,
    const ObjectInstance*               object_instance,
    const float                         object_area,
    const MaterialArray&                materials)
{
    for (size_t i = 0, e = materials.size(); i < e; ++i)
    {
        if (const Material* m = materials[i])
        {
            if (const ShaderGroup* sg = m->get_uncached_osl_surface())
            {
                if (sg->has_emission())
                    sg->set_surface_area(assembly_instance, object_instance, object_area);
            }
        }
    }
}

void LightSamplerBase::sample_emitting_triangle(
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
    assert(light_sample.m_probability > 0.0f);
}

void LightSamplerBase::sample_emitting_triangles(
    const ShadingRay::Time&             time,
    const Vector3f&                     s,
    LightSample&                        light_sample) const
{
    assert(m_emitting_triangles_cdf.valid());

    const EmitterCDF::ItemWeightPair result = m_emitting_triangles_cdf.sample(s[0]);
    const size_t emitter_index = result.first;
    const float emitter_prob = result.second;

    light_sample.m_light = nullptr;
    sample_emitting_triangle(
        time,
        Vector2f(s[1], s[2]),
        emitter_index,
        emitter_prob,
        light_sample);

    assert(light_sample.m_triangle);
    assert(light_sample.m_probability > 0.0f);
}


//
// LightSamplerBase::Parameters class implementation.
//

LightSamplerBase::Parameters::Parameters(const ParamArray& params)
  : m_importance_sampling(params.get_optional<bool>("enable_importance_sampling", false))
{
}

}   // namespace renderer

