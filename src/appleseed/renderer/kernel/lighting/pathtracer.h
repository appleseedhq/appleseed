
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/subsurfacesampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/objectinstance.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/dual.h"
#include "foundation/math/ray.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// A generic path tracer.
//

template <typename PathVisitor, bool Adjoint>
class PathTracer
  : public foundation::NonCopyable
{
  public:
    PathTracer(
        PathVisitor&            path_visitor,
        const size_t            rr_min_path_length,
        const size_t            max_path_length,
        const size_t            max_iterations = 1000,
        const double            near_start = 0.0);          // abort tracing if the first ray is shorter than this

    size_t trace(
        SamplingContext&        sampling_context,
        const ShadingContext&   shading_context,
        const ShadingRay&       ray,
        const ShadingPoint*     parent_shading_point = 0);

    size_t trace(
        SamplingContext&        sampling_context,
        const ShadingContext&   shading_context,
        const ShadingPoint&     shading_point);

  private:
    struct SubsurfaceSampleVisitor
    {
        enum { MaxSampleCount = 16 };

        ShadingPoint            m_incoming_points[MaxSampleCount];
        float                   m_probabilities[MaxSampleCount];
        size_t                  m_sample_count;

        SubsurfaceSampleVisitor();

        bool visit(
            const BSSRDFSample& bssrdf_sample,
            const ShadingPoint& incoming_point,
            const float         probability);
    };

    PathVisitor&                m_path_visitor;
    const size_t                m_rr_min_path_length;
    const size_t                m_max_path_length;
    const size_t                m_max_iterations;
    const double                m_near_start;

    // Determine whether a ray can pass through a surface with a given alpha value.
    static bool pass_through(
        SamplingContext&        sampling_context,
        const Alpha             alpha);
};


//
// PathTracer class implementation.
//

template <typename PathVisitor, bool Adjoint>
inline PathTracer<PathVisitor, Adjoint>::PathTracer(
    PathVisitor&                path_visitor,
    const size_t                rr_min_path_length,
    const size_t                max_path_length,
    const size_t                max_iterations,
    const double                near_start)
  : m_path_visitor(path_visitor)
  , m_rr_min_path_length(rr_min_path_length)
  , m_max_path_length(max_path_length)
  , m_max_iterations(max_iterations)
  , m_near_start(near_start)
{
}

template <typename PathVisitor, bool Adjoint>
inline size_t PathTracer<PathVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingRay&           ray,
    const ShadingPoint*         parent_shading_point)
{
    ShadingPoint shading_point;
    shading_context.get_intersector().trace(ray, shading_point, parent_shading_point);

    return
        trace(
            sampling_context,
            shading_context,
            shading_point);
}

template <typename PathVisitor, bool Adjoint>
size_t PathTracer<PathVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point)
{
    // Terminate the path if the first hit is too close to the origin.
    if (shading_point.hit() && shading_point.get_distance() < m_near_start)
        return 1;

    ShadingPoint shading_points[2];
    size_t shading_point_index = 0;

    PathVertex vertex(sampling_context);
    vertex.m_path_length = 1;
    vertex.m_throughput.set(1.0f);
    vertex.m_shading_point = &shading_point;
    vertex.m_prev_mode = ScatteringMode::Specular;
    vertex.m_prev_prob = BSDF::DiracDelta;

    // This variable tracks the beginning of the path segment inside the current medium.
    // While it is properly initialized when entering a medium, we also initialize it
    // here to silence a gcc warning.
    foundation::Vector3d medium_start(0.0);

    size_t iterations = 0;

    while (true)
    {
#ifndef NDEBUG
        // Save the sampling context at the beginning of the iteration.
        const SamplingContext backup_sampling_context(sampling_context);

        // Resume execution here to reliably reproduce problems downstream.
        sampling_context = backup_sampling_context;
#endif

        // Put a hard limit on the number of iterations.
        if (++iterations >= m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking path trace loop.",
                foundation::pretty_int(m_max_iterations).c_str());
            break;
        }

        // Retrieve the ray.
        const ShadingRay& ray = vertex.get_ray();
        assert(foundation::is_normalized(ray.m_dir));

        // Compute the outgoing direction at this vertex.
        // Derivation:
        //   ray.dir + d(ray.dir)/dx = ray.dx.dir
        //   outgoing = -ray.dir
        //   d(outgoing)/dx = d(-ray.dir)/dx = ray.dir - ray.dx.dir
        vertex.m_outgoing =
            ray.m_has_differentials
                ? foundation::Dual3d(
                    -ray.m_dir,
                    ray.m_dir - ray.m_rx.m_dir,
                    ray.m_dir - ray.m_ry.m_dir)
                : foundation::Dual3d(-ray.m_dir);

        // Terminate the path if the ray didn't hit anything.
        if (!vertex.m_shading_point->hit())
        {
            m_path_visitor.visit_environment(vertex);
            break;
        }

        // Retrieve the material at the shading point.
        const Material* material = vertex.get_material();

        // Terminate the path if the surface has no material.
        if (material == 0)
            break;

        // Retrieve the material's render data.
        const Material::RenderData& material_data = material->get_render_data();

        // Retrieve the object instance at the shading point.
        const ObjectInstance& object_instance = vertex.m_shading_point->get_object_instance();

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = vertex.m_shading_point->is_entering();

        // Handle false intersections.
        if (ray.get_current_medium() &&
            ray.get_current_medium()->m_object_instance->get_medium_priority() > object_instance.get_medium_priority() &&
            material_data.m_bsdf != 0)
        {
            // Construct a ray that continues in the same direction as the incoming ray.
            ShadingRay next_ray(
                vertex.get_point(),
                ray.m_dir,
                ray.m_time,
                ray.m_flags,
                ray.m_depth);

            // Advance the differentials if the ray has them.
            if (ray.m_has_differentials)
            {
                next_ray.m_rx = ray.m_rx;
                next_ray.m_ry = ray.m_ry;
                next_ray.m_rx.m_org = ray.m_rx.point_at(ray.m_tmax);
                next_ray.m_ry.m_org = ray.m_ry.point_at(ray.m_tmax);
                next_ray.m_has_differentials = true;
            }

            // Initialize the ray's medium list.
            if (entering)
            {
#ifdef APPLESEED_WITH_OSL
                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        *vertex.m_shading_point);
                }
#endif
                InputEvaluator input_evaluator(shading_context.get_texture_cache());
                material_data.m_bsdf->evaluate_inputs(
                    shading_context,
                    input_evaluator,
                    *vertex.m_shading_point);
                const float ior =
                    material_data.m_bsdf->sample_ior(
                        sampling_context,
                        input_evaluator.data());
                next_ray.add_medium(ray, &object_instance, material_data.m_bsdf, ior);
            }
            else next_ray.remove_medium(ray, &object_instance);

            // Trace the ray.
            shading_points[shading_point_index].clear();
            shading_context.get_intersector().trace(
                next_ray,
                shading_points[shading_point_index],
                vertex.m_shading_point);

            // Update the pointers to the shading points and loop.
            vertex.m_shading_point = &shading_points[shading_point_index];
            shading_point_index = 1 - shading_point_index;
            continue;
        }

        // Handle alpha mapping.
        if (vertex.m_path_length > 1)
        {
            Alpha alpha = vertex.m_shading_point->get_alpha();

#ifdef APPLESEED_WITH_OSL
            // Apply OSL transparency if needed.
            if (material_data.m_shader_group &&
                material_data.m_shader_group->has_transparency())
            {
                Alpha a;
                shading_context.execute_osl_transparency(
                    *material_data.m_shader_group,
                    *vertex.m_shading_point,
                    a);
                alpha *= a;
            }
#endif

            if (pass_through(sampling_context, alpha))
            {
                // Construct a ray that continues in the same direction as the incoming ray.
                ShadingRay next_ray(
                    vertex.get_point(),
                    ray.m_dir,
                    ray.m_time,
                    ray.m_flags,
                    ray.m_depth);   // ray depth does not increase when passing through an alpha-mapped surface

                // Advance the differentials if the ray has them.
                if (ray.m_has_differentials)
                {
                    next_ray.m_rx = ray.m_rx;
                    next_ray.m_ry = ray.m_ry;
                    next_ray.m_rx.m_org = ray.m_rx.point_at(ray.m_tmax);
                    next_ray.m_ry.m_org = ray.m_ry.point_at(ray.m_tmax);
                    next_ray.m_has_differentials = true;
                }

                // Inherit the medium list from the parent ray.
                next_ray.copy_media_from(ray);

                // Trace the ray.
                shading_points[shading_point_index].clear();
                shading_context.get_intersector().trace(
                    next_ray,
                    shading_points[shading_point_index],
                    vertex.m_shading_point);

                // Update the pointers to the shading points and loop.
                vertex.m_shading_point = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;
                continue;
            }
        }

#ifdef APPLESEED_WITH_OSL
        // Execute the OSL shader if there is one.
        if (material_data.m_shader_group)
        {
            shading_context.execute_osl_shading(
                *material_data.m_shader_group,
                *vertex.m_shading_point);
        }
#endif

        // Retrieve the EDF, the BSDF and the BSSRDF.
        vertex.m_edf =
            vertex.m_shading_point->is_curve_primitive() ? 0 : material_data.m_edf;
        vertex.m_bsdf = material_data.m_bsdf;
        vertex.m_bssrdf = material_data.m_bssrdf;

        // If there is both a BSDF and a BSSRDF, pick one to extend the path.
        if (vertex.m_bsdf && vertex.m_bssrdf)
        {
            sampling_context.split_in_place(1, 1);
            if (sampling_context.next2<float>() < 0.5f)
                vertex.m_bsdf = 0;
            else vertex.m_bssrdf = 0;
            vertex.m_throughput *= 2.0f;
        }

        // Evaluate the inputs of the BSDF.
        InputEvaluator bsdf_input_evaluator(shading_context.get_texture_cache());
        if (vertex.m_bsdf)
        {
            vertex.m_bsdf->evaluate_inputs(
                shading_context,
                bsdf_input_evaluator,
                *vertex.m_shading_point);
            vertex.m_bsdf_data = bsdf_input_evaluator.data();
        }

        // Evaluate the inputs of the BSSRDF.
        InputEvaluator bssrdf_input_evaluator(shading_context.get_texture_cache());
        if (vertex.m_bssrdf)
        {
            vertex.m_bssrdf->evaluate_inputs(
                shading_context,
                bssrdf_input_evaluator,
                *vertex.m_shading_point);
            vertex.m_bssrdf_data = bssrdf_input_evaluator.data();
        }

        // If we picked the BSSRDF, find an incoming point.
        // The subsurface visitor must live outside the 'if' block below because
        // it owns the incoming points that were found during subsurface sampling,
        // one of which will become the parent point for the next ray.
        SubsurfaceSampleVisitor subsurf_visitor;
        if (vertex.m_bssrdf)
        {
            // Find possible incoming points.
            const SubsurfaceSampler sampler(shading_context);
            sampler.sample(
                sampling_context,
                *vertex.m_shading_point,
                *vertex.m_bssrdf,
                vertex.m_bssrdf_data,
                subsurf_visitor);

            // Terminate the path if no incoming point could be found.
            if (subsurf_visitor.m_sample_count == 0)
                break;

            // Select one of the incoming points at random.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            const size_t i = foundation::truncate<size_t>(s * subsurf_visitor.m_sample_count);
            vertex.m_incoming_point = &subsurf_visitor.m_incoming_points[i];
            vertex.m_incoming_point_prob = subsurf_visitor.m_probabilities[i] / subsurf_visitor.m_sample_count;
        }

        // Pass this vertex to the path visitor.
        vertex.m_cos_on = foundation::dot(vertex.m_outgoing.get_value(), vertex.get_shading_normal());
        m_path_visitor.visit_vertex(vertex);

        // Honor the user bounce limit.
        if (vertex.m_path_length >= m_max_path_length)
            break;

        Spectrum value;
        const ShadingPoint* parent_shading_point;
        foundation::Dual3d incoming;

        if (vertex.m_bsdf)
        {
            // Sample the BSDF.
            BSDFSample sample(*vertex.m_shading_point, vertex.m_outgoing);
            vertex.m_bsdf->sample(
                sampling_context,
                vertex.m_bsdf_data,
                Adjoint,
                true,       // multiply by |cos(incoming, normal)|
                sample);

            // Terminate the path if it gets absorbed.
            if (sample.m_mode == ScatteringMode::Absorption)
                break;

            // Terminate the path if this scattering event is not accepted.
            if (!m_path_visitor.accept_scattering(vertex.m_prev_mode, sample.m_mode))
                break;

            // Compute the path throughput multiplier.
            value = sample.m_value;
            if (sample.m_probability != BSDF::DiracDelta)
                value /= sample.m_probability;

            // Properties of this scattering event.
            vertex.m_prev_mode = sample.m_mode;
            vertex.m_prev_prob = sample.m_probability;

            // Origin and direction of the scattered ray.
            parent_shading_point = vertex.m_shading_point;
            incoming = foundation::Dual3d(sample.m_incoming);
        }
        else if (vertex.m_bssrdf)
        {
            // Pick the direction of the scattered ray at random.
            sampling_context.split_in_place(2, 1);
            const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
            foundation::Vector3f incoming_vector = foundation::sample_hemisphere_cosine(s);
            const float cos_in = incoming_vector.y;
            const float incoming_prob = cos_in * foundation::RcpPi<float>();
            incoming_vector = vertex.m_incoming_point->get_shading_basis().transform_to_parent(incoming_vector);
            if (vertex.m_incoming_point->get_side() == ObjectInstance::BackSide)
                incoming_vector = -incoming_vector;
            incoming = foundation::Dual3d(foundation::Vector3d(incoming_vector));

            // Evaluate the BSSRDF.
            vertex.m_bssrdf->evaluate(
                vertex.m_bssrdf_data,
                *vertex.m_shading_point,
                foundation::Vector3f(vertex.m_outgoing.get_value()),
                *vertex.m_incoming_point,
                incoming_vector,
                value);

            // Compute the path throughput multiplier.
            value *= cos_in / vertex.m_incoming_point_prob;

            // Properties of this scattering event.
            vertex.m_prev_mode = ScatteringMode::Diffuse;
            vertex.m_prev_prob = incoming_prob;

            // Origin of the scattered ray.
            parent_shading_point = vertex.m_incoming_point;
        }
        else
        {
            // No scattering possible, terminate the path.
            break;
        }

        // Update the path throughput.
        vertex.m_throughput *= value;

        // Use Russian Roulette to cut the path without introducing bias.
        if (vertex.m_path_length >= m_rr_min_path_length)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();

            // Compute the probability of extending this path.
            const float scattering_prob = std::min(foundation::max_value(value), 1.0f);

            // Russian Roulette.
            if (!foundation::pass_rr(scattering_prob, s))
                break;

            // Adjust throughput to account for terminated paths.
            assert(scattering_prob > 0.0f);
            vertex.m_throughput /= scattering_prob;
        }

        // Keep track of the number of bounces.
        ++vertex.m_path_length;

        // Construct the scattered ray.
        ShadingRay next_ray(
            parent_shading_point->get_biased_point(incoming.get_value()),
            incoming.get_value(),
            ray.m_time,
            ScatteringMode::get_vis_flags(vertex.m_prev_mode),
            ray.m_depth + 1);
        next_ray.m_dir = foundation::improve_normalization<2>(next_ray.m_dir);

        // Compute scattered ray differentials.
        if (incoming.has_derivatives())
        {
            next_ray.m_rx.m_org = next_ray.m_org + vertex.m_shading_point->get_dpdx();
            next_ray.m_ry.m_org = next_ray.m_org + vertex.m_shading_point->get_dpdy();
            next_ray.m_rx.m_dir = next_ray.m_dir + incoming.get_dx();
            next_ray.m_ry.m_dir = next_ray.m_dir + incoming.get_dy();
            next_ray.m_has_differentials = true;
        }

        // Build the medium list of the scattered ray.
        const foundation::Vector3d& geometric_normal = vertex.get_geometric_normal();
        const bool crossing_interface =
            foundation::dot(vertex.m_outgoing.get_value(), geometric_normal) *
            foundation::dot(next_ray.m_dir, geometric_normal) < 0.0;
        if (vertex.m_bsdf != 0 && crossing_interface)
        {
            // Refracted ray: inherit the medium list of the parent ray and add/remove the current medium.
            if (entering)
            {
                const float ior =
                    vertex.m_bsdf->sample_ior(
                        sampling_context,
                        bsdf_input_evaluator.data());
                next_ray.add_medium(ray, &object_instance, vertex.m_bsdf, ior);
            }
            else next_ray.remove_medium(ray, &object_instance);

            // Compute absorption for the segment inside the medium the path is leaving.
            const ShadingRay::Medium* prev_medium = ray.get_current_medium();
            if (prev_medium != 0 &&
                prev_medium != next_ray.get_current_medium() &&
                prev_medium->m_bsdf != 0)
            {
                prev_medium->m_bsdf->evaluate_inputs(
                    shading_context,
                    bsdf_input_evaluator,
                    *vertex.m_shading_point);
                const float distance = static_cast<float>(norm(vertex.get_point() - medium_start));
                Spectrum absorption;
                prev_medium->m_bsdf->compute_absorption(
                    bsdf_input_evaluator.data(),
                    distance,
                    absorption);
                vertex.m_throughput *= absorption;
            }

            medium_start = vertex.get_point();
        }
        else
        {
            // Reflected ray: inherit the medium list of the parent ray.
            next_ray.copy_media_from(ray);
        }

        // Trace the ray.
        shading_points[shading_point_index].clear();
        shading_context.get_intersector().trace(
            next_ray,
            shading_points[shading_point_index],
            parent_shading_point);

        // Update the pointers to the shading points.
        vertex.m_shading_point = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;
    }

    return vertex.m_path_length;
}

template <typename PathVisitor, bool Adjoint>
inline bool PathTracer<PathVisitor, Adjoint>::pass_through(
    SamplingContext&            sampling_context,
    const Alpha                 alpha)
{
    if (alpha[0] <= 0.0f)
        return true;

    if (alpha[0] >= 1.0f)
        return false;

    sampling_context.split_in_place(1, 1);

    return sampling_context.next2<float>() >= alpha[0];
}


//
// PathTracer::SubsurfaceSampleVisitor class implementation.
//

template <typename PathVisitor, bool Adjoint>
PathTracer<PathVisitor, Adjoint>::SubsurfaceSampleVisitor::SubsurfaceSampleVisitor()
  : m_sample_count(0)
{
}

template <typename PathVisitor, bool Adjoint>
bool PathTracer<PathVisitor, Adjoint>::SubsurfaceSampleVisitor::visit(
    const BSSRDFSample&         bssrdf_sample,
    const ShadingPoint&         incoming_point,
    const float                 probability)
{
    if (m_sample_count >= MaxSampleCount)
    {
        // Stop visiting samples.
        return false;
    }

    m_incoming_points[m_sample_count] = incoming_point;
    m_probabilities[m_sample_count] = probability;
    ++m_sample_count;

    // Continue visiting samples.
    return true;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
