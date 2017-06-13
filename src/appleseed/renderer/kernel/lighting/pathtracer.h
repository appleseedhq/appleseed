
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/phasefunction/phasefunction.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/dual.h"
#include "foundation/math/ray.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/arena.h"
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

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
class PathTracer
  : public foundation::NonCopyable
{
  public:
    PathTracer(
        PathVisitor&            path_visitor,
        VolumeVisitor&          volume_visitor,
        const size_t            rr_min_path_length,
        const size_t            max_bounces,
        const size_t            max_diffuse_bounces,
        const size_t            max_glossy_bounces,
        const size_t            max_specular_bounces,
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

    // This method performs raymarching across the volume.
    // Returns the number of scattering events occured during the marching.
    size_t march(
        SamplingContext&        sampling_context,
        const ShadingContext&   shading_context,
        const ShadingRay&       ray,
        PathVertex&             vertex,
        ShadingPoint&           shading_point);

  private:
    PathVisitor&                m_path_visitor;
    VolumeVisitor&              m_volume_visitor;
    const size_t                m_rr_min_path_length;
    const size_t                m_max_bounces;
    const size_t                m_max_diffuse_bounces;
    const size_t                m_max_glossy_bounces;
    const size_t                m_max_specular_bounces;
    const size_t                m_max_iterations;
    const double                m_near_start;
    size_t                      m_diffuse_bounces;
    size_t                      m_glossy_bounces;
    size_t                      m_specular_bounces;
    size_t                      m_iterations;

    // Determine whether a ray can pass through a surface with a given alpha value.
    static bool pass_through(
        SamplingContext&        sampling_context,
        const Alpha             alpha);

    // Apply path visitor and sample BSDF in a given path vertex.
    // If all checks are passed, build a bounced ray that continues in the sampled direction
    // and return true, otherwise return false.
    bool process_bounce(
        SamplingContext&    sampling_context,
        PathVertex&         vertex,
        BSDFSample&         sample,
        ShadingRay&         ray);

};


//
// PathTracer class implementation.
//

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline PathTracer<PathVisitor, VolumeVisitor, Adjoint>::PathTracer(
    PathVisitor&                path_visitor,
    VolumeVisitor&              volume_visitor,
    const size_t                rr_min_path_length,
    const size_t                max_bounces,
    const size_t                max_diffuse_bounces,
    const size_t                max_glossy_bounces,
    const size_t                max_specular_bounces,
    const size_t                max_iterations,
    const double                near_start)
  : m_path_visitor(path_visitor)
  , m_volume_visitor(volume_visitor)
  , m_rr_min_path_length(rr_min_path_length)
  , m_max_bounces(max_bounces)
  , m_max_diffuse_bounces(max_diffuse_bounces)
  , m_max_glossy_bounces(max_glossy_bounces)
  , m_max_specular_bounces(max_specular_bounces)
  , m_max_iterations(max_iterations)
  , m_near_start(near_start)
{
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline size_t PathTracer<PathVisitor, VolumeVisitor, Adjoint>::trace(
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

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
size_t PathTracer<PathVisitor, VolumeVisitor, Adjoint>::trace(
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
    vertex.m_scattering_modes = ScatteringMode::All;
    vertex.m_throughput.set(1.0f);
    vertex.m_shading_point = &shading_point;
    vertex.m_prev_mode = ScatteringMode::Specular;
    vertex.m_prev_prob = BSDF::DiracDelta;

    // This variable tracks the beginning of the path segment inside the current medium.
    // While it is properly initialized when entering a medium, we also initialize it
    // here to silence a gcc warning.
    foundation::Vector3d medium_start(0.0);

    m_diffuse_bounces = 0;
    m_glossy_bounces = 0;
    m_specular_bounces = 0;
    m_iterations = 0;

    while (true)
    {
        shading_context.get_arena().clear();

#ifndef NDEBUG
        // Save the sampling context at the beginning of the iteration.
        const SamplingContext backup_sampling_context(sampling_context);

        // Resume execution here to reliably reproduce problems downstream.
        sampling_context = backup_sampling_context;
#endif

        // Put a hard limit on the number of iterations.
        if (m_iterations++ == m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking path tracing loop.",
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
            m_path_visitor.on_miss(vertex);
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
                // Execute the OSL shader if there is one.
                if (material_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *material_data.m_shader_group,
                        *vertex.m_shading_point);
                }

                const void* data = material_data.m_bsdf->evaluate_inputs(shading_context, *vertex.m_shading_point);
                const float ior = material_data.m_bsdf->sample_ior(sampling_context, data);
                next_ray.add_medium(ray, &object_instance, material, ior);
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

        // Execute the OSL shader if there is one.
        if (material_data.m_shader_group)
        {
            shading_context.execute_osl_shading(
                *material_data.m_shader_group,
                *vertex.m_shading_point);
        }

        // Retrieve the EDF, the BSDF and the BSSRDF.
        vertex.m_edf =
            vertex.m_shading_point->is_curve_primitive() ? 0 : material_data.m_edf;
        vertex.m_bsdf = material_data.m_bsdf;
        vertex.m_bssrdf = material_data.m_bssrdf;

        // We allow materials with both a BSDF and a BSSRDF.
        // When both are present, pick one to extend the path.
        if (vertex.m_bsdf && vertex.m_bssrdf)
        {
            sampling_context.split_in_place(1, 1);
            if (sampling_context.next2<float>() < 0.5f)
                vertex.m_bsdf = 0;
            else vertex.m_bssrdf = 0;
            vertex.m_throughput *= 2.0f;
        }

        // Evaluate the inputs of the BSDF.
        if (vertex.m_bsdf)
            vertex.m_bsdf_data = vertex.m_bsdf->evaluate_inputs(shading_context, *vertex.m_shading_point);

        // Evaluate the inputs of the BSSRDF.
        if (vertex.m_bssrdf)
            vertex.m_bssrdf_data = vertex.m_bssrdf->evaluate_inputs(shading_context, *vertex.m_shading_point);

        BSDFSample bsdf_sample(
            vertex.m_shading_point,
            foundation::Dual3f(vertex.m_outgoing));

        // Subsurface scattering.
        if (vertex.m_bssrdf)
        {
            // Sample the BSSRDF and terminate the path if no incoming point is found.
            BSSRDFSample bssrdf_sample;
            if (!vertex.m_bssrdf->sample(
                    shading_context,
                    sampling_context,
                    vertex.m_bssrdf_data,
                    *vertex.m_shading_point,
                    foundation::Vector3f(vertex.m_outgoing.get_value()),
                    bssrdf_sample,
                    bsdf_sample))
                break;

            // Update the path throughput.
            vertex.m_throughput *= bssrdf_sample.m_value;
            vertex.m_throughput /= bssrdf_sample.m_probability;

            // Switch to the BSSRDF's BRDF.
            vertex.m_shading_point = &bssrdf_sample.m_incoming_point;
            vertex.m_bsdf = bssrdf_sample.m_brdf;
            vertex.m_bsdf_data = bssrdf_sample.m_brdf_data;
        }

        // Let the path visitor handle a hit.
        // cos(outgoing, normal) is used for changes of probability measure. In the case of subsurface
        // scattering, we purposely compute it at the outgoing vertex even though it may be used at
        // the incoming vertex if we need to change the probability of reaching this vertex by BSDF
        // sampling from projected solid angle measure to area measure.
        vertex.m_cos_on = foundation::dot(vertex.m_outgoing.get_value(), vertex.get_shading_normal());
        m_path_visitor.on_hit(vertex);

        // Use Russian Roulette to cut the path without introducing bias.
        float scattering_prob;
        if (vertex.m_path_length > m_rr_min_path_length)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();

            // Compute the probability of extending this path.
            // todo: make max scattering prob lower (0.99) to avoid getting stuck?
            scattering_prob = std::min(foundation::max_value(vertex.m_throughput), 1.0f);

            // Russian Roulette.
            if (!foundation::pass_rr(scattering_prob, s))
                break;

            // Adjust throughput to account for terminated paths.
            assert(scattering_prob > 0.0f);
            vertex.m_throughput /= scattering_prob;
        }

        // Honor the global bounce limit.
        const size_t bounces = vertex.m_path_length - 1;
        if (bounces == m_max_bounces)
            break;

        // Terminate the path if no above-surface scattering possible.
        if (vertex.m_bsdf == 0 && material->get_render_data().m_phase_function == 0)
            break;

        ShadingRay next_ray;
        bool continue_path;
        if (vertex.m_bsdf == nullptr)
        {
            // If there is no BSDF, just continue the current ray without increasing its depth.
            next_ray = ShadingRay(
                vertex.get_point(),
                ray.m_dir,
                ray.m_time,
                ray.m_flags,
                ray.m_depth);
            continue_path = true;
        }
        else
        {
            // If there is a BSDF, compute the bounce.
            continue_path = process_bounce(sampling_context, vertex, bsdf_sample, next_ray);
        }

        // Terminate the path if there is BSDF, but the scattering event is not accepted.
        if (!continue_path) break;

        // Build the medium list of the scattered ray.
        const foundation::Vector3d& geometric_normal = vertex.get_geometric_normal();
        const bool crossing_interface =
            foundation::dot(vertex.m_outgoing.get_value(), geometric_normal) *
            foundation::dot(next_ray.m_dir, geometric_normal) < 0.0;
        if (crossing_interface)
        {
            // Ray goes under the surface:
            // inherit the medium list of the parent ray and add/remove the current medium.
            if (entering)
            {
                const float ior = (vertex.m_bsdf == nullptr) ? 1.0f :
                    vertex.m_bsdf->sample_ior(
                        sampling_context,
                        vertex.m_bsdf_data);
                next_ray.add_medium(ray, &object_instance, vertex.get_material(), ior);
            }
            else next_ray.remove_medium(ray, &object_instance);
        }
        else
        {
            // Reflected ray:
            // inherit the medium list of the parent ray.
            next_ray.copy_media_from(ray);
        }

        // Compute absorption for the segment inside the medium.
        const ShadingRay::Medium* prev_medium = ray.get_current_medium();
        if (prev_medium != 0)
        {
            const Material::RenderData& render_data = prev_medium->m_material->get_render_data();

            if (render_data.m_bsdf)
            {
                // Execute the OSL shader if there is one.
                if (render_data.m_shader_group)
                {
                    shading_context.execute_osl_shading(
                        *render_data.m_shader_group,
                        *vertex.m_shading_point);
                }

                const void* data = render_data.m_bsdf->evaluate_inputs(shading_context, *vertex.m_shading_point);
                const float distance = static_cast<float>(norm(vertex.get_point() - medium_start));
                Spectrum absorption;
                render_data.m_bsdf->compute_absorption(data, distance, absorption);
                vertex.m_throughput *= absorption;
            }
        }

        medium_start = vertex.get_point();

        shading_points[shading_point_index].clear();
        const ShadingRay::Medium* current_medium = next_ray.get_current_medium();
        if (current_medium != 0 && 
            current_medium->m_material->get_render_data().m_phase_function != 0)
        {
            // This ray is being cast into a participating medium:
            march(sampling_context, shading_context,
                next_ray, vertex, shading_points[shading_point_index]); 
        }
        else
        {
            // This ray is being cast into an ordinary medium:
            shading_context.get_intersector().trace(
                next_ray,
                shading_points[shading_point_index],
                vertex.m_shading_point);
        }

        // Update the pointers to the shading points.
        vertex.m_shading_point = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;
    }

    return vertex.m_path_length;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::process_bounce(
    SamplingContext&    sampling_context,
    PathVertex&         vertex,
    BSDFSample&         sample,
    ShadingRay&         ray)
{
    // Determine which scattering modes are still enabled.
    if (m_diffuse_bounces >= m_max_diffuse_bounces)
        vertex.m_scattering_modes &= ~ScatteringMode::Diffuse;
    if (m_glossy_bounces >= m_max_glossy_bounces)
        vertex.m_scattering_modes &= ~ScatteringMode::Glossy;
    if (m_specular_bounces >= m_max_specular_bounces)
        vertex.m_scattering_modes &= ~ScatteringMode::Specular;

    // Let the path visitor handle the scattering event.
    m_path_visitor.on_scatter(vertex);

    // Terminate the path if all scattering modes are disabled.
    if (vertex.m_scattering_modes == ScatteringMode::None)
        return false;

    // Above-surface scattering.
    if (vertex.m_bssrdf == nullptr)
    {
        vertex.m_bsdf->sample(
            sampling_context,
            vertex.m_bsdf_data,
            Adjoint,
            true,       // multiply by |cos(incoming, normal)|
            vertex.m_scattering_modes,
            sample);
    }

    // Terminate the path if it gets absorbed.
    if (sample.m_mode == ScatteringMode::None)
        return false;

    // Terminate the path if this scattering event is not accepted.
    if (!m_path_visitor.accept_scattering(vertex.m_prev_mode, sample.m_mode))
        return false;

    // Save the scattering properties for MIS at light-emitting vertices.
    vertex.m_prev_mode = sample.m_mode;
    vertex.m_prev_prob = sample.m_probability;

    // Update path throughput.
    if (sample.m_probability != BSDF::DiracDelta)
        sample.m_value /= sample.m_probability;
    vertex.m_throughput *= sample.m_value;

    // Update bounce counters.
    ++vertex.m_path_length;
    m_diffuse_bounces +=  (sample.m_mode >> ScatteringMode::DiffuseBitShift)  & 1;
    m_glossy_bounces +=   (sample.m_mode >> ScatteringMode::GlossyBitShift)   & 1;
    m_specular_bounces += (sample.m_mode >> ScatteringMode::SpecularBitShift) & 1;

    // Construct the scattered ray.
    const foundation::Vector3d incoming(sample.m_incoming.get_value());
    ray = ShadingRay(
        vertex.m_shading_point->get_biased_point(incoming),
        incoming,
        ray.m_time,
        ScatteringMode::get_vis_flags(sample.m_mode),
        ray.m_depth + 1);
    ray.m_dir = foundation::improve_normalization<2>(ray.m_dir);

    // Compute scattered ray differentials.
    if (sample.m_incoming.has_derivatives())
    {
        ray.m_rx.m_org = ray.m_org + vertex.m_shading_point->get_dpdx();
        ray.m_ry.m_org = ray.m_org + vertex.m_shading_point->get_dpdy();
        ray.m_rx.m_dir = ray.m_dir + foundation::Vector3d(sample.m_incoming.get_dx());
        ray.m_ry.m_dir = ray.m_dir + foundation::Vector3d(sample.m_incoming.get_dy());
        ray.m_has_differentials = true;
    }

    return true;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
size_t PathTracer<PathVisitor, VolumeVisitor, Adjoint>::march(
    SamplingContext&        sampling_context,
    const ShadingContext&   shading_context,
    const ShadingRay&       ray,
    PathVertex&             vertex,
    ShadingPoint&           exit_point)
{
    const ShadingRay::Medium* medium = ray.get_current_medium();
    const PhaseFunction* phase_function = medium->m_material->get_render_data().m_phase_function;

    shading_context.get_intersector().trace(
        ray,
        exit_point,
        vertex.m_shading_point);

    const ShadingRay& volume_ray = exit_point.get_ray();

    void* data = phase_function->evaluate_inputs(shading_context, volume_ray);
    phase_function->prepare_inputs(shading_context.get_arena(), volume_ray, data);

    m_volume_visitor.visit(volume_ray);

    if (vertex.m_shading_point->hit())
    {
        Spectrum transmission;
        phase_function->evaluate_transmission(volume_ray, data, transmission);

        vertex.m_throughput *= transmission;
    }

    return 0; // number of scattering events
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::pass_through(
    SamplingContext&            sampling_context,
    const Alpha                 alpha)
{
    if (alpha[0] >= 1.0f)
        return false;

    if (alpha[0] <= 0.0f)
        return true;

    sampling_context.split_in_place(1, 1);

    return sampling_context.next2<float>() >= alpha[0];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
