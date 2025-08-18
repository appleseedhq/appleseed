
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovcomponents.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/lighting/lightpathstream.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/volume/volume.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/dual.h"
#include "foundation/math/ray.h"
#include "foundation/math/rr.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/string/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// A generic path tracer.
//
// The PathVisitor class must conform to the following prototype:
//
//   struct PathVisitor
//   {
//       void on_first_diffuse_bounce(
//           const PathVertex&              vertex,
//           const Spectrum&                albedo);
//
//       bool accept_scattering(
//           const ScatteringMode::Mode     prev_mode,
//           const ScatteringMode::Mode     next_mode) const;
//
//       void on_miss(const PathVertex& vertex);
//       void on_hit(const PathVertex& vertex);
//       void on_scatter(PathVertex& vertex);
//   };
//
// The VolumeVisitor class must conform to the following prototype:
//
//   struct VolumeVisitor
//   {
//       bool accept_scattering(
//           const ScatteringMode::Mode  prev_mode);
//
//       void on_scatter(PathVertex& vertex);
//       void visit_ray(PathVertex& vertex, const ShadingRay& volume_ray);
//   };
//

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
class PathTracer
  : public foundation::NonCopyable
{
  public:
    PathTracer(
        PathVisitor&                path_visitor,
        VolumeVisitor&              volume_visitor,
        const size_t                rr_min_path_length,
        const size_t                max_bounces,
        const size_t                max_diffuse_bounces,
        const size_t                max_glossy_bounces,
        const size_t                max_specular_bounces,
        const size_t                max_volume_bounces,
        const bool                  clamp_roughness,
        const size_t                max_iterations = 1000,
        const double                near_start = 0.0);          // abort tracing if the first ray is shorter than this

    size_t trace(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingRay&           ray,
        const ShadingPoint*         parent_shading_point = nullptr,
        const bool                  clear_arena = true);

    size_t trace(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        const bool                  clear_arena = true);

    const ShadingPoint& get_path_vertex(const size_t i) const;

  private:
    PathVisitor&                    m_path_visitor;
    VolumeVisitor&                  m_volume_visitor;
    const size_t                    m_rr_min_path_length;
    const size_t                    m_max_bounces;
    const size_t                    m_max_diffuse_bounces;
    const size_t                    m_max_glossy_bounces;
    const size_t                    m_max_specular_bounces;
    const size_t                    m_max_volume_bounces;
    const bool                      m_clamp_roughness;
    const size_t                    m_max_iterations;
    const double                    m_near_start;
    size_t                          m_diffuse_bounces;
    size_t                          m_glossy_bounces;
    size_t                          m_specular_bounces;
    size_t                          m_volume_bounces;
    size_t                          m_iterations;
    foundation::Arena               m_shading_point_arena;

    // Determine whether a ray can pass through a surface with a given alpha value.
    static bool pass_through(
        SamplingContext&            sampling_context,
        const Alpha                 alpha);

    // Use Russian Roulette to cut the path without introducing bias.
    bool continue_path_rr(
        SamplingContext&            sampling_context,
        PathVertex&                 vertex);

    // Apply path visitor and sample BSDF in a given path vertex.
    // If all checks are passed, build a bounced ray that continues in the sampled direction
    // and return true, otherwise return false.
    bool process_bounce(
        SamplingContext&            sampling_context,
        PathVertex&                 vertex,
        const BSDF::LocalGeometry&  local_geometry,
        BSDFSample&                 sample,
        ShadingRay&                 ray);

    // This method performs raymarching across the volume.
    // Returns whether the path should be continued.
    bool march(
        SamplingContext&            sampling_context,
        const ShadingContext&       shading_context,
        const ShadingRay&           ray,
        PathVertex&                 vertex,
        ShadingPoint&               shading_point);
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
    const size_t                max_volume_bounces,
    const bool                  clamp_roughness,
    const size_t                max_iterations,
    const double                near_start)
  : m_path_visitor(path_visitor)
  , m_volume_visitor(volume_visitor)
  , m_rr_min_path_length(rr_min_path_length)
  , m_max_bounces(max_bounces)
  , m_max_diffuse_bounces(max_diffuse_bounces)
  , m_max_glossy_bounces(max_glossy_bounces)
  , m_max_specular_bounces(max_specular_bounces)
  , m_max_volume_bounces(max_volume_bounces)
  , m_clamp_roughness(clamp_roughness)
  , m_max_iterations(max_iterations)
  , m_near_start(near_start)
{
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline size_t PathTracer<PathVisitor, VolumeVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingRay&           ray,
    const ShadingPoint*         parent_shading_point,
    const bool                  clear_arena)
{
    ShadingPoint shading_point;
    shading_context.get_intersector().trace(ray, shading_point, parent_shading_point);

    return
        trace(
            sampling_context,
            shading_context,
            shading_point,
            clear_arena);
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
size_t PathTracer<PathVisitor, VolumeVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    const bool                  clear_arena)
{
    // Terminate the path if the first hit is too close to the origin.
    if (shading_point.hit_surface() && shading_point.get_distance() < m_near_start)
        return 1;

    PathVertex vertex(sampling_context);
    vertex.m_path_length = 1;
    vertex.m_scattering_modes = ScatteringMode::All;
    vertex.m_throughput.set(1.0f);
    vertex.m_shading_point = &shading_point;
    vertex.m_prev_mode = ScatteringMode::Specular;
    vertex.m_prev_prob = BSDF::DiracDelta;
    vertex.m_aov_mode = ScatteringMode::None;

    // This variable tracks the beginning of the path segment inside the current medium.
    // While it is properly initialized when entering a medium, we also initialize it
    // here to silence a gcc warning.
    foundation::Vector3d medium_start(0.0);

    m_diffuse_bounces = 0;
    m_glossy_bounces = 0;
    m_specular_bounces = 0;
    m_volume_bounces = 0;
    m_iterations = 0;

    while (true)
    {
        if (clear_arena)
            shading_context.get_arena().clear();

        ShadingPoint* next_shading_point = m_shading_point_arena.allocate<ShadingPoint>();

#ifndef NDEBUG
        // Save the sampling context at the beginning of the iteration.
        const SamplingContext backup_sampling_context(sampling_context);

        // Resume execution here to reliably reproduce problems downstream.
        sampling_context = backup_sampling_context;
#endif

        // Put a hard limit on the number of iterations.
        if (m_iterations++ == m_max_iterations)
        {
            if (m_max_bounces < m_max_iterations)
            {
                RENDERER_LOG_WARNING(
                    "reached hard iteration limit (%s), breaking path tracing loop.",
                    foundation::pretty_int(m_max_iterations).c_str());
            }
            break;
        }

        // Retrieve the ray.
        const ShadingRay& ray = vertex.get_ray();
        assert(foundation::is_normalized(ray.m_dir));

        // Compute the outgoing direction at this vertex.
        vertex.m_outgoing =
            ray.m_has_differentials
                ? foundation::Dual3d(
                    -ray.m_dir,
                    -ray.m_rx_dir,
                    -ray.m_ry_dir)
                : foundation::Dual3d(-ray.m_dir);

        // Terminate the path if the ray didn't hit anything.
        if (!vertex.m_shading_point->hit_surface())
        {
            m_path_visitor.on_miss(vertex);
            break;
        }

        // Retrieve the material at the shading point.
        const Material* material = vertex.get_material();

        // Terminate the path if the surface has no material.
        if (material == nullptr)
        {
            //m_path_visitor.on_miss(vertex);
            m_path_visitor.on_terminate(TerminateType::NoMaterialTerminate);
            break;
        }

        // Retrieve the material's render data.
        const Material::RenderData& material_data = material->get_render_data();

        // Retrieve the object instance at the shading point.
        const ObjectInstance& object_instance = vertex.m_shading_point->get_object_instance();

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = vertex.m_shading_point->is_entering();

        // Handle false intersections.
        if (ray.get_current_medium() &&
            ray.get_current_medium()->m_object_instance->get_medium_priority() > object_instance.get_medium_priority() &&
            material_data.m_bsdf != nullptr)
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
                next_ray.m_rx_org = ray.m_rx_org + ray.m_tmax * ray.m_rx_dir;
                next_ray.m_ry_org = ray.m_ry_org + ray.m_tmax * ray.m_ry_dir;
                next_ray.m_rx_dir = ray.m_rx_dir;
                next_ray.m_ry_dir = ray.m_ry_dir;
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
            shading_context.get_intersector().trace(
                next_ray,
                *next_shading_point,
                vertex.m_shading_point);

            // Update the pointers to the shading points and loop.
            vertex.m_shading_point = next_shading_point;
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
                    next_ray.m_rx_org = ray.m_rx_org + ray.m_tmax * ray.m_rx_dir;
                    next_ray.m_ry_org = ray.m_ry_org + ray.m_tmax * ray.m_ry_dir;
                    next_ray.m_rx_dir = ray.m_rx_dir;
                    next_ray.m_ry_dir = ray.m_ry_dir;
                    next_ray.m_has_differentials = true;
                }

                // Inherit the medium list from the parent ray.
                next_ray.copy_media_from(ray);

                // Trace the ray.
                shading_context.get_intersector().trace(
                    next_ray,
                    *next_shading_point,
                    vertex.m_shading_point);

                // Update the pointers to the shading points and loop.
                vertex.m_shading_point = next_shading_point;
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
            vertex.m_shading_point->is_curve_primitive() ? nullptr : material_data.m_edf;
        vertex.m_bsdf = material_data.m_bsdf;
        vertex.m_bssrdf = material_data.m_bssrdf;

        // We allow materials with both a BSDF and a BSSRDF.
        // When both are present, pick one to extend the path.
        if (vertex.m_bsdf && vertex.m_bssrdf)
        {
            sampling_context.split_in_place(1, 1);
            if (sampling_context.next2<float>() < 0.5f)
                vertex.m_bsdf = nullptr;
            else vertex.m_bssrdf = nullptr;
            vertex.m_throughput *= 2.0f;
        }

        // Evaluate the inputs of the BSDF.
        if (vertex.m_bsdf)
        {
            vertex.m_bsdf_data =
                vertex.m_bsdf->evaluate_inputs(shading_context, *vertex.m_shading_point);
        }

        // Evaluate the inputs of the BSSRDF.
        if (vertex.m_bssrdf)
        {
            vertex.m_bssrdf_data =
                vertex.m_bssrdf->evaluate_inputs(shading_context, *vertex.m_shading_point);
        }

        // Let the path visitor handle a hit.
        // cos(outgoing, normal) is used for changes of probability measure. In the case of subsurface
        // scattering, we purposely compute it at the outgoing vertex even though it may be used at
        // the incoming vertex if we need to change the probability of reaching this vertex by BSDF
        // sampling from projected solid angle measure to area measure.
        vertex.m_cos_on = foundation::dot(vertex.m_outgoing.get_value(), vertex.get_shading_normal());
        //m_path_visitor.on_hit(vertex);

        // Use Russian Roulette to cut the path without introducing bias.
        if (!continue_path_rr(sampling_context, vertex))
        {
            m_path_visitor.on_terminate(TerminateType::RussianRouletteTerminate);
            break;
        }

        // Honor the global bounce limit.
        const size_t bounces = vertex.m_path_length - 1;
        if (bounces == m_max_bounces)
        {
            m_path_visitor.on_terminate(TerminateType::BounceLimitTerminate);
            break;
        }

        // Determine which scattering modes are still enabled.
        if (m_diffuse_bounces >= m_max_diffuse_bounces)
            vertex.m_scattering_modes &= ~ScatteringMode::Diffuse;
        if (m_glossy_bounces >= m_max_glossy_bounces)
            vertex.m_scattering_modes &= ~ScatteringMode::Glossy;
        if (m_specular_bounces >= m_max_specular_bounces)
            vertex.m_scattering_modes &= ~ScatteringMode::Specular;
        if (m_volume_bounces >= m_max_volume_bounces)
            vertex.m_scattering_modes &= ~ScatteringMode::Volume;

        // Terminate path if no scattering event is possible.
        if (vertex.m_scattering_modes == ScatteringMode::None)
        {
            m_path_visitor.on_terminate(TerminateType::NoScatteringPossibleTerminate);
            break;
        }

        BSDF::LocalGeometry local_geometry;
        local_geometry.m_shading_point = vertex.m_shading_point;
        local_geometry.m_geometric_normal = foundation::Vector3f(vertex.get_geometric_normal());
        local_geometry.m_shading_basis = foundation::Basis3f(vertex.get_shading_basis());

        BSDFSample bsdf_sample;

        // Subsurface scattering.
        BSSRDFSample bssrdf_sample;
        if (vertex.m_bssrdf)
        {
            // Sample the BSSRDF and terminate the path if no incoming point is found.
            if (!vertex.m_bssrdf->sample(
                    shading_context,
                    sampling_context,
                    vertex.m_bssrdf_data,
                    *vertex.m_shading_point,
                    foundation::Vector3f(vertex.m_outgoing.get_value()),
                    vertex.m_scattering_modes,
                    bssrdf_sample,
                    bsdf_sample))
            {
                m_path_visitor.on_terminate(TerminateType::NoIncomingPointTerminate);
                break;
            }

            // Update the path throughput.
            vertex.m_throughput *= bssrdf_sample.m_value;
            vertex.m_throughput /= bssrdf_sample.m_probability;

            // Switch to the BSSRDF's BRDF.
            vertex.m_shading_point = &bssrdf_sample.m_incoming_point;
            vertex.m_bsdf = bssrdf_sample.m_brdf;
            vertex.m_bsdf_data = bssrdf_sample.m_brdf_data;

            // Record scattering mode for bssrdf sample.
            vertex.m_prev_mode = bsdf_sample.get_mode();
        }

        // Terminate the path if no above-surface scattering possible.
        if (vertex.m_bsdf == nullptr && material->get_render_data().m_volume == nullptr)
        {
            m_path_visitor.on_terminate(TerminateType::NoAboveSurfaceTerminate);
            break;
        }

        // In case there is no BSDF, the current ray will be continued without increasing its depth.
        ShadingRay next_ray(
            vertex.get_point(),
            ray.m_dir,
            ray.m_time,
            ray.m_flags,
            ray.m_depth);

        if (vertex.m_bsdf != nullptr)
        {
            // If there is a BSDF, compute the bounce.
            const bool continue_path =
                process_bounce(sampling_context, vertex, local_geometry, bsdf_sample, next_ray);

            // Terminate the path if this scattering event is not accepted.
            if (!continue_path)
            {
                m_path_visitor.on_terminate(TerminateType::ScatteringNotAcceptedTerminate);
                break;
            }
        }

        m_path_visitor.on_hit(vertex);

        // Build the medium list of the scattered ray.
        if (vertex.m_crossing_interface)
        {
            // Ray goes under the surface:
            // inherit the medium list of the parent ray and add/remove the current medium.
            if (entering)
            {
                const float ior =
                    vertex.m_bsdf == nullptr
                        ? 1.0f
                        : vertex.m_bsdf->sample_ior(sampling_context, vertex.m_bsdf_data);
                next_ray.add_medium(ray, &object_instance, vertex.get_material(), ior);
            }
            else
            {
                next_ray.remove_medium(ray, &object_instance);
            }
        }
        else
        {
            // Reflected ray:
            // inherit the medium list of the parent ray.
            next_ray.copy_media_from(ray);
        }

        // Compute absorption for the segment inside the medium.
        const ShadingRay::Medium* prev_medium = ray.get_current_medium();
        if (prev_medium != nullptr && prev_medium->m_material != nullptr)
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

        const ShadingRay::Medium* current_medium = next_ray.get_current_medium();
        if (current_medium != nullptr &&
            current_medium->get_volume() != nullptr)
        {
            // This ray is being cast into a participating medium.
            if (!march(
                    sampling_context,
                    shading_context,
                    next_ray,
                    vertex,
                    *next_shading_point))
            {
                m_path_visitor.on_terminate(TerminateType::MediaMarchErrorTerminate);
                break;
            }
        }
        else
        {
            // This ray is being cast into an ordinary medium.
            shading_context.get_intersector().trace(
                next_ray,
                *next_shading_point,
                vertex.m_shading_point);
        }

        // Update the pointers to the shading points.
        vertex.m_parent_shading_point = vertex.m_shading_point;
        vertex.m_shading_point = next_shading_point;
    }

    return vertex.m_path_length;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::pass_through(
    SamplingContext&            sampling_context,
    const Alpha                 alpha)
{
    // Fully opaque: never pass through.
    if (alpha[0] >= 1.0f)
        return false;

    // Fully transparent: always pass through.
    if (alpha[0] <= 0.0f)
        return true;

    // Generate a uniform sample in [0,1).
    sampling_context.split_in_place(1, 1);
    const float s = sampling_context.next2<float>();

    return s >= alpha[0];
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::continue_path_rr(
    SamplingContext&            sampling_context,
    PathVertex&                 vertex)
{
    // Don't apply Russian Roulette for the first few bounces.
    if (vertex.m_path_length <= m_rr_min_path_length)
        return true;

    // Generate a uniform sample in [0,1).
    sampling_context.split_in_place(1, 1);
    const float s = sampling_context.next2<float>();

    // Compute the probability of extending this path.
    const float scattering_prob =
        std::min(foundation::max_value(vertex.m_throughput), 0.99f);

    // Russian Roulette.
    if (!foundation::pass_rr(scattering_prob, s))
        return false;

    // Adjust throughput to account for terminated paths.
    assert(scattering_prob > 0.0f);
    vertex.m_throughput /= scattering_prob;

    return true;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::process_bounce(
    SamplingContext&            sampling_context,
    PathVertex&                 vertex,
    const BSDF::LocalGeometry&  local_geometry,
    BSDFSample&                 sample,
    ShadingRay&                 next_ray)
{
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
            local_geometry,
            foundation::Dual3f(vertex.m_outgoing),
            vertex.m_scattering_modes,
            sample);

        next_ray.m_min_roughness = m_clamp_roughness ? sample.m_min_roughness : 0.0f;

        if (vertex.m_path_length == 1 && sample.get_mode() == ScatteringMode::Diffuse)
            m_path_visitor.on_first_diffuse_bounce(vertex, sample.m_aov_components.m_albedo);
    }
    else
    {
        // Since the BSDF is already sampled during BSSRDF sampling, it is not sampled here.
        // However, we need to check if the corresponding mode is still enabled.
        if ((sample.get_mode() & vertex.m_scattering_modes) == 0)
            sample.set_to_absorption();
    }

    // Terminate the path if it gets absorbed.
    if (sample.get_mode() == ScatteringMode::None)
        return false;

    // Terminate the path if this scattering event is not accepted.
    if (!m_path_visitor.accept_scattering(vertex.m_prev_mode, sample.get_mode()))
        return false;

    // Save the scattering properties for MIS at light-emitting vertices.
    vertex.m_prev_mode = sample.get_mode();
    vertex.m_prev_prob = sample.get_probability();

    // Update the AOV scattering mode only for the first bounce.
    if (vertex.m_path_length == 1)
        vertex.m_aov_mode = sample.get_mode();

    // Update path throughput.
    if (sample.get_probability() != BSDF::DiracDelta)
        sample.m_value /= sample.get_probability();
    vertex.m_throughput *= sample.m_value.m_beauty;

    // Update bounce counters.
    ++vertex.m_path_length;
    m_diffuse_bounces +=  (sample.get_mode() >> ScatteringMode::DiffuseBitShift)  & 1;
    m_glossy_bounces +=   (sample.get_mode() >> ScatteringMode::GlossyBitShift)   & 1;
    m_specular_bounces += (sample.get_mode() >> ScatteringMode::SpecularBitShift) & 1;

    // Construct the scattered ray.
    const ShadingRay& ray = vertex.get_ray();
    next_ray.m_org = vertex.m_shading_point->get_point();
    next_ray.m_dir = foundation::improve_normalization<2>(foundation::Vector3d(sample.m_incoming.get_value()));
    next_ray.m_time = ray.m_time;
    next_ray.m_flags = ScatteringMode::get_vis_flags(sample.get_mode()),
    next_ray.m_depth = ray.m_depth + 1;

    // Compute scattered ray differentials.
    if (sample.m_incoming.has_derivatives())
    {
        next_ray.m_rx_org = next_ray.m_org + vertex.m_shading_point->get_dpdx();
        next_ray.m_ry_org = next_ray.m_org + vertex.m_shading_point->get_dpdy();
        next_ray.m_rx_dir = foundation::Vector3d(sample.m_incoming.get_dx());
        next_ray.m_ry_dir = foundation::Vector3d(sample.m_incoming.get_dy());
        next_ray.m_has_differentials = true;
    }

    // Determine if it is an interface crossing.
    const foundation::Vector3d& geometric_normal = vertex.get_geometric_normal();
    vertex.m_crossing_interface = vertex.m_bssrdf == nullptr &&
        foundation::dot(vertex.m_outgoing.get_value(), geometric_normal) *
        foundation::dot(next_ray.m_dir, geometric_normal) < 0.0;

    return true;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
bool PathTracer<PathVisitor, VolumeVisitor, Adjoint>::march(
    SamplingContext&            sampling_context,
    const ShadingContext&       shading_context,
    const ShadingRay&           ray,
    PathVertex&                 vertex,
    ShadingPoint&               exit_point)
{
    if (!continue_path_rr(sampling_context, vertex))
        return false;

    if (vertex.m_scattering_modes == ScatteringMode::None)
        return false;

    const ShadingRay::Medium* medium = ray.get_current_medium();
    const Volume* volume = medium->get_volume();

    // Trace the ray across the volume.
    exit_point.clear();
    shading_context.get_intersector().trace(
        ray,
        exit_point,
        vertex.m_shading_point);

    while (true)
    {
        shading_context.get_arena().clear();

        // Put a hard limit on the number of iterations.
        if (m_iterations++ == m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking path tracing loop.",
                foundation::pretty_int(m_max_iterations).c_str());
            return false;
        }

        // Apply Russian Roulette.
        if (!continue_path_rr(sampling_context, vertex))
            return false;

        if (m_volume_bounces >= m_max_volume_bounces)
            vertex.m_scattering_modes &= ~ScatteringMode::Volume;

        const ShadingRay& volume_ray = exit_point.get_ray();

        // Evaluate and prepare volume inputs.
        void* data = volume->evaluate_inputs(shading_context, volume_ray);
        volume->prepare_inputs(shading_context.get_arena(), volume_ray, data);
        vertex.m_volume_data = data;

        // Apply the visitor to this ray.
        m_volume_visitor.visit_ray(vertex, volume_ray);

        if ((vertex.m_scattering_modes & ScatteringMode::Volume) == 0)
        {
            // No more scattering events are allowed:
            // update the ray transmission and continue path tracing.
            Spectrum transmission;
            volume->evaluate_transmission(
                vertex.m_volume_data,
                volume_ray,
                transmission);
            vertex.m_throughput *= transmission;
            break;
        }

        // Retrieve extinction spectrum.
        const Spectrum& extinction_coef =
            volume->extinction_coefficient(vertex.m_volume_data, volume_ray);

        // Sample channel uniformly at random.
        sampling_context.split_in_place(1, 1);
        const float s = sampling_context.next2<float>();
        const size_t channel = foundation::truncate<size_t>(s * Spectrum::size());
        const bool extinction_is_null = extinction_coef[channel] < 1.0e-6f;

        // Sample distance.
        float distance_sample, distance_pdf;
        if (extinction_is_null)
        {
            distance_sample = 0.0f;
            distance_pdf = 0.0f;
        }
        else
        {
            sampling_context.split_in_place(1, 1);
            distance_sample =
                foundation::sample_exponential_distribution(
                    sampling_context.next2<float>(),
                    extinction_coef[channel]);
            distance_pdf =
                foundation::exponential_distribution_pdf(
                    distance_sample,
                    extinction_coef[channel]);
        }

        // Continue path tracing if sampled distance exceeds total length of the ray,
        // otherwise process the scattering event.
        if (extinction_is_null || volume_ray.m_tmax < distance_sample)
        {
            Spectrum transmission;
            volume->evaluate_transmission(
                vertex.m_volume_data,
                volume_ray,
                transmission);
            vertex.m_throughput *= transmission;
            vertex.m_throughput /=                       // equivalent to multiplying by MIS weight
                foundation::average_value(transmission); // and then dividing by transmission[channel]
            break;
        }

        //
        // Bounce.
        //

        ShadingPoint* next_shading_point = m_shading_point_arena.allocate<ShadingPoint>();
        shading_context.get_intersector().make_volume_shading_point(
            *next_shading_point,
            volume_ray,
            distance_sample);

        // Terminate the path if this scattering event is not accepted.
        if (!m_volume_visitor.accept_scattering(vertex.m_prev_mode))
            return false;

        // Let the volume visitor handle the scattering event.
        m_volume_visitor.on_scatter(vertex);

        // Terminate the path if all scattering modes are disabled.
        if (vertex.m_scattering_modes == ScatteringMode::None)
            return false;

        // Retrieve scattering spectrum.
        const Spectrum& scattering_coef =
            volume->scattering_coefficient(vertex.m_volume_data, volume_ray);

        // Evaluate transmission between the origin and the sampled distance.
        Spectrum transmission;
        volume->evaluate_transmission(
            vertex.m_volume_data,
            volume_ray,
            distance_sample,
            transmission);

        // Compute MIS weight.
        // MIS terms are:
        //  - scattering albedo,
        //  - throughput of the entire path up to the sampled point.
        // Reference: "Practical and Controllable Subsurface Scattering
        // for Production Path Tracing", p. 1 [ACM 2016 Article].
        float mis_weights_sum = 0.0f;
        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
        {
            if (extinction_coef[i] > 1.0e-6f)
            {
                const float probability =
                    foundation::exponential_distribution_pdf(
                        distance_sample,
                        extinction_coef[i]);
                mis_weights_sum += foundation::square(probability);
            }
        }
        if (mis_weights_sum < 1.0e-6f)
            return false;  // no scattering
        const float current_mis_weight =
            Spectrum::size() *
            foundation::square(distance_pdf) /
            mis_weights_sum;

        vertex.m_throughput *= scattering_coef;
        vertex.m_throughput *= transmission;
        vertex.m_throughput *= current_mis_weight / distance_pdf;

        // Sample phase function.
        foundation::Vector3f incoming;
        const float pdf =
            volume->sample(
                sampling_context,
                vertex.m_volume_data,
                volume_ray,
                distance_sample,
                incoming);

        // Save the scattering properties for MIS at light-emitting vertices.
        vertex.m_prev_mode = ScatteringMode::Volume;
        vertex.m_prev_prob = pdf;

        // Update the AOV scattering mode only for the first bounce.
        if (vertex.m_path_length == 1)
            vertex.m_aov_mode = ScatteringMode::Volume;

        ++m_volume_bounces;
        ++vertex.m_path_length;

        // Continue the ray in in-scattered direction.
        ShadingRay next_ray(
            volume_ray.m_org + static_cast<double>(distance_sample) * volume_ray.m_dir,
            foundation::improve_normalization<2>(foundation::Vector3d(incoming)),
            volume_ray.m_time,
            volume_ray.m_flags,
            volume_ray.m_depth + 1);
        next_ray.copy_media_from(volume_ray);

        // Update the pointer to the shading points.
        vertex.m_shading_point = next_shading_point;

        // Trace the ray across the volume.
        exit_point.clear();
        shading_context.get_intersector().trace(next_ray, exit_point, nullptr);
    }

    return true;
}

template <typename PathVisitor, typename VolumeVisitor, bool Adjoint>
inline const ShadingPoint& PathTracer<PathVisitor, VolumeVisitor, Adjoint>::get_path_vertex(const size_t i) const
{
    return reinterpret_cast<const ShadingPoint*>(m_shading_point_arena.get_storage())[i];
}

}   // namespace renderer
