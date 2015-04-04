
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/visibilityflags.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/ray.h"
#include "foundation/math/rr.h"
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
    PathVisitor&                m_path_visitor;
    const size_t                m_rr_min_path_length;
    const size_t                m_max_path_length;
    const size_t                m_max_iterations;
    const double                m_near_start;

    // Determine the appropriate ray type for a given scattering mode.
    static VisibilityFlags::Type bsdf_mode_to_ray_flags(
        const BSDFSample::ScatteringMode mode);

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
    vertex.m_shading_point = &shading_point;
    vertex.m_path_length = 1;
    vertex.m_prev_bsdf_mode = BSDFSample::Specular;
    vertex.m_prev_bsdf_prob = BSDF::DiracDelta;
    vertex.m_throughput.set(1.0f);

    size_t iterations = 0;

    while (true)
    {
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

        // Handle alpha mapping.
        if (vertex.m_path_length > 1)
        {
            Alpha alpha = vertex.m_shading_point->get_alpha();

#ifdef APPLESEED_WITH_OSL
            // Apply OSL transparency if needed.
            if (material->get_osl_surface() && material->get_osl_surface()->has_transparency())
            {
                Alpha a;
                shading_context.execute_osl_transparency(
                    *material->get_osl_surface(),
                    *vertex.m_shading_point,
                    a);

                alpha *= a;
            }
#endif

            if (pass_through(sampling_context, alpha))
            {
                // Construct a ray that continues in the same direction as the incoming ray.
                ShadingRay cutoff_ray(
                    vertex.get_point(),
                    ray.m_dir,
                    ray.m_time,
                    ray.m_flags,
                    ray.m_depth);   // ray depth does not increase when passing through an alpha-mapped surface

                // Advance the differentials if the ray has them.
                if (ray.m_has_differentials)
                {
                    cutoff_ray.m_rx = ray.m_rx;
                    cutoff_ray.m_rx.m_org = ray.m_rx.point_at(ray.m_tmax);

                    cutoff_ray.m_ry = ray.m_ry;
                    cutoff_ray.m_ry.m_org = ray.m_ry.point_at(ray.m_tmax);

                    cutoff_ray.m_has_differentials = true;
                }

                // Trace the ray.
                shading_points[shading_point_index].clear();
                shading_context.get_intersector().trace(
                    cutoff_ray,
                    shading_points[shading_point_index],
                    vertex.m_shading_point);

                // Update the pointers to the shading points.
                vertex.m_shading_point = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;

                continue;
            }
        }

#ifdef APPLESEED_WITH_OSL
        // Execute the OSL shader if there is one.
        if (material->get_osl_surface())
        {
            shading_context.execute_osl_shading(
                *material->get_osl_surface(),
                *vertex.m_shading_point);
        }
#endif

        // Retrieve the EDF and the BSDF.
        vertex.m_edf =
            vertex.m_shading_point->is_curve_primitive() ? 0 : material->get_edf();
        vertex.m_bsdf = material->get_bsdf();

        // Evaluate the input values of the BSDF.
        InputEvaluator bsdf_input_evaluator(shading_context.get_texture_cache());
        if (vertex.m_bsdf)
        {
            vertex.m_bsdf->evaluate_inputs(
                shading_context,
                bsdf_input_evaluator,
                *vertex.m_shading_point);
            vertex.m_bsdf_data = bsdf_input_evaluator.data();
        }

        // Compute radiance contribution at this vertex.
        vertex.m_cos_on = foundation::dot(vertex.m_outgoing.get_value(), vertex.get_shading_normal());
        m_path_visitor.visit_vertex(vertex);

        // Terminate the path if the material doesn't have a BSDF.
        if (vertex.m_bsdf == 0)
            break;

        // Sample the BSDF.
        BSDFSample sample(
            *vertex.m_shading_point,
            sampling_context,
            vertex.m_outgoing);
        vertex.m_bsdf->sample(
            vertex.m_bsdf_data,
            Adjoint,
            true,       // multiply by |cos(incoming, normal)|
            sample);

        if (sample.is_absorption())
            break;

        // Terminate the path if this scattering event is not accepted.
        if (!m_path_visitor.accept_scattering(vertex.m_prev_bsdf_mode, sample.get_mode()))
            break;

        vertex.m_prev_bsdf_prob = sample.get_probability();
        vertex.m_prev_bsdf_mode = sample.get_mode();

        if (sample.get_probability() != BSDF::DiracDelta)
            sample.value() /= static_cast<float>(sample.get_probability());

        // Update the path throughput.
        vertex.m_throughput *= sample.value();

        // Use Russian Roulette to cut the path without introducing bias.
        if (vertex.m_path_length >= m_rr_min_path_length)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next_double2();

            // Compute the probability of extending this path.
            const double scattering_prob =
                std::min(
                    static_cast<double>(foundation::max_value(sample.value())),
                    1.0);

            // Russian Roulette.
            if (!foundation::pass_rr(scattering_prob, s))
                break;

            // Adjust throughput to account for terminated paths.
            assert(scattering_prob > 0.0);
            vertex.m_throughput /= static_cast<float>(scattering_prob);
        }

        // Honor the user bounce limit.
        if (vertex.m_path_length >= m_max_path_length)
            break;

        // Keep track of the number of bounces.
        ++vertex.m_path_length;

        // Construct the scattered ray.
        ShadingRay scattered_ray(
            vertex.m_shading_point->get_biased_point(sample.get_incoming_vector()),
            sample.get_incoming_vector(),
            ray.m_time,
            bsdf_mode_to_ray_flags(sample.get_mode()),
            ray.m_depth + 1);

        if (sample.get_incoming().has_derivatives())
        {
            scattered_ray.m_rx.m_org =
                scattered_ray.m_org + vertex.m_shading_point->get_dpdx();

            scattered_ray.m_ry.m_org =
                scattered_ray.m_org + vertex.m_shading_point->get_dpdy();

            scattered_ray.m_rx.m_dir =
                scattered_ray.m_dir + sample.get_incoming().get_dx();

            scattered_ray.m_ry.m_dir =
                scattered_ray.m_dir + sample.get_incoming().get_dy();

            scattered_ray.m_has_differentials = true;
        }

        // Trace the ray.
        shading_points[shading_point_index].clear();
        shading_context.get_intersector().trace(
            scattered_ray,
            shading_points[shading_point_index],
            vertex.m_shading_point);

        // Update the pointers to the shading points.
        vertex.m_shading_point = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;
    }

    return vertex.m_path_length;
}

template <typename PathVisitor, bool Adjoint>
inline VisibilityFlags::Type PathTracer<PathVisitor, Adjoint>::bsdf_mode_to_ray_flags(
    const BSDFSample::ScatteringMode mode)
{
    switch (mode)
    {
      case BSDFSample::Diffuse:   return VisibilityFlags::DiffuseRay;
      case BSDFSample::Glossy:    return VisibilityFlags::GlossyRay;
      case BSDFSample::Specular:  return VisibilityFlags::SpecularRay;
      default:
        assert(!"Invalid scattering mode.");
        return VisibilityFlags::DiffuseRay;
    }
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

    return sampling_context.next_double2() >= alpha[0];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
