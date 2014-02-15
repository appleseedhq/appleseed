
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/basis.h"
#include "foundation/math/rr.h"
#include "foundation/math/vector.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class TextureCache; }

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
        const size_t            max_iterations = 1000);

    size_t trace(
        SamplingContext&        sampling_context,
        const Intersector&      intersector,
        TextureCache&           texture_cache,
        const ShadingRay&       ray,
        const ShadingPoint*     parent_shading_point = 0);

    size_t trace(
        SamplingContext&        sampling_context,
        const Intersector&      intersector,
        TextureCache&           texture_cache,
        const ShadingPoint&     shading_point);

  private:
    PathVisitor&                m_path_visitor;
    const size_t                m_rr_min_path_length;
    const size_t                m_max_path_length;
    const size_t                m_max_iterations;

    // Determine the appropriate ray type for a given scattering mode.
    static ShadingRay::Type bsdf_mode_to_ray_type(
        const BSDF::Mode        mode);

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
    const size_t                max_iterations)
  : m_path_visitor(path_visitor)
  , m_rr_min_path_length(rr_min_path_length)
  , m_max_path_length(max_path_length)
  , m_max_iterations(max_iterations)
{
}

template <typename PathVisitor, bool Adjoint>
inline size_t PathTracer<PathVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const Intersector&          intersector,
    TextureCache&               texture_cache,
    const ShadingRay&           ray,
    const ShadingPoint*         parent_shading_point)
{
    ShadingPoint shading_point;
    intersector.trace(ray, shading_point, parent_shading_point);

    return
        trace(
            sampling_context,
            intersector,
            texture_cache,
            shading_point);
}

template <typename PathVisitor, bool Adjoint>
size_t PathTracer<PathVisitor, Adjoint>::trace(
    SamplingContext&            sampling_context,
    const Intersector&          intersector,
    TextureCache&               texture_cache,
    const ShadingPoint&         shading_point)
{
    ShadingPoint shading_points[2];
    size_t shading_point_index = 0;

    PathVertex vertex(sampling_context);
    vertex.m_shading_point = &shading_point;
    vertex.m_path_length = 1;
    vertex.m_prev_bsdf_mode = BSDF::Specular;
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

        // Terminate the path if the ray didn't hit anything.
        if (!vertex.m_shading_point->hit())
        {
            m_path_visitor.visit_environment(
                *vertex.m_shading_point,
                foundation::normalize(-ray.m_dir),
                vertex.m_prev_bsdf_mode,
                vertex.m_prev_bsdf_prob,
                vertex.m_throughput);
            break;
        }

        // Retrieve the material at the shading point.
        const Material* material = vertex.get_material();

        // Terminate the path if the surface has no material.
        if (material == 0)
            break;

        // Handle alpha mapping.
        if (vertex.m_path_length > 1 && material->get_alpha_map())
        {
            // Evaluate the alpha map at the shading point.
            Alpha alpha;
            material->get_alpha_map()->evaluate(
                texture_cache, 
                vertex.get_uv(0),
                alpha);

            if (pass_through(sampling_context, alpha))
            {
                // Construct a ray that continues in the same direction as the incoming ray.
                const ShadingRay cutoff_ray(
                    vertex.get_point(),
                    ray.m_dir,
                    ray.m_time,
                    ray.m_type,
                    ray.m_depth);   // ray depth does not increase when passing through an alpha-mapped surface

                // Trace the ray.
                shading_points[shading_point_index].clear();
                intersector.trace(
                    cutoff_ray,
                    shading_points[shading_point_index],
                    vertex.m_shading_point);

                // Update the pointers to the shading points.
                vertex.m_shading_point = &shading_points[shading_point_index];
                shading_point_index = 1 - shading_point_index;

                continue;
            }
        }

        // Retrieve the EDF and the BSDF.
        vertex.m_edf = material->get_edf();
        vertex.m_bsdf = material->get_bsdf();

        // Evaluate the input values of the BSDF.
        InputEvaluator bsdf_input_evaluator(texture_cache);
        if (vertex.m_bsdf)
        {
            vertex.m_bsdf->evaluate_inputs(bsdf_input_evaluator, *vertex.m_shading_point);
            vertex.m_bsdf_data = bsdf_input_evaluator.data();
        }

        // Compute the outgoing direction.
        vertex.m_outgoing = foundation::normalize(-ray.m_dir);
        vertex.m_cos_on = foundation::dot(vertex.m_outgoing, vertex.get_shading_normal());

        // Compute radiance contribution at this vertex.
        m_path_visitor.visit_vertex(vertex);

        // Terminate the path if the material doesn't have a BSDF.
        if (vertex.m_bsdf == 0)
            break;

        // Sample the BSDF.
        foundation::Vector3d incoming;
        Spectrum bsdf_value;
        double bsdf_prob;
        const BSDF::Mode bsdf_mode =
            vertex.m_bsdf->sample(
                sampling_context,
                vertex.m_bsdf_data,
                Adjoint,
                true,       // multiply by |cos(incoming, normal)|
                vertex.get_geometric_normal(),
                vertex.get_shading_basis(),
                vertex.m_outgoing,
                incoming,
                bsdf_value,
                bsdf_prob);
        if (bsdf_mode == BSDF::Absorption)
            break;

        // Terminate the path if this scattering event is not accepted.
        if (!m_path_visitor.accept_scattering(vertex.m_prev_bsdf_mode, bsdf_mode))
            break;

        vertex.m_prev_bsdf_prob = bsdf_prob;
        vertex.m_prev_bsdf_mode = bsdf_mode;

        if (bsdf_prob != BSDF::DiracDelta)
            bsdf_value /= static_cast<float>(bsdf_prob);

        // Update the path throughput.
        vertex.m_throughput *= bsdf_value;

        // Use Russian Roulette to cut the path without introducing bias.
        if (vertex.m_path_length >= m_rr_min_path_length)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next_double2();

            // Compute the probability of extending this path.
            const double scattering_prob =
                std::min(
                    static_cast<double>(foundation::max_value(bsdf_value)),
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
        const ShadingRay scattered_ray(
            vertex.m_shading_point->get_biased_point(incoming),
            incoming,
            ray.m_time,
            bsdf_mode_to_ray_type(bsdf_mode),
            ray.m_depth + 1);

        // Trace the ray.
        shading_points[shading_point_index].clear();
        intersector.trace(
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
inline ShadingRay::Type PathTracer<PathVisitor, Adjoint>::bsdf_mode_to_ray_type(
    const BSDF::Mode            mode)
{
    switch (mode)
    {
      case BSDF::Diffuse:   return ShadingRay::DiffuseRay;
      case BSDF::Glossy:    return ShadingRay::GlossyRay;
      case BSDF::Specular:  return ShadingRay::SpecularRay;
      default:
        assert(!"Invalid scattering mode.");
        return ShadingRay::DiffuseRay;
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
