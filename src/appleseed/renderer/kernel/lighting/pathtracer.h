
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
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
        const size_t            max_iterations = 10000);

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
    const ShadingPoint* shading_point_ptr = &shading_point;

    Spectrum throughput(1.0f);
    BSDF::Mode prev_bsdf_mode = BSDF::Specular;
    double prev_bsdf_prob = BSDF::DiracDelta;
    size_t path_length = 1;
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
        const ShadingRay& ray = shading_point_ptr->get_ray();

        // Terminate the path if the ray didn't hit anything.
        if (!shading_point_ptr->hit())
        {
            m_path_visitor.visit_environment(
                *shading_point_ptr,
                foundation::normalize(-ray.m_dir),
                prev_bsdf_mode,
                throughput);
            break;
        }

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        // Handle alpha mapping.
        if (path_length > 1 && material->get_alpha_map())
        {
            // Evaluate the alpha map at the shading point.
            Alpha alpha;
            material->get_alpha_map()->evaluate(
                texture_cache, 
                shading_point_ptr->get_uv(0),
                alpha);

            if (alpha[0] < 1.0)
            {
                // Generate a uniform sample in [0,1).
                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();

                if (s >= alpha[0])
                {
                    // Construct a ray that continues in the same direction as the incoming ray.
                    const ShadingRay cutoff_ray(
                        shading_point_ptr->get_point(),
                        ray.m_dir,
                        ray.m_time,
                        ~0);            // ray flags

                    // Trace the ray.
                    shading_points[shading_point_index].clear();
                    intersector.trace(
                        cutoff_ray,
                        shading_points[shading_point_index],
                        shading_point_ptr);

                    // Update the pointers to the shading points.
                    shading_point_ptr = &shading_points[shading_point_index];
                    shading_point_index = 1 - shading_point_index;

                    continue;
                }
            }
        }

        // Retrieve the BSDF.
        const BSDF* bsdf = material->get_bsdf();

        // Evaluate the input values of the BSDF.
        InputEvaluator bsdf_input_evaluator(texture_cache);
        if (bsdf)
        {
            bsdf->evaluate_inputs(
                bsdf_input_evaluator,
                shading_point_ptr->get_uv(0));
        }

        // Compute the outgoing direction.
        const foundation::Vector3d outgoing = foundation::normalize(-ray.m_dir);

        // Compute radiance contribution at this vertex.
        if (!m_path_visitor.visit_vertex(
                sampling_context,
                *shading_point_ptr,
                outgoing,
                bsdf,
                bsdf_input_evaluator.data(),
                path_length,
                prev_bsdf_mode,
                prev_bsdf_prob,
                throughput))
            break;

        // Terminate the path if the material doesn't have a BSDF.
        if (bsdf == 0)
            break;

        // Sample the BSDF.
        foundation::Vector3d incoming;
        Spectrum bsdf_value;
        double bsdf_prob;
        const BSDF::Mode bsdf_mode =
            bsdf->sample(
                sampling_context,
                bsdf_input_evaluator.data(),
                Adjoint,
                true,       // multiply by |cos(incoming, normal)|
                shading_point_ptr->get_geometric_normal(),
                shading_point_ptr->get_shading_basis(),
                outgoing,
                incoming,
                bsdf_value,
                bsdf_prob);

        // Terminate the path if this scattering mode is not accepted.
        if (!m_path_visitor.accept_scattering_mode(prev_bsdf_mode, bsdf_mode))
            break;

        prev_bsdf_prob = bsdf_prob;
        prev_bsdf_mode = bsdf_mode;

        if (bsdf_prob != BSDF::DiracDelta)
            bsdf_value /= static_cast<float>(bsdf_prob);

        // Update the path throughput.
        throughput *= bsdf_value;

        // Use Russian Roulette to cut the path without introducing bias.
        if (m_rr_min_path_length > 0 && path_length >= m_rr_min_path_length)
        {
            // Generate a uniform sample in [0,1).
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next_double2();

            const double scattering_prob =
                std::min(
                    static_cast<double>(foundation::max_value(bsdf_value)),
                    1.0);

            if (!foundation::pass_rr(scattering_prob, s))
                break;

            assert(scattering_prob > 0.0);
            throughput /= static_cast<float>(scattering_prob);
        }

        // Honor the user bounce limit.
        if (m_max_path_length > 0 && path_length >= m_max_path_length)
            break;

        // Keep track of the number of bounces.
        ++path_length;

        // Construct the scattered ray.
        const ShadingRay scattered_ray(
            shading_point_ptr->get_point(),
            incoming,
            ray.m_time,
            ~0);            // ray flags

        // Trace the ray.
        shading_points[shading_point_index].clear();
        intersector.trace(
            scattered_ray,
            shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;
    }

    return path_length;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_PATHTRACER_H
