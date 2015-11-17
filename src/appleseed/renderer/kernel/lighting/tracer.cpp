
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

// Interface header.
#include "tracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/kernel/shading/oslshadergroupexec.h"
#endif
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

Tracer::Tracer(
    const Scene&                scene,
    const Intersector&          intersector,
    TextureCache&               texture_cache,
#ifdef APPLESEED_WITH_OSL
    OSLShaderGroupExec&         shadergroup_exec,
#endif
    const float                 transparency_threshold,
    const size_t                max_iterations,
    const bool                  print_details)
  : m_intersector(intersector)
  , m_texture_cache(texture_cache)
#ifdef APPLESEED_WITH_OSL
  , m_shadergroup_exec(shadergroup_exec)
#endif
  , m_assume_no_alpha_mapping(!scene.uses_alpha_mapping())
  , m_transmission_threshold(static_cast<double>(transparency_threshold))
  , m_max_iterations(max_iterations)
{
    if (print_details)
    {
        if (m_assume_no_alpha_mapping)
            RENDERER_LOG_DEBUG("the scene does not rely on alpha mapping; using probe tracing.");
        else RENDERER_LOG_DEBUG("the scene uses alpha mapping; using standard tracing.");
    }
}

const ShadingPoint& Tracer::do_trace(
    const Vector3d&             origin,
    const Vector3d&             direction,
    const ShadingRay::Time&     ray_time,
    const VisibilityFlags::Type ray_flags,
    const ShadingRay::DepthType ray_depth,
    double&                     transmission,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(direction));

    transmission = 1.0;

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    while (true)
    {
        // Put a hard limit on the number of iterations.
        if (++iterations >= m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking trace loop.",
                pretty_int(m_max_iterations).c_str());
            break;
        }

        // Construct the visibility ray.
        const ShadingRay ray(
            point,
            direction,
            ray_time,
            ray_flags,
            ray_depth);         // ray depth does not increase when passing through an alpha-mapped surface

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the ray escaped the scene.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        Alpha alpha;
        evaluate_alpha(*material, *shading_point_ptr, alpha);

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0 - static_cast<double>(alpha[0]);

        // Stop once we hit full opacity.
        if (transmission < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();
    }

    return *shading_point_ptr;
}

const ShadingPoint& Tracer::do_trace_between(
    const Vector3d&             origin,
    const Vector3d&             target,
    const ShadingRay::Time&     ray_time,
    const VisibilityFlags::Type ray_flags,
    const ShadingRay::DepthType ray_depth,
    double&                     transmission,
    const ShadingPoint*         parent_shading_point)
{
    transmission = 1.0;

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    while (true)
    {
        // Put a hard limit on the number of iterations.
        if (++iterations >= m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking trace loop.",
                pretty_int(m_max_iterations).c_str());
            break;
        }

        // Construct the visibility ray.
        const Vector3d direction = target - point;
        const double dist = norm(direction);

        const ShadingRay ray(
            point,
            direction / dist,
            0.0,                    // ray tmin
            dist * (1.0 - 1.0e-6),  // ray tmax
            ray_time,
            ray_flags,
            ray_depth);         // ray depth does not increase when passing through an alpha-mapped surface

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the ray reached the target point.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        // Evaluate the alpha map at the shading point.
        Alpha alpha;
        evaluate_alpha(*material, *shading_point_ptr, alpha);

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0 - static_cast<double>(alpha[0]);

        // Stop once we hit full opacity.
        if (transmission < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();
    }

    return *shading_point_ptr;
}

void Tracer::evaluate_alpha(
    const Material&             material,
    const ShadingPoint&         shading_point,
    Alpha&                      alpha) const
{
    alpha = shading_point.get_alpha();

#ifdef APPLESEED_WITH_OSL
    // Apply OSL transparency if needed.
    if (const ShaderGroup* sg = material.get_render_data().m_shader_group)
    {
        if (sg->has_transparency())
        {
            Alpha a;
            m_shadergroup_exec.execute_shadow(*sg, shading_point, a);
            alpha *= a;
        }
    }
#endif
}

}   // namespace renderer
