
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

// Interface header.
#include "tracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/volume/volume.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

Tracer::Tracer(
    const Scene&                scene,
    const Intersector&          intersector,
    OSLShaderGroupExec&         shadergroup_exec,
    const float                 transparency_threshold,
    const size_t                max_iterations,
    const bool                  print_details)
  : m_intersector(intersector)
  , m_shadergroup_exec(shadergroup_exec)
  , m_assume_no_alpha_mapping(!scene.uses_alpha_mapping())
  , m_assume_no_participating_media(!scene.has_participating_media())
  , m_transmission_threshold(transparency_threshold)
  , m_max_iterations(max_iterations)
{
    if (print_details)
    {
        if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
        {
            RENDERER_LOG_INFO(
                "the scene does not rely on alpha mapping "
                "and does not contain participating media; using probe tracing.");
        }
        else
        {
            RENDERER_LOG_INFO(
                "the scene uses alpha mapping "
                "or contains participating media; using standard tracing.");
        }
    }
}

const ShadingPoint& Tracer::do_trace(
    const ShadingContext&       shading_context,
    const ShadingRay&           ray,
    Spectrum&                   transmission,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(ray.m_dir));

    transmission.set(1.0f);

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = ray.m_org;
    size_t iterations = 0;

    m_shading_points[shading_point_index].clear();
    m_intersector.trace(
        ray,
        m_shading_points[shading_point_index],
        shading_point_ptr);

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

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        const ShadingRay& current_ray = shading_point_ptr->get_ray();

        const ShadingRay::Medium* medium = current_ray.get_current_medium();
        const Volume* volume = medium == nullptr ? nullptr : medium->get_volume();

        // Stop if the ray escaped the scene.
        if (!shading_point_ptr->hit_surface())
        {
            if (volume != nullptr)
            {
                // The ray escaped the scene filled with volume, thus its transmission is 0.
                transmission.set(0.0f);
            }
            break;
        }

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == nullptr)
            break;

        const Material::RenderData& render_data = material->get_render_data();

        // Compute transmission of participating media.
        const ShadingRay& volume_ray = shading_point_ptr->get_ray();
        if (volume != nullptr)
        {
            Spectrum volume_transmission;
            void* data = volume->evaluate_inputs(shading_context, volume_ray);
            volume->prepare_inputs(shading_context.get_arena(), volume_ray, data);
            volume->evaluate_transmission(data, volume_ray, volume_transmission);
            transmission *= volume_transmission;
        }

        // Compute alpha.
        Alpha alpha;
        evaluate_alpha(*material, *shading_point_ptr, alpha);
        if (render_data.m_bsdf == nullptr &&
            render_data.m_bssrdf == nullptr &&
            render_data.m_volume != nullptr)
        {
            alpha[0] = 0.0f;
        }

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0f - alpha[0];

        // Stop once we hit full opacity.
        if (max_value(transmission) < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();

        // Continue the ray in the same direction.
        ShadingRay next_ray(
            point,
            current_ray.m_dir,
            current_ray.m_time,
            current_ray.m_flags,
            current_ray.m_depth + (volume == nullptr ? 0 : 1));

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = shading_point_ptr->is_entering();

        // Update the medium list.
        const ObjectInstance& object_instance = shading_point_ptr->get_object_instance();
        if (entering)
            next_ray.add_medium(current_ray, &object_instance, material, 1.0f);
        else
            next_ray.remove_medium(current_ray, &object_instance);

        // Trace ray further.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            next_ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);
    }

    return *shading_point_ptr;
}

const ShadingPoint& Tracer::do_trace_between(
    const ShadingContext&       shading_context,
    const foundation::Vector3d& target,
    const ShadingRay&           ray,
    Spectrum&                   transmission,
    const ShadingPoint*         parent_shading_point)
{
    transmission.set(1.0f);

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = ray.m_org;
    size_t iterations = 0;

    m_shading_points[shading_point_index].clear();
    m_intersector.trace(
        ray,
        m_shading_points[shading_point_index],
        shading_point_ptr);

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

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        const ShadingRay& current_ray = shading_point_ptr->get_ray();

        // Get information about the medium that contains the shadow ray.
        const ShadingRay::Medium* medium = current_ray.get_current_medium();
        const Volume* volume =
            (medium == nullptr) ? nullptr : medium->get_volume();
        const ShadingRay& volume_ray = shading_point_ptr->get_ray();

        // Compute transmission of participating media.
        if (volume != nullptr)
        {
            Spectrum volume_transmission;
            void* data = volume->evaluate_inputs(shading_context, volume_ray);
            volume->prepare_inputs(shading_context.get_arena(), volume_ray, data);
            volume->evaluate_transmission(data, volume_ray, volume_transmission);
            transmission *= volume_transmission;
        }

        // Stop if the ray hit the target.
        if (!shading_point_ptr->hit_surface())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == nullptr)
            break;

        const Material::RenderData& render_data = material->get_render_data();

        // Compute alpha.
        Alpha alpha;
        evaluate_alpha(*material, *shading_point_ptr, alpha);
        if (
            render_data.m_bsdf == nullptr &&
            render_data.m_bssrdf == nullptr &&
            render_data.m_volume != nullptr)
        {
            alpha[0] = 0.0f;
        }

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0f - alpha[0];

        // Stop once we hit full opacity.
        if (max_value(transmission) < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();

        // Continue the ray in the same direction.
        const Vector3d direction = target - point;
        const double dist = norm(direction);

        ShadingRay next_ray(
            point,
            direction / dist,
            0.0,                    // ray tmin
            dist * (1.0 - 1.0e-6),  // ray tmax
            current_ray.m_time,
            current_ray.m_flags,
            current_ray.m_depth + (volume == nullptr ? 0 : 1));

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = shading_point_ptr->is_entering();

        // Update the medium list.
        const ObjectInstance& object_instance = shading_point_ptr->get_object_instance();
        if (entering)
            next_ray.add_medium(current_ray, &object_instance, material, 1.0f);
        else
            next_ray.remove_medium(current_ray, &object_instance);

        // Trace ray further.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            next_ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);
    }

    return *shading_point_ptr;
}

void Tracer::evaluate_alpha(
    const Material&             material,
    const ShadingPoint&         shading_point,
    Alpha&                      alpha) const
{
    alpha = shading_point.get_alpha();

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
}

}   // namespace renderer
