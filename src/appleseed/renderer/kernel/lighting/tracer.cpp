
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

// Interface header.
#include "tracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/phasefunction/phasefunction.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/shadergroup/shadergroup.h"

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
    OSLShaderGroupExec&         shadergroup_exec,
    const float                 transparency_threshold,
    const size_t                max_iterations,
    const bool                  print_details)
  : m_intersector(intersector)
  , m_texture_cache(texture_cache)
  , m_shadergroup_exec(shadergroup_exec)
  , m_assume_no_alpha_mapping(!scene.uses_alpha_mapping())
  , m_transmission_threshold(transparency_threshold)
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
    float&                      transmission,
    const ShadingPoint*         parent_shading_point)
{
    assert(is_normalized(direction));

    transmission = 1.0f;

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
        transmission *= 1.0f - alpha[0];

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
    float&                      transmission,
    const ShadingPoint*         parent_shading_point)
{
    transmission = 1.0f;

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
        transmission *= 1.0f - alpha[0];

        // Stop once we hit full opacity.
        if (transmission < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();
    }

    return *shading_point_ptr;
}

const ShadingPoint& Tracer::do_trace(
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     direction,
    const VisibilityFlags::Type     ray_flags,
    const ShadingRay&               parent_ray,
    Spectrum&                       transmission,
    const ShadingPoint*             parent_shading_point)
{
    assert(is_normalized(direction));

    transmission = Spectrum(1.0f);

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    ShadingRay ray(
        point,
        direction,
        parent_ray.m_time,
        ray_flags,
        parent_ray.m_depth);         // ray depth does not increase when passing through an alpha-mapped surface
    ray.copy_media_from(parent_ray);

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

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        const ShadingRay::Medium* medium = ray.get_current_medium();
        const PhaseFunction* phase_function =
            (medium == nullptr) ? nullptr : medium->get_phase_function();

        // The ray escaped the scene filled with volume, thus its transmission is 0
        if (!shading_point_ptr->hit() && phase_function != nullptr)
            transmission = Spectrum(0.0f);

        // Stop if the ray escaped the scene.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        const Material::RenderData& render_data = material->get_render_data();

        // Compute transmission of participating media.
        const ShadingRay& volume_ray = shading_point_ptr->get_ray();
        if (phase_function != nullptr)
        {
            Spectrum volume_transmission;
            void* data = phase_function->evaluate_inputs(shading_context, volume_ray);
            phase_function->prepare_inputs(shading_context.get_arena(), volume_ray, data);
            phase_function->evaluate_transmission(volume_ray, data, volume_transmission);
            transmission *= volume_transmission;
        }

        // Compute alpha
        float alpha_coef = 0.0f;
        if (render_data.m_bsdf != nullptr)
        {
            Alpha alpha;
            evaluate_alpha(*material, *shading_point_ptr, alpha);
            alpha_coef = alpha[0];
        }
        else if (render_data.m_phase_function == nullptr)
        {
            alpha_coef = 1.0f;
        }

        // Update the transmission factor.
        transmission *= 1.0f - alpha_coef;

        // Stop at the first fully opaque occluder.
        if (alpha_coef >= 1.0f)
            break;

        // Stop once we hit full opacity.
        if (max_value(transmission) < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();

        // Continue the ray in the same direction.
        ray = ShadingRay(
            point,
            direction,
            parent_ray.m_time,
            ray_flags,
            parent_ray.m_depth);

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = shading_point_ptr->is_entering();

        // Update the medium list.
        const ObjectInstance& object_instance = shading_point_ptr->get_object_instance();
        if (entering)
        {
            ray.add_medium(ray, &object_instance, material, 1.0f);
        }
        else ray.remove_medium(ray, &object_instance);
    }

    return *shading_point_ptr;
}

const ShadingPoint& Tracer::do_trace_between(
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     target,
    const VisibilityFlags::Type     ray_flags,
    const ShadingRay&               parent_ray,
    Spectrum&                       transmission,
    const ShadingPoint*             parent_shading_point)
{
    transmission = Spectrum(1.0f);

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    const Vector3d direction = target - point;
    const double dist = norm(direction);

    ShadingRay ray(
        point,
        direction / dist,
        0.0,                    // ray tmin
        dist * (1.0 - 1.0e-6),  // ray tmax
        parent_ray.m_time,
        ray_flags,
        parent_ray.m_depth + 1);
    ray.copy_media_from(parent_ray);

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

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Compute transmission of participating media.
        const ShadingRay::Medium* medium = ray.get_current_medium();
        const PhaseFunction* phase_function =
            (medium == nullptr) ? nullptr : medium->get_phase_function();
        const ShadingRay& volume_ray = shading_point_ptr->get_ray();

        if (phase_function != nullptr)
        {
            Spectrum volume_transmission;
            void* data = phase_function->evaluate_inputs(shading_context, volume_ray);
            phase_function->prepare_inputs(shading_context.get_arena(), volume_ray, data);
            phase_function->evaluate_transmission(volume_ray, data, volume_transmission);
            transmission *= volume_transmission;
        }

        // Stop if the ray hit the target.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == nullptr)
            break;

        const Material::RenderData& render_data = material->get_render_data();

        // Compute alpha.
        float alpha_coef = 0.0f;
        if (render_data.m_bsdf != nullptr)
        {
            Alpha alpha;
            evaluate_alpha(*material, *shading_point_ptr, alpha);
            alpha_coef = alpha[0];
        }
        else if (render_data.m_phase_function == nullptr)
        {
            alpha_coef = 1.0f;
        }

        // Update the transmission factor.
        transmission *= 1.0f - alpha_coef;

        // Stop at the first fully opaque occluder.
        if (alpha_coef >= 1.0f)
            break;

        // Stop once we hit full opacity.
        if (max_value(transmission) < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();

        // Continue the ray in the same direction.
        const Vector3d direction = target - point;
        const double dist = norm(direction);

        ray = ShadingRay(
            point,
            direction / dist,
            0.0,                    // ray tmin
            dist * (1.0 - 1.0e-6),  // ray tmax
            parent_ray.m_time,
            ray_flags,
            parent_ray.m_depth + (phase_function == nullptr) ? 0 : 1);

        // Determine whether the ray is entering or leaving a medium.
        const bool entering = shading_point_ptr->is_entering();

        // Update the medium list.
        const ObjectInstance& object_instance = shading_point_ptr->get_object_instance();
        if (entering)
        {
            ray.add_medium(ray, &object_instance, material, 1.0f);
        }
        else ray.remove_medium(ray, &object_instance);
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
