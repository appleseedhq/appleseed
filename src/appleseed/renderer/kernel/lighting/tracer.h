
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Material;}
namespace renderer  { class OSLShaderGroupExec; }
namespace renderer  { class Scene; }
namespace renderer  { class ShadingContext; }

namespace renderer
{

//
// The Tracer class wraps the Intersector class and allows to compute
// visibility from a given point along a given direction, as well as
// point-to-point visibility. It automatically takes into account alpha
// transparency.
//

class Tracer
  : public foundation::NonCopyable
{
  public:
    // Constructors.
    Tracer(
        const Scene&                    scene,
        const Intersector&              intersector,
        OSLShaderGroupExec&             shadergroup_exec,
        const float                     transparency_threshold = 0.001f,
        const size_t                    max_iterations = 1000,
        const bool                      print_details = true);

    // Compute the transmission in a given direction.
    // Returns the transmission factor up to (but excluding) this occluder.
    void trace_simple(
        const ShadingContext&           shading_context,
        const ShadingRay&               ray,
        Spectrum&                       transmission);
    void trace_simple(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const ShadingRay&               ray,
        Spectrum&                       transmission);

    // Compute the transmission between two points.
    // Returns the transmission factor up to (but excluding) this occluder.
    void trace_between_simple(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    void trace_between_simple(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const ShadingRay&               parent_ray,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    void trace_between_simple(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingRay&               parent_ray,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    void trace_between_simple(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingRay::Time&         ray_time,
        const VisibilityFlags::Type     ray_flags,
        const ShadingRay::DepthType     ray_depth,
        Spectrum&                       transmission);

    // Compute the transmission in a given direction.
    // Returns the intersection with the closest fully opaque occluder
    // and the transmission factor up to (but excluding) this occluder,
    // or a miss if there is no fully opaque occluder in this direction.
    const ShadingPoint& trace_full(
        const ShadingContext&           shading_context,
        const ShadingRay&               ray,
        Spectrum&                       transmission);
    const ShadingPoint& trace_full(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const ShadingRay&               ray,
        Spectrum&                       transmission);

    // Compute the transmission between two points.
    // Returns the intersection with the closest fully opaque occluder
    // and the transmission factor up to (but excluding) this occluder,
    // or a miss if there is no fully opaque occluder in the segment [origin, target).
    const ShadingPoint& trace_between_full(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    const ShadingPoint& trace_between_full(
        const ShadingContext&           shading_context,
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const ShadingRay&               parent_ray,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    const ShadingPoint& trace_between_full(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingRay&               parent_ray,
        const VisibilityFlags::Type     ray_flags,
        Spectrum&                       transmission);
    const ShadingPoint& trace_between_full(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingRay::Time&         ray_time,
        const VisibilityFlags::Type     ray_flags,
        const ShadingRay::DepthType     ray_depth,
        Spectrum&                       transmission);

  private:
    const Intersector&                  m_intersector;
    OSLShaderGroupExec&                 m_shadergroup_exec;
    const bool                          m_assume_no_alpha_mapping;
    const bool                          m_assume_no_participating_media;
    const float                         m_transmission_threshold;
    const size_t                        m_max_iterations;
    ShadingPoint                        m_shading_points[2];

    const ShadingPoint& do_trace(
        const ShadingContext&           shading_context,
        const ShadingRay&               ray,
        Spectrum&                       transmission,
        const ShadingPoint*             parent_shading_point);

    const ShadingPoint& do_trace_between(
        const ShadingContext&           shading_context,
        const foundation::Vector3d&     target,
        const ShadingRay&               ray,
        Spectrum&                       transmission,
        const ShadingPoint*             parent_shading_point);

    void evaluate_alpha(
        const Material&                 material,
        const ShadingPoint&             shading_point,
        Alpha&                          alpha) const;
};


//
// Tracer class implementation.
//

inline void Tracer::trace_simple(
    const ShadingContext&               shading_context,
    const ShadingRay&                   ray,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
        transmission.set(m_intersector.trace_probe(ray) ? 0.0f : 1.0f);
    else
    {
        const ShadingPoint& shading_point =
            trace_full(
                shading_context,
                ray,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline void Tracer::trace_simple(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const ShadingRay&                   ray,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
        transmission.set(m_intersector.trace_probe(ray, &origin) ? 0.0f : 1.0f);
    else
    {
        const ShadingPoint& shading_point =
            trace_full(
                shading_context,
                origin,
                ray,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline void Tracer::trace_between_simple(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
    {
        const foundation::Vector3d direction = target - origin.get_point();
        const double dist = foundation::norm(direction);

        const ShadingRay ray(
            origin.get_point(),
            direction / dist,
            0.0,                        // ray tmin
            dist * (1.0 - 1.0e-6),      // ray tmax
            origin.get_time(),
            ray_flags,
            origin.get_ray().m_depth + 1);

        transmission.set(m_intersector.trace_probe(ray, &origin) ? 0.0f : 1.0f);
    }
    else
    {
        const ShadingPoint& shading_point =
            trace_between_full(
                shading_context,
                origin,
                target,
                ray_flags,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline void Tracer::trace_between_simple(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const ShadingRay&                   parent_ray,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
    {
        const foundation::Vector3d direction = target - origin.get_point();
        const double dist = foundation::norm(direction);

        const ShadingRay ray(
            origin.get_point(),
            direction / dist,
            0.0,                        // ray tmin
            dist * (1.0 - 1.0e-6),      // ray tmax
            parent_ray.m_time,
            ray_flags,
            parent_ray.m_depth);

        transmission.set(m_intersector.trace_probe(ray, &origin) ? 0.0f : 1.0f);
    }
    else
    {
        const ShadingPoint& shading_point =
            trace_between_full(
                shading_context,
                origin,
                target,
                parent_ray,
                ray_flags,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline void Tracer::trace_between_simple(
    const ShadingContext&               shading_context,
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const ShadingRay&                   parent_ray,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
    {
        const foundation::Vector3d direction = target - origin;
        const double dist = foundation::norm(direction);

        const ShadingRay ray(
            origin,
            direction / dist,
            0.0,                        // ray tmin
            dist * (1.0 - 1.0e-6),      // ray tmax
            parent_ray.m_time,
            ray_flags,
            parent_ray.m_depth);

        transmission.set(m_intersector.trace_probe(ray) ? 0.0f : 1.0f);
    }
    else
    {
        const ShadingPoint& shading_point =
            trace_between_full(
                shading_context,
                origin,
                target,
                parent_ray,
                ray_flags,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline void Tracer::trace_between_simple(
    const ShadingContext&               shading_context,
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const ShadingRay::Time&             ray_time,
    const VisibilityFlags::Type         ray_flags,
    const ShadingRay::DepthType         ray_depth,
    Spectrum&                           transmission)
{
    if (m_assume_no_alpha_mapping && m_assume_no_participating_media)
    {
        const foundation::Vector3d direction = target - origin;
        const double dist = foundation::norm(direction);

        const ShadingRay ray(
            origin,
            direction / dist,
            0.0,                        // ray tmin
            dist * (1.0 - 1.0e-6),      // ray tmax
            ray_time,
            ray_flags,
            ray_depth);

        transmission.set(m_intersector.trace_probe(ray) ? 0.0f : 1.0f);
    }
    else
    {
        const ShadingPoint& shading_point =
            trace_between_full(
                shading_context,
                origin,
                target,
                ray_time,
                ray_flags,
                ray_depth,
                transmission);

        if (shading_point.hit_surface())
            transmission.set(0.0f);
    }
}

inline const ShadingPoint& Tracer::trace_full(
    const ShadingContext&               shading_context,
    const ShadingRay&                   ray,
    Spectrum&                           transmission)
{
    return
        do_trace(
            shading_context,
            ray,
            transmission,
            nullptr);
}

inline const ShadingPoint& Tracer::trace_full(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const ShadingRay&                   ray,
    Spectrum&                           transmission)
{
    return
        do_trace(
            shading_context,
            ray,
            transmission,
            &origin);
}

inline const ShadingPoint& Tracer::trace_between_full(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    const foundation::Vector3d direction = target - origin.get_point();
    const double dist = foundation::norm(direction);

    ShadingRay ray(
        origin.get_point(),
        direction / dist,
        0.0,
        dist * (1.0 - 1.0e-6),
        origin.get_ray().m_time,
        ray_flags,
        origin.get_ray().m_depth + 1);

    return
        do_trace_between(
            shading_context,
            target,
            ray,
            transmission,
            &origin);
}

inline const ShadingPoint& Tracer::trace_between_full(
    const ShadingContext&               shading_context,
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const ShadingRay&                   parent_ray,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    const foundation::Vector3d direction = target - origin.get_point();
    const double dist = foundation::norm(direction);

    ShadingRay ray(
        origin.get_point(),
        direction / dist,
        0.0,
        dist * (1.0 - 1.0e-6),
        parent_ray.m_time,
        ray_flags,
        parent_ray.m_depth + 1);

    ray.copy_media_from(parent_ray);

    return
        do_trace_between(
            shading_context,
            target,
            ray,
            transmission,
            &origin);
}

inline const ShadingPoint& Tracer::trace_between_full(
    const ShadingContext&               shading_context,
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const ShadingRay&                   parent_ray,
    const VisibilityFlags::Type         ray_flags,
    Spectrum&                           transmission)
{
    const double dist = foundation::norm(target - origin);

    ShadingRay ray(
        origin,
        (target - origin) / dist,
        0.0,
        dist * (1.0 - 1.0e-6),
        parent_ray.m_time,
        ray_flags,
        parent_ray.m_depth + 1);

    ray.copy_media_from(parent_ray);

    return
        do_trace_between(
            shading_context,
            target,
            ray,
            transmission,
            nullptr);
}

inline const ShadingPoint& Tracer::trace_between_full(
    const ShadingContext&               shading_context,
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const ShadingRay::Time&             ray_time,
    const VisibilityFlags::Type         ray_flags,
    const ShadingRay::DepthType         ray_depth,
    Spectrum&                           transmission)
{
    const double dist = foundation::norm(target - origin);

    const ShadingRay ray(
        origin,
        (target - origin) / dist,
        dist * 1.0e-6,
        dist * (1.0 - 1.0e-6),
        ray_time,
        ray_flags,
        ray_depth + 1);

    return
        do_trace_between(
            shading_context,
            target,
            ray,
            transmission,
            nullptr);
}

}   // namespace renderer
