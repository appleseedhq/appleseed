
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Intersector; }
namespace renderer  { class Material;}
#ifdef WITH_OSL
namespace renderer  { class OSLShaderGroupExec; }
#endif
namespace renderer  { class Scene; }
namespace renderer  { class ShadingPoint; }
namespace renderer  { class TextureCache; }

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
        TextureCache&                   texture_cache,
#ifdef WITH_OSL
        OSLShaderGroupExec*             shadergroup_exec,
#endif
        const float                     transparency_threshold = 0.001f,
        const size_t                    max_iterations = 1000,
        const bool                      print_details = true);

    // Compute the transmission in a given direction. Returns the intersection
    // with the closest fully opaque occluder and the transmission factor up
    // to (but excluding) this occluder, or a miss if there is no fully opaque
    // occluder in this direction.
    const ShadingPoint& trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth,
        double&                         transmission);
    const ShadingPoint& trace(
        const ShadingPoint&             origin,
        const foundation::Vector3d&     direction,
        const ShadingRay::Type          ray_type,
        double&                         transmission);

    // Compute the transmission in a given direction. This variant may take
    // advantage of the fact that the intersection with the closest occluder
    // is not required to deliver higher performances.
    double trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth);
    double trace(
        const ShadingPoint&             origin,
        const foundation::Vector3d&     direction,
        const ShadingRay::Type          ray_type);

    // Compute the transmission between two points. Returns the intersection
    // with the closest fully opaque occluder and the transmission factor up
    // to (but excluding) this occluder, or a miss if there is no fully opaque
    // occluder in the segment [origin, target).
    const ShadingPoint& trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth,
        double&                         transmission);
    const ShadingPoint& trace_between(
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const ShadingRay::Type          ray_type,
        double&                         transmission);

    // Compute the transmission between two points. This variant may take
    // advantage of the fact that the intersection with the closest occluder
    // is not required to deliver higher performances.
    double trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth);
    double trace_between(
        const ShadingPoint&             origin,
        const foundation::Vector3d&     target,
        const ShadingRay::Type          ray_type);

  private:
    const Intersector&                  m_intersector;
    TextureCache&                       m_texture_cache;
#ifdef WITH_OSL
    // TODO: it would be better if Tracer had a reference to the OSLShaderGroupExec
    // instance instead of a pointer, but that'd mess with the unit tests.
    // It's probably better to change it when OSL support is an integral part of appleseed.
    OSLShaderGroupExec*                 m_shadergroup_exec;
#endif
    const bool                          m_assume_no_alpha_mapping;
    const double                        m_transmission_threshold;
    const size_t                        m_max_iterations;
    ShadingPoint                        m_shading_points[2];

    const ShadingPoint& do_trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth,
        double&                         transmission,
        const ShadingPoint*             parent_shading_point);

    const ShadingPoint& do_trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const double                    time,
        const ShadingRay::Type          ray_type,
        const ShadingRay::DepthType     ray_depth,
        double&                         transmission,
        const ShadingPoint*             parent_shading_point);

    void evaluate_alpha(
        const Material&                 material,
        const ShadingPoint&             shading_point,
        Alpha&                          alpha) const;
};


//
// Tracer class implementation.
//

inline const ShadingPoint& Tracer::trace(
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         direction,
    const double                        time,
    const ShadingRay::Type              ray_type,
    const ShadingRay::DepthType         ray_depth,
    double&                             transmission)
{
    return
        do_trace(
            origin,
            direction,
            time,
            ray_type,
            ray_depth,
            transmission,
            0);
}

inline const ShadingPoint& Tracer::trace(
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         direction,
    const ShadingRay::Type              ray_type,
    double&                             transmission)
{
    return
        do_trace(
            origin.get_biased_point(direction),
            direction,
            origin.get_time(),
            ray_type,
            origin.get_ray().m_depth + 1,
            transmission,
            &origin);
}

inline double Tracer::trace(
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         direction,
    const double                        time,
    const ShadingRay::Type              ray_type,
    const ShadingRay::DepthType         ray_depth)
{
    if (m_assume_no_alpha_mapping)
    {
        const ShadingRay ray(
            origin,
            direction,
            time,
            ray_type,
            ray_depth);

        return m_intersector.trace_probe(ray) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace(
                origin,
                direction,
                time,
                ray_type,
                ray_depth,
                transmission);

        return shading_point.hit() ? 0.0 : transmission;
    }
}

inline double Tracer::trace(
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         direction,
    const ShadingRay::Type              type)
{
    if (m_assume_no_alpha_mapping)
    {
        const ShadingRay ray(
            origin.get_biased_point(direction),
            direction,
            origin.get_time(),
            type,
            origin.get_ray().m_depth + 1);

        return m_intersector.trace_probe(ray, &origin) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace(
                origin,
                direction,
                type,
                transmission);

        return shading_point.hit() ? 0.0 : transmission;
    }
}

inline const ShadingPoint& Tracer::trace_between(
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const double                        time,
    const ShadingRay::Type              ray_type,
    const ShadingRay::DepthType         ray_depth,
    double&                             transmission)
{
    return
        do_trace_between(
            origin,
            target,
            time,
            ray_type,
            ray_depth,
            transmission,
            0);
}

inline const ShadingPoint& Tracer::trace_between(
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const ShadingRay::Type              type,
    double&                             transmission)
{
    return
        do_trace_between(
            origin.get_biased_point(target - origin.get_point()),
            target,
            origin.get_time(),
            type,
            origin.get_ray().m_depth + 1,
            transmission,
            &origin);
}

inline double Tracer::trace_between(
    const foundation::Vector3d&         origin,
    const foundation::Vector3d&         target,
    const double                        time,
    const ShadingRay::Type              ray_type,
    const ShadingRay::DepthType         ray_depth)
{
    if (m_assume_no_alpha_mapping)
    {
        const ShadingRay ray(
            origin,
            target - origin,
            0.0,                        // ray tmin
            1.0 - 1.0e-6,               // ray tmax
            time,
            ray_type,
            ray_depth);

        return m_intersector.trace_probe(ray) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace_between(
                origin,
                target,
                time,
                ray_type,
                ray_depth,
                transmission);

        return shading_point.hit() ? 0.0 : transmission;
    }
}

inline double Tracer::trace_between(
    const ShadingPoint&                 origin,
    const foundation::Vector3d&         target,
    const ShadingRay::Type              type)
{
    if (m_assume_no_alpha_mapping)
    {
        const foundation::Vector3d direction = target - origin.get_point();

        const ShadingRay ray(
            origin.get_biased_point(direction),
            direction,
            0.0,                        // ray tmin
            1.0 - 1.0e-6,               // ray tmax
            origin.get_time(),
            type,
            origin.get_ray().m_depth + 1);

        return m_intersector.trace_probe(ray, &origin) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace_between(
                origin,
                target,
                type,
                transmission);

        return shading_point.hit() ? 0.0 : transmission;
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H
