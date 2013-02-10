
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Intersector; }
namespace renderer  { class Scene; }
namespace renderer  { class TextureCache; }

namespace renderer
{

class Tracer
  : public foundation::NonCopyable
{
  public:
    // Constructors.
    Tracer(
        const Scene&                    scene,
        const Intersector&              intersector,
        TextureCache&                   texture_cache,
        const float                     transparency_threshold = 0.001f,
        const size_t                    max_iterations = 10000);

    // Compute the transmission in a given direction. Returns the intersection
    // with the closest fully opaque occluder and the transmission factor up
    // to (but excluding) this occluder, or a miss if there is no fully opaque
    // occluder in this direction.
    const ShadingPoint& trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const double                    time,
        double&                         transmission,
        const ShadingPoint*             parent_shading_point = 0);

    // Compute the transmission in a given direction. This variant may take
    // advantage of the fact that the intersection with the closest occluder
    // is not required to deliver higher performances.
    double trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const double                    time,
        const ShadingPoint*             parent_shading_point = 0);

    // Compute the transmission between two points. Returns the intersection
    // with the closest fully opaque occluder and the transmission factor up
    // to (but excluding) this occluder, or a miss if there is no fully opaque
    // occluder in the segment [origin, target).
    const ShadingPoint& trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const double                    time,
        double&                         transmission,
        const ShadingPoint*             parent_shading_point = 0);

    // Compute the transmission between two points. This variant may take
    // advantage of the fact that the intersection with the closest occluder
    // is not required to deliver higher performances.
    double trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const double                    time,
        const ShadingPoint*             parent_shading_point = 0);

  private:
    const Intersector&                  m_intersector;
    TextureCache&                       m_texture_cache;
    const bool                          m_assume_no_alpha_mapping;
    const double                        m_transmission_threshold;
    const size_t                        m_max_iterations;
    ShadingPoint                        m_shading_points[2];
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H
