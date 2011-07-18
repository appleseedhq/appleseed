
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"

// Forward declarations.
namespace renderer      { class Intersector; }
namespace renderer      { class TextureCache; }

namespace renderer
{

class Tracer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    Tracer(
        const Intersector&              intersector,
        TextureCache&                   texture_cache,
        SamplingContext&                sampling_context);

    // Compute occlusion in a given direction.
    const ShadingPoint& trace(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     direction,
        const ShadingPoint*             parent_shading_point = 0);

    // Compute occlusion between two points.
    const ShadingPoint& trace_between(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingPoint*             parent_shading_point = 0);

    // Return the transmission factor between the origin and the closest opaque occluder.
    double get_transmission() const;

  private:
    const Intersector&                  m_intersector;
    TextureCache&                       m_texture_cache;
    SamplingContext&                    m_sampling_context;
    ShadingPoint                        m_shading_points[2];
    double                              m_transmission;
};


//
// Implementation.
//

inline Tracer::Tracer(
    const Intersector&                  intersector,
    TextureCache&                       texture_cache,
    SamplingContext&                    sampling_context)
  : m_intersector(intersector)
  , m_texture_cache(texture_cache)
  , m_sampling_context(sampling_context)
  , m_transmission(1.0)
{
}

inline double Tracer::get_transmission() const
{
    return m_transmission;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_TRACER_H
