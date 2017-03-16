
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGENGINE_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGENGINE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/autoreleaseptr.h"

// Forward declarations.
namespace renderer  { class AOVAccumulatorContainer; }
namespace renderer  { class ParamArray; }
namespace renderer  { class PixelContext; }
namespace renderer  { class ShadingContext; }

namespace renderer
{

//
// Shading engine.
//

class ShadingEngine
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit ShadingEngine(const ParamArray& params);

    // Shade a given intersection point.
    void shade(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators) const;

  private:
    foundation::auto_release_ptr<SurfaceShader> m_diagnostic_surface_shader;

    void create_diagnostic_surface_shader(const ParamArray& params);

    void shade_hit_point(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators) const;

    void shade_environment(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators) const;
};


//
// ShadingEngine class implementation.
//

inline void ShadingEngine::shade(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators) const
{
    if (shading_point.hit())
    {
        return
            shade_hit_point(
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                aov_accumulators);
    }
    else
    {
        return
            shade_environment(
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                aov_accumulators);
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGENGINE_H
