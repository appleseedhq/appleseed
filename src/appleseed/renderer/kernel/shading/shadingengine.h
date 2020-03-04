
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/autoreleaseptr.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class AOVAccumulatorContainer; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class PixelContext; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingResult; }

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

    // Please refer to the documentation of Entity::on_render_begin().
    bool on_render_begin(
        const Project&              project,
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Please refer to the documentation of Entity::on_frame_begin().
    bool on_frame_begin(
        const Project&              project,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Shade a given intersection point.
    // Returns true if the path should be terminated.
    bool shade(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators,
        ShadingResult&              shading_result) const;

  private:
    foundation::auto_release_ptr<SurfaceShader> m_diagnostic_surface_shader;

    void create_diagnostic_surface_shader(const ParamArray& params);

    bool shade_hit_point(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators,
        ShadingResult&              shading_result) const;

    void shade_environment(
        SamplingContext&            sampling_context,
        const PixelContext&         pixel_context,
        const ShadingContext&       shading_context,
        const ShadingPoint&         shading_point,
        AOVAccumulatorContainer&    aov_accumulators,
        ShadingResult&              shading_result) const;
};


//
// ShadingEngine class implementation.
//

inline bool ShadingEngine::shade(
    SamplingContext&            sampling_context,
    const PixelContext&         pixel_context,
    const ShadingContext&       shading_context,
    const ShadingPoint&         shading_point,
    AOVAccumulatorContainer&    aov_accumulators,
    ShadingResult&              shading_result) const
{
    if (shading_point.hit_surface())
    {
        return shade_hit_point(
            sampling_context,
            pixel_context,
            shading_context,
            shading_point,
            aov_accumulators,
            shading_result);
    }
    else
    {
        shade_environment(
            sampling_context,
            pixel_context,
            shading_context,
            shading_point,
            aov_accumulators,
            shading_result);
        return true;
    }
}

}   // namespace renderer
