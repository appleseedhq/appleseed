
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_LIGHTTRACING_LIGHTTRACINGSAMPLEGENERATOR_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_LIGHTTRACING_LIGHTTRACINGSAMPLEGENERATOR_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/isamplegenerator.h"
#include "renderer/utility/paramarray.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer      { class AccumulationFramebuffer; }
namespace renderer      { class Frame; }
namespace renderer      { class LightSampler; }
namespace renderer      { class Scene; }
namespace renderer      { class TraceContext; }

namespace renderer
{

class DLLSYMBOL LightTracingSampleGeneratorFactory
  : public ISampleGeneratorFactory
{
  public:
    // Constructor.
    LightTracingSampleGeneratorFactory(
        const Scene&            scene,
        const Frame&            frame,
        const TraceContext&     trace_context,
        const LightSampler&     light_sampler,
        const ParamArray&       params);

    // Delete this instance.
    virtual void release();

    // Return a new sample generator instance.
    virtual ISampleGenerator* create(
        const size_t            generator_index,
        const size_t            generator_count);

    // Create an accumulation framebuffer that fit this sample generator.
    virtual AccumulationFramebuffer* create_accumulation_framebuffer(
        const size_t            canvas_width,
        const size_t            canvas_height);

  private:
    const Scene&                m_scene;
    const Frame&                m_frame;
    const TraceContext&         m_trace_context;
    const LightSampler&         m_light_sampler;
    const ParamArray            m_params;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_LIGHTTRACING_LIGHTTRACINGSAMPLEGENERATOR_H
