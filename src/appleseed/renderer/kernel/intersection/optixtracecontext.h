
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_OPTIXTRACECONTEXT_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_OPTIXTRACECONTEXT_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/optixcontext.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class Scene; }

namespace renderer
{

//
// The main purpose of the trace context is to hold the root acceleration structures
// needed for ray tracing.
//
// The trace context is shared amongst threads. All methods are thread-safe.
//

class APPLESEED_DLLSYMBOL OptixTraceContext
  : public foundation::NonCopyable
{
  public:
    // Constructor, initializes the trace context for a given scene.
    OptixTraceContext(
        const Scene&    scene,
        const int       device_number);

    // Destructor.
    ~OptixTraceContext();

    // Get the scene.
    const Scene& get_scene() const;

    // Synchronize the trace context with the scene.
    void update();

    // Get the OptiX context.
    OptixContext& get_optix_context() const;

    // Validate the OptiX context.
    void validate() const;

  private:
    struct Impl;
    Impl* impl;

    const Scene&            m_scene;
    mutable OptixContext    m_context;
};


//
// OptixTraceContext class implementation.
//

inline const Scene& OptixTraceContext::get_scene() const
{
    return m_scene;
}

inline OptixContext& OptixTraceContext::get_optix_context() const
{
    return m_context;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_OPTIXTRACECONTEXT_H
