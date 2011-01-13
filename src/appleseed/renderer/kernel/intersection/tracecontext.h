
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TRACECONTEXT_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TRACECONTEXT_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Forward declarations.
namespace renderer  { class AssemblyTree; }
namespace renderer  { class Scene; }

namespace renderer
{

//
// The trace context main purpose is to hold the acceleration structures
// needed for ray tracing,
//
// The trace context is shared amongst threads. All methods are thread-safe.
//

class RENDERERDLL TraceContext
  : public foundation::NonCopyable
{
  public:
    // Constructor, initializes the trace context for a given scene.
    explicit TraceContext(const Scene& scene);

    // Destructor.
    ~TraceContext();

    // Get the scene.
    const Scene& get_scene() const;

    // Get the assembly tree.
    const AssemblyTree& get_assembly_tree() const;

  private:
    const Scene&        m_scene;
    const AssemblyTree* m_assembly_tree;
};


//
// TraceContext class implementation.
//

// Get the scene.
inline const Scene& TraceContext::get_scene() const
{
    return m_scene;
}

// Get the assembly tree.
inline const AssemblyTree& TraceContext::get_assembly_tree() const
{
    return *m_assembly_tree;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TRACECONTEXT_H
