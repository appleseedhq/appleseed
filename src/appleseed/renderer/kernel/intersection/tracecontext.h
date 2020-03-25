
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace renderer  { class AssemblyTree; }
namespace renderer  { class Scene; }

namespace renderer
{

//
// The main purpose of the trace context is to hold the root acceleration structures
// needed for ray tracing.
//
// The trace context is shared amongst threads. All methods are thread-safe.
//

class APPLESEED_DLLSYMBOL TraceContext
  : public foundation::NonCopyable
{
  public:
    // Constructor, initializes the trace context for a given scene.
    explicit TraceContext(const Project& project);

    // Destructor.
    ~TraceContext();

    // Get the scene.
    const Scene& get_scene() const;

    // Get the assembly tree.
    const AssemblyTree& get_assembly_tree() const;

    // Synchronize the trace context with the scene.
    void update();

#ifdef APPLESEED_WITH_EMBREE
    void set_use_embree(const bool value);
#endif

  private:
    const Project&  m_project;
    AssemblyTree*   m_assembly_tree;
};


//
// TraceContext class implementation.
//

inline const Scene& TraceContext::get_scene() const
{
    return *m_project.get_scene();
}

inline const AssemblyTree& TraceContext::get_assembly_tree() const
{
    return *m_assembly_tree;
}

}   // namespace renderer
