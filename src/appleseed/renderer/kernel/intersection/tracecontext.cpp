
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

// Interface header.
#include "tracecontext.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/assemblytree.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"

// appleseed.foundation headers.
#include "foundation/math/bvh.h"
#include "foundation/string/string.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// TraceContext class implementation.
//

TraceContext::TraceContext(const Project& project)
  : m_project(project)
  , m_assembly_tree(new AssemblyTree(project))
{
    RENDERER_LOG_DEBUG(
        "data structures size:\n"
        "  bvh::NodeType                 %s\n"
        "  GTriangleType                 %s\n"
        "  ShadingPoint                  %s\n"
        "  ShadingRay                    %s\n"
        "  ShadingResult                 %s\n"
        "  TriangleKey                   %s",
        pretty_size(sizeof(TriangleTree::NodeType)).c_str(),
        pretty_size(sizeof(GTriangleType)).c_str(),
        pretty_size(sizeof(ShadingPoint)).c_str(),
        pretty_size(sizeof(ShadingRay)).c_str(),
        pretty_size(sizeof(ShadingResult)).c_str(),
        pretty_size(sizeof(TriangleKey)).c_str());
}

TraceContext::~TraceContext()
{
    delete m_assembly_tree;
}

void TraceContext::update()
{
    m_assembly_tree->update();
}

#ifdef APPLESEED_WITH_EMBREE

void TraceContext::set_use_embree(const bool value)
{
    m_assembly_tree->set_use_embree(value);
}

#endif

}   // namespace renderer
