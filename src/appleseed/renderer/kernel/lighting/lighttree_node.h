
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_NODE_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_NODE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/lighttypes.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"

// Forward declarations.
namespace renderer      { class Scene; }
namespace renderer      { class NonPhysicalLightInfo; }
namespace renderer      { class EmittingTriangle; }

namespace renderer
{

//
// LightTreeNode implementation
//

template<typename AABB> 
class LightTreeNode
    : public foundation::bvh::Node<AABB>
{
  public:
    LightTreeNode()
      : m_node_luminance(0)
    {
    }

    float get_luminance() const
    {
        return m_node_luminance;
    }

    size_t get_level() const
    {
        return m_node_level;
    }

    void set_luminance(const float luminance)
    {
        m_node_luminance = luminance;
    }

    void set_level(const size_t node_level)
    {
        m_node_level = node_level;
    }
  
  private:
    float  m_node_luminance;
    size_t m_node_level;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTREE_NODE_H