
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// LightTreeNode class implementation.
//

template <typename AABB>
class LightTreeNode
  : public foundation::bvh::Node<AABB>
{
  public:
    LightTreeNode()
      : m_importance(0.0f)
      , m_root(false)
      , m_parent(0)
    {
    }

    float get_importance() const
    {
        return m_importance;
    }

    size_t get_level() const
    {
        return m_tree_level;
    }

    size_t get_parent() const
    {
        return m_parent;
    }

    bool is_root() const
    {
        return m_root;
    }

    void set_importance(const float importance)
    {
        m_importance = importance;
    }

    // todo: set this during the construction
    void set_level(const size_t node_level)
    {
        m_tree_level = node_level;
    }

    // todo: set this during the construction
    void set_parent(const size_t node_parent)
    {
        m_parent = node_parent;
    }

    // todo: set this during the construction
    void set_root()
    {
        m_root = true;
    }

  private:
    float   m_importance;
    size_t  m_tree_level;
    size_t  m_parent;
    bool    m_root;
};

}   // namespace renderer
