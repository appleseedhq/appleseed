
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
#include "foundation/math/ray.h"
#include "foundation/math/voxel/voxel_statistics.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Enable or disable voxel tree traversal statistics.
#undef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS

namespace foundation {
namespace voxel {

//
// Voxel tree intersector.
//

template <typename T, typename Tree, size_t S = 64>
class Intersector
  : public NonCopyable
{
  public:
    // Types.
    typedef T ValueType;
    typedef typename Tree::NodeType NodeType;
    typedef Ray<T, Tree::Dimension> RayType;
    typedef RayInfo<T, Tree::Dimension> RayInfoType;

    // Intersect a ray with a given voxel tree.
    // If an intersection was found, the distance to the voxel
    // is stored in distance; otherwise distance is left unaltered.
    bool intersect(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        const bool              solid,
        ValueType&              distance
#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

  private:
    // Node stack size.
    static const size_t StackSize = S;

    // Entry of the node stack.
    struct NodeEntry
    {
        ValueType           m_tfar;
        const NodeType*     m_node;
    };
};


//
// Intersector class implementation.
//

#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
#define FOUNDATION_VOXEL_TRAVERSAL_STATS(x) x
#else
#define FOUNDATION_VOXEL_TRAVERSAL_STATS(x)
#endif

template <typename T, typename Tree, size_t S>
bool Intersector<T, Tree, S>::intersect(
    const Tree&             tree,
    const RayType&          ray,
    const RayInfoType&      ray_info,
    const bool              solid,
    ValueType&              distance
#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&  stats
#endif
    ) const
{
    assert(!tree.m_nodes.empty());

    // Initialize traversal statistics.
    FOUNDATION_VOXEL_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_VOXEL_TRAVERSAL_STATS(size_t visited_interior = 0);
    FOUNDATION_VOXEL_TRAVERSAL_STATS(size_t visited_leaves = 0);

    // Initialize the node stack.
    NodeEntry  stack[StackSize];
    NodeEntry* stack_ptr = stack;

    // Start at the root node.
    const NodeType* node = &tree.m_nodes.front();
    ValueType tnear = ray.m_tmin;
    ValueType tfar = ray.m_tmax;

    // Traverse the tree and intersect leaf nodes.
    while (true)
    {
        // Traverse the tree until a leaf is reached.
        while (node->is_interior())
        {
            // Update statistics.
            FOUNDATION_VOXEL_TRAVERSAL_STATS(++visited_interior);

            // Get the splitting dimension and abscissa.
            const size_t split_dim = node->get_split_dim();
            const ValueType split_abs = node->get_split_abs();

            // Compute the intersection of the splitting plane with the ray.
            const ValueType t = (split_abs - ray.m_org[split_dim]) * ray_info.m_rcp_dir[split_dim];

            // Get child node index and ray direction sign.
            node = &tree.m_nodes[node->get_child_node_index()];
            const size_t sgn_dir = ray_info.m_sgn_dir[split_dim];

            if (t < tnear)
            {
                // Follow the back node.
                node += sgn_dir;
            }
            else
            {
                // Push the back node on the stack.
                if (t < tfar)
                {
                    assert(stack_ptr < &stack[StackSize]);
                    stack_ptr->m_tfar = tfar;
                    stack_ptr->m_node = node + sgn_dir;
                    ++stack_ptr;
                    tfar = t;
                }

                // Follow the front node.
                node += 1 - sgn_dir;
            }
        }

        // Update statistics.
        FOUNDATION_VOXEL_TRAVERSAL_STATS(++visited_leaves);

        // Terminate traversal as soon as a solid/empty leaf is hit.
        if (node->is_solid() == solid)
        {
            // Store traversal statistics.
            FOUNDATION_VOXEL_TRAVERSAL_STATS(stats.m_visited_interior.insert(visited_interior));
            FOUNDATION_VOXEL_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));

            // Report intersection.
            distance = tnear;
            return true;
        }

        // Terminate traversal if there is no more nodes to visit.
        if (stack_ptr == stack)
        {
            // Store traversal statistics.
            FOUNDATION_VOXEL_TRAVERSAL_STATS(stats.m_visited_interior.insert(visited_interior));
            FOUNDATION_VOXEL_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));

            // No intersection.
            return false;
        }

        // Pop the next node from the stack, and update tnear and tfar.
        --stack_ptr;
        tnear = tfar;
        tfar = stack_ptr->m_tfar;
        node = stack_ptr->m_node;
    }
}

#undef FOUNDATION_VOXEL_TRAVERSAL_STATS

}   // namespace voxel
}   // namespace foundation
