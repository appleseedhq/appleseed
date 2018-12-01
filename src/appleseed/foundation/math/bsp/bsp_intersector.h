
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
#include "foundation/math/bsp/bsp_statistics.h"
#include "foundation/math/ray.h"

// Standard headers.
#include <cassert>
#include <cstddef>

// Enable or disable BSP tree traversal statistics.
#undef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS

namespace foundation {
namespace bsp {

//
// BSP tree intersector.
//
// The Visitor class must conform to the following prototype:
//
//      class Visitor
//        : public foundation::NonCopyable
//      {
//        public:
//          // Return the distance to the closest intersection so far.
//          ValueType visit(
//              const Leaf*         leaf,
//              const RayType&      ray,
//              const RayInfoType&  ray_info);
//      };
//
// The fact that the visit() method of the Visitor must return the distance
// to the closest intersection found so far implies that it is responsible
// for tracking this distance across calls to visit().
//

template <typename T, typename Tree, typename Visitor, typename Ray, size_t S = 64>
class Intersector
  : public NonCopyable
{
  public:
    // Types.
    typedef T ValueType;
    typedef typename Tree::NodeType NodeType;
    typedef typename Tree::LeafType LeafType;
    typedef Ray RayType;
    typedef RayInfo<T, Tree::Dimension> RayInfoType;

    // Intersect a ray with a given BSP tree.
    void intersect(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
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

#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
#define FOUNDATION_BSP_TRAVERSAL_STATS(x) x
#else
#define FOUNDATION_BSP_TRAVERSAL_STATS(x)
#endif

template <typename T, typename Tree, typename Visitor, typename Ray, size_t S>
void Intersector<T, Tree, Visitor, Ray, S>::intersect(
    const Tree&             tree,
    const RayType&          ray,
    const RayInfoType&      ray_info,
    Visitor&                visitor
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&  stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Initialize traversal statistics.
    FOUNDATION_BSP_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BSP_TRAVERSAL_STATS(size_t visited_interior = 0);
    FOUNDATION_BSP_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BSP_TRAVERSAL_STATS(size_t intersected_items = 0);

    // Initialize the node stack.
    NodeEntry stack[StackSize];
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
            FOUNDATION_BSP_TRAVERSAL_STATS(++visited_interior);

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
                if (!(t >= tfar))
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

        if (node->get_leaf_size() > 0)
        {
            // Update statistics.
            FOUNDATION_BSP_TRAVERSAL_STATS(++visited_leaves);
            FOUNDATION_BSP_TRAVERSAL_STATS(intersected_items += node->get_leaf_size());

            // Fetch the leaf.
            const LeafType* leaf = tree.m_leaves[node->get_leaf_index()];

            // Visit the leaf.
            const ValueType distance = visitor.visit(leaf, ray, ray_info);

            // If an intersection was found within the bounds of the leaf,
            // it means we have found the closest intersection. Otherwise,
            // we cannot conclude immediately and we must continue traversal.
            if (distance < tfar)
                break;
        }

        // Terminate traversal if there is no more nodes to visit.
        if (stack_ptr == stack)
            break;

        // Pop the next node from the stack, and update tnear and tfar.
        --stack_ptr;
        tnear = tfar;
        tfar = stack_ptr->m_tfar;
        node = stack_ptr->m_node;
    }

    // Store traversal statistics.
    FOUNDATION_BSP_TRAVERSAL_STATS(stats.m_visited_interior.insert(visited_interior));
    FOUNDATION_BSP_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BSP_TRAVERSAL_STATS(stats.m_intersected_items.insert(intersected_items));
}

#undef FOUNDATION_BSP_TRAVERSAL_STATS

}   // namespace bsp
}   // namespace foundation
