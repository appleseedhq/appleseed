
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh/bvh_statistics.h"
#include "foundation/math/aabb.h"
#include "foundation/math/intersection.h"
#include "foundation/math/ray.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

// Enable or disable BVH traversal statistics.
#undef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS

namespace foundation {
namespace bvh {

//
// BVH intersector.
//
// The Visitor class must conform to the following prototype:
//
//      class Visitor
//        : public foundation::NonCopyable
//      {
//        public:
//          // Return whether BVH traversal should continue or not.
//          // 'distance' should be set to the distance to the closest hit so far.
//          bool visit(
//              const std::vector<Item>&    items,
//              const std::vector<AABB>&    bboxes,
//              const size_t                begin,
//              const size_t                end,
//              const RayType&              ray,
//              const RayInfoType&          ray_info,
//              const ValueType             tmin,
//              const ValueType             tmax,
//              ValueType&                  distance);
//      };
//

template <
    typename T,
    typename Tree,
    typename Visitor,
    size_t StackSize = 64,
    size_t SortSize = 2
>
class Intersector
  : public NonCopyable
{
  public:
    // Types.
    typedef T ValueType;
    typedef typename Tree::ItemType ItemType;
    typedef typename Tree::NodeType NodeType;
    typedef AABB<T, Tree::Dimension> AABBType;
    typedef Ray<T, Tree::Dimension> RayType;
    typedef RayInfo<T, Tree::Dimension> RayInfoType;

    // Intersect a ray with a given BVH.
    void intersect(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

  private:
    // Entry of the node stack.
    struct NodeEntry
    {
        ValueType   m_tmin;
        ValueType   m_tmax;
        size_t      m_index;
    };
};


//
// Intersector class implementation.
//

#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
#define FOUNDATION_BVH_TRAVERSAL_STATS(x) x
#else
#define FOUNDATION_BVH_TRAVERSAL_STATS(x)
#endif

template <
    typename T,
    typename Tree,
    typename Visitor,
    size_t StackSize,
    size_t SortSize
>
void Intersector<T, Tree, Visitor, StackSize, SortSize>::intersect(
    const Tree&             tree,
    const RayType&          ray,
    const RayInfoType&      ray_info,
    Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&  stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Handle empty trees now so that no leaf is ever empty.
    if (tree.size() == 0)
       return;

    // Check the intersection between the ray and the bounding box of the tree.
    const AABBType root_bbox = AABBType(tree.get_bbox());
    ValueType tmin, tmax;
    if (!foundation::intersect(ray, ray_info, root_bbox, tmin, tmax))
        return;

    // Initialize traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t fetched_nodes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t intersected_items = 0);

    // Initialize the node stack.
    NodeEntry stack[StackSize];
    NodeEntry* stack_ptr = stack;

    // Push the root node to the stack.
    stack_ptr->m_tmin = tmin;
    stack_ptr->m_tmax = tmax;
    stack_ptr->m_index = 0;
    ++stack_ptr;

    // Traverse the tree and intersect leaf nodes.
    ValueType tfar = ray.m_tmax;
    while (stack_ptr > stack)
    {
        // Pop a node from the stack.
        --stack_ptr;

        const double tmin = stack_ptr->m_tmin;
        const double tmax = stack_ptr->m_tmax;

        // Skip nodes that are farther than the closest intersection found so far.
        if (tmin >= tfar)
            continue;

        // Fetch the node.
        FOUNDATION_BVH_TRAVERSAL_STATS(++fetched_nodes);
        const NodeType& node = tree.m_nodes[stack_ptr->m_index];

        if (node.is_leaf())
        {
            const size_t item_begin = node.get_item_index();
            const size_t item_end = item_begin + node.get_item_count();
            assert(item_begin < item_end);

            FOUNDATION_BVH_TRAVERSAL_STATS(++visited_leaves);
            FOUNDATION_BVH_TRAVERSAL_STATS(intersected_items += item_end - item_begin);

            // Visit the leaf.
            ValueType distance;
#ifndef NDEBUG
            distance = ValueType(-1.0);
#endif
            const bool proceed =
                visitor.visit(
                    tree.m_items,
                    tree.m_bboxes,
                    item_begin,
                    item_end,
                    ray,
                    ray_info,
                    tmin,
                    tmax,
                    distance);
            assert(!proceed || distance >= ValueType(0.0));

            // Terminate traversal if the visitor decided so.
            if (!proceed)
                break;

            // Keep track of the distance to the closest intersection.
            if (tfar > distance)
                tfar = distance;
        }
        else
        {
            // Push child nodes to the stack.
            size_t child_index = node.get_child_node_index();
            for (size_t i = 0; i < 2; ++i, ++child_index)
            {
                // Fetch the bounding box of the child node.
                FOUNDATION_BVH_TRAVERSAL_STATS(++fetched_nodes);
                const NodeType& child_node = tree.m_nodes[child_index];
                const AABBType child_bbox = AABBType(child_node.get_bbox());

                // Discard the child node if it isn't intersected by the ray.
                ValueType tmin, tmax;
                if (!foundation::intersect(ray, ray_info, child_bbox, tmin, tmax))
                    continue;

                // Discard the child node if it is farther than the closest intersection so far.
                if (tmin >= tfar)
                    continue;

                // Push the child node to the stack.
                stack_ptr->m_tmin = tmin;
                stack_ptr->m_tmax = tmax;
                stack_ptr->m_index = child_index;
                ++stack_ptr;
            }
        }

        // Move the closest node to the top of the stack.
        const size_t stack_size = stack_ptr - stack;
        if (stack_size > 1)
        {
            const size_t n = stack_size < SortSize ? stack_size : SortSize;
            NodeEntry* ptr = stack_ptr - n;
            NodeEntry* top = stack_ptr - 1;
            for (; ptr < top; ++ptr)
            {
                if (ptr->m_tmin < top->m_tmin)
                    std::swap(*ptr, *top);
            }
        }
    }

    // Store traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_fetched_nodes.insert(fetched_nodes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(intersected_items));
}

#undef FOUNDATION_BVH_TRAVERSAL_STATS

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H
