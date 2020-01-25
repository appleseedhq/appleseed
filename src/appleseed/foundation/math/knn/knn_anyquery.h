
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/distance.h"
#include "foundation/math/knn/knn_statistics.h"
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>

// Enable or disable k-nn query statistics.
#undef FOUNDATION_KNN_ENABLE_QUERY_STATS

namespace foundation {
namespace knn {

template <typename T, std::size_t N>
class AnyQuery
  : public NonCopyable
{
  public:
    typedef T ValueType;
    static const std::size_t Dimension = N;

    typedef Vector<T, N> VectorType;
    typedef Tree<T, N> TreeType;

    explicit AnyQuery(const TreeType& tree);

    bool run(
        const VectorType&   query_point,
        const ValueType     query_max_square_distance
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , QueryStatistics&  stats
#endif
        ) const;

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    bool run(
        const VectorType&   query_point,
        const ValueType     query_max_square_distance) const;
#endif

  private:
    typedef typename TreeType::NodeType NodeType;

    struct NodeEntry
    {
        ValueType           m_dvec_square_norm;
        const NodeType*     m_node;
        VectorType          m_dvec;

        NodeEntry() = default;

        NodeEntry(
            const NodeType*     node,
            const VectorType&   dvec,
            const ValueType     dvec_square_norm)
          : m_dvec_square_norm(dvec_square_norm)
          , m_node(node)
          , m_dvec(dvec)
        {
        }
    };

    const TreeType& m_tree;
};

typedef AnyQuery<float, 2>  AnyQuery2f;
typedef AnyQuery<double, 2> AnyQuery2d;
typedef AnyQuery<float, 3>  AnyQuery3f;
typedef AnyQuery<double, 3> AnyQuery3d;


//
// Implementation.
//

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
#define FOUNDATION_KNN_QUERY_STATS(x) x
#else
#define FOUNDATION_KNN_QUERY_STATS(x)
#endif

template <typename T, std::size_t N>
inline AnyQuery<T, N>::AnyQuery(const TreeType& tree)
  : m_tree(tree)
{
}

template <typename T, std::size_t N>
inline bool AnyQuery<T, N>::run(
    const VectorType&       query_point,
    const ValueType         query_max_square_distance
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    , QueryStatistics&      stats
#endif
    ) const
{
    assert(!m_tree.empty());

    FOUNDATION_KNN_QUERY_STATS(++stats.m_query_count);
    FOUNDATION_KNN_QUERY_STATS(std::size_t fetched_node_count = 0);
    FOUNDATION_KNN_QUERY_STATS(std::size_t visited_leaf_count = 0);
    FOUNDATION_KNN_QUERY_STATS(std::size_t tested_point_count = 0);

    const VectorType* APPLESEED_RESTRICT points = &m_tree.m_points.front();
    const NodeType* APPLESEED_RESTRICT root_node = &m_tree.m_nodes.front();
    const NodeType* APPLESEED_RESTRICT node;

    const std::size_t IdealLeafSize = 40;

    //
    // Step 1:
    //
    //   Find the deepest node of the tree (leaf node or interior node) that contains
    //   the query point and enough other points to compute a maximum search distance.
    //   This descent is very fast since no node is pushed to the stack, and it allows
    //   us to compute an initial maximum search distance which that speed up the real
    //   search later on.
    //

    // Start at the root.
    node = root_node;

    FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

    // In any case, we must stop when we reach a leaf node.
    while (node->is_interior())
    {
        const std::size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();
        const ValueType split_dist = query_point[split_dim] - split_abs;

        // Figure out which child node to follow (the one that contains the query point).
        // Points on the split plane belong to the right child node.
        const std::size_t follow_index = split_dist >= ValueType(0.0) ? 1 : 0;
        const NodeType* APPLESEED_RESTRICT follow_node = root_node + node->get_child_node_index() + follow_index;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        // Stop at the parent node if the child node contains too few points.
        if (follow_node->get_point_count() < IdealLeafSize)
            break;

        // Continue with the child node.
        node = follow_node;
    }

    //
    // Step 2:
    //
    //   Check if one of the points in this node (if it's a leaf node) or below this
    //   node (if it's an interior node) is within the query distance.
    //

    {
        FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

        const VectorType* APPLESEED_RESTRICT point_ptr = points + node->get_point_index();
        const VectorType* APPLESEED_RESTRICT point_end = point_ptr + node->get_point_count();

        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType square_dist = square_distance(*point_ptr++, query_point);

            if (square_dist <= query_max_square_distance)
            {
                FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
                FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(visited_leaf_count));
                FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(tested_point_count));

                return true;
            }
        }
    }

    //
    // Step 3:
    //
    //   Traverse again the tree, starting at the top, but this time we will push
    //   nodes that we don't visit (but that potentially contain candidate points)
    //   to a partially-ordered stack.
    //

#define ORDER_NODE_ENTRIES(LhsIndex, RhsIndex)                                                  \
    if (node_stack[node_stack_size - (LhsIndex)].m_dvec_square_norm <                           \
        node_stack[node_stack_size - (RhsIndex)].m_dvec_square_norm)                            \
    {                                                                                           \
        const NodeEntry tmp = node_stack[node_stack_size - (LhsIndex)];                         \
        node_stack[node_stack_size - (LhsIndex)] = node_stack[node_stack_size - (RhsIndex)];    \
        node_stack[node_stack_size - (RhsIndex)] = tmp;                                         \
    }

    static constexpr std::size_t NodeStackSize = 128;
    NodeEntry node_stack[NodeStackSize];
    std::size_t node_stack_size = 0;

    // Start at the root.
    node = root_node;

    FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

    while (node->is_interior())
    {
        const std::size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();
        const ValueType split_dist = query_point[split_dim] - split_abs;

        // Figure out which child node to follow (the one that contains the query point) and which one to push.
        // Points on the split plane belong to the right child node.
        const std::size_t follow_index = split_dist >= ValueType(0.0) ? 1 : 0;
        const NodeType* APPLESEED_RESTRICT left_child_node = root_node + node->get_child_node_index();
        const NodeType* APPLESEED_RESTRICT follow_node = left_child_node + follow_index;
        const NodeType* APPLESEED_RESTRICT stack_node = left_child_node + 1 - follow_index;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        // Like in the initial step, we stop as soon as we reached a node with enough points.
        if (follow_node->get_point_count() < IdealLeafSize)
            break;

        // Push the node that we don't visit now to the node stack.
        const ValueType square_split_dist = square(split_dist);
        if (square_split_dist < query_max_square_distance)
        {
            VectorType dvec(0.0);
            dvec[split_dim] = split_dist;

            // Push the node to the node stack.
            assert(node_stack_size < NodeStackSize);
            node_stack[node_stack_size++] = NodeEntry(stack_node, dvec, square_split_dist);

            // Order the top levels of the node stack.
            if (node_stack_size >= 4)
            {
                ORDER_NODE_ENTRIES(4, 3);
                ORDER_NODE_ENTRIES(2, 1);
                ORDER_NODE_ENTRIES(4, 2);
                ORDER_NODE_ENTRIES(3, 1);
                ORDER_NODE_ENTRIES(3, 2);
            }
        }

        // Continue with the child node.
        node = follow_node;
    }

    //
    // Step 4:
    //
    //   Visit the nodes in the node stack, from the closest to the farthest,
    //   updating the stack with new nodes to visit later. We terminate as soon
    //   as the closest node is farther than our current maximum search distance.
    //

    while (node_stack_size > 0)
    {
        --node_stack_size;

        const NodeEntry* top_entry = node_stack + node_stack_size;
        if (top_entry->m_dvec_square_norm >= query_max_square_distance)
            continue;

        node = top_entry->m_node;

        const VectorType parent_dvec = top_entry->m_dvec;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        while (node->is_interior())
        {
            const std::size_t split_dim = node->get_split_dim();
            const ValueType split_abs = node->get_split_abs();
            const ValueType split_dist = query_point[split_dim] - split_abs;

            // Figure out which child node to follow (the one that contains the query point) and which one to push.
            // Points on the split plane belong to the right child node.
            const std::size_t follow_index = split_dist >= ValueType(0.0) ? 1 : 0;
            const NodeType* APPLESEED_RESTRICT left_child_node = root_node + node->get_child_node_index();
            const NodeType* APPLESEED_RESTRICT follow_node = left_child_node + follow_index;
            const NodeType* APPLESEED_RESTRICT stack_node = left_child_node + 1 - follow_index;

            FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

            // Try to target nodes with an "ideal" number of points.
            if (follow_node->get_point_count() < IdealLeafSize)
                break;

            // Push the node that we don't visit now to the node stack.
            if (square(split_dist) < query_max_square_distance)
            {
                VectorType dvec = parent_dvec;
                dvec[split_dim] = split_dist;

                // Push the node to the node stack.
                assert(node_stack_size < NodeStackSize);
                node_stack[node_stack_size++] = NodeEntry(stack_node, dvec, square_norm(dvec));

                // Order the top levels of the node stack.
                if (node_stack_size >= 4)
                {
                    ORDER_NODE_ENTRIES(4, 3);
                    ORDER_NODE_ENTRIES(2, 1);
                    ORDER_NODE_ENTRIES(4, 2);
                    ORDER_NODE_ENTRIES(3, 1);
                    ORDER_NODE_ENTRIES(3, 2);
                }
            }

            // Continue with the child node.
            node = follow_node;
        }

        FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

        const VectorType* APPLESEED_RESTRICT point_ptr = points + node->get_point_index();
        const VectorType* APPLESEED_RESTRICT point_end = point_ptr + node->get_point_count();

        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType square_dist = square_distance(*point_ptr++, query_point);

            if (square_dist <= query_max_square_distance)
            {
                FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
                FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(visited_leaf_count));
                FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(tested_point_count));

                return true;
            }
        }
    }

#undef ORDER_NODE_ENTRIES

    FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(visited_leaf_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(tested_point_count));

    return false;
}

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS

template <typename T, std::size_t N>
inline bool AnyQuery<T, N>::run(
    const VectorType&       query_point,
    const ValueType         query_max_square_distance) const
{
    QueryStatistics stats;
    return run(query_point, query_max_square_distance, stats);
}

#endif

}   // namespace knn
}   // namespace foundation
