
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_QUERY_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_QUERY_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/knn/knn_answer.h"
#include "foundation/math/knn/knn_statistics.h"
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/distance.h"
#include "foundation/math/fp.h"
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

template <typename T, size_t N>
class Query
  : public NonCopyable
{
  public:
    typedef T ValueType;
    static const size_t Dimension = N;

    typedef Vector<T, N> VectorType;
    typedef Tree<T, N> TreeType;
    typedef Answer<T> AnswerType;

    Query(
        const TreeType&     tree,
        AnswerType&         answer);

    void run(
        const VectorType&   query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , QueryStatistics&  stats
#endif
        ) const;

    void run(
        const VectorType&   query_point,
        const ValueType     query_max_square_distance
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , QueryStatistics&  stats
#endif
        ) const;

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    void run(
        const VectorType&   query_point) const;

    void run(
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

        NodeEntry() {}

        NodeEntry(
            const NodeType*     node,
            const VectorType&   dvec,
            const ValueType     dvec_square_norm)
          : m_dvec_square_norm(dvec_square_norm)
          , m_node(node)
          , m_dvec(dvec)
        {
        }

        bool operator<(const NodeEntry& rhs) const
        {
            return m_dvec_square_norm > rhs.m_dvec_square_norm;
        }
    };

    const TreeType&         m_tree;
    AnswerType&             m_answer;
};

typedef Query<float, 2>  Query2f;
typedef Query<double, 2> Query2d;
typedef Query<float, 3>  Query3f;
typedef Query<double, 3> Query3d;


//
// Implementation.
//

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
#define FOUNDATION_KNN_QUERY_STATS(x) x
#else
#define FOUNDATION_KNN_QUERY_STATS(x)
#endif

template <typename T, size_t N>
inline Query<T, N>::Query(
    const TreeType&         tree,
    AnswerType&             answer)
  : m_tree(tree)
  , m_answer(answer)
{
}

template <typename T, size_t N>
inline void Query<T, N>::run(
    const VectorType&       query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    , QueryStatistics&      stats
#endif
    ) const
{
    run(
        query_point,
        std::numeric_limits<ValueType>::max()
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , stats
#endif
        );
}

template <typename T, size_t N>
inline void Query<T, N>::run(
    const VectorType&       query_point,
    const ValueType         query_max_square_distance
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    , QueryStatistics&      stats
#endif
    ) const
{
    assert(!m_tree.empty());

    m_answer.clear();

    FOUNDATION_KNN_QUERY_STATS(++stats.m_query_count);
    FOUNDATION_KNN_QUERY_STATS(size_t fetched_node_count = 0);
    FOUNDATION_KNN_QUERY_STATS(size_t visited_leaf_count = 0);
    FOUNDATION_KNN_QUERY_STATS(size_t tested_point_count = 0);

    const VectorType* APPLESEED_RESTRICT points = &m_tree.m_points.front();
    const NodeType* APPLESEED_RESTRICT nodes = &m_tree.m_nodes.front();
    const size_t max_answer_size = m_answer.m_max_size;

    //
    // Step 1:
    //
    //   Find the deepest node of the tree (leaf node or interior node) that contains
    //   the query point and enough other points to compute a maximum search distance.
    //   This descent is very fast since no nodes are pushed to a stack, and it will
    //   allow us to compute an initial maximum search distance, which will speed up
    //   the "real" search later on.
    //

    const NodeType* APPLESEED_RESTRICT node = nodes;

    FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

    // In any case, we must stop when we reach a leaf node.
    while (node->is_interior())
    {
        const size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();
        const ValueType split_dist = query_point[split_dim] - split_abs;

        const NodeType* APPLESEED_RESTRICT child_node = nodes + node->get_child_node_index();

        if (split_dist > ValueType(0.0))
            ++child_node;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        // The child node contains too few points.
        if (child_node->get_point_count() < max_answer_size)
        {
            // Keep the child node anyway if its sibling is too far.
            // Otherwise keep the current node.
            if (square(split_dist) > query_max_square_distance)
                node = child_node;
            break;
        }

        // The child node contains enough points.
        node = child_node;
    }

    //
    // Step 2:
    //
    //   Collect all the points in this node (if it's a leaf node) or below this node
    //   (if it's an interior node), and compute an initial maximum search distance.
    //

    ValueType max_square_dist(0.0);

    {
        FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

        size_t point_index = node->get_point_index();
        const VectorType* APPLESEED_RESTRICT point_ptr = points + point_index;
        const VectorType* APPLESEED_RESTRICT point_end = point_ptr + node->get_point_count();

        // First, we fill up the answer like an array.
        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType square_dist = square_distance(*point_ptr++, query_point);

            if (square_dist <= query_max_square_distance)
            {
                m_answer.array_insert(point_index, square_dist);

                if (max_square_dist < square_dist)
                    max_square_dist = square_dist;

                ++point_index;

                if (m_answer.m_size == max_answer_size)
                    break;

                continue;
            }

            ++point_index;
        }

        if (m_answer.m_size == max_answer_size)
        {
            // The answer is full, so we transform it into a heap.
            m_answer.make_heap();

            // Then, we insert the remaining points into the answer.
            while (point_ptr < point_end)
            {
                FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

                const ValueType square_dist = square_distance(*point_ptr++, query_point);

                if (square_dist < max_square_dist)
                {
                    m_answer.heap_insert(point_index, square_dist);
                    max_square_dist = m_answer.top().m_square_dist;
                }

                ++point_index;
            }
        }
        else
        {
            // We ran out of points.
            max_square_dist = query_max_square_distance;
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
    if (node_queue[node_queue_size - (LhsIndex)].m_dvec_square_norm <                           \
        node_queue[node_queue_size - (RhsIndex)].m_dvec_square_norm)                            \
    {                                                                                           \
        const NodeEntry tmp = node_queue[node_queue_size - (LhsIndex)];                         \
        node_queue[node_queue_size - (LhsIndex)] = node_queue[node_queue_size - (RhsIndex)];    \
        node_queue[node_queue_size - (RhsIndex)] = tmp;                                         \
    }

    const size_t NodeQueueSize = 128;
    NodeEntry node_queue[NodeQueueSize];
    size_t node_queue_size = 0;

    node = nodes;

    while (node->is_interior())
    {
        const size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();
        const ValueType distance = query_point[split_dim] - split_abs;

        // Figure out which node to follow and which node to push.
        const int select = static_cast<int>(FP<ValueType>::sign(distance));
        const NodeType* APPLESEED_RESTRICT left_child_node = nodes + node->get_child_node_index();
        const NodeType* APPLESEED_RESTRICT follow_node = left_child_node + 1 - select;
        const NodeType* APPLESEED_RESTRICT queue_node = left_child_node + select;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        // Like in the initial step, we stop as soon as we reached a node with enough points.
        if (follow_node->get_point_count() < max_answer_size)
            break;

        // Push the node that we don't visit now to the node stack.
        const ValueType square_distance = distance * distance;
        if (square_distance < max_square_dist)
        {
            VectorType dvec(0.0);
            dvec[split_dim] = distance;

            // Push the node to the node stack.
            assert(node_queue_size < NodeQueueSize);
            node_queue[node_queue_size++] = NodeEntry(queue_node, dvec, square_distance);

            // Order the top levels of the node stack.
            if (node_queue_size >= 4)
            {
                ORDER_NODE_ENTRIES(4, 3);
                ORDER_NODE_ENTRIES(2, 1);
                ORDER_NODE_ENTRIES(4, 2);
                ORDER_NODE_ENTRIES(3, 1);
                ORDER_NODE_ENTRIES(3, 2);
            }
        }

        node = follow_node;
    }

    //
    // Step 4:
    //
    //   Visit the nodes in the node stack, from the closest to the farthest,
    //   updating the stack with new nodes to visit later. We terminate as soon
    //   as the closest node is farther than our current maximum search distance.
    //

    const size_t IdealLeafSize = 20;

    while (node_queue_size-- > 0)
    {
        const NodeEntry* top_entry = node_queue + node_queue_size;

        if (top_entry->m_dvec_square_norm >= max_square_dist)
            continue;

        node = top_entry->m_node;

        const VectorType parent_dvec = top_entry->m_dvec;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        while (node->is_interior())
        {
            const size_t split_dim = node->get_split_dim();
            const ValueType split_abs = node->get_split_abs();
            const ValueType distance = query_point[split_dim] - split_abs;

            // Figure out which node to follow and which node to push.
            const int select = static_cast<int>(FP<ValueType>::sign(distance));
            const NodeType* APPLESEED_RESTRICT left_child_node = nodes + node->get_child_node_index();
            const NodeType* APPLESEED_RESTRICT follow_node = left_child_node + 1 - select;
            const NodeType* APPLESEED_RESTRICT queue_node = left_child_node + select;

            FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

            // Try to target nodes with an "ideal" number of points.
            if (follow_node->get_point_count() < IdealLeafSize)
                break;

            // Push the node that we don't visit now to the node stack.
            if (distance * distance < max_square_dist)
            {
                VectorType dvec = parent_dvec;
                dvec[split_dim] = distance;

                // Push the node to the node stack.
                assert(node_queue_size < NodeQueueSize);
                node_queue[node_queue_size++] = NodeEntry(queue_node, dvec, square_norm(dvec));

                // Order the top levels of the node stack.
                if (node_queue_size >= 4)
                {
                    ORDER_NODE_ENTRIES(4, 3);
                    ORDER_NODE_ENTRIES(2, 1);
                    ORDER_NODE_ENTRIES(4, 2);
                    ORDER_NODE_ENTRIES(3, 1);
                    ORDER_NODE_ENTRIES(3, 2);
                }
            }

            node = follow_node;
        }

        FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

        size_t point_index = node->get_point_index();
        const VectorType* APPLESEED_RESTRICT point_ptr = points + point_index;
        const VectorType* APPLESEED_RESTRICT point_end = point_ptr + node->get_point_count();

        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType square_dist = square_distance(*point_ptr++, query_point);

            if (square_dist < max_square_dist)
            {
                if (m_answer.m_size == max_answer_size)
                {
                    m_answer.heap_insert(point_index, square_dist);
                    max_square_dist = m_answer.top().m_square_dist;
                }
                else
                {
                    m_answer.array_insert(point_index, square_dist);

                    if (m_answer.m_size == max_answer_size)
                        m_answer.make_heap();
                }
            }

            ++point_index;
        }
    }

#undef ORDER_NODE_ENTRIES

    FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(visited_leaf_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(tested_point_count));
}

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS

template <typename T, size_t N>
inline void Query<T, N>::run(
    const VectorType&       query_point) const
{
    QueryStatistics stats;
    run(query_point, stats);
}

template <typename T, size_t N>
inline void Query<T, N>::run(
    const VectorType&       query_point,
    const ValueType         query_max_square_distance) const
{
    QueryStatistics stats;
    run(query_point, query_max_square_distance, stats);
}

#endif

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_QUERY_H
