
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

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

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    void run(const VectorType& query_point) const;
#endif

  private:
    typedef typename TreeType::NodeType NodeType;

    struct NodeEntry
    {
        const NodeType*     m_node;
        ValueType           m_distance;

        NodeEntry() {}

        NodeEntry(const NodeType* node, const ValueType distance)
          : m_node(node)
          , m_distance(distance)
        {
        }

        bool operator<(const NodeEntry& rhs) const
        {
            return m_distance > rhs.m_distance;
        }
    };

    const TreeType&         m_tree;
    AnswerType&             m_answer;

    void find_single_nearest_neighbor(
        const VectorType&   query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , QueryStatistics&  stats
#endif
        ) const;

    void find_multiple_nearest_neighbors(
        const VectorType&   query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        , QueryStatistics&  stats
#endif
        ) const;
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
    assert(!m_tree.empty());

    m_answer.clear();

    if (m_answer.m_max_size == 1)
    {
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        find_single_nearest_neighbor(query_point, stats);
#else
        find_single_nearest_neighbor(query_point);
#endif
    }
    else
    {
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
        find_multiple_nearest_neighbors(query_point, stats);
#else
        find_multiple_nearest_neighbors(query_point);
#endif
    }
}

#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS

template <typename T, size_t N>
inline void Query<T, N>::run(const VectorType& query_point) const
{
    QueryStatistics stats;
    run(query_point, stats);
}

#endif

template <typename T, size_t N>
inline void Query<T, N>::find_single_nearest_neighbor(
    const VectorType&       query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    , QueryStatistics&      stats
#endif
    ) const
{
    FOUNDATION_KNN_QUERY_STATS(++stats.m_query_count);
    FOUNDATION_KNN_QUERY_STATS(size_t fetched_node_count = 0);

    const NodeType* RESTRICT nodes = &m_tree.m_nodes.front();
    const NodeType* RESTRICT node = nodes;

    FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

    while (node->is_interior())
    {
        const size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();

        node = nodes + node->get_child_node_index();

        if (query_point[split_dim] >= split_abs)
            ++node;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);
    }

    const size_t point_index = node->get_point_index();
    const ValueType distance = square_distance(m_tree.m_points[point_index], query_point);

    m_answer.array_insert(m_tree.m_indices[point_index], distance);

    FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(1));
    FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(1));
}

template <typename T, size_t N>
inline void Query<T, N>::find_multiple_nearest_neighbors(
    const VectorType&       query_point
#ifdef FOUNDATION_KNN_ENABLE_QUERY_STATS
    , QueryStatistics&      stats
#endif
    ) const
{
    FOUNDATION_KNN_QUERY_STATS(++stats.m_query_count);
    FOUNDATION_KNN_QUERY_STATS(size_t fetched_node_count = 0);
    FOUNDATION_KNN_QUERY_STATS(size_t visited_leaf_count = 0);
    FOUNDATION_KNN_QUERY_STATS(size_t tested_point_count = 0);

    const VectorType* RESTRICT points = &m_tree.m_points.front();
    const NodeType* RESTRICT nodes = &m_tree.m_nodes.front();

    //
    // 1. Find the deepest node that contains the query point and enough other points
    //    to compute a maximum search distance.
    //

    const NodeType* RESTRICT start_node = nodes;

    FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

    while (start_node->is_interior())
    {
        const size_t split_dim = start_node->get_split_dim();
        const ValueType split_abs = start_node->get_split_abs();

        const NodeType* RESTRICT child_node = nodes + start_node->get_child_node_index();

        if (query_point[split_dim] >= split_abs)
            ++child_node;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        if (child_node->get_point_count() < m_answer.m_max_size)
            break;

        start_node = child_node;
    }

    //
    // 2. Collect the points in or below this node and compute a maximum search distance.
    //

    FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

    ValueType max_distance(0.0);

    {
        size_t point_index = start_node->get_point_index();
        const VectorType* RESTRICT point_ptr = points + point_index;
        const VectorType* RESTRICT point_end = point_ptr + start_node->get_point_count();
        const VectorType* RESTRICT array_end = std::min(point_ptr + m_answer.m_max_size, point_end);

        while (point_ptr < array_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType distance = square_distance(*point_ptr, query_point);

            m_answer.array_insert(point_index, distance);

            if (max_distance < distance)
                max_distance = distance;

            ++point_index;
            ++point_ptr;
        }

        m_answer.make_heap();

        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType distance = square_distance(*point_ptr, query_point);

            if (distance < max_distance)
            {
                m_answer.heap_insert(point_index, distance);
                max_distance = m_answer.top().m_distance;
            }

            ++point_index;
            ++point_ptr;
        }
    }

    //
    // 3. Traverse again the tree and update the set of neighbors.
    //

    const size_t NodeQueueSize = 256;

    NodeEntry node_queue[NodeQueueSize];
    node_queue->m_node = nodes;
    node_queue->m_distance = ValueType(0.0);

    size_t node_queue_size = 1;

    const size_t MaxLeafSize = 25;
    bool first_leaf = true;

    while (node_queue_size > 0 && node_queue->m_distance < max_distance)
    {
        const NodeType* RESTRICT node = node_queue->m_node;

        std::pop_heap(node_queue, node_queue + node_queue_size);
        --node_queue_size;

        FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

        while (node->is_interior())
        {
            const size_t split_dim = node->get_split_dim();
            const ValueType split_abs = node->get_split_abs();
            ValueType distance = query_point[split_dim] - split_abs;

            const int select = static_cast<int>(FP<ValueType>::sign(distance));
            const NodeType* RESTRICT left_child_node = nodes + node->get_child_node_index();
            const NodeType* RESTRICT follow_node = left_child_node + 1 - select;
            const NodeType* RESTRICT queue_node = left_child_node + select;

            FOUNDATION_KNN_QUERY_STATS(++fetched_node_count);

            if (first_leaf)
            {
                if (follow_node->get_point_count() < m_answer.m_max_size)
                    break;
            }
            else
            {
                if (follow_node->get_point_count() < MaxLeafSize)
                    break;
            }

            distance *= distance;

            if (distance < max_distance)
            {
                node_queue[node_queue_size++] = NodeEntry(queue_node, distance);
                std::push_heap(node_queue, node_queue + node_queue_size);
            }

            node = follow_node;
        }

        first_leaf = false;

        if (node == start_node)
            continue;

        FOUNDATION_KNN_QUERY_STATS(++visited_leaf_count);

        size_t point_index = node->get_point_index();
        const VectorType* RESTRICT point_ptr = points + point_index;
        const VectorType* RESTRICT point_end = point_ptr + node->get_point_count();

        while (point_ptr < point_end)
        {
            FOUNDATION_KNN_QUERY_STATS(++tested_point_count);

            const ValueType distance = square_distance(*point_ptr, query_point);

            if (distance < max_distance)
            {
                m_answer.heap_insert(point_index, distance);
                max_distance = m_answer.top().m_distance;
            }

            ++point_index;
            ++point_ptr;
        }
    }

    AnswerType::Entry* RESTRICT entry_ptr = m_answer.m_entries;
    const AnswerType::Entry* RESTRICT entry_end = entry_ptr + m_answer.m_size;
    const size_t* RESTRICT indices = &m_tree.m_indices.front();

    while (entry_ptr < entry_end)
    {
        entry_ptr->m_index = indices[entry_ptr->m_index];
        ++entry_ptr;
    }

    FOUNDATION_KNN_QUERY_STATS(stats.m_fetched_nodes.insert(fetched_node_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_visited_leaves.insert(visited_leaf_count));
    FOUNDATION_KNN_QUERY_STATS(stats.m_tested_points.insert(tested_point_count));
}

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_QUERY_H
