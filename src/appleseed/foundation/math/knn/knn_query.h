
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
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/distance.h"
#include "foundation/math/fp.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>

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

    void run(const VectorType& query_point) const;

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

    void find_single_nearest_neighbor(const VectorType& query_point) const;

    void find_multiple_nearest_neighbors(const VectorType& query_point) const;
};

typedef Query<float, 2>  Query2f;
typedef Query<double, 2> Query2d;
typedef Query<float, 3>  Query3f;
typedef Query<double, 3> Query3d;


//
// Implementation.
//

template <typename T, size_t N>
inline Query<T, N>::Query(
    const TreeType&         tree,
    AnswerType&             answer)
  : m_tree(tree)
  , m_answer(answer)
{
}

template <typename T, size_t N>
inline void Query<T, N>::run(const VectorType& query_point) const
{
    assert(!m_tree.empty());

    m_answer.clear();

    if (m_answer.m_max_size == 1)
        find_single_nearest_neighbor(query_point);
    else find_multiple_nearest_neighbors(query_point);
}

template <typename T, size_t N>
inline void Query<T, N>::find_single_nearest_neighbor(const VectorType& query_point) const
{
    const NodeType* RESTRICT nodes = &m_tree.m_nodes.front();
    const NodeType* RESTRICT node = nodes;

    while (node->is_interior())
    {
        const size_t split_dim = node->get_split_dim();
        const ValueType split_abs = node->get_split_abs();

        node = nodes + node->get_child_node_index();

        if (query_point[split_dim] >= split_abs)
            ++node;
    }

    const size_t point_index = m_tree.m_indices[node->get_point_index()];
    const VectorType& point = m_tree.m_points[point_index];
    const ValueType distance = square_distance(point, query_point);

    m_answer.insert(point_index, distance);
}

template <typename T, size_t N>
inline void Query<T, N>::find_multiple_nearest_neighbors(const VectorType& query_point) const
{
    const VectorType* RESTRICT points = &m_tree.m_points.front();
    const size_t* RESTRICT indices = &m_tree.m_indices.front();
    const NodeType* RESTRICT nodes = &m_tree.m_nodes.front();

    //
    // 1. Find the deepest node that contains the query point and enough other points
    //    to compute a maximum search distance.
    //

    const NodeType* RESTRICT start_node = nodes;

    while (start_node->is_interior())
    {
        const size_t split_dim = start_node->get_split_dim();
        const ValueType split_abs = start_node->get_split_abs();

        const NodeType* RESTRICT child_node = nodes + start_node->get_child_node_index();

        if (query_point[split_dim] >= split_abs)
            ++child_node;

        if (child_node->get_point_count() < m_answer.m_max_size)
            break;

        start_node = child_node;
    }

    //
    // 2. Collect the points in or below this node and compute a maximum search distance.
    //

    ValueType max_distance(0.0);

    const size_t* RESTRICT index_ptr = indices + start_node->get_point_index();
    const size_t* RESTRICT index_end = index_ptr + start_node->get_point_count();

    while (index_ptr < index_end)
    {
        // Fetch the point and compute its distance to the query point.
        const size_t point_index = *index_ptr++;
        const VectorType& point = points[point_index];
        const ValueType distance = square_distance(point, query_point);

        // Add this point to the answer.
        m_answer.insert(point_index, distance);

        // Update the maximum search distance.
        if (max_distance < distance)
            max_distance = distance;
    }

    //
    // 3. Traverse again the tree and update the set of neighbors.
    //

    const size_t NodeQueueSize = 256;

    NodeEntry node_queue[NodeQueueSize];
    node_queue->m_node = nodes;
    node_queue->m_distance = ValueType(0.0);

    size_t node_queue_size = 1;

    while (node_queue_size > 0)
    {
        // If the closest node is farther than our farthest point, bail out.
        if (node_queue->m_distance >= max_distance)
            break;

        const NodeType* RESTRICT node = node_queue->m_node;

        // Remove the closest node from the queue.
        std::pop_heap(node_queue, node_queue + node_queue_size);
        --node_queue_size;

        while (node->is_interior())
        {
            const size_t split_dim = node->get_split_dim();
            const ValueType split_abs = node->get_split_abs();
            ValueType distance = query_point[split_dim] - split_abs;

            const int select = static_cast<int>(FP<ValueType>::sign(distance));
            const NodeType* RESTRICT left_child_node = nodes + node->get_child_node_index();
            const NodeType* RESTRICT follow_node = left_child_node + 1 - select;
            const NodeType* RESTRICT queue_node = left_child_node + select;

            if (follow_node->get_point_count() < m_answer.m_max_size)
                break;

            distance *= distance;

            if (distance < max_distance)
            {
                node_queue[node_queue_size++] = NodeEntry(queue_node, distance);
                std::push_heap(node_queue, node_queue + node_queue_size);
            }

            node = follow_node;
        }

        if (node == start_node)
            continue;

        const size_t* RESTRICT index_ptr = indices + node->get_point_index();
        const size_t* RESTRICT index_end = index_ptr + node->get_point_count();

        while (index_ptr < index_end)
        {
            // Fetch the point and compute its distance to the query point.
            const size_t point_index = *index_ptr++;
            const VectorType& point = points[point_index];
            const ValueType distance = square_distance(point, query_point);

            // Insert this point to the answer.
            m_answer.insert(point_index, distance);

            // Update the maximum search distance.
            max_distance = m_answer.top().m_distance;
        }
    }
}

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_QUERY_H
