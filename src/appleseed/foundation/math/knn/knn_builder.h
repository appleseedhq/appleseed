
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/knn/knn_node.h"
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/aabb.h"
#include "foundation/math/split.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>

namespace foundation {
namespace knn {

template <typename T, size_t N>
class Builder
  : public NonCopyable
{
  public:
    typedef T ValueType;
    static const size_t Dimension = N;

    typedef Vector<T, N> VectorType;
    typedef Tree<T, N> TreeType;

    Builder(
        TreeType&               tree,
        const size_t            max_answer_size);

    // Build a tree for a given set of points.
    template <typename Timer>
    void build(
        const VectorType        points[],
        const size_t            count);
    void build(
        const VectorType        points[],
        const size_t            count);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef AABB<T, N> BboxType;
    typedef Split<T> SplitType;
    typedef Node<T> NodeType;

    struct SortPredicate
    {
        TreeType&               m_tree;
        const size_t            m_dimension;

        SortPredicate(
            TreeType&           tree,
            const size_t        dimension);

        bool operator()(
            const size_t        lhs_index,
            const size_t        rhs_index) const;
    };

    struct UpperBoundPredicate
    {
        TreeType&               m_tree;
        const SplitType         m_split;

        UpperBoundPredicate(
            TreeType&           tree,
            const SplitType&    split);

        bool operator()(
            const size_t        lhs_index,
            const size_t        rhs_index) const;
    };

    TreeType&                   m_tree;
    const size_t                m_max_answer_size;
    double                      m_build_time;

    void partition_median(
        const size_t            parent_node_index,
        const size_t            begin,
        const size_t            end) const;

    void partition_middle(
        const size_t            parent_node_index,
        const size_t            begin,
        const size_t            end) const;

    BboxType compute_bbox(
        const size_t            begin,
        const size_t            end) const;
};

typedef Builder<float, 2>  Builder2f;
typedef Builder<double, 2> Builder2d;
typedef Builder<float, 3>  Builder3f;
typedef Builder<double, 3> Builder3d;


//
// Implementation.
//

template <typename T, size_t N>
inline Builder<T, N>::Builder(
    TreeType&                   tree,
    const size_t                max_answer_size)
  : m_tree(tree)
  , m_max_answer_size(max_answer_size)
  , m_build_time(0.0)
{
}

template <typename T, size_t N>
template <typename Timer>
void Builder<T, N>::build(
    const VectorType            points[],
    const size_t                count)
{
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    m_tree.m_max_answer_size = m_max_answer_size;

    if (count > 0)
    {
        assert(points);

        m_tree.m_points.resize(count);
        std::memcpy(&m_tree.m_points[0], points, count * sizeof(VectorType));

        m_tree.m_indices.resize(count);
        for (size_t i = 0; i < count; ++i)
            m_tree.m_indices[i] = i;
    }

    m_tree.m_nodes.push_back(NodeType());

    if (m_max_answer_size == 1)
        partition_median(0, 0, count);
    else partition_middle(0, 0, count);

    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

template <typename T, size_t N>
void Builder<T, N>::build(
    const VectorType            points[],
    const size_t                count)
{
    build<DefaultWallclockTimer>(points, count);
}

template <typename T, size_t N>
inline double Builder<T, N>::get_build_time() const
{
    return m_build_time;
}

template <typename T, size_t N>
inline Builder<T, N>::SortPredicate::SortPredicate(
    TreeType&                   tree,
    const size_t                dimension)
  : m_tree(tree)
  , m_dimension(dimension)
{
}

template <typename T, size_t N>
inline bool Builder<T, N>::SortPredicate::operator()(
    const size_t                lhs_index,
    const size_t                rhs_index) const
{
    const ValueType lhs_value = m_tree.m_points[lhs_index][m_dimension];
    const ValueType rhs_value = m_tree.m_points[rhs_index][m_dimension];
    return lhs_value < rhs_value;
}

template <typename T, size_t N>
inline Builder<T, N>::UpperBoundPredicate::UpperBoundPredicate(
    TreeType&                   tree,
    const SplitType&            split)
  : m_tree(tree)
  , m_split(split)
{
}

template <typename T, size_t N>
inline bool Builder<T, N>::UpperBoundPredicate::operator()(
    const size_t                lhs_index,  // not used
    const size_t                rhs_index) const
{
    const ValueType rhs_value = m_tree.m_points[rhs_index][m_split.m_dimension];
    return m_split.m_abscissa < rhs_value;
}

template <typename T, size_t N>
void Builder<T, N>::partition_median(
    const size_t                parent_node_index,
    const size_t                begin,
    const size_t                end) const
{
    const size_t count = end - begin;

    if (count < 2 * m_max_answer_size)
    {
        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.set_type(NodeType::Leaf);
        parent_node.set_point_index(begin);
        parent_node.set_point_count(count);
    }
    else
    {
        const BboxType bbox = compute_bbox(begin, end);
        const size_t split_dimension = max_index(bbox.extent());

        std::sort(
            &m_tree.m_indices[0] + begin,
            &m_tree.m_indices[0] + end,
            SortPredicate(m_tree, split_dimension));

        const size_t median = (begin + end) / 2;
        const VectorType& median_point = m_tree.m_points[m_tree.m_indices[median]];
        const ValueType split_abscissa = median_point[split_dimension];

        const size_t left_node_index = m_tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        m_tree.m_nodes.push_back(NodeType());
        m_tree.m_nodes.push_back(NodeType());

        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.set_type(NodeType::Interior);
        parent_node.set_split_dim(split_dimension);
        parent_node.set_split_abs(split_abscissa);
        parent_node.set_child_node_index(left_node_index);

        partition_median(left_node_index, begin, median);
        partition_median(right_node_index, median, end);
    }
}

template <typename T, size_t N>
void Builder<T, N>::partition_middle(
    const size_t                parent_node_index,
    const size_t                begin,
    const size_t                end) const
{
    const size_t count = end - begin;

    if (count < 2 * m_max_answer_size)
    {
        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.set_type(NodeType::Leaf);
        parent_node.set_point_index(begin);
        parent_node.set_point_count(count);
    }
    else
    {
        const BboxType bbox = compute_bbox(begin, end);
        SplitType split = SplitType::middle(bbox);

        std::sort(
            &m_tree.m_indices[0] + begin,
            &m_tree.m_indices[0] + end,
            SortPredicate(m_tree, split.m_dimension));

        const size_t* bound =
            std::upper_bound(
                &m_tree.m_indices[0] + begin,
                &m_tree.m_indices[0] + end,
                0,  // not used
                UpperBoundPredicate(m_tree, split));

        size_t pivot = bound - &m_tree.m_indices[0];
        assert(pivot > begin);
        assert(pivot <= end);

        // Switch to median split if one of the two leaf doesn't contain enough points.
        if (pivot - begin < m_max_answer_size || end - pivot < m_max_answer_size)
        {
            pivot = (begin + end) / 2;
            const VectorType& median_point = m_tree.m_points[m_tree.m_indices[pivot]];
            split.m_abscissa = median_point[split.m_dimension];
        }

        const size_t left_node_index = m_tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        m_tree.m_nodes.push_back(NodeType());
        m_tree.m_nodes.push_back(NodeType());

        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.set_type(NodeType::Interior);
        parent_node.set_split_dim(split.m_dimension);
        parent_node.set_split_abs(split.m_abscissa);
        parent_node.set_child_node_index(left_node_index);

        partition_middle(left_node_index, begin, pivot);
        partition_middle(right_node_index, pivot, end);
    }
}

template <typename T, size_t N>
inline typename Builder<T, N>::BboxType Builder<T, N>::compute_bbox(
    const size_t                begin,
    const size_t                end) const
{
    BboxType bbox;
    bbox.invalidate();

    for (size_t i = begin; i < end; ++i)
    {
        const size_t index = m_tree.m_indices[i];
        bbox.insert(m_tree.m_points[index]);
    }

    return bbox;
}

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
