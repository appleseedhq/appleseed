
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
#include "foundation/math/aabb.h"
#include "foundation/math/knn/knn_node.h"
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/permutation.h"
#include "foundation/math/split.h"
#include "foundation/math/vector.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <vector>

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

    // Constructor.
    explicit Builder(TreeType& tree);

    // Build a tree for a given set of points.
    template <typename Timer>
    void build(
        const VectorType            points[],
        const size_t                count);

    // Like build() but the points will be moved into the tree rather than copied.
    // todo: take rvalue reference.
    template <typename Timer>
    void build_move_points(
        std::vector<VectorType>&    points);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef typename TreeType::NodeType NodeType;
    typedef AABB<T, N> BboxType;
    typedef Split<T> SplitType;

    struct PartitionPredicate
    {
        typedef std::vector<VectorType> PointVector;

        const PointVector&          m_points;
        const SplitType             m_split;

        PartitionPredicate(
            const PointVector&      points,
            const SplitType&        split);

        bool operator()(
            const size_t            index) const;
    };

    TreeType&   m_tree;
    double      m_build_time;

    void partition(
        const size_t                parent_node_index,
        const size_t                begin,
        const size_t                end) const;

    BboxType compute_bbox(
        const size_t                begin,
        const size_t                end) const;
};

typedef Builder<float, 2>  Builder2f;
typedef Builder<double, 2> Builder2d;
typedef Builder<float, 3>  Builder3f;
typedef Builder<double, 3> Builder3d;


//
// Implementation.
//

template <typename T, size_t N>
inline Builder<T, N>::Builder(TreeType& tree)
  : m_tree(tree)
  , m_build_time(0.0)
{
}

template <typename T, size_t N>
template <typename Timer>
void Builder<T, N>::build(
    const VectorType            points[],
    const size_t                count)
{
    std::vector<VectorType> vec(count);

    if (count > 0)
    {
        assert(points);
        std::memcpy(&vec[0], points, count * sizeof(VectorType));
    }

    build_move_points<Timer>(vec);
}

template <typename T, size_t N>
template <typename Timer>
void Builder<T, N>::build_move_points(
    std::vector<VectorType>&    points)
{
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    const size_t count = points.size();

    if (count > 0)
    {
        m_tree.m_points.swap(points);

        m_tree.m_indices.resize(count);

        for (size_t i = 0; i < count; ++i)
            m_tree.m_indices[i] = i;
    }

    m_tree.m_nodes.reserve(count * 2 + 1);
    m_tree.m_nodes.push_back(NodeType());

    partition(0, 0, count);

    if (count > 0)
    {
        std::vector<VectorType> temp(count);

        small_item_reorder(
            &m_tree.m_points[0],
            &temp[0],
            &m_tree.m_indices[0],
            count);
    }

    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

template <typename T, size_t N>
inline double Builder<T, N>::get_build_time() const
{
    return m_build_time;
}

template <typename T, size_t N>
inline Builder<T, N>::PartitionPredicate::PartitionPredicate(
    const PointVector&          points,
    const SplitType&            split)
  : m_points(points)
  , m_split(split)
{
}

template <typename T, size_t N>
inline bool Builder<T, N>::PartitionPredicate::operator()(
    const size_t                index) const
{
    // Points on the split plane belong to the right child node.
    return m_points[index][m_split.m_dimension] < m_split.m_abscissa;
}

template <typename T, size_t N>
void Builder<T, N>::partition(
    const size_t                parent_node_index,
    const size_t                begin,
    const size_t                end) const
{
    const size_t count = end - begin;

    if (count <= 1)
    {
        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.make_leaf();
        parent_node.set_point_index(begin);
        parent_node.set_point_count(count);
    }
    else
    {
        const BboxType bbox = compute_bbox(begin, end);
        SplitType split = SplitType::middle(bbox);

        const size_t* bound =
            std::partition(
                &m_tree.m_indices[0] + begin,
                &m_tree.m_indices[0] + end,
                PartitionPredicate(m_tree.m_points, split));

        size_t pivot = bound - &m_tree.m_indices[0];
        assert(pivot >= begin);
        assert(pivot <= end);

        // Given a split-the-longest-axis-in-the-middle strategy, the only case where
        // the left or right leaf may be empty is when all the points are coincident.
        // In that degenerate case, we simply split the point set in two and recurse.
        // Without this treatment, we would recurse until we exhaust stack space.
        if (pivot == begin || pivot == end)
            pivot = (begin + end) / 2;

        const size_t left_node_index = m_tree.m_nodes.size();
        const size_t right_node_index = left_node_index + 1;

        m_tree.m_nodes.push_back(NodeType());
        m_tree.m_nodes.push_back(NodeType());

        NodeType& parent_node = m_tree.m_nodes[parent_node_index];
        parent_node.make_interior();
        parent_node.set_split_dim(split.m_dimension);
        parent_node.set_split_abs(split.m_abscissa);
        parent_node.set_child_node_index(left_node_index);
        parent_node.set_point_index(begin);
        parent_node.set_point_count(count);

        partition(left_node_index, begin, pivot);
        partition(right_node_index, pivot, end);
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

}   // namespace knn
}   // namespace foundation
