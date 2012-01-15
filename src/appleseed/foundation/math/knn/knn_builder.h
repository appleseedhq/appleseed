
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

#ifndef APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
#define APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/knn/knn_node.h"
#include "foundation/math/knn/knn_tree.h"
#include "foundation/math/aabb.h"
#include "foundation/math/permutation.h"
#include "foundation/math/split.h"
#include "foundation/math/vector.h"

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

    explicit Builder(TreeType& tree);

    // Build a tree for a given set of points.
    void build(
        const VectorType        points[],
        const size_t            count);

  private:
    typedef typename TreeType::NodeType NodeType;
    typedef AABB<T, N> BboxType;
    typedef Split<T> SplitType;

    struct PartitionPredicate
    {
        typedef std::vector<VectorType> PointVector;

        const PointVector&      m_points;
        const SplitType         m_split;

        PartitionPredicate(
            const PointVector&  points,
            const SplitType&    split);

        bool operator()(
            const size_t        index) const;
    };

    TreeType& m_tree;

    void partition(
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
inline Builder<T, N>::Builder(TreeType& tree)
  : m_tree(tree)
{
}

template <typename T, size_t N>
void Builder<T, N>::build(
    const VectorType            points[],
    const size_t                count)
{
    if (count > 0)
    {
        assert(points);

        m_tree.m_points.resize(count);
        std::memcpy(&m_tree.m_points[0], points, count * sizeof(VectorType));

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
        parent_node.set_type(NodeType::Leaf);
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
        assert(pivot > begin);
        assert(pivot <= end);

        // Switch to median split if one of the two leaf is empty.
        if (pivot == begin || pivot == end)
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

}       // namespace knn
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_KNN_KNN_BUILDER_H
