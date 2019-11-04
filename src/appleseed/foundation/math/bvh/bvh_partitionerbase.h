
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
#include "foundation/math/bvh/bvh_bboxsortpredicate.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace foundation {
namespace bvh {

//
// A base class for BVH partitioners.
//

template <typename AABBVector>
class PartitionerBase
  : public NonCopyable
{
  public:
    typedef AABBVector AABBVectorType;
    typedef typename AABBVectorType::value_type AABBType;

    // Constructor.
    explicit PartitionerBase(
        const AABBVectorType&   bboxes);

    // Compute the bounding box of a given set of items.
    AABBType compute_bbox(
        const size_t            begin,
        const size_t            end) const;

    // Return the items ordering.
    const std::vector<size_t>& get_item_ordering(
        const size_t            dimension = 0) const;

  protected:
    static const size_t Dimension = AABBType::Dimension;

    const AABBVectorType&       m_bboxes;
    std::vector<size_t>         m_indices[Dimension];

    void sort_indices(
        const size_t            dimension,
        const size_t            begin,
        const size_t            end,
        const size_t            pivot);

  private:
    std::vector<size_t>         m_tmp;
    std::vector<std::uint8_t>   m_tags;
};


//
// PartitionerBase class implementation.
//

template <typename AABBVector>
PartitionerBase<AABBVector>::PartitionerBase(
    const AABBVectorType&       bboxes)
  : m_bboxes(bboxes)
{
    const size_t size = m_bboxes.size();

    for (size_t d = 0; d < Dimension; ++d)
    {
        std::vector<size_t>& indices = m_indices[d];

        // Identity ordering.
        indices.resize(size);
        for (size_t i = 0; i < size; ++i)
            indices[i] = i;

        // Sort the items according to the center of their bounding boxes.
        BboxSortPredicate<AABBVectorType> predicate(m_bboxes, d);
        std::sort(indices.begin(), indices.end(), predicate);
    }

    m_tmp.resize(size);
    m_tags.resize(size);
}

template <typename AABBVector>
typename AABBVector::value_type PartitionerBase<AABBVector>::compute_bbox(
    const size_t                begin,
    const size_t                end) const
{
    const std::vector<size_t>& indices = m_indices[0];

    AABBType bbox;
    bbox.invalidate();

    for (size_t i = begin; i < end; ++i)
        bbox.insert(m_bboxes[indices[i]]);

    return bbox;
}

template <typename AABBVector>
void PartitionerBase<AABBVector>::sort_indices(
    const size_t                dimension,
    const size_t                begin,
    const size_t                end,
    const size_t                pivot)
{
    const std::vector<size_t>& split_indices = m_indices[dimension];

    enum { Left = 0, Right = 1 };

    for (size_t i = begin; i < pivot; ++i)
        m_tags[split_indices[i]] = Left;

    for (size_t i = pivot; i < end; ++i)
        m_tags[split_indices[i]] = Right;

    for (size_t d = 0; d < Dimension; ++d)
    {
        if (d != dimension)
        {
            std::vector<size_t>& indices = m_indices[d];

            size_t left = begin;
            size_t right = pivot;

            for (size_t i = begin; i < end; ++i)
            {
                const size_t index = indices[i];

                if (m_tags[index] == Left)
                {
                    assert(left < pivot);
                    m_tmp[left++] = index;
                }
                else
                {
                    assert(right < end);
                    m_tmp[right++] = index;
                }
            }

            assert(left == pivot);
            assert(right == end);

            const size_t size = indices.size();

            if (end - begin > size / 2)
            {
                for (size_t i = 0; i < begin; ++i)
                    m_tmp[i] = indices[i];

                for (size_t i = end; i < size; ++i)
                    m_tmp[i] = indices[i];

                m_tmp.swap(indices);
            }
            else
            {
                for (size_t i = begin; i < end; ++i)
                    indices[i] = m_tmp[i];
            }
        }
    }
}

template <typename Tree>
inline const std::vector<size_t>& PartitionerBase<Tree>::get_item_ordering(
    const size_t  dimension) const
{
    assert(dimension < Dimension);
    return m_indices[dimension];
}

}   // namespace bvh
}   // namespace foundation
