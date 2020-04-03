
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/array/algorithm.h"
#include "foundation/array/arraytraits.h"

// Standard headers.
#include <algorithm>
#include <cstdint>

namespace foundation
{
namespace
{

template <typename SrcType, typename DstType>
void convert_array(Array& array)
{
    Array tmp(ArrayTraits<DstType>::array_type());
    tmp.reserve(array.size());

    ArrayView<SrcType> src(array);
    ArrayRef<DstType> dst(tmp);

    for (const auto value : src)
        dst.push_back(static_cast<DstType>(value));

    array = std::move(tmp);
}

struct ComputeBBoxVisitor
{
    AABB3f m_bbox;

    ComputeBBoxVisitor()
    {
        m_bbox.invalidate();
    }

    explicit ComputeBBoxVisitor(const AABB3f& bbox)
    : m_bbox(bbox)
    {
    }

    void operator()(const ArrayView<Vector3f>& view)
    {
        for (const Vector3f& p : view)
            m_bbox.insert(p);
    }

    template <typename T>
    void operator()(const ArrayView<T>& view)
    {
        throw BadArrayTypeException();
    }
};

}

void convert_to_smallest_type(Array& array)
{
    if (array.empty())
        return;

    switch (array.type())
    {
      case UInt16Type:
      {
        const ArrayView<std::uint16_t> view(array);
        const std::uint16_t max_element = *std::max_element(view.begin(), view.end());

        if (max_element <= std::numeric_limits<std::uint8_t>::max())
            convert_array<std::uint16_t, std::uint8_t>(array);
      }
      break;

      case UInt32Type:
      {
        const ArrayView<std::uint32_t> view(array);
        const std::uint32_t max_element = *std::max_element(view.begin(), view.end());

        if (max_element <= std::numeric_limits<std::uint8_t>::max())
            convert_array<std::uint32_t, std::uint8_t>(array);
        else if (max_element <= std::numeric_limits<std::uint16_t>::max())
            convert_array<std::uint32_t, std::uint16_t>(array);
      }
      break;

      default:
        break;
    }
}

AABB3f compute_bounding_box(const Array& vertices)
{
    ComputeBBoxVisitor v;
    apply_visitor(vertices, v);
    return v.m_bbox;
}

AABB3f compute_bounding_box(const Array& vertices, const AABB3f& initial_bbox)
{
    ComputeBBoxVisitor v(initial_bbox);
    apply_visitor(vertices, v);
    return v.m_bbox;
}

}       // namespace foundation
