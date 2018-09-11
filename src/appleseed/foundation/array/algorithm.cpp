
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
#include "foundation/platform/types.h"

// Standard headers.
#include <algorithm>

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

        std::copy(src.begin(), src.end(), std::back_inserter(dst));
        array = std::move(tmp);
    }
}

void convert_to_smallest_type(Array& array)
{
    if (array.empty())
        return;

    switch (array.type())
    {
      case UInt16Type:
      {
        const ArrayView<uint16> view(array);
        const uint16 max_element = *std::max_element(view.begin(), view.end());

        if (max_element <= std::numeric_limits<uint8>::max())
            convert_array<uint16, uint8>(array);
      }
      break;

      case UInt32Type:
      {
        const ArrayView<uint32> view(array);
        const uint32 max_element = *std::max_element(view.begin(), view.end());

        if (max_element <= std::numeric_limits<uint8>::max())
            convert_array<uint32, uint8>(array);
        else if (max_element <= std::numeric_limits<uint16>::max())
            convert_array<uint32, uint16>(array);
      }
      break;

      default:
        break;
    }
}

}       // namespace foundation
