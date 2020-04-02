
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

// Standard headers.
#include <cstdint>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Array_Algorithm)
{
    TEST_CASE(CopyIndexed)
    {
        Array src(ArrayType::UInt32Type);
        ArrayRef<std::uint32_t> src_ref(src);

        const std::uint32_t items[] = {1, 2, 3, 4, 5, 6, 7};
        src_ref.assign(items, items + countof(items));

        const size_t indices[] = {6, 2, 3, 1, 1, 4, 3, 1, 2};

        Array dst(
            copy_indexed(
                src, indices, indices + countof(indices)));

        EXPECT_EQ(dst.size(), countof(indices));

        const ArrayView<std::uint32_t> dst_ref(dst);

        for (size_t i = 0, e = countof(indices); i < e; ++i)
        {
            size_t src_index = indices[i];
            EXPECT_LT(src_ref.size(), src_index);

            EXPECT_EQ(dst_ref[i], src_ref[src_index]);
        }
    }

    TEST_CASE(ConvertToSmallestType16to8)
    {
        Array src(ArrayType::UInt16Type);
        ArrayRef<std::uint16_t> src_ref(src);
        const std::uint32_t items[] = {1, 2, 132, 4, 255, 6, 77};
        src_ref.assign(items, items + countof(items));

        Array array(src);
        convert_to_smallest_type(array);
        EXPECT_EQ(array.type(), ArrayType::UInt8Type);

        const ArrayView<std::uint16_t> src_view(src);
        const ArrayView<std::uint8_t> dst_view(array);

        EXPECT_TRUE(std::equal(src_view.begin(), src_view.end(), dst_view.begin()));
    }

    TEST_CASE(ConvertToSmallestType32to8)
    {
        Array src(ArrayType::UInt32Type);
        ArrayRef<std::uint32_t> src_ref(src);
        const std::uint32_t items[] = {1, 2, 132, 4, 255, 6, 77};
        src_ref.assign(items, items + countof(items));

        Array array(src);
        convert_to_smallest_type(array);
        EXPECT_EQ(array.type(), ArrayType::UInt8Type);

        const ArrayView<std::uint32_t> src_view(src);
        const ArrayView<std::uint8_t> dst_view(array);

        EXPECT_TRUE(std::equal(src_view.begin(), src_view.end(), dst_view.begin()));
    }

    TEST_CASE(ConvertToSmallestType32to16)
    {
        Array src(ArrayType::UInt32Type);
        ArrayRef<std::uint32_t> src_ref(src);
        const std::uint32_t items[] = {1, 2156, 132, 4, 65535, 6, 77};
        src_ref.assign(items, items + countof(items));

        Array array(src);
        convert_to_smallest_type(array);
        EXPECT_EQ(array.type(), ArrayType::UInt16Type);

        const ArrayView<std::uint32_t> src_view(src);
        const ArrayView<std::uint16_t> dst_view(array);

        EXPECT_TRUE(std::equal(src_view.begin(), src_view.end(), dst_view.begin()));
    }

    TEST_CASE(ConvertToSmallestType32NoOp)
    {
        Array src(ArrayType::UInt32Type);
        ArrayRef<std::uint32_t> src_ref(src);
        const std::uint32_t items[] = {1, 2156, 132, 4, 65537, 6, 77};
        src_ref.assign(items, items + countof(items));

        Array array(src);
        convert_to_smallest_type(array);
        EXPECT_EQ(array.type(), ArrayType::UInt32Type);

        const ArrayView<std::uint32_t> src_view(src);
        const ArrayView<std::uint32_t> dst_view(array);

        EXPECT_TRUE(std::equal(src_view.begin(), src_view.end(), dst_view.begin()));
    }
}
