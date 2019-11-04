
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/array/arrayref.h"
#include "foundation/array/keyframedarray.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>
#include <utility>

using namespace foundation;

TEST_SUITE(Foundation_Array_KeyframedArray)
{
    template <typename T>
    void set_keyframes(
        size_t          key_count,
        const T*        first,
        const T*        last,
        KeyFramedArray& k)
    {
        ArrayRef<T> k0_ref(k.get_key(0));
        k0_ref.assign(first, last);

        k.set_key_count(key_count);
        for (size_t i = 1, e = k.get_key_count(); i < e; ++i)
        {
            ArrayRef<T> k_ref(k.get_key(i));
            k_ref.assign(first, last);
        }
    }

    TEST_CASE(Construct)
    {
        KeyFramedArray x(FloatType);
        EXPECT_EQ(1, x.get_key_count());
        EXPECT_TRUE(x.get_key(0).empty());
        EXPECT_EQ(FloatType, x.type());

        KeyFramedArray y(UInt32Type, 101);
        EXPECT_EQ(1, y.get_key_count());
        EXPECT_FALSE(y.get_key(0).empty());
        EXPECT_EQ(UInt32Type, y.type());
        EXPECT_EQ(101, y.get_key(0).size());
    }

    TEST_CASE(CopyConstruct)
    {
        KeyFramedArray k(UInt32Type);

        const std::uint32_t items[] = {1, 5, 7, 11, 17, 21, 23};
        set_keyframes(7, items, items + countof(items), k);

        KeyFramedArray kcopy(k);
        EXPECT_TRUE(k == kcopy);
    }

    TEST_CASE(MoveConstruct)
    {
        KeyFramedArray k(FloatType);

        const float items[] = {1.0f, 5.0f, 7.0f, 11.0f};
        set_keyframes(2, items, items + countof(items), k);

        KeyFramedArray kmoved(std::move(k));
        EXPECT_TRUE(k.is_moved());
    }

    TEST_CASE(SetKeyCount)
    {
        KeyFramedArray k(UInt32Type);

        const std::uint32_t items[] = {1, 5, 7, 11, 17};
        set_keyframes(5, items, items + countof(items), k);

        EXPECT_TRUE(k.check_consistency());
        EXPECT_TRUE(k.all_keyframes_equal());
    }
}
