
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
#include "foundation/memory/stampedptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Utility_StampedPtr)
{
    TEST_CASE(TestWithStackPointer)
    {
        const int value = 11;
        const std::uint16_t stamp = 7;

        stamped_ptr<const int> x(&value, stamp);

        EXPECT_EQ(&value, x.get_ptr());
        EXPECT_EQ(stamp, x.get_stamp());
        EXPECT_EQ(value, *x.get_ptr());
    }

    TEST_CASE(TestWithHeapPointer)
    {
        const std::unique_ptr<int> ptr(new int(11));
        const std::uint16_t stamp = 7;

        stamped_ptr<const int> x(ptr.get(), stamp);

        EXPECT_EQ(ptr.get(), x.get_ptr());
        EXPECT_EQ(stamp, x.get_stamp());
        EXPECT_EQ(*ptr, *x.get_ptr());
    }

    TEST_CASE(TestWithNullPtr)
    {
        const std::uint16_t stamp = 7;
        stamped_ptr<const int> x(nullptr, stamp);

        EXPECT_EQ(nullptr, x.get_ptr());
        EXPECT_EQ(stamp, x.get_stamp());
    }
}
