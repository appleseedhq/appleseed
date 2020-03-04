
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
#include "foundation/memory/copyonwrite.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <utility>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Utility_CopyOnWrite)
{
    TEST_CASE(ConstructWithCopyableType)
    {
        CopyOnWrite<int> cow(7);
        EXPECT_TRUE(cow.unique());
        EXPECT_EQ(7, cow.read());
    }

    TEST_CASE(ConstructWithMovableType)
    {
        std::vector<int> x(1, 11);
        CopyOnWrite<std::vector<int>> cow(std::move(x));
        EXPECT_TRUE(cow.unique());
        EXPECT_EQ(11, cow.read()[0]);
    }

    TEST_CASE(CopyConstruct)
    {
        CopyOnWrite<int> a(7);
        EXPECT_TRUE(a.unique());

        {
            CopyOnWrite<int> b(a);
            EXPECT_EQ(7, b.read());

            EXPECT_FALSE(a.unique());
            EXPECT_FALSE(b.unique());
        }

        EXPECT_TRUE(a.unique());
    }

    TEST_CASE(MoveConstruct)
    {
        CopyOnWrite<int> a(11);
        EXPECT_TRUE(a.unique());

        CopyOnWrite<int> b(std::move(a));
        EXPECT_TRUE(a.is_null());
        EXPECT_TRUE(b.unique());

        EXPECT_EQ(11, b.read());
    }

    TEST_CASE(CopyAssign)
    {
        CopyOnWrite<int> a(7);
        EXPECT_TRUE(a.unique());

        {
            CopyOnWrite<int> b;
            b = a;
            EXPECT_EQ(7, b.read());

            EXPECT_FALSE(a.unique());
            EXPECT_FALSE(b.unique());
        }

        EXPECT_TRUE(a.unique());
    }

    TEST_CASE(MoveAssign)
    {
        CopyOnWrite<int> a(11);
        EXPECT_TRUE(a.unique());

        CopyOnWrite<int> b;
        b = std::move(a);
        EXPECT_TRUE(a.is_null());
        EXPECT_TRUE(b.unique());

        EXPECT_EQ(11, b.read());
    }

    TEST_CASE(Write)
    {
        CopyOnWrite<int> a(7);
        CopyOnWrite<int> b(a);

        EXPECT_FALSE(a.unique());
        EXPECT_FALSE(b.unique());

        EXPECT_EQ(7, a.read());
        EXPECT_EQ(7, b.read());

        a.write() = 11;

        EXPECT_TRUE(a.unique());
        EXPECT_TRUE(b.unique());

        EXPECT_EQ(11, a.read());
        EXPECT_EQ(7, b.read());
    }
}
