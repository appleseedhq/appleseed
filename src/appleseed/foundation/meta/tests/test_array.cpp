
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
#include "foundation/array/array.h"
#include "foundation/array/arrayref.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <utility>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Array_Array)
{
    TEST_CASE(Construct)
    {
        Array x(FloatType);
        EXPECT_TRUE(x.empty());
        EXPECT_EQ(FloatType, x.type());

        Array y(UInt32Type, 100);
        EXPECT_FALSE(y.empty());
        EXPECT_EQ(UInt32Type, y.type());
        EXPECT_EQ(100, y.size());
    }

    TEST_CASE(CopyConstruct)
    {
        Array x(FloatType);
        ArrayRef<float> xref(x);

        const float items[] = {1.0f, 5.0f, 7.0f, 11.0f};
        xref.assign(items, items + countof(items));

        Array y(x);
        EXPECT_TRUE(x == y);

        ArrayRef<float> yref(y);
        EXPECT_EQ(yref[2], 7.0f);
    }

    TEST_CASE(MoveConstruct)
    {
        Array x(UInt32Type);
        ArrayRef<uint32> xref(x);

        const uint32 items[] = {1, 5, 7, 11};
        xref.assign(items, items + countof(items));

        Array y(x);
        Array z(move(y));

        EXPECT_TRUE(y.is_moved());
        EXPECT_TRUE(x == z);

        ArrayRef<uint32> zref(z);
        EXPECT_EQ(zref[2], 7);
    }
}
