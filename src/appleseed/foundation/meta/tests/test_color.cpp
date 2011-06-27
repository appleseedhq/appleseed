
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/image/color.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Image_Color)
{
    TEST_CASE(TestMin)
    {
        const Color3d a(2.0, -4.0, 1.0);
        const Color3d b(-3.0, -2.0, 0.0);

        EXPECT_EQ(Color3d(-3.0, -4.0, 0.0), min(a, b));
    }

    TEST_CASE(TestMax)
    {
        const Color3d a(2.0, -4.0, 1.0);
        const Color3d b(-3.0, -2.0, 0.0);

        EXPECT_EQ(Color3d(2.0, -2.0, 1.0), max(a, b));
    }

    TEST_CASE(TestAverageValue)
    {
        EXPECT_FEQ(0.0, average_value(Color4d(0.0, 0.0, 0.0, 0.0)));
        EXPECT_FEQ(0.0, average_value(Color4d(-2.0, -1.0, 1.0, 2.0)));
        EXPECT_FEQ(2.5, average_value(Color4d(1.0, 2.0, 3.0, 4.0)));
    }
}

TEST_SUITE(Foundation_Image_Color4)
{
    TEST_CASE(Constructor_GivenColor3AndSeparateAlpha)
    {
        EXPECT_EQ(Color4d(1.0, 2.0, 3.0, 4.0), Color4d(Color3d(1.0, 2.0, 3.0), 4.0));
    }
}
