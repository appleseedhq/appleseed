
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/aabb.h"
#include "foundation/math/distance.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Distance)
{
    TEST_CASE(SquareDistance_PointIsInsideTheBoundingBox_ReturnsZero)
    {
        const AABB2d bbox(Vector2d(-1.0), Vector2d(+1.0));
        const Vector2d point(0.0, 0.0);

        EXPECT_EQ(0.0, square_distance(point, bbox));
        EXPECT_EQ(0.0, square_distance(bbox, point));
    }

    TEST_CASE(SquareDistance_PointIsToTheLeftOfTheBoundingBox_ReturnsSquareDistance)
    {
        const AABB2d bbox(Vector2d(-1.0), Vector2d(+1.0));
        const Vector2d point(-4.0, 3.0);

        EXPECT_FEQ(13.0, square_distance(point, bbox));
        EXPECT_FEQ(13.0, square_distance(bbox, point));
    }

    TEST_CASE(SquareDistance_PointIsToTheRightOfTheBoundingBox_ReturnsSquareDistance)
    {
        const AABB2d bbox(Vector2d(-1.0), Vector2d(+1.0));
        const Vector2d point(4.0, 3.0);

        EXPECT_FEQ(13.0, square_distance(point, bbox));
        EXPECT_FEQ(13.0, square_distance(bbox, point));
    }
}
