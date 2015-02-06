
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/intersection/planesegment.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Intersection_PlaneSegment)
{
    TEST_CASE(Clip_GivenSegmentParallelToPlaneInsideNegativeHalfSpace_ReturnsTrueAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(-1.0, 0.0, +1.0);
        const Vector3d OriginalB(-1.0, 0.0, -1.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentParallelToPlaneInsidePositiveHalfSpace_ReturnsFalseAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(1.0, 0.0, +1.0);
        const Vector3d OriginalB(1.0, 0.0, -1.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_FALSE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentFullyInsideNegativeHalfSpaceTowardPlane_ReturnsTrueAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(-2.0, 0.0, 0.0);
        const Vector3d OriginalB(-1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentFullyInsideNegativeHalfSpaceAwayFromPlane_ReturnsTrueAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(-1.0, 0.0, 0.0);
        const Vector3d OriginalB(-2.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentFullyInsidePositiveHalfSpaceTowardPlane_ReturnsFalseAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(2.0, 0.0, 0.0);
        const Vector3d OriginalB(1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_FALSE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentFullyInsidePositiveHalfSpaceAwayFromPlane_ReturnsFalseAndLeavesSegmentUnchanged)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(1.0, 0.0, 0.0);
        const Vector3d OriginalB(2.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_FALSE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenPlaneWithNonZeroOriginAndSegmentFullyInsideNegativeHalfSpace_ReturnsTrueAndLeavesSegmentUnchanged)
    {
        const Vector4d Plane(1.0, 0.0, 0.0, -1.0);
        const Vector3d OriginalA(-2.0, 0.0, 0.0);
        const Vector3d OriginalB(1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(Plane, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentStraddlingPlane1_ReturnsTrueAndClipsSegmentAgainstPlane)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(-1.0, 0.0, 0.0);
        const Vector3d OriginalB(+1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_FEQ(Vector3d(0.0, 0.0, 0.0), b);
    }

    TEST_CASE(Clip_GivenSegmentStraddlingPlane2_ReturnsTrueAndClipsSegmentAgainstPlane)
    {
        const Vector3d N(1.0, 0.0, 0.0);
        const Vector3d OriginalA(+1.0, 0.0, 0.0);
        const Vector3d OriginalB(-1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(N, a, b);

        EXPECT_TRUE(inside);
        EXPECT_FEQ(Vector3d(0.0, 0.0, 0.0), a);
        EXPECT_EQ(OriginalB, b);
    }

    TEST_CASE(Clip_GivenSegmentStraddlingPlaneWithNonZeroOrigin1_ReturnsTrueAndClipsSegmentAgainstPlane)
    {
        const Vector4d Plane(1.0, 0.0, 0.0, -1.0);
        const Vector3d OriginalA(-1.0, 0.0, 0.0);
        const Vector3d OriginalB(+2.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(Plane, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(OriginalA, a);
        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), b);
    }

    TEST_CASE(Clip_GivenSegmentStraddlingPlaneWithNonZeroOrigin2_ReturnsTrueAndClipsSegmentAgainstPlane)
    {
        const Vector4d Plane(1.0, 0.0, 0.0, -1.0);
        const Vector3d OriginalA(+2.0, 0.0, 0.0);
        const Vector3d OriginalB(-1.0, 0.0, 0.0);

        Vector3d a = OriginalA, b = OriginalB;
        const bool inside = clip(Plane, a, b);

        EXPECT_TRUE(inside);
        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), a);
        EXPECT_EQ(OriginalB, b);
    }
}
