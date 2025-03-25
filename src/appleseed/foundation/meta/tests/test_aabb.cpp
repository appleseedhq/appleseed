
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathBox.h"
#include "foundation/platform/_endexrheaders.h"
#endif

using namespace foundation;

TEST_SUITE(Foundation_Math_AABB)
{
    TEST_CASE(ConstructWithMinMax)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), bbox.min);
        EXPECT_EQ(Vector3d(4.0, 5.0, 6.0), bbox.max);
    }

    TEST_CASE(ConstructByTypeConversion)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3f bboxf(bbox);

        EXPECT_FEQ(Vector3f(1.0f, 2.0f, 3.0f), bboxf.min);
        EXPECT_FEQ(Vector3f(4.0f, 5.0f, 6.0f), bboxf.max);
    }

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    TEST_CASE(ConstructFromImathBox)
    {
        const Imath::Box2d source(
            Imath::V2d(1.0, 2.0),
            Imath::V2d(3.0, 4.0));

        const AABB2d copy(source);

        EXPECT_EQ(Vector2d(1.0, 2.0), copy.min);
        EXPECT_EQ(Vector2d(3.0, 4.0), copy.max);
    }

    TEST_CASE(ConvertToImathBox)
    {
        const AABB2d source(
            Vector2d(1.0, 2.0),
            Vector2d(3.0, 4.0));

        const Imath::Box2d copy(source);

        EXPECT_EQ(Imath::V2d(1.0, 2.0), copy.min);
        EXPECT_EQ(Imath::V2d(3.0, 4.0), copy.max);
    }

#endif

    TEST_CASE(ConstructInvalidAABB)
    {
        const AABB3d bbox(AABB3d::invalid());

        EXPECT_FALSE(bbox.is_valid());
    }

    TEST_CASE(TestArraySubscripting)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), bbox[0]);
        EXPECT_EQ(Vector3d(4.0, 5.0, 6.0), bbox[1]);
    }

    TEST_CASE(TestInvalidateOnFloatingPointAABB)
    {
        AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        bbox.invalidate();

        EXPECT_FALSE(bbox.is_valid());
    }

    TEST_CASE(VerifyThatRank0AABBOverlapsWithItself)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(1.0, 2.0, 3.0));

        EXPECT_TRUE(AABB3d::overlap(bbox, bbox));
    }

    TEST_CASE(VerifyThatRank3AABBOverlapsWithItself)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_TRUE(AABB3d::overlap(bbox, bbox));
    }

    TEST_CASE(TestOverlapWithOverlappingRank3AABB)
    {
        const AABB3d bbox1(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox2(
            Vector3d(0.0, 1.0, 5.0),
            Vector3d(5.0, 3.0, 7.0));

        EXPECT_TRUE(AABB3d::overlap(bbox1, bbox2));
        EXPECT_TRUE(AABB3d::overlap(bbox2, bbox1));
    }

    TEST_CASE(TestOverlapWithNonOverlappingRank3AABB)
    {
        const AABB3d bbox1(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox2(
            Vector3d(0.0, 1.0, 5.0),
            Vector3d(5.0, 3.0, 7.0));

        EXPECT_TRUE(AABB3d::overlap(bbox1, bbox2));
        EXPECT_TRUE(AABB3d::overlap(bbox2, bbox1));
    }

    TEST_CASE(TestOverlapRatio)
    {
        EXPECT_FEQ(0.0,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(0.0, 0.0), Vector2d(1.0, 1.0)),
                AABB2d(Vector2d(2.0, 0.0), Vector2d(3.0, 1.0))));

        EXPECT_FEQ(0.0,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(2.0, 0.0), Vector2d(3.0, 1.0)),
                AABB2d(Vector2d(0.0, 0.0), Vector2d(1.0, 1.0))));

        EXPECT_FEQ(1.0,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(1.0, 1.0), Vector2d(2.0, 2.0)),
                AABB2d(Vector2d(1.0, 1.0), Vector2d(2.0, 2.0))));

        EXPECT_FEQ(1.0,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(1.0, 1.0), Vector2d(2.0, 2.0)),
                AABB2d(Vector2d(0.0, 0.0), Vector2d(3.0, 3.0))));

        EXPECT_FEQ(1.0,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(0.0, 0.0), Vector2d(3.0, 3.0)),
                AABB2d(Vector2d(1.0, 1.0), Vector2d(2.0, 2.0))));

        EXPECT_FEQ(0.5,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(0.0, 0.0), Vector2d(2.0, 2.0)),
                AABB2d(Vector2d(1.0, 0.0), Vector2d(3.0, 2.0))));

        EXPECT_FEQ(0.5,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(1.0, 0.0), Vector2d(3.0, 2.0)),
                AABB2d(Vector2d(0.0, 0.0), Vector2d(2.0, 2.0))));

        EXPECT_FEQ(0.25,
            AABB2d::overlap_ratio(
                AABB2d(Vector2d(0.0, 0.0), Vector2d(2.0, 2.0)),
                AABB2d(Vector2d(1.0, 1.0), Vector2d(3.0, 3.0))));
    }

    TEST_CASE(TestExtentRatio)
    {
        EXPECT_FEQ(1.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(0.0, 0.0, 0.0)),
                AABB3d(Vector3d(0.0), Vector3d(0.0, 0.0, 0.0))));

        EXPECT_FEQ(1.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 1.0)),
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 1.0))));

        EXPECT_FEQ(1.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(0.0, 1.0, 1.0)),
                AABB3d(Vector3d(0.0), Vector3d(0.0, 1.0, 1.0))));

        EXPECT_FEQ(1.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(1.0, 0.0, 1.0)),
                AABB3d(Vector3d(0.0), Vector3d(1.0, 0.0, 1.0))));

        EXPECT_FEQ(1.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 0.0)),
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 0.0))));

        EXPECT_FEQ(2.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(2.0, 1.0, 1.0)),
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 1.0))));

        EXPECT_FEQ(8.0,
            AABB3d::extent_ratio(
                AABB3d(Vector3d(0.0), Vector3d(2.0, 2.0, 2.0)),
                AABB3d(Vector3d(0.0), Vector3d(1.0, 1.0, 1.0))));
    }

    TEST_CASE(TestInsertPointIntoInvalidAABB)
    {
        AABB3d bbox(AABB3d::invalid());

        bbox.insert(Vector3d(1.0, 2.0, 3.0));

        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), bbox.min);
        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), bbox.max);
    }

    TEST_CASE(TestInsertPointIntoValidAABB)
    {
        AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        bbox.insert(Vector3d(-1.0, 50.0, 60.0));

        EXPECT_EQ(Vector3d(-1.0, 2.0, 3.0), bbox.min);
        EXPECT_EQ(Vector3d(4.0, 50.0, 60.0), bbox.max);
    }

    TEST_CASE(TestInsertAABBIntoInvalidAABB)
    {
        AABB3d bbox(AABB3d::invalid());

        bbox.insert(
            AABB3d(
                Vector3d(1.0, 2.0, 3.0),
                Vector3d(4.0, 5.0, 6.0)));

        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), bbox.min);
        EXPECT_EQ(Vector3d(4.0, 5.0, 6.0), bbox.max);
    }

    TEST_CASE(TestInsertAABBIntoValidAABB)
    {
        AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        bbox.insert(
            AABB3d(
                Vector3d(7.0, 0.0, 2.0),
                Vector3d(8.0, 3.0, 9.0)));

        EXPECT_EQ(Vector3d(1.0, 0.0, 2.0), bbox.min);
        EXPECT_EQ(Vector3d(8.0, 5.0, 9.0), bbox.max);
    }

    TEST_CASE(TestGrow)
    {
        AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        bbox.grow(Vector3d(2.0, 0.0, -1.0));

        EXPECT_FEQ(Vector3d(-1.0, 2.0, 4.0), bbox.min);
        EXPECT_FEQ(Vector3d(6.0, 5.0, 5.0), bbox.max);
    }

    TEST_CASE(TestRobustGrow)
    {
        const Vector3d a(1.0, 2.0, 3.0);
        const Vector3d b(4.0, 5.0, 6.0);

        AABB3d bbox(a, b);

        bbox.robust_grow(1.0);

        EXPECT_LT(a[0], bbox.min[0]);
        EXPECT_LT(a[1], bbox.min[1]);
        EXPECT_LT(a[2], bbox.min[2]);

        EXPECT_GT(b[0], bbox.max[0]);
        EXPECT_GT(b[1], bbox.max[1]);
        EXPECT_GT(b[2], bbox.max[2]);
    }

    TEST_CASE(TestIsValid)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_TRUE(bbox.is_valid());

        EXPECT_FALSE(AABB3d::invalid().is_valid());
    }

    TEST_CASE(TestRank1)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(1.0, 2.0, 3.0));

        EXPECT_EQ(0, bbox.rank());
    }

    TEST_CASE(TestRank2)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(10.0, 20.0, 30.0));

        EXPECT_EQ(3, bbox.rank());
    }

    TEST_CASE(TestCenter)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(5.0, 6.0, 7.0));

        EXPECT_FEQ(Vector3d(3.0, 4.0, 5.0), bbox.center());
    }

    TEST_CASE(TestExtent)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(Vector3d(5.0, 7.0, 9.0), bbox.extent());
    }

    TEST_CASE(TestDiameter)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(std::sqrt(5.0 * 5.0 + 7.0 * 7.0 + 9.0 * 9.0), bbox.diameter());
    }

    TEST_CASE(TestSquareDiameter)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(5.0 * 5.0 + 7.0 * 7.0 + 9.0 * 9.0, bbox.square_diameter());
    }

    TEST_CASE(TestRadius)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        const double square_diameter = 5.0 * 5.0 + 7.0 * 7.0 + 9.0 * 9.0;
        const double radius = std::sqrt(square_diameter / 4.0);

        EXPECT_FEQ(radius, bbox.radius());
    }

    TEST_CASE(TestSquareRadius)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        const double square_diameter = 5.0 * 5.0 + 7.0 * 7.0 + 9.0 * 9.0;
        const double square_radius = square_diameter / 4.0;

        EXPECT_FEQ(square_radius, bbox.square_radius());
    }

    TEST_CASE(TestVolume)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(5.0 * 7.0 * 9.0, bbox.volume());
    }

    TEST_CASE(TestHalfSurfaceArea)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(5.0 * 7.0 + 5.0 * 9.0 + 7.0 * 9.0, half_surface_area(bbox));
    }

    TEST_CASE(TestSurfaceArea)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FEQ(2.0 * (5.0 * 7.0 + 5.0 * 9.0 + 7.0 * 9.0), surface_area(bbox));
    }

    TEST_CASE(TestGetCornerCount)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_EQ(8, bbox.get_corner_count());
    }

    TEST_CASE(TestComputeCorner)
    {
        const AABB3d bbox(
            Vector3d(-1.0, -2.0, -3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_EQ(Vector3d(-1.0, -2.0, -3.0), bbox.compute_corner(0));
        EXPECT_EQ(Vector3d( 4.0, -2.0, -3.0), bbox.compute_corner(1));
        EXPECT_EQ(Vector3d(-1.0,  5.0, -3.0), bbox.compute_corner(2));
        EXPECT_EQ(Vector3d( 4.0,  5.0, -3.0), bbox.compute_corner(3));
        EXPECT_EQ(Vector3d(-1.0, -2.0,  6.0), bbox.compute_corner(4));
        EXPECT_EQ(Vector3d( 4.0, -2.0,  6.0), bbox.compute_corner(5));
        EXPECT_EQ(Vector3d(-1.0,  5.0,  6.0), bbox.compute_corner(6));
        EXPECT_EQ(Vector3d( 4.0,  5.0,  6.0), bbox.compute_corner(7));
    }

    TEST_CASE(TestContainsOnRank0AABB)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(1.0, 2.0, 3.0));

        EXPECT_TRUE(bbox.contains(Vector3d(1.0, 2.0, 3.0)));
        EXPECT_FALSE(bbox.contains(Vector3d(1.0, 1.0, 3.0)));
    }

    TEST_CASE(TestContainsOnRank3AABB)
    {
        const AABB3d bbox(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_TRUE(bbox.contains(Vector3d(2.0, 3.0, 4.0)));
        EXPECT_FALSE(bbox.contains(Vector3d(2.0, 6.0, 4.0)));
    }

    TEST_CASE(TestEquality)
    {
        const AABB3d bbox1(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox2(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox3(
            Vector3d(0.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_TRUE(bbox1 == bbox2);
        EXPECT_FALSE(bbox1 == bbox3);
    }

    TEST_CASE(TestInequality)
    {
        const AABB3d bbox1(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox2(
            Vector3d(1.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        const AABB3d bbox3(
            Vector3d(0.0, 2.0, 3.0),
            Vector3d(4.0, 5.0, 6.0));

        EXPECT_FALSE(bbox1 != bbox2);
        EXPECT_TRUE(bbox1 != bbox3);
    }
}

TEST_SUITE(Foundation_Math_IntegralAABB)
{
    TEST_CASE(TestIntersectOnClearlyDisjointIntegralAABB)
    {
        const AABB2i bbox(
            Vector2i(1, 0),
            Vector2i(4, 2));

        const AABB2i bbox2(
            Vector2i(5, 0),
            Vector2i(8, 2));

        const AABB2i bbox3 = AABB2i::intersect(bbox, bbox2);

        EXPECT_FALSE(bbox3.is_valid());
    }

    TEST_CASE(TestIntersectOnAABBTouchingOnABorder)
    {
        const AABB2i bbox(
            Vector2i(1, 0),
            Vector2i(4, 2));

        const AABB2i bbox2(
            Vector2i(4, 0),
            Vector2i(8, 2));

        const AABB2i bbox3 = AABB2i::intersect(bbox, bbox2);

        EXPECT_TRUE(bbox3.is_valid());
    }

    TEST_CASE(TestInvalidateOnUnsignedIntegerAABB)
    {
        AABB3u bbox(
            Vector3u(1, 2, 3),
            Vector3u(4, 5, 6));

        bbox.invalidate();

        EXPECT_FALSE(bbox.is_valid());
    }

    TEST_CASE(TestOverlapOnIntegralAABB)
    {
        const AABB2i bbox1(Vector2i(1, 2), Vector2i(5, 4));
        const AABB2i bbox2(Vector2i(5, 3), Vector2i(8, 3));

        EXPECT_TRUE(AABB2i::overlap(bbox1, bbox2));
        EXPECT_TRUE(AABB2i::overlap(bbox2, bbox1));
    }

    TEST_CASE(TestExtentOnIntegralAABB)
    {
        const AABB3i bbox(
            Vector3i(-1, -2, -3),
            Vector3i(4, 5, 6));

        EXPECT_EQ(Vector3i(6, 8, 10), bbox.extent());
    }

    TEST_CASE(TestExtentOn1x1x1IntegralAABB)
    {
        const AABB3i bbox(
            Vector3i(0, 0, 0),
            Vector3i(0, 0, 0));

        EXPECT_TRUE(bbox.is_valid());
        EXPECT_EQ(Vector3i(1, 1, 1), bbox.extent());
    }

    TEST_CASE(TestVolumeOnIntegralAABB)
    {
        const AABB3i bbox(
            Vector3i(-1, -2, -3),
            Vector3i(4, 5, 6));

        EXPECT_EQ(6 * 8 * 10, bbox.volume());
    }
}

