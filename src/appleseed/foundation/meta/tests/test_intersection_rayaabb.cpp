
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
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <limits>

using namespace foundation;

TEST_SUITE(Foundation_Math_Intersection_RayAABB)
{
#pragma warning (push)
#pragma warning (disable : 4723)    // potential division by 0

    TEST_CASE(Intersect_GivenRayNotPiercingTheAABB_ReturnsFalse)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingPosXFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 0.0, 0.0), Vector3d(-1.0, 0.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingNegXFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(-2.0, 0.0, 0.0), Vector3d(1.0, 0.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingPosYFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 2.0, 0.0), Vector3d(0.0, -1.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingNegYFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, -2.0, 0.0), Vector3d(0.0, 1.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingPosZFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayPiercingNegZFaceInTheMiddle_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, -2.0), Vector3d(0.0, 0.0, 1.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosXFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(1.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegXFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosYFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 1.0, 0.0), Vector3d(-1.0, 0.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegYFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 0.0, 0.0), Vector3d(-1.0, 0.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosZFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 2.0, 1.0), Vector3d(0.0, -1.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegZFace_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 2.0, 0.0), Vector3d(0.0, -1.0, 0.0));

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsTrue)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.0, 10.0);

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
    }

    TEST_CASE(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsFalse)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 1.0);

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
    }

    TEST_CASE(Intersect_GivenRayWithTMinLargerThanHitDistance_ReturnsFalse)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.1, 10.0);

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
    }

    TEST_CASE(Intersect_GivenRayWithTMaxSmallerThanHitDistance_ReturnsFalse)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 0.9);

        const bool hit = intersect(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
    }

    TEST_CASE(Intersect_GivenRayNotPiercingTheAABB_ReturnsFalseAndLeavesDistanceUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        EXPECT_FALSE(hit);
        EXPECT_EQ(42.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosXFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(1.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegXFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosYFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 1.0, 0.0), Vector3d(-1.0, 0.0, 0.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegYFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(2.0, 0.0, 0.0), Vector3d(-1.0, 0.0, 0.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInPosZFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 2.0, 1.0), Vector3d(0.0, -1.0, 0.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayEmbeddedInNegZFace_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(0.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 2.0, 0.0), Vector3d(0.0, -1.0, 0.0));

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, distance);
    }

    TEST_CASE(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsTrueAndDistanceToHit)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.0, 10.0);

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        EXPECT_TRUE(hit);
        EXPECT_EQ(3.0, distance);
    }

    TEST_CASE(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsFalseAndLeavesDistanceUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 1.0);

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        EXPECT_FALSE(hit);
        EXPECT_EQ(42.0, distance);
    }

    TEST_CASE(Intersect_GivenRayWithTMinLargerThanHitDistance_ReturnsFalseAndLeavesDistanceUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.1, 10.0);

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        EXPECT_FALSE(hit);
        EXPECT_EQ(42.0, distance);
    }

    TEST_CASE(Intersect_GivenRayWithTMaxSmallerThanHitDistance_ReturnsFalseAndLeavesDistanceUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        const Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 0.9);

        double distance = 42.0;
        const bool hit = intersect(ray, RayInfo3d(ray), aabb, distance);

        EXPECT_FALSE(hit);
        EXPECT_EQ(42.0, distance);
    }

    TEST_CASE(Clip_GivenRayNotPiercingTheAABB_ReturnsFalseAndLeavesTMinAndTMaxUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(2.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
        EXPECT_EQ(0.0, ray.m_tmin);
        EXPECT_EQ(std::numeric_limits<double>::max(), ray.m_tmax);
    }

    TEST_CASE(Clip_GivenRayPiercingPosZFaceInTheMiddle_ReturnsTrueAndUpdatesTMinAndTMax)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0));

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
        EXPECT_FEQ(1.0, ray.m_tmin);
        EXPECT_FEQ(3.0, ray.m_tmax);
    }

    TEST_CASE(Clip_GivenRayWithTMinEqualToHitDistance_ReturnsTrueAndUpdatesTMinAndTMax)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.0, 10.0);

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_TRUE(hit);
        EXPECT_EQ(3.0, ray.m_tmin);
        EXPECT_EQ(3.0, ray.m_tmax);
    }

    TEST_CASE(Clip_GivenRayWithTMaxEqualToHitDistance_ReturnsFalseAndLeavesTMinAndTMaxUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 1.0);

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
        EXPECT_EQ(0.0, ray.m_tmin);
        EXPECT_EQ(1.0, ray.m_tmax);
    }

    TEST_CASE(Clip_GivenRayWithTMinLargerThanHitDistance_ReturnsFalseAndLeavesTMinAndTMaxUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 3.1, 10.0);

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
        EXPECT_EQ(3.1, ray.m_tmin);
        EXPECT_EQ(10.0, ray.m_tmax);
    }

    TEST_CASE(Clip_GivenRayWithTMaxSmallerThanHitDistance_ReturnsFalseAndLeavesTMinAndTMaxUnchanged)
    {
        const AABB3d aabb(Vector3d(-1.0), Vector3d(1.0));
        Ray3d ray(Vector3d(0.0, 0.0, 2.0), Vector3d(0.0, 0.0, -1.0), 0.0, 0.9);

        const bool hit = clip(ray, RayInfo3d(ray), aabb);

        EXPECT_FALSE(hit);
        EXPECT_EQ(0.0, ray.m_tmin);
        EXPECT_EQ(0.9, ray.m_tmax);
    }

#pragma warning (pop)
}
