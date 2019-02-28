
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/fp.h"
#include "foundation/math/intersection/raysphere.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

namespace
{
    const Vector3d SphereCenter(6.0, 4.0, 0.0);
    const double SphereRadius = 2.0;

    const Vector3d RayOrigin(2.0, 1.0, 0.0);
    const Vector3d RayDirection(2.0, 1.0, 0.0);     // not unit-length
}

TEST_SUITE(Foundation_Math_Intersection_RaySphere)
{
    //
    // The ray misses the sphere.
    //

    TEST_CASE(IntersectSphere_RayMissesSphere_ReturnsFalse)
    {
        const Ray3d ray(RayOrigin + Vector3d(6.0, 0.0, 0.0), RayDirection, 1.0, 6.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_FALSE(hit);
    }

    TEST_CASE(IntersectSphere_RayMissesSphere_ReturnsFalseAndLeavesTMinUntouched)
    {
        const Ray3d ray(RayOrigin + Vector3d(6.0, 0.0, 0.0), RayDirection, 1.0, 6.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_FALSE(hit);
        EXPECT_TRUE(FP<double>::is_snan(tmin));
    }

    TEST_CASE(IntersectSphere_RayMissesSphere_ReturnsFalseAndLeavesTOutUntouched)
    {
        const Ray3d ray(RayOrigin + Vector3d(6.0, 0.0, 0.0), RayDirection, 1.0, 6.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(0, hits);
        EXPECT_TRUE(FP<double>::is_snan(t_out[0]));
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray's support line intersects the sphere but tmax is too small.
    //

    TEST_CASE(IntersectSphere_TMaxTooSmall_ReturnsFalse)
    {
        const Ray3d ray(RayOrigin, RayDirection, 0.5, 1.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_FALSE(hit);
    }

    TEST_CASE(IntersectSphere_TMaxTooSmall_ReturnsFalseAndLeavesTMinUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 0.5, 1.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_FALSE(hit);
        EXPECT_TRUE(FP<double>::is_snan(tmin));
    }

    TEST_CASE(IntersectSphere_TMaxTooSmall_ReturnsFalseAndLeavesTOutUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 0.5, 1.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(0, hits);
        EXPECT_TRUE(FP<double>::is_snan(t_out[0]));
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray's support line intersects the sphere but tmin is too big.
    //

    TEST_CASE(IntersectSphere_TMinTooBig_ReturnsFalse)
    {
        const Ray3d ray(RayOrigin, RayDirection, 6.0, 8.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_FALSE(hit);
    }

    TEST_CASE(IntersectSphere_TMinTooBig_ReturnsFalseAndLeavesTMinUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 6.0, 8.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_FALSE(hit);
        EXPECT_TRUE(FP<double>::is_snan(tmin));
    }

    TEST_CASE(IntersectSphere_TMinTooBig_ReturnsFalseAndLeavesTOutUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 6.0, 8.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(0, hits);
        EXPECT_TRUE(FP<double>::is_snan(t_out[0]));
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray's support line intersects the sphere but tmin and tmax are inside the sphere.
    //

    TEST_CASE(IntersectSphere_TMinAndTMaxInsideSphere_ReturnsFalse)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 3.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_FALSE(hit);
    }

    TEST_CASE(IntersectSphere_TMinAndTMaxInsideSphere_ReturnsFalseAndLeavesTMinUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 3.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_FALSE(hit);
        EXPECT_TRUE(FP<double>::is_snan(tmin));
    }

    TEST_CASE(IntersectSphere_TMinAndTMaxInsideSphere_ReturnsFalseAndLeavesTOutUntouched)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 3.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(0, hits);
        EXPECT_TRUE(FP<double>::is_snan(t_out[0]));
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray intersects the sphere in a single point because tmin is inside the sphere.
    //

    TEST_CASE(IntersectSphere_TMinInsideSphere_ReturnsTrue)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 6.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_TRUE(hit);
    }

    TEST_CASE(IntersectSphere_TMinInsideSphere_ReturnsTrueAndDistanceToClosestHit)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 6.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(3.0, tmin);
    }

    TEST_CASE(IntersectSphere_TMinInsideSphere_ReturnsTrueAndDistancesToUniqueHit)
    {
        const Ray3d ray(RayOrigin, RayDirection, 2.0, 6.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(1, hits);
        EXPECT_FEQ(3.0, t_out[0]);
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray intersects the sphere in a single point because tmax is inside the sphere.
    //

    TEST_CASE(IntersectSphere_TMaxInsideSphere_ReturnsTrue)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 2.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_TRUE(hit);
    }

    TEST_CASE(IntersectSphere_TMaxInsideSphere_ReturnsTrueAndDistanceToClosestHit)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 2.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.4, tmin);
    }

    TEST_CASE(IntersectSphere_TMaxInsideSphere_ReturnsTrueAndDistancesToUniqueHit)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 2.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(1, hits);
        EXPECT_FEQ(1.4, t_out[0]);
        EXPECT_TRUE(FP<double>::is_snan(t_out[1]));
    }

    //
    // The ray intersects the sphere in two points.
    //

    TEST_CASE(IntersectSphere_RaySegmentsSpansEntireSphere_ReturnsTrue)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 6.0);

        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius);

        ASSERT_TRUE(hit);
    }

    TEST_CASE(IntersectSphereUnitDirection_RaySegmentsSpansEntireSphere_ReturnsTrue)
    {
        const double K = norm(RayDirection);
        const Ray3d ray(RayOrigin, RayDirection / K, 1.0 * K, 6.0 * K);

        const bool hit = intersect_sphere_unit_direction(ray, SphereCenter, SphereRadius);

        ASSERT_TRUE(hit);
    }

    TEST_CASE(IntersectSphere_RaySegmentsSpansEntireSphere_ReturnsTrueAndDistanceToClosestHit)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 6.0);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.4, tmin);
    }

    TEST_CASE(IntersectSphereUnitDirection_RaySegmentsSpansEntireSphere_ReturnsTrueAndDistanceToClosestHit)
    {
        const double K = norm(RayDirection);
        const Ray3d ray(RayOrigin, RayDirection / K, 1.0 * K, 6.0 * K);

        double tmin = FP<double>::snan();
        const bool hit = intersect_sphere(ray, SphereCenter, SphereRadius, tmin);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.4 * K, tmin);
    }

    TEST_CASE(IntersectSphere_RaySegmentsSpansEntireSphere_ReturnsTrueAndDistancesToBothHits)
    {
        const Ray3d ray(RayOrigin, RayDirection, 1.0, 6.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(2, hits);
        EXPECT_FEQ(1.4, t_out[0]);
        EXPECT_FEQ(3.0, t_out[1]);
    }

    TEST_CASE(IntersectSphereUnitDirection_RaySegmentsSpansEntireSphere_ReturnsTrueAndDistancesToBothHits)
    {
        const double K = norm(RayDirection);
        const Ray3d ray(RayOrigin, RayDirection / K, 1.0 * K, 6.0 * K);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(2, hits);
        EXPECT_FEQ(1.4 * K, t_out[0]);
        EXPECT_FEQ(3.0 * K, t_out[1]);
    }

    TEST_CASE(IntersectSphere_RaySegmentsSpansEntireSphere_ReversedDirection_ReturnsTrueAndDistancesToBothHits)
    {
        const Ray3d ray(2.0 * SphereCenter - RayOrigin, -RayDirection, 1.0, 6.0);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(2, hits);
        EXPECT_FEQ(1.4, t_out[0]);
        EXPECT_FEQ(3.0, t_out[1]);
    }

    TEST_CASE(IntersectSphereUnitDirection_RaySegmentsSpansEntireSphere_ReversedDirection_ReturnsTrueAndDistancesToBothHits)
    {
        const double K = norm(RayDirection);
        const Ray3d ray(2.0 * SphereCenter - RayOrigin, -RayDirection / K, 1.0 * K, 6.0 * K);

        double t_out[2] = { FP<double>::snan(), FP<double>::snan() };
        const size_t hits = intersect_sphere(ray, SphereCenter, SphereRadius, t_out);

        ASSERT_EQ(2, hits);
        EXPECT_FEQ(1.4 * K, t_out[0]);
        EXPECT_FEQ(3.0 * K, t_out[1]);
    }
}
