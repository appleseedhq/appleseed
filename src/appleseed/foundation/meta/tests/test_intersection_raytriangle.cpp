
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/intersection/raytrianglemt.h"
#include "foundation/math/intersection/raytrianglessk.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

using namespace foundation;

namespace
{
    template <typename TriangleType>
    struct RayTriangleFixture
    {
        const TriangleType  m_triangle;

        RayTriangleFixture()
          : m_triangle(
                Vector3d(0.5, 0.0, 0.5),
                Vector3d(-0.5, 0.0, 0.5),
                Vector3d(-0.5, 0.0, -0.5))
        {
        }
    };
}

TEST_SUITE(Foundation_Math_Intersection_RayTriangleMT)
{
    typedef RayTriangleFixture<TriangleMT<double> > Fixture;

    TEST_CASE_F(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsTrue, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 1.0, 10.0);

        const bool hit = m_triangle.intersect(ray);

        ASSERT_TRUE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsFalse, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 0.0, 1.0);

        const bool hit = m_triangle.intersect(ray);

        ASSERT_FALSE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsHit, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 1.0, 10.0);

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, t);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsNoHit, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 0.0, 1.0);

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_FALSE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayHittingDiagonalOfQuad_ReturnsHit, Fixture)
    {
        const Ray3d ray(Vector3d(0.0, 1.0, 0.0), Vector3d(0.0, -1.0, 0.0));

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, t);
        EXPECT_FEQ(0.0, u);
        EXPECT_FEQ(0.5, v);
    }
}

TEST_SUITE(Foundation_Math_Intersection_RayTriangleSSK)
{
    typedef RayTriangleFixture<TriangleSSK<double> > Fixture;

    TEST_CASE_F(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsTrue, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 1.0, 10.0);

        const bool hit = m_triangle.intersect(ray);

        ASSERT_TRUE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsFalse, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 0.0, 1.0);

        const bool hit = m_triangle.intersect(ray);

        ASSERT_FALSE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMinEqualToHitDistance_ReturnsHit, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 1.0, 10.0);

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, t);
    }

    TEST_CASE_F(Intersect_GivenRayWithTMaxEqualToHitDistance_ReturnsNoHit, Fixture)
    {
        const Ray3d ray(Vector3d(-0.2, 1.0, 0.2), Vector3d(0.0, -1.0, 0.0), 0.0, 1.0);

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_FALSE(hit);
    }

    TEST_CASE_F(Intersect_GivenRayHittingDiagonalOfQuad_ReturnsHit, Fixture)
    {
        const Ray3d ray(Vector3d(0.0, 1.0, 0.0), Vector3d(0.0, -1.0, 0.0));

        double t, u, v;
        const bool hit = m_triangle.intersect(ray, t, u, v);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(1.0, t);
        EXPECT_FEQ(0.0, u);
        EXPECT_FEQ(0.5, v);
    }
}
