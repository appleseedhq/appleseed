
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/frustum.h"
#include "foundation/math/intersection/frustumaabb.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Intersection_FrustumAABB)
{
    Frustum<double, 4> make_frustum()
    {
        Frustum<double, 4> frustum;
        frustum.set_plane(0, Vector4d(0.0, +1.0, 0.0, -1.0));   // top plane
        frustum.set_plane(1, Vector4d(0.0, -1.0, 0.0, -1.0));   // bottom plane
        frustum.set_plane(2, Vector4d(-1.0, 0.0, 0.0, -1.0));   // left plane
        frustum.set_plane(3, Vector4d(+1.0, 0.0, 0.0, -1.0));   // right plane
        return frustum;
    }

    TEST_CASE(Intersect_GivenAABBFullyAboveFrustum_ReturnsFalse)
    {
        const Frustum<double, 4> frustum = make_frustum();
        const AABB3d aabb(Vector3d(-0.5, 1.5, -0.5), Vector3d(0.5, 2.5, 0.5));

        EXPECT_FALSE(intersect(frustum, aabb));
    }

    TEST_CASE(Intersect_GivenAABBFullyBelowFrustum_ReturnsFalse)
    {
        const Frustum<double, 4> frustum = make_frustum();
        const AABB3d aabb(Vector3d(-0.5, -2.5, -0.5), Vector3d(0.5, -1.5, 0.5));

        EXPECT_FALSE(intersect(frustum, aabb));
    }

    TEST_CASE(Intersect_GivenAABBFullyLeftOfFrustum_ReturnsFalse)
    {
        const Frustum<double, 4> frustum = make_frustum();
        const AABB3d aabb(Vector3d(-2.5, -0.5, -0.5), Vector3d(-1.5, 0.5, 0.5));

        EXPECT_FALSE(intersect(frustum, aabb));
    }

    TEST_CASE(Intersect_GivenAABBFullyRightOfFrustum_ReturnsFalse)
    {
        const Frustum<double, 4> frustum = make_frustum();
        const AABB3d aabb(Vector3d(1.5, -0.5, -0.5), Vector3d(2.5, 0.5, 0.5));

        EXPECT_FALSE(intersect(frustum, aabb));
    }

    TEST_CASE(Intersect_GivenAABBFullyInsideFrustum_ReturnsTrue)
    {
        const Frustum<double, 4> frustum = make_frustum();
        const AABB3d aabb(Vector3d(-0.5, -0.5, -0.5), Vector3d(0.5, 0.5, 0.5));

        EXPECT_TRUE(intersect(frustum, aabb));
    }
}
