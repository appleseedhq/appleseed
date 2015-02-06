
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
#include "foundation/math/intersection/frustumsegment.h"
#include "foundation/math/frustum.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Intersection_FrustumSegment)
{
    TEST_CASE(Clip_GivenIntersectingFrustumAndSegment_ReturnsTrueAndClipsSegmentAgainstFrustum)
    {
        Frustum<double, 4> frustum;
        frustum.set_plane(0, normalize(Vector3d(0.0, 1.0, 1.0)));
        frustum.set_plane(1, normalize(Vector3d(0.0, -1.0, 1.0)));
        frustum.set_plane(2, normalize(Vector3d(-1.0, 0.0, 1.0)));
        frustum.set_plane(3, normalize(Vector3d(1.0, 0.0, 1.0)));

        Vector3d a(-3.0, 0.0, -1.0);
        Vector3d b(+3.0, 0.0, -1.0);

        const bool inside = clip(frustum, a, b);

        EXPECT_TRUE(inside);
        EXPECT_EQ(Vector3d(-1.0, 0.0, -1.0), a);
        EXPECT_FEQ(Vector3d(1.0, 0.0, -1.0), b);
    }
}
