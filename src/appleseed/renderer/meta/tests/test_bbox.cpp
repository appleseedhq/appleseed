
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.renderer headers.
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_BBox)
{
    TEST_CASE(ComputeUnion)
    {
        const AABB3d bboxes[2] =
        {
            AABB3d(Vector3d( 1.0), Vector3d(2.0)),
            AABB3d(Vector3d(-1.0), Vector3d(1.0))
        };

        const AABB3d result = compute_union<AABB3d>(bboxes, bboxes + 2);

        EXPECT_EQ(AABB3d(Vector3d(-1.0), Vector3d(2.0)), result);
    }

    TEST_CASE(Interpolate_GivenTimeIs0_ReturnsFirstBBox)
    {
        const AABB3d bboxes[2] =
        {
            AABB3d(Vector3d( 1.0), Vector3d(2.0)),
            AABB3d(Vector3d(-1.0), Vector3d(1.0))
        };

        const AABB3d result = interpolate<AABB3d>(bboxes, bboxes + 2, 0.0);

        EXPECT_EQ(bboxes[0], result);
    }

    TEST_CASE(Interpolate_GivenTimeIs1_ReturnsLastBBox)
    {
        const AABB3d bboxes[2] =
        {
            AABB3d(Vector3d( 1.0), Vector3d(2.0)),
            AABB3d(Vector3d(-1.0), Vector3d(1.0))
        };

        const AABB3d result = interpolate<AABB3d>(bboxes, bboxes + 2, 1.0 - 1.0e-16);

        EXPECT_FEQ(bboxes[1], result);
    }
}
