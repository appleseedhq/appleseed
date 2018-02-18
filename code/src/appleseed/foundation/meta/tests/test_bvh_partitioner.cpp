
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Petra Gospodnetic, The appleseedhq Organization
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
#include "foundation/math/bvh.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <vector>

using namespace foundation;
using namespace bvh;

TEST_SUITE(Foundation_Math_BVH_MiddlePartitioner)
{
    typedef std::vector<AABB2d> AABB2dVector;

    //                 .         |
    //                 .     __  |
    //                 .    |  | |
    //  --10-9---------4----2--1---1--2-----
    //    |__|         .         | |__|
    //                 .         |
    //                 .         |
    //     [0]               [1]    [2]

    TEST_CASE(Partition_BBoxesOrderedAlongLongestDimension_ReturnsFirstElementAfterCenter)
    {
        AABB2dVector bboxes;
        bboxes.emplace_back(Vector2d(-10.0, -1.0 ), Vector2d( -9.0,  0.0 ));
        bboxes.emplace_back(Vector2d( -2.0,  0.0 ), Vector2d( -1.0,  1.0 ));
        bboxes.emplace_back(Vector2d(  1.0, -1.0 ), Vector2d(  2.0,  0.0 ));

        MiddlePartitioner<AABB2dVector> partitioner(bboxes, 1);
        const AABB2d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        const size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);

        EXPECT_EQ(1, pivot);
    }

    //                 .         |  __
    //                 .     __  | |__|
    //                 .    |  | |
    //  --10-9---------4----2--1---1--2-----
    //    |__|         .         |
    //                 .         |
    //                 .         |
    //     [1]               [0]    [2]

    TEST_CASE(Partition_BBoxesUnorderedAlongAllDimensions_ReturnsFirstElementAfterCenter)
    {
        AABB2dVector bboxes;
        bboxes.emplace_back(Vector2d( -2.0,  0.0 ), Vector2d( -1.0,  1.0 ));
        bboxes.emplace_back(Vector2d(-10.0, -1.0 ), Vector2d( -9.0,  0.0 ));
        bboxes.emplace_back(Vector2d(  1.0,  1.0 ), Vector2d(  2.0,  2.0 ));

        MiddlePartitioner<AABB2dVector> partitioner(bboxes, 1);
        const AABB2d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        const size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);

        EXPECT_EQ(1, pivot);
    }

    //      [0]    [2]
    //      __  |  __
    //     |__| | |__|
    //          |
    //  ---2--1---1--2---
    //      __  |  __
    //     |__| | |__|
    //          |
    //      [1]    [3]

    TEST_CASE(Partition_BBoxesFormingRectangle_ReturnsFirstElementAfterCenter)
    {
        AABB2dVector bboxes;
        bboxes.emplace_back(Vector2d(-2.0, 1.0 ), Vector2d(-1.0, 2.0 ));
        bboxes.emplace_back(Vector2d(-2.0,-2.0 ), Vector2d(-1.0,-1.0 ));
        bboxes.emplace_back(Vector2d( 1.0, 1.0 ), Vector2d( 2.0, 2.0 ));
        bboxes.emplace_back(Vector2d( 1.0,-2.0 ), Vector2d( 2.0,-1.0 ));

        MiddlePartitioner<AABB2dVector> partitioner(bboxes, 1);
        const AABB2d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        const size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);

        EXPECT_EQ(2, pivot);
    }

    //      [0]    [2]
    //      __  |  __
    //     |__| | |__|
    //          |
    //  ---2--1---1--2---
    //         _|_
    //        |_|_|
    //          |
    //         [1]

    TEST_CASE(Partition_BBoxesFormingEvenTriangle_ReturnsMiddleElement)
    {
        AABB2dVector bboxes;
        bboxes.emplace_back(Vector2d(-2.0, 1.0 ), Vector2d(-1.0, 2.0 ));
        bboxes.emplace_back(Vector2d(-1.0,-2.0 ), Vector2d( 1.0,-1.0 ));
        bboxes.emplace_back(Vector2d( 1.0, 1.0 ), Vector2d( 2.0, 2.0 ));

        MiddlePartitioner<AABB2dVector> partitioner(bboxes, 1);
        const AABB2d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        const size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);

        EXPECT_EQ(1, pivot);
    }

    TEST_CASE(Partition_BBoxesOverlapping_ReturnMiddleElement)
    {
        AABB2dVector bboxes;
        bboxes.emplace_back(Vector2d(-1.0,-1.0 ), Vector2d( 1.0, 1.0 ));
        bboxes.emplace_back(Vector2d(-1.0,-1.0 ), Vector2d( 1.0, 1.0 ));
        bboxes.emplace_back(Vector2d(-1.0,-1.0 ), Vector2d( 1.0, 1.0 ));
        bboxes.emplace_back(Vector2d(-1.0,-1.0 ), Vector2d( 1.0, 1.0 ));

        MiddlePartitioner<AABB2dVector> partitioner(bboxes, 1);
        const AABB2d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        const size_t pivot1 = partitioner.partition(0, bboxes.size(), root_bbox);
        // Check partitioning in case the begin is not 0.
        const size_t pivot2 = partitioner.partition(2, bboxes.size(), root_bbox);

        EXPECT_EQ(2, pivot1);
        EXPECT_EQ(3, pivot2);
    }
}
