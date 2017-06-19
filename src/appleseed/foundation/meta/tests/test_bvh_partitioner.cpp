
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
#include "foundation/math/bvh.h"
#include "foundation/utility/test.h"

using namespace foundation;
using namespace bvh;
using namespace std;

TEST_SUITE(Foundation_Math_BVH_MiddlePartitioner)
{
    TEST_CASE(TestSingleBBoxPartition)
    {
        typedef std::vector<AABB3d> AABB3dVector;
        
        const AABB3d bbox(Vector3d(1.0, 2.0, 3.0), Vector3d(4.0, 5.0, 6.0));
        
        AABB3dVector bboxes;
        bboxes.push_back(bbox);

        MiddlePartitioner<AABB3dVector> partitioner(bboxes, 1);

        size_t pivot = partitioner.partition(0, 1, bbox);

        // Expect to return the end.
        EXPECT_EQ(1, pivot);
    }

    TEST_CASE(TestBBoxesOrderedAlongLongestDimension)
    {
        typedef std::vector<AABB3d> AABB3dVector;
        
        AABB3dVector bboxes;
        
        bboxes.push_back(AABB3d(Vector3d(-7.0, -1.0, -1.0), Vector3d(1.0,  1.0, 1.0)));
        bboxes.push_back(AABB3d(Vector3d( 1.0, -5.0, -1.0), Vector3d(3.0, -3.0, 1.0)));
        bboxes.push_back(AABB3d(Vector3d( 3.0,  3.0, -1.0), Vector3d(5.0,  5.0, 1.0)));

        MiddlePartitioner<AABB3dVector> partitioner(bboxes, 1);
        const AABB3d root_bbox(partitioner.compute_bbox(0, bboxes.size()));
        size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);

        // Expect to return the end.
        EXPECT_EQ(1, pivot);
    }

    TEST_CASE(TestBBoxesUnorderedAlongAllDimensions)
    {
        typedef std::vector<AABB3d> AABB3dVector;
        
        AABB3dVector bboxes;
        
        bboxes.push_back(AABB3d(Vector3d( 3.0,  13.0, -3.0), Vector3d(5.0,  15.0, -1.0)));
        bboxes.push_back(AABB3d(Vector3d( 1.0, -15.0,  1.0), Vector3d(3.0, -13.0,  2.0)));
        bboxes.push_back(AABB3d(Vector3d(-7.0,  -1.0, -1.0), Vector3d(1.0,   1.0,  1.0)));

        MiddlePartitioner<AABB3dVector> partitioner(bboxes, 1);
        const AABB3d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);
        // Expect to return the end.
        EXPECT_EQ(1, pivot);
    }

    TEST_CASE(TestBBoxes3PointLightsTestScene)
    {
        typedef std::vector<AABB3d> AABB3dVector;
        
        AABB3dVector bboxes;
        
        bboxes.push_back(AABB3d(Vector3d( -0.01, 0.09, 0.99 ), Vector3d( 0.01, 0.11, 1.01 )));
        bboxes.push_back(AABB3d(Vector3d( -0.01, 0.09,-0.51 ), Vector3d( 0.01, 0.11,-0.49 )));
        bboxes.push_back(AABB3d(Vector3d( -0.49, 0.09,-0.01 ), Vector3d( 0.51, 0.11, 0.01 )));

        MiddlePartitioner<AABB3dVector> partitioner(bboxes, 1);
        const AABB3d root_bbox(partitioner.compute_bbox(0, bboxes.size()));

        size_t pivot = partitioner.partition(0, bboxes.size(), root_bbox);
        // Expect to return the end.
        EXPECT_EQ(2, pivot);
    }
}