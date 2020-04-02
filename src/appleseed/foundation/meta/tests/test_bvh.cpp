
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
#include "foundation/containers/alignedvector.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Math_BVH_Node)
{
    TEST_CASE(TestStorageAndRetrievalOf2DBoundingBoxes)
    {
        static const AABB2d LeftBBox(Vector2d(1.0, 2.0), Vector2d(3.0, 4.0));
        static const AABB2d RightBBox(Vector2d(5.0, 6.0), Vector2d(7.0, 8.0));

        bvh::Node<AABB2d> node;
        node.make_interior();

        node.set_left_bbox(LeftBBox);
        node.set_right_bbox(RightBBox);

        EXPECT_EQ(LeftBBox, node.get_left_bbox());
        EXPECT_EQ(RightBBox, node.get_right_bbox());
    }

    TEST_CASE(TestStorageAndRetrievalOf3DBoundingBoxes)
    {
        static const AABB3d LeftBBox(Vector3d(1.0, 2.0, 3.0), Vector3d(4.0, 5.0, 6.0));
        static const AABB3d RightBBox(Vector3d(7.0, 8.0, 9.0), Vector3d(10.0, 11.0, 12.0));

        bvh::Node<AABB3d> node;
        node.make_interior();

        node.set_left_bbox(LeftBBox);
        node.set_right_bbox(RightBBox);

        EXPECT_EQ(LeftBBox, node.get_left_bbox());
        EXPECT_EQ(RightBBox, node.get_right_bbox());
    }
}

TEST_SUITE(Foundation_Math_BVH_SpatialBuilder)
{
    struct ItemHandler
    {
        double get_bbox_grow_eps() const
        {
            return 1.0e-9;
        }

        AABB3d clip(
            const size_t    item_index,
            const size_t    dimension,
            const double    bin_min,
            const double    bin_max) const
        {
            return AABB3d();
        }

        bool intersect(
            const size_t    item_index,
            const AABB3d&   bbox) const
        {
            return false;
        }
    };

    TEST_CASE(InstantiateSpatialBuilderAndSBVHPartitioner)
    {
        typedef AlignedVector<bvh::Node<AABB3d>> NodeVector;
        typedef std::vector<AABB3d> AABBVector;

        typedef bvh::Tree<NodeVector> Tree;
        typedef bvh::SBVHPartitioner<ItemHandler, AABBVector> Partitioner;

        AABBVector bboxes;
        bboxes.emplace_back(Vector3d(0.0), Vector3d(1.0));

        ItemHandler item_handler;
        Partitioner partitioner(item_handler, bboxes);

        Tree tree;
        bvh::SpatialBuilder<Tree, Partitioner> builder;
    }
}

TEST_SUITE(Foundation_Math_BVH_Intersector_2D)
{
    typedef bvh::Node<AABB2d> NodeType;

    struct Visitor
    {
        bool visit(
            const NodeType&             node,
            const Ray2d&                ray,
            const RayInfo2d&            ray_info,
            double&                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
            , bvh::TraversalStatistics& stats
#endif
            )
        {
            return false;
        }
    };

    TEST_CASE(InstantiateIntersectorIn2DCase)
    {
        bvh::Intersector<
            bvh::Tree<AlignedVector<NodeType>>,
            Ray2d,
            Visitor
        > intersector;
    }
}
