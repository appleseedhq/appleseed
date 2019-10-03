
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bsp.h"
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/math/ray.h"
#include "foundation/math/split.h"
#include "foundation/math/vector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <limits>
#include <memory>
#include <utility>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Math_BSP_Node)
{
    typedef bsp::Node<double> NodeType;

    TEST_CASE(TestLeafNode)
    {
        NodeType node;

        node.make_leaf();
        EXPECT_TRUE(node.is_leaf());

        node.set_leaf_index(42);
        EXPECT_TRUE(node.is_leaf());
        EXPECT_EQ(42, node.get_leaf_index());

        const size_t LeafIndex = (size_t(1) << 31) - 1;
        node.set_leaf_index(LeafIndex);
        EXPECT_TRUE(node.is_leaf());
        EXPECT_EQ(LeafIndex, node.get_leaf_index());

        node.set_leaf_size(33);
        EXPECT_TRUE(node.is_leaf());
        EXPECT_EQ(LeafIndex, node.get_leaf_index());
    }

    TEST_CASE(TestInteriorNode)
    {
        NodeType node;

        node.make_interior();
        EXPECT_TRUE(node.is_interior());

        node.set_child_node_index(42);
        EXPECT_TRUE(node.is_interior());
        EXPECT_EQ(42, node.get_child_node_index());

        const size_t ChildIndex = (size_t(1) << 29) - 1;
        node.set_child_node_index(ChildIndex);
        EXPECT_TRUE(node.is_interior());
        EXPECT_EQ(ChildIndex, node.get_child_node_index());

        node.set_split_dim(1);
        EXPECT_TRUE(node.is_interior());
        EXPECT_EQ(ChildIndex, node.get_child_node_index());
        EXPECT_EQ(1, node.get_split_dim());

        node.set_split_abs(66.0);
        EXPECT_TRUE(node.is_interior());
        EXPECT_EQ(ChildIndex, node.get_child_node_index());
        EXPECT_EQ(1, node.get_split_dim());
        EXPECT_EQ(66.0, node.get_split_abs());
    }
}

TEST_SUITE(Foundation_Math_BSP_Intersector)
{
    class Leaf
      : public NonCopyable
    {
      public:
        void clear()
        {
            m_boxes.clear();
        }

        size_t get_size() const
        {
            return m_boxes.size();
        }

        void insert(const AABB3d& box)
        {
            m_boxes.push_back(box);
        }

        const AABB3d& get_box(const size_t i) const
        {
            return m_boxes[i];
        }

        AABB3d get_bbox() const
        {
            AABB3d bbox;
            bbox.invalidate();

            for (size_t i = 0; i < m_boxes.size(); ++i)
                bbox.insert(m_boxes[i]);

            return bbox;
        }

        size_t get_memory_size() const
        {
            return 0;
        }

      private:
        std::vector<AABB3d>  m_boxes;
    };

    struct LeafFactory
      : public NonCopyable
    {
        Leaf* create_leaf()
        {
            return new Leaf();
        }
    };

    struct LeafSplitter
      : public NonCopyable
    {
        typedef bsp::LeafInfo<double, 3> LeafInfoType;
        typedef Split<double> SplitType;

        bool m_first_leaf;

        LeafSplitter()
          : m_first_leaf(true)
        {
        }

        double get_priority(
            const Leaf&             leaf,
            const LeafInfoType&     leaf_info)
        {
            const double priority = m_first_leaf ? 1.0 : 0.0;
            m_first_leaf = false;
            return priority;
        }

        bool split(
            const Leaf&             leaf,
            const LeafInfoType&     leaf_info,
            SplitType&              split)
        {
            split.m_dimension = 0;
            split.m_abscissa = 0.0;
            return true;
        }

        void sort(
            const Leaf&             leaf,
            const LeafInfoType&     leaf_info,
            const SplitType&        split,
            Leaf&                   left_leaf,
            const LeafInfoType&     left_leaf_info,
            Leaf&                   right_leaf,
            const LeafInfoType&     right_leaf_info)
        {
            for (size_t i = 0; i < leaf.get_size(); ++i)
            {
                const AABB3d& box = leaf.get_box(i);

                if (box.max[split.m_dimension] <= split.m_abscissa)
                {
                    left_leaf.insert(box);
                }
                else if (box.min[split.m_dimension] >= split.m_abscissa)
                {
                    right_leaf.insert(box);
                }
                else
                {
                    left_leaf.insert(box);
                    right_leaf.insert(box);
                }
            }
        }
    };

    class LeafVisitor
      : public NonCopyable
    {
      public:
        LeafVisitor()
          : m_visited_leaf_count(0)
          , m_closest_hit(std::numeric_limits<double>::max())
        {
        }

        double visit(
            const Leaf*             leaf,       // todo: why not a reference?
            const Ray3d&            ray,
            const RayInfo3d&        ray_info)
        {
            ++m_visited_leaf_count;

            for (size_t i = 0; i < leaf->get_size(); ++i)
            {
                const AABB3d& box = leaf->get_box(i);

                double distance;

                if (intersect(ray, ray_info, box, distance))
                    m_closest_hit = std::min(m_closest_hit, distance);
            }

            return m_closest_hit;
        }

        size_t get_visited_leaf_count() const
        {
            return m_visited_leaf_count;
        }

        double get_closest_hit() const
        {
            return m_closest_hit;
        }

      private:
        size_t  m_visited_leaf_count;
        double  m_closest_hit;
    };

    struct Fixture
    {
        typedef bsp::Tree<double, 3, Leaf> Tree;
        typedef bsp::Intersector<double, Tree, LeafVisitor, Ray3d> Intersector;

        Tree                        m_tree;
        LeafVisitor                 m_leaf_visitor;     // todo: Visitor or LeafVisitor?
        Intersector                 m_intersector;
        bsp::TraversalStatistics    m_traversal_stats;

        Fixture()
        {
            std::unique_ptr<Leaf> root_leaf(new Leaf());
            root_leaf->insert(AABB3d(Vector3d(-1.0, -0.5, -0.2), Vector3d(0.0, 0.5, 0.2)));
            root_leaf->insert(AABB3d(Vector3d(0.0, -0.5, -0.7), Vector3d(1.0, 0.5, 0.7)));

            bsp::Builder<Tree, LeafFactory, LeafSplitter> builder;
            LeafFactory leaf_factory;
            LeafSplitter leaf_splitter;

            builder.build(m_tree, std::move(root_leaf), leaf_factory, leaf_splitter);
        }
    };

#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
#define TRAVERSAL_STATISTICS , m_traversal_stats
#else
#define TRAVERSAL_STATISTICS
#endif

#pragma warning (push)
#pragma warning (disable : 4723)    // potential division by 0

    TEST_CASE_F(Intersect_GivenRayEmbeddedInSplitPlane_VisitsBothLeaves, Fixture)
    {
        Ray3d ray(Vector3d(0.0, 0.0, 1.0), Vector3d(0.0, 0.0, -1.0));

        m_intersector.intersect(m_tree, ray, RayInfo3d(ray), m_leaf_visitor TRAVERSAL_STATISTICS);

        EXPECT_EQ(2, m_leaf_visitor.get_visited_leaf_count());
        EXPECT_FEQ(1.0 - 0.7, m_leaf_visitor.get_closest_hit());
    }

    TEST_CASE_F(Intersect_GivenRayPiercingLeftNode_VisitsLeftNode, Fixture)
    {
        Ray3d ray(Vector3d(-0.5, 0.0, 1.0), Vector3d(0.0, 0.0, -1.0));

        m_intersector.intersect(m_tree, ray, RayInfo3d(ray), m_leaf_visitor TRAVERSAL_STATISTICS);

        EXPECT_EQ(1, m_leaf_visitor.get_visited_leaf_count());
        EXPECT_FEQ(1.0 - 0.2, m_leaf_visitor.get_closest_hit());
    }

    TEST_CASE_F(Intersect_GivenRayPiercingRightNode_VisitsRightNode, Fixture)
    {
        Ray3d ray(Vector3d(0.5, 0.0, 1.0), Vector3d(0.0, 0.0, -1.0));

        m_intersector.intersect(m_tree, ray, RayInfo3d(ray), m_leaf_visitor TRAVERSAL_STATISTICS);

        EXPECT_EQ(1, m_leaf_visitor.get_visited_leaf_count());
        EXPECT_FEQ(1.0 - 0.7, m_leaf_visitor.get_closest_hit());
    }

#pragma warning (pop)

#undef TRAVERSAL_STATISTICS
}
