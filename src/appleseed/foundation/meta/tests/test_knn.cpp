
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

//
// Compiling the STANN library on 64-bit Windows generates the following warnings:
//
//   warning C4267: 'var' : conversion from 'size_t' to 'type', possible loss of data
//   warning C4800: 'type' : forcing value to bool 'true' or 'false' (performance warning)
//
// Since they are mostly harmless, we simply disable them for this file.
//

#pragma warning (push)
#pragma warning (disable : 4800)
#pragma warning (disable : 4267)

// appleseed.foundation headers.
#include "foundation/math/knn.h"
#include "foundation/math/rng.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// STANN headers.
#include "sfcnn.hpp"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Knn_Tree)
{
    TEST_CASE(Empty_GivenDefaultConstructedTree_ReturnsTrue)
    {
        knn::Tree3d tree;

        EXPECT_TRUE(tree.empty());
    }
}

TEST_SUITE(Foundation_Math_Knn_Builder)
{
    TEST_CASE(Build_GivenZeroPoint_BuildsEmptyTree)
    {
        knn::Tree3d tree;

        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(0, 0);

        EXPECT_TRUE(tree.empty());

        EXPECT_EQ(0, tree.m_points.size());
        EXPECT_EQ(0, tree.m_indices.size());

        ASSERT_EQ(1, tree.m_nodes.size());

        EXPECT_TRUE(tree.m_nodes[0].is_leaf());
        EXPECT_EQ(0, tree.m_nodes[0].get_point_count());
        EXPECT_EQ(0, tree.m_nodes[0].get_point_index());
    }

    TEST_CASE(Build_GivenTwoPoints_BuildsCorrectTree)
    {
        const Vector3d Points[] =
        {
            Vector3d(0.0, 0.0, 0.0),
            Vector3d(1.0, 0.0, 0.0)
        };

        knn::Tree3d tree;

        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(Points, 2);

        ASSERT_EQ(2, tree.m_points.size());
        EXPECT_EQ(Points[0], tree.m_points[0]);
        EXPECT_EQ(Points[1], tree.m_points[1]);

        ASSERT_EQ(2, tree.m_indices.size());
        EXPECT_EQ(0, tree.m_indices[0]);
        EXPECT_EQ(1, tree.m_indices[1]);

        ASSERT_EQ(3, tree.m_nodes.size());

        ASSERT_TRUE(tree.m_nodes[0].is_interior());
        EXPECT_EQ(2, tree.m_nodes[0].get_point_count());
        EXPECT_EQ(0, tree.m_nodes[0].get_point_index());

        ASSERT_TRUE(tree.m_nodes[1].is_leaf());
        EXPECT_EQ(1, tree.m_nodes[1].get_point_count());
        EXPECT_EQ(0, tree.m_nodes[1].get_point_index());

        ASSERT_TRUE(tree.m_nodes[2].is_leaf());
        EXPECT_EQ(1, tree.m_nodes[2].get_point_count());
        EXPECT_EQ(1, tree.m_nodes[2].get_point_index());
    }

    TEST_CASE(Build_GivenEightPoints_GeneratesFifteenNodes)
    {
        const size_t PointCount = 8;

        Vector3d points[PointCount];
        for (size_t i = 0; i < PointCount; ++i)
            points[i] = Vector3d(static_cast<double>(PointCount - i - 1), 0.0, 0.0);

        knn::Tree3d tree;

        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(points, PointCount);

        EXPECT_EQ(8 + 4 + 2 + 1, tree.m_nodes.size());
    }
}

TEST_SUITE(Foundation_Math_Knn_Answer)
{
    TEST_CASE(Size_AfterZeroInsertion_ReturnsZero)
    {
        knn::Answer<double> answer(3);

        EXPECT_EQ(0, answer.size());
    }

    TEST_CASE(Size_AfterOneInsertion_ReturnsOne)
    {
        knn::Answer<double> answer(3);
        answer.array_insert(42, 12.0);

        EXPECT_EQ(1, answer.size());
    }

    TEST_CASE(Empty_AfterZeroInsertion_ReturnsTrue)
    {
        knn::Answer<double> answer(3);

        EXPECT_TRUE(answer.empty());
    }

    TEST_CASE(Empty_AfterOneInsertion_ReturnsFalse)
    {
        knn::Answer<double> answer(3);
        answer.array_insert(42, 12.0);

        EXPECT_FALSE(answer.empty());
    }

    TEST_CASE(Clear_GivenOneItem_EmptiesAnswer)
    {
        knn::Answer<double> answer(3);
        answer.array_insert(42, 12.0);

        answer.clear();

        EXPECT_TRUE(answer.empty());
    }

    TEST_CASE(Sort_GivenFourItemsInSizeFiveAnswer_SortsItems)
    {
        knn::Answer<double> answer(5);
        answer.array_insert(4, 4.0);
        answer.array_insert(3, 3.0);
        answer.array_insert(1, 1.0);
        answer.array_insert(2, 2.0);

        answer.sort();

        EXPECT_EQ(1, answer.get(0).m_index);
        EXPECT_EQ(2, answer.get(1).m_index);
        EXPECT_EQ(3, answer.get(2).m_index);
        EXPECT_EQ(4, answer.get(3).m_index);
    }

    TEST_CASE(MakeHeap_GivenVector_TransformsVectorToHeap)
    {
        knn::Answer<double> answer(5);
        answer.array_insert(5, 5.0);
        answer.array_insert(1, 1.0);
        answer.array_insert(4, 4.0);
        answer.array_insert(3, 3.0);
        answer.array_insert(2, 2.0);

        answer.make_heap();

        for (size_t i = 0; i < answer.size() / 2; ++i)
        {
            const size_t left = 2 * i + 1;
            const size_t right = left + 1;
            EXPECT_LT(answer.get(i).m_distance, answer.get(left).m_distance);
            EXPECT_LT(answer.get(i).m_distance, answer.get(right).m_distance);
        }
    }

    TEST_CASE(Insert_GivenCloserItem_KeepsItem)
    {
        knn::Answer<double> answer(3);
        answer.array_insert(1, 1.0);
        answer.array_insert(3, 3.0);
        answer.array_insert(4, 4.0);
        answer.make_heap();

        answer.heap_insert(2, 2.0);

        answer.sort();
        EXPECT_EQ(1, answer.get(0).m_index);
        EXPECT_EQ(2, answer.get(1).m_index);
        EXPECT_EQ(3, answer.get(2).m_index);
    }
}

TEST_SUITE(Foundation_Math_Knn_Query)
{
    TEST_CASE(Run_GivenEightPointsAndQuerySizeFour_ReturnsFourNearestNeighbors)
    {
        const size_t PointCount = 8;
        const size_t AnswerSize = 4;

        Vector3d points[PointCount];
        for (size_t i = 0; i < PointCount; ++i)
            points[i] = Vector3d(static_cast<double>(PointCount - i), 0.0, 0.0);

        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(points, PointCount);

        knn::Answer<double> answer(AnswerSize);
        knn::Query3d query(tree, answer);
        query.run(Vector3d(4.5, 0.0, 0.0));

        EXPECT_EQ(4, answer.size());
    }

    TEST_CASE(Run_ReturnsSameResultsAsSTANN)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 1000;
        const size_t AnswerSize = 100;

        vector<Vector3d> points;
        points.reserve(PointCount);

        MersenneTwister rng;
        for (size_t i = 0; i < PointCount; ++i)
        {
            Vector3d p;
            p.x = rand_double1(rng);
            p.y = rand_double1(rng);
            p.z = rand_double1(rng);
            points.push_back(p);
        }

        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(&points[0], PointCount);

        sfcnn<foundation::Vector3d, 3, double> stann_tree(&points[0], PointCount);

        knn::Answer<double> answer(AnswerSize);
        knn::Query3d query(tree, answer);

        vector<long unsigned int> stann_answer(AnswerSize);
        vector<double> stann_distances(AnswerSize);

        for (size_t i = 0; i < QueryCount; ++i)
        {
            Vector3d q;
            q.x = rand_double1(rng);
            q.y = rand_double1(rng);
            q.z = rand_double1(rng);

            stann_tree.ksearch(q, AnswerSize, stann_answer, stann_distances);

            query.run(q);
            answer.sort();

            ASSERT_EQ(AnswerSize, answer.size());

            size_t our_answer[AnswerSize];
            for (size_t j = 0; j < AnswerSize; ++j)
                our_answer[j] = answer.get(j).m_index;

            EXPECT_SEQUENCE_EQ(AnswerSize, &stann_answer[0], &our_answer[0]);
        }
    }
}

#pragma warning (pop)
