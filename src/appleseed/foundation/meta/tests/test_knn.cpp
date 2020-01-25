
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
#include "foundation/math/distance.h"
#include "foundation/math/knn.h"
#include "foundation/math/permutation.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <vector>

using namespace foundation;

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
        builder.build<DefaultWallclockTimer>(nullptr, 0);

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

    TEST_CASE(Build_GivenTwoCoincidentPoints_Terminates)
    {
        const size_t PointCount = 2;
        const Vector3d points[PointCount] = { Vector3d(0.0), Vector3d(0.0) };

        knn::Tree3d tree;

        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(points, PointCount);
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
            EXPECT_LT(answer.get(i).m_square_dist, answer.get(left).m_square_dist);
            EXPECT_LT(answer.get(i).m_square_dist, answer.get(right).m_square_dist);
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

    TEST_CASE(Run_GivenEightPointsAndMaxSearchDistance_ReturnsFourNearestNeighbors)
    {
        const size_t PointCount = 8;
        const size_t AnswerSize = PointCount;
        const double QueryMaxSquareDistance = square(0.5);

        Vector3d points[PointCount];
        for (size_t i = 0; i < PointCount; ++i)
            points[i] = Vector3d(static_cast<double>(PointCount - i), 0.0, 0.0);

        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(points, PointCount);

        knn::Answer<double> answer(AnswerSize);
        knn::Query3d query(tree, answer);
        query.run(Vector3d(4.75, 0.0, 0.0), QueryMaxSquareDistance);

        EXPECT_EQ(1, answer.size());
    }

    void generate_random_points(
        MersenneTwister&            rng,
        std::vector<Vector3d>&      points,
        const size_t                count)
    {
        assert(points.empty());

        points.reserve(count);

        for (size_t i = 0; i < count; ++i)
            points.push_back(rand_vector1<Vector3d>(rng));
    }

    TEST_CASE(Run_GivenMaxSearchDistance_ReturnsCorrectResults)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 1000;
        const size_t AnswerSize = 100;
        const double QueryMaxSquareDistance = square(0.1);

        MersenneTwister rng;

        std::vector<Vector3d> points;
        generate_random_points(rng, points, PointCount);

        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(&points[0], PointCount);

        knn::Answer<double> full_answer(AnswerSize);
        knn::Query3d full_query(tree, full_answer);

        knn::Answer<double> limited_answer(AnswerSize);
        knn::Query3d limited_query(tree, limited_answer);

        for (size_t i = 0; i < QueryCount; ++i)
        {
            const Vector3d q = rand_vector1<Vector3d>(rng);

            full_query.run(q);
            full_answer.sort();

            limited_query.run(q, QueryMaxSquareDistance);
            limited_answer.sort();

            for (size_t j = 0; j < AnswerSize; ++j)
            {
                if (full_answer.get(j).m_square_dist > QueryMaxSquareDistance)
                {
                    EXPECT_EQ(j, limited_answer.size());
                    break;
                }

                EXPECT_EQ(full_answer.get(j).m_index, limited_answer.get(j).m_index);
                EXPECT_EQ(full_answer.get(j).m_square_dist, limited_answer.get(j).m_square_dist);
            }
        }
    }

    struct SortPointByDistancePredicate
    {
        const std::vector<Vector3d>&     m_points;
        const Vector3d&                  m_q;

        SortPointByDistancePredicate(
            const std::vector<Vector3d>& points,
            const Vector3d&              q)
          : m_points(points)
          , m_q(q)
        {
        }

        bool operator()(const size_t lhs, const size_t rhs) const
        {
            return
                square_distance(m_q, m_points[lhs]) <
                square_distance(m_q, m_points[rhs]);
        }
    };

    void naive_query(
        const std::vector<Vector3d>&     points,
        const Vector3d&                  q,
        std::vector<size_t>&             indices)
    {
        assert(indices.size() == points.size());

        SortPointByDistancePredicate pred(points, q);
        std::sort(indices.begin(), indices.end(), pred);
    }

    bool do_results_match_naive_algorithm(
        const std::vector<Vector3d>&     points,
        const size_t                     answer_size,
        const size_t                     query_count,
        std::function<Vector3d()>        make_query_point)
    {
        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(&points[0], points.size());

        knn::Answer<double> answer(answer_size);
        knn::Query3d query(tree, answer);

        std::vector<size_t> ref_answer(points.size());
        identity_permutation(ref_answer.size(), &ref_answer[0]);

        for (size_t i = 0; i < query_count; ++i)
        {
            const Vector3d q = make_query_point();

            naive_query(points, q, ref_answer);

            query.run(q);
            answer.sort();

            if (answer.size() != answer_size)
                return false;

            for (size_t j = 0; j < answer_size; ++j)
            {
                if (tree.remap(answer.get(j).m_index) != ref_answer[j])
                    return false;
            }
        }

        return true;
    }

    TEST_CASE(Run_UniformPointDistribution_ReturnsIdenticalResultsAsNaiveAlgorithm)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 200;
        const size_t AnswerSize = 20;

        MersenneTwister rng;

        std::vector<Vector3d> points;
        generate_random_points(rng, points, PointCount);

        auto make_query_point = [&rng]() { return rand_vector1<Vector3d>(rng); };
        EXPECT_TRUE(do_results_match_naive_algorithm(points, AnswerSize, QueryCount, make_query_point));
    }

    TEST_CASE(Run_SkewedPointDistribution_ReturnsIdenticalResultsAsNaiveAlgorithm)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 200;
        const size_t AnswerSize = 20;

        MersenneTwister rng;

        std::vector<Vector3d> points;
        generate_random_points(rng, points, PointCount);

        points[0] = Vector3d(0.0);

        for (size_t i = 1; i < points.size(); ++i)
            points[i] = Vector3d(0.55) + 0.45 * points[i];

        auto make_query_point = [&rng]() { return rand_vector1<Vector3d>(rng); };
        EXPECT_TRUE(do_results_match_naive_algorithm(points, AnswerSize, QueryCount, make_query_point));
    }

    TEST_CASE(Run_QueryPointsArePointsFromTheDataSet_ReturnsIdenticalResultsAsNaiveAlgorithm)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 200;
        const size_t AnswerSize = 20;

        MersenneTwister rng;

        std::vector<Vector3d> points;
        generate_random_points(rng, points, PointCount);

        auto make_query_point = [&rng, &points]() { return points[rand_int1(rng, 0, static_cast<std::int32_t>(points.size()) - 1)]; };
        EXPECT_TRUE(do_results_match_naive_algorithm(points, AnswerSize, QueryCount, make_query_point));
    }
}

TEST_SUITE(Foundation_Math_Knn_AnyQuery)
{
    void generate_random_points(
        MersenneTwister&            rng,
        std::vector<Vector3d>&      points,
        const size_t                count)
    {
        assert(points.empty());

        points.reserve(count);

        for (size_t i = 0; i < count; ++i)
            points.push_back(rand_vector1<Vector3d>(rng));
    }

    TEST_CASE(Run_UniformPointDistribution_ReturnsIdenticalResultAsNaiveAlgorithm)
    {
        const size_t PointCount = 1000;
        const size_t QueryCount = 1000;
        const double QueryMaxSquareDistance = square(0.06);  // tuned such that about 50% of the queries result in a neighbor being found

        MersenneTwister rng;

        std::vector<Vector3d> points;
        generate_random_points(rng, points, PointCount);

        knn::Tree3d tree;
        knn::Builder3d builder(tree);
        builder.build<DefaultWallclockTimer>(&points[0], points.size());

        knn::AnyQuery3d query(tree);

        for (size_t i = 0; i < QueryCount; ++i)
        {
            const Vector3d q = rand_vector1<Vector3d>(rng);

            const bool result = query.run(q, QueryMaxSquareDistance);

            const bool ref_result =
                std::any_of(std::begin(points), std::end(points),
                    [&](const Vector3d& p) -> bool { return square_distance(p, q) <= QueryMaxSquareDistance; });

            EXPECT_EQ(ref_result, result);
        }
    }
}
