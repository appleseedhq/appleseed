
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

// appleseed.foundation headers.
#include "foundation/math/distance.h"
#include "foundation/math/rng.h"
#include "foundation/utility/test.h"

#if 0

namespace foundation
{

//
// foundation::square_distance(Point, AABB) unit test.
//

namespace
{
    template <typename T, size_t N>
    void test_square_distance_point_aabb(
        ITestListener*  listener,
        ITest*          test)
    {
        // Types.
        typedef T ValueType;
        typedef Vector<T, N> VectorType;
        typedef AABB<T, N> AABBType;

        // Create a bounding box.
        const AABBType bbox(VectorType(-1.0), VectorType(+1.0));

        // The point is inside the bounding box.
        FOUNDATION_CHECK_EQ(square_distance(bbox, VectorType(0.0)), ValueType(0.0));

        // The point is outside the bounding box.
        {
            VectorType p(0.0);
            p[0] = ValueType(4.0);
            p[1] = ValueType(3.0);
            FOUNDATION_CHECK_FEQ(square_distance(p, bbox), ValueType(13.0));
            FOUNDATION_CHECK_FEQ(square_distance(bbox, p), ValueType(13.0));
        }
        {
            VectorType p(0.0);
            p[0] = ValueType(-4.0);
            p[1] = ValueType(3.0);
            FOUNDATION_CHECK_FEQ(square_distance(p, bbox), ValueType(13.0));
            FOUNDATION_CHECK_FEQ(square_distance(bbox, p), ValueType(13.0));
        }
    }
}

FOUNDATION_IMPLEMENT_TEST(UnitTestSquareDistancePointAABB, UnitTest, "foundation::square_distance(Point, AABB)")
{
    test_square_distance_point_aabb<float,  2>(listener, test);
    test_square_distance_point_aabb<double, 2>(listener, test);
    test_square_distance_point_aabb<float,  3>(listener, test);
    test_square_distance_point_aabb<double, 3>(listener, test);
}


//
// foundation::square_distance(Point, AABB) timing test.
//

namespace
{
    // Number of measurement runs.
    const size_t Runs = 1000;

    // Number of iterations per measurement run.
    const size_t Iterations = 100;

    template <typename T>
    class SqDistPointAABBTask
      : public ITimeableTask
    {
      public:
        SqDistPointAABBTask()
        {
            MersenneTwister rng;
            for (size_t i = 0; i < Iterations; ++i)
            {
                m_points[i][0] = static_cast<T>(rand_double1(rng, -1.0, 1.0));
                m_points[i][1] = static_cast<T>(rand_double1(rng, -1.0, 1.0));
                m_points[i][2] = static_cast<T>(rand_double1(rng, -1.0, 1.0));

                m_bboxes[i][0][0] = static_cast<T>(rand_double1(rng, -0.9, -0.1));
                m_bboxes[i][0][1] = static_cast<T>(rand_double1(rng, -0.9, -0.1));
                m_bboxes[i][0][2] = static_cast<T>(rand_double1(rng, -0.9, -0.1));
                m_bboxes[i][1][0] = static_cast<T>(rand_double1(rng,  0.1,  0.9));
                m_bboxes[i][1][1] = static_cast<T>(rand_double1(rng,  0.1,  0.9));
                m_bboxes[i][1][2] = static_cast<T>(rand_double1(rng,  0.1,  0.9));
                assert(m_bboxes[i].rank() == 3);
            }
        }

        virtual void run(bool do_run)
        {
            if (do_run)
            {
                m_dist = T(0.0);
                for (size_t i = 0; i < Iterations; ++i)
                {
                    m_dist += square_distance(m_points[i], m_bboxes[i]);
                }
            }
        }

      private:
        Vector<T, 3>    m_points[Iterations];
        AABB<T, 3>      m_bboxes[Iterations];
        T               m_dist;
    };
}

FOUNDATION_IMPLEMENT_TEST(TimingTestSquareDistancePointAABB, TimingTest, "foundation::square_distance(Point, AABB)")
{
    TaskTimer<> task_timer;
    
    {
        FOUNDATION_COMMENT("foundation::square_distance<float>(Point, AABB):");
        SqDistPointAABBTask<float> task;
        FOUNDATION_TIMING(task_timer.run(task, Runs) / Iterations);
    }
    {
        FOUNDATION_COMMENT("foundation::square_distance<double>(Point, AABB):");
        SqDistPointAABBTask<double> task;
        FOUNDATION_TIMING(task_timer.run(task, Runs) / Iterations);
    }
}

}   // namespace foundation

#endif
