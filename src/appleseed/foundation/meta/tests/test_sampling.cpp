
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/math/rng.h"
#include "foundation/math/sampling.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Foundation_Math_Sampling_QMCSamplingContext)
{
    using namespace foundation;

    typedef MersenneTwister RNG;
    typedef QMCSamplingContext<RNG> QMCSamplingContext;
    typedef QMCSamplingContext::VectorType VectorType;

    FOUNDATION_TEST_CASE(InitialStateIsCorrect)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);

        FOUNDATION_EXPECT_EQ(0, context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(0, context.m_base_instance);
        FOUNDATION_EXPECT_EQ(2, context.m_dimension);
        FOUNDATION_EXPECT_EQ(7, context.m_instance);
        FOUNDATION_EXPECT_EQ(VectorType(0.0), context.m_offset);
    }

    FOUNDATION_TEST_CASE(TestAssignmentOperator)
    {
        RNG rng;
        QMCSamplingContext original_parent(rng, 2, 64, 7);
        QMCSamplingContext original = original_parent.split(3, 16);
        original.set_instance(6);

        QMCSamplingContext copy(rng, 5, 16, 9);
        copy = original;

        FOUNDATION_EXPECT_EQ(2, copy.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, copy.m_base_instance);
        FOUNDATION_EXPECT_EQ(3, copy.m_dimension);
        FOUNDATION_EXPECT_EQ(6, copy.m_instance);
    }

    FOUNDATION_TEST_CASE(TestSplitting)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);
        QMCSamplingContext child_context = context.split(3, 16);

        FOUNDATION_EXPECT_EQ(2, child_context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, child_context.m_base_instance);
        FOUNDATION_EXPECT_EQ(3, child_context.m_dimension);
        FOUNDATION_EXPECT_EQ(0, child_context.m_instance);
    }

    FOUNDATION_TEST_CASE(TestDoubleSplitting)
    {
        RNG rng;
        QMCSamplingContext context(rng, 2, 64, 7);
        QMCSamplingContext child_context = context.split(3, 16);
        QMCSamplingContext child_child_context = child_context.split(4, 8);

        FOUNDATION_EXPECT_EQ(5, child_child_context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, child_child_context.m_base_instance);
        FOUNDATION_EXPECT_EQ(4, child_child_context.m_dimension);
        FOUNDATION_EXPECT_EQ(0, child_child_context.m_instance);
    }
}

FOUNDATION_TEST_SUITE(Foundation_Math_Sampling_RQMCSamplingContext)
{
    using namespace foundation;

    typedef MersenneTwister RNG;
    typedef RQMCSamplingContext<RNG> RQMCSamplingContext;
    typedef RQMCSamplingContext::VectorType VectorType;

    FOUNDATION_TEST_CASE(InitialStateIsCorrect)
    {
        RNG rng;
        RQMCSamplingContext context(rng, 2, 64, 7);

        FOUNDATION_EXPECT_EQ(0, context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(0, context.m_base_instance);
        FOUNDATION_EXPECT_EQ(2, context.m_dimension);
        FOUNDATION_EXPECT_EQ(7, context.m_instance);
        FOUNDATION_EXPECT_EQ(VectorType(0.0), context.m_offset);
    }

    FOUNDATION_TEST_CASE(TestAssignmentOperator)
    {
        RNG rng;
        RQMCSamplingContext original_parent(rng, 2, 64, 7);
        RQMCSamplingContext original = original_parent.split(3, 16);
        original.set_instance(6);

        RQMCSamplingContext copy(rng, 5, 16, 9);
        copy = original;

        FOUNDATION_EXPECT_EQ(2, copy.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, copy.m_base_instance);
        FOUNDATION_EXPECT_EQ(3, copy.m_dimension);
        FOUNDATION_EXPECT_EQ(6, copy.m_instance);
    }

    FOUNDATION_TEST_CASE(TestSplitting)
    {
        RNG rng;
        RQMCSamplingContext context(rng, 2, 64, 7);
        RQMCSamplingContext child_context = context.split(3, 16);

        FOUNDATION_EXPECT_EQ(2, child_context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, child_context.m_base_instance);
        FOUNDATION_EXPECT_EQ(3, child_context.m_dimension);
        FOUNDATION_EXPECT_EQ(0, child_context.m_instance);
    }

    FOUNDATION_TEST_CASE(TestDoubleSplitting)
    {
        RNG rng;
        RQMCSamplingContext context(rng, 2, 64, 7);
        RQMCSamplingContext child_context = context.split(3, 16);
        RQMCSamplingContext child_child_context = child_context.split(4, 8);

        FOUNDATION_EXPECT_EQ(5, child_child_context.m_base_dimension);
        FOUNDATION_EXPECT_EQ(7, child_child_context.m_base_instance);
        FOUNDATION_EXPECT_EQ(4, child_child_context.m_dimension);
        FOUNDATION_EXPECT_EQ(0, child_child_context.m_instance);
    }
}
