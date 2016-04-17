
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/population.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Math_Population)
{
    TEST_CASE(EmptyPopulation)
    {
        Population<int> pop;

        EXPECT_EQ(0, pop.get_size());
        EXPECT_EQ(0, pop.get_min());
        EXPECT_EQ(0, pop.get_max());
        EXPECT_EQ(0.0, pop.get_mean());
        EXPECT_EQ(0.0, pop.get_dev());
        EXPECT_EQ(0.0, pop.get_var());
    }

    TEST_CASE(IntegerPopulationWithSingleZeroValue)
    {
        Population<int> pop;
        pop.insert(0);

        EXPECT_EQ(1, pop.get_size());
        EXPECT_EQ(0, pop.get_min());
        EXPECT_EQ(0, pop.get_max());
        EXPECT_EQ(0.0, pop.get_mean());
        EXPECT_EQ(0.0, pop.get_dev());
        EXPECT_EQ(0.0, pop.get_var());
    }

    TEST_CASE(FloatingPointPopulationWithSingleZeroValue)
    {
        Population<double> pop;
        pop.insert(0.0);

        EXPECT_EQ(1, pop.get_size());
        EXPECT_EQ(0.0, pop.get_min());
        EXPECT_EQ(0.0, pop.get_max());
        EXPECT_EQ(0.0, pop.get_mean());
        EXPECT_EQ(0.0, pop.get_dev());
        EXPECT_EQ(0.0, pop.get_var());
    }

    TEST_CASE(FloatingPointPopulationWithSingleNonZeroValue)
    {
        Population<double> pop;
        pop.insert(5.0);

        EXPECT_EQ(1, pop.get_size());
        EXPECT_EQ(5.0, pop.get_min());
        EXPECT_EQ(5.0, pop.get_max());
        EXPECT_EQ(5.0, pop.get_mean());
        EXPECT_EQ(0.0, pop.get_dev());
        EXPECT_EQ(0.0, pop.get_var());
    }

    TEST_CASE(NonEmptyIntegerPopulation)
    {
        Population<int> pop;
        pop.insert(2);
        pop.insert(4);
        pop.insert(4);
        pop.insert(4);
        pop.insert(5);
        pop.insert(5);
        pop.insert(7);
        pop.insert(9);

        EXPECT_EQ(8, pop.get_size());
        EXPECT_EQ(2, pop.get_min());
        EXPECT_EQ(9, pop.get_max());
        EXPECT_FEQ(5.0, pop.get_mean());
        EXPECT_FEQ(2.0, pop.get_dev());
        EXPECT_FEQ(0.4, pop.get_var());
    }

    TEST_CASE(MergeEmptyPopulationIntoEmptyPopulation)
    {
        Population<int> pop;

        Population<int> other_pop;
        pop.merge(other_pop);

        EXPECT_EQ(0, pop.get_size());
        EXPECT_EQ(0, pop.get_min());
        EXPECT_EQ(0, pop.get_max());
        EXPECT_EQ(0.0, pop.get_mean());
        EXPECT_EQ(0.0, pop.get_dev());
    }

    TEST_CASE(MergeEmptyPopulationIntoNonEmptyPopulation)
    {
        Population<int> pop;
        pop.insert(2);
        pop.insert(4);
        pop.insert(4);
        pop.insert(4);
        pop.insert(5);
        pop.insert(5);
        pop.insert(7);
        pop.insert(9);

        Population<int> other_pop;
        pop.merge(other_pop);

        EXPECT_EQ(8, pop.get_size());
        EXPECT_EQ(2, pop.get_min());
        EXPECT_EQ(9, pop.get_max());
        EXPECT_EQ(5.0, pop.get_mean());
        EXPECT_EQ(2.0, pop.get_dev());
    }

    TEST_CASE(MergeNonEmptyPopulationIntoEmptyPopulation)
    {
        Population<int> other_pop;
        other_pop.insert(2);
        other_pop.insert(4);
        other_pop.insert(4);
        other_pop.insert(4);
        other_pop.insert(5);
        other_pop.insert(5);
        other_pop.insert(7);
        other_pop.insert(9);

        Population<int> pop;
        pop.merge(other_pop);

        EXPECT_EQ(8, pop.get_size());
        EXPECT_EQ(2, pop.get_min());
        EXPECT_EQ(9, pop.get_max());
        EXPECT_EQ(5.0, pop.get_mean());
        EXPECT_EQ(2.0, pop.get_dev());
    }

    TEST_CASE(MergeNonPopulationIntoNonEmptyPopulation)
    {
        Population<int> pop;
        pop.insert(2);
        pop.insert(4);
        pop.insert(4);
        pop.insert(4);
        pop.insert(5);
        pop.insert(5);
        pop.insert(7);
        pop.insert(9);

        Population<int> other_pop;
        other_pop.insert(1);
        other_pop.insert(11);

        pop.merge(other_pop);

        Population<int> expected;
        expected.insert(2);
        expected.insert(4);
        expected.insert(4);
        expected.insert(4);
        expected.insert(5);
        expected.insert(5);
        expected.insert(7);
        expected.insert(9);
        expected.insert(1);
        expected.insert(11);

        EXPECT_EQ(expected.get_size(), pop.get_size());
        EXPECT_EQ(expected.get_min(), pop.get_min());
        EXPECT_EQ(expected.get_max(), pop.get_max());
        EXPECT_FEQ(expected.get_mean(), pop.get_mean());
        EXPECT_FEQ(expected.get_dev(), pop.get_dev());
    }
}
