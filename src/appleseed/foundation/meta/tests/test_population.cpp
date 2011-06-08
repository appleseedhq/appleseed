
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
    TEST_CASE(TestEmptyPopulation)
    {
        Population<int> pop;
        EXPECT_EQ(0, pop.get_size());
        EXPECT_EQ(0, pop.get_min());
        EXPECT_EQ(0, pop.get_max());
        EXPECT_EQ(0.0, pop.get_avg());
        EXPECT_EQ(0.0, pop.get_dev());
    }

    TEST_CASE(TestNonEmptyPopulation)
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
        EXPECT_EQ(5.0, pop.get_avg());
        EXPECT_EQ(2.0, pop.get_dev());
    }
}
