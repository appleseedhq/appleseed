
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
#include "foundation/utility/containers/pagedarray.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

TEST_SUITE(Foundation_Utility_Containers_PagedArray)
{
    using namespace foundation;

    typedef PagedArray<int, 2> PagedArrayType;

    TEST_CASE(InitialStateIsCorrect)
    {
        PagedArrayType array;

        EXPECT_TRUE(array.empty());
        EXPECT_EQ(0, array.size());
    }

    struct FixturePagedArrayWithThreeElements
    {
        PagedArrayType array;
        
        FixturePagedArrayWithThreeElements()
        {
            array.push_back(10);
            array.push_back(20);
            array.push_back(30);
        }
    };

    TEST_CASE_WITH_FIXTURE(TestInsertion, FixturePagedArrayWithThreeElements)
    {
        EXPECT_FALSE(array.empty());
        EXPECT_EQ(3, array.size());
    }

    TEST_CASE(TestStabilityUponInsertion)
    {
        PagedArrayType array;
        
        const size_t Count = 5;
        int* address[Count];

        for (size_t i = 0; i < Count; ++i)
            address[i] = &array.push_back(static_cast<int>(i));

        for (size_t i = 0; i < Count; ++i)
            EXPECT_EQ(address[i], &array[i]);
    }

    TEST_CASE_WITH_FIXTURE(TestArraySubscripting, FixturePagedArrayWithThreeElements)
    {
        EXPECT_EQ(10, array[0]);
        EXPECT_EQ(20, array[1]);
        EXPECT_EQ(30, array[2]);
    }

    TEST_CASE_WITH_FIXTURE(TestExceptionCheckedAccessWithinBounds, FixturePagedArrayWithThreeElements)
    {
        EXPECT_EQ(10, array.at(0));
        EXPECT_EQ(20, array.at(1));
        EXPECT_EQ(30, array.at(2));
    }

    TEST_CASE_WITH_FIXTURE(TestExceptionCheckedAccessOutOfBounds, FixturePagedArrayWithThreeElements)
    {
        EXPECT_EXCEPTION(
            PagedArrayType::ExceptionOutOfRange,
            {
                array.at(3);
            }
        );
    }

    TEST_CASE_WITH_FIXTURE(TestResizeToSameSize, FixturePagedArrayWithThreeElements)
    {
        array.resize(3);

        EXPECT_EQ(3, array.size());
    }

    TEST_CASE_WITH_FIXTURE(TestResizeToLargerSize, FixturePagedArrayWithThreeElements)
    {
        array.resize(5, 555);

        EXPECT_EQ(5, array.size());
        EXPECT_EQ(555, array[3]);
        EXPECT_EQ(555, array[4]);
    }
}
