
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

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

FOUNDATION_TEST_SUITE(Renderer_Global_ParamArray)
{
    using namespace renderer;

    FOUNDATION_TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithDifferentNames_InsertsDestIntIntoSource)
    {
        ParamArray dst;
        dst.insert("A", 1);

        ParamArray src;
        src.insert("B", 2);

        dst.merge(src);

        FOUNDATION_EXPECT_EQ(2, dst.size());
        FOUNDATION_EXPECT_EQ(1, dst.get<int>("A"));
        FOUNDATION_EXPECT_EQ(2, dst.get<int>("B"));
    }

    FOUNDATION_TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithSameNames_OverwritesDestValueWithSourceValue)
    {
        ParamArray dst;
        dst.insert("A", 1);

        ParamArray src;
        src.insert("A", 2);

        dst.merge(src);

        FOUNDATION_EXPECT_EQ(1, dst.size());
        FOUNDATION_EXPECT_EQ(2, dst.get<int>("A"));
    }

    FOUNDATION_TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithDifferentNames_MergesDestDicIntoSource)
    {
        ParamArray dst_child;
        dst_child.insert("AA", 1);

        ParamArray dst;
        dst.dictionaries().insert("A", dst_child);

        ParamArray src_child;
        src_child.insert("BB", 2);

        ParamArray src;
        src.dictionaries().insert("B", src_child);

        dst.merge(src);

        FOUNDATION_EXPECT_EQ(2, dst.size());
        FOUNDATION_EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        FOUNDATION_EXPECT_EQ(2, dst.dictionary("B").get<int>("BB"));
    }

    FOUNDATION_TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithSameNames_MergesDicContents)
    {
        ParamArray dst_child;
        dst_child.insert("AA", 1);

        ParamArray dst;
        dst.dictionaries().insert("A", dst_child);

        ParamArray src_child;
        src_child.insert("BB", 2);

        ParamArray src;
        src.dictionaries().insert("A", src_child);

        dst.merge(src);

        FOUNDATION_EXPECT_EQ(1, dst.size());
        FOUNDATION_EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        FOUNDATION_EXPECT_EQ(2, dst.dictionary("A").get<int>("BB"));
    }
}
