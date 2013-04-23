
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

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(Renderer_Global_ParamArray)
{
    TEST_CASE(InsertPath_GivenItemName_InsertsItem)
    {
        ParamArray params;
        params.insert_path("x", 42);

        const char* value = params.strings().get("x");

        EXPECT_EQ("42", string(value));
    }

    TEST_CASE(InsertPath_GivenParentNameAndItemName_InsertsItem)
    {
        ParamArray params;
        params.insert_path("parent.x", 42);

        const char* value = params.dictionaries().get("parent").strings().get("x");

        EXPECT_EQ("42", string(value));
    }

    TEST_CASE(ExistPath_GivenNameOfExistingItem_ReturnsTrue)
    {
        ParamArray params;
        params.insert_path("a", 42);

        EXPECT_TRUE(params.exist_path("a"));
    }

    TEST_CASE(ExistPath_GivenNameOfNonExistingItem_ReturnsFalse)
    {
        ParamArray params;
        params.insert_path("a", 42);

        EXPECT_FALSE(params.exist_path("b"));
    }

    TEST_CASE(ExistPath_GivenPathToExistingItem_ReturnsTrue)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        EXPECT_TRUE(params.exist_path("a.b.c"));
    }

    TEST_CASE(ExistPath_GivenPath1ToNonExistingItem_ReturnsFalse)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        EXPECT_FALSE(params.exist_path("a.b.x"));
    }

    TEST_CASE(ExistPath_GivenPath2ToNonExistingItem_ReturnsFalse)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        EXPECT_FALSE(params.exist_path("a.x.c"));
    }

    TEST_CASE(GetPath_GivenItemName_ReturnsItemValue)
    {
        ParamArray params;
        params.insert("x", 42);

        const char* value = params.get_path("x");

        EXPECT_EQ("42", string(value));
    }

    TEST_CASE(GetPath_GivenParentNameAndItemName_ReturnsItemValue)
    {
        ParamArray params;
        params.push("parent").insert("x", 42);

        const char* value = params.get_path("parent.x");

        EXPECT_EQ("42", string(value));
    }

    TEST_CASE(GetPath_GivenInvalidItemName_ThrowsExceptionDictionaryItemNotFound)
    {
        ParamArray params;
        params.insert("x", 42);

        EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const char* value = params.get_path("y");
        });
    }

    TEST_CASE(GetPath_GivenInvalidParentName_ThrowsExceptionDictionaryItemNotFound)
    {
        ParamArray params;
        params.push("parent").insert("x", 42);

        EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const char* value = params.get_path("other.x");
        });
    }

    TEST_CASE(RemovePath_GivenNameOfExistingItem_RemovesItem)
    {
        ParamArray params;
        params.insert("a", 42);

        params.remove_path("a");

        EXPECT_FALSE(params.exist_path("a"));
    }

    TEST_CASE(RemovePath_GivenNameOfNonExistingItem_DoesNothing)
    {
        ParamArray params;
        params.insert("a", 42);

        params.remove_path("b");

        EXPECT_TRUE(params.exist_path("a"));
    }

    TEST_CASE(RemovePath_GivenPathToExistingItem_RemovesItemButKeepsParents)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        params.remove_path("a.b.c");

        EXPECT_FALSE(params.exist_path("a.b.c"));
        EXPECT_TRUE(params.dictionaries().exist("a"));
        EXPECT_TRUE(params.dictionaries().get("a").dictionaries().exist("b"));
    }

    TEST_CASE(RemovePath_GivenPath1ToNonExistingItem_DoesNothing)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        params.remove_path("a.b.x");

        EXPECT_TRUE(params.exist_path("a.b.c"));
    }

    TEST_CASE(RemovePath_GivenPath2ToNonExistingItem_DoesNothing)
    {
        ParamArray params;
        params.insert_path("a.b.c", 42);

        params.remove_path("a.x.c");

        EXPECT_TRUE(params.exist_path("a.b.c"));
    }

    TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithDifferentNames_InsertsDestIntIntoSource)
    {
        ParamArray dst;
        dst.insert("A", 1);

        ParamArray src;
        src.insert("B", 2);

        dst.merge(src);

        EXPECT_EQ(2, dst.size());
        EXPECT_EQ(1, dst.get<int>("A"));
        EXPECT_EQ(2, dst.get<int>("B"));
    }

    TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithSameNames_OverwritesDestValueWithSourceValue)
    {
        ParamArray dst;
        dst.insert("A", 1);

        ParamArray src;
        src.insert("A", 2);

        dst.merge(src);

        EXPECT_EQ(1, dst.size());
        EXPECT_EQ(2, dst.get<int>("A"));
    }

    TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithDifferentNames_MergesDestDicIntoSource)
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

        EXPECT_EQ(2, dst.size());
        EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        EXPECT_EQ(2, dst.dictionary("B").get<int>("BB"));
    }

    TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithSameNames_MergesDicContents)
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

        EXPECT_EQ(1, dst.size());
        EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        EXPECT_EQ(2, dst.dictionary("A").get<int>("BB"));
    }

    TEST_CASE(TestConstructionOfDictionaryFromParamArray)
    {
        ParamArray params;
        params.insert("x", 42);

        const Dictionary dic = params;

        ASSERT_EQ(1, dic.strings().size());
        EXPECT_EQ(42, dic.get<int>("x"));
    }
}
