
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

// appleseed.renderer headers.
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Utility_ParamArray)
{
    TEST_CASE(InsertPath_GivenItemName_InsertsItem)
    {
        ParamArray params;
        params.insert_path("x", 42);

        const char* value = params.strings().get("x");

        EXPECT_EQ("42", std::string(value));
    }

    TEST_CASE(InsertPath_GivenParentNameAndItemName_InsertsItem)
    {
        ParamArray params;
        params.insert_path("parent.x", 42);

        const char* value = params.dictionaries().get("parent").strings().get("x");

        EXPECT_EQ("42", std::string(value));
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

        EXPECT_EQ("42", std::string(value));
    }

    TEST_CASE(GetPath_GivenParentNameAndItemName_ReturnsItemValue)
    {
        ParamArray params;
        params.push("parent").insert("x", 42);

        const char* value = params.get_path("parent.x");

        EXPECT_EQ("42", std::string(value));
    }

    TEST_CASE(GetPath_GivenInvalidItemName_ThrowsExceptionDictionaryKeyNotFound)
    {
        ParamArray params;
        params.insert("x", 42);

        EXPECT_EXCEPTION(ExceptionDictionaryKeyNotFound,
        {
            APPLESEED_UNUSED const char* value = params.get_path("y");
        });
    }

    TEST_CASE(GetPath_GivenInvalidParentName_ThrowsExceptionDictionaryKeyNotFound)
    {
        ParamArray params;
        params.push("parent").insert("x", 42);

        EXPECT_EXCEPTION(ExceptionDictionaryKeyNotFound,
        {
            APPLESEED_UNUSED const char* value = params.get_path("other.x");
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

    TEST_CASE(TestConstructionOfDictionaryFromParamArray)
    {
        ParamArray params;
        params.insert("x", 42);

        const Dictionary dic = params;

        ASSERT_EQ(1, dic.strings().size());
        EXPECT_EQ(42, dic.get<int>("x"));
    }

    TEST_CASE(GetOptional_GivenNonExistingParameter_IsNotOverlySlow)
    {
        const ParamArray params;

        for (size_t i = 0; i < 10000; ++i)
            params.get_optional<int>("x", 0);
    }
}
