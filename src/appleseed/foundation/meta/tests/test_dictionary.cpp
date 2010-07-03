
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
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

FOUNDATION_TEST_SUITE(Foundation_Utility_StringDictionary)
{
    FOUNDATION_TEST_CASE(Get_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryItemNotFound)
    {
        StringDictionary sd;

        FOUNDATION_EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const char* item = sd.get("key");
        });
    }

    FOUNDATION_TEST_CASE(GetAsInt_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryItemNotFound)
    {
        StringDictionary sd;

        FOUNDATION_EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const int item = sd.get<int>("key");
        });
    }

    FOUNDATION_TEST_CASE(GetAsInt_GivenStdStringKeyOfNonExistingItem_ThrowsExceptionDictionaryItemNotFound)
    {
        StringDictionary sd;

        FOUNDATION_EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const int item = sd.get<int>(string("key"));
        });
    }

    FOUNDATION_TEST_CASE(Remove_GivenCStringKeyOfExistingItem_RemovesItem)
    {
        StringDictionary sd;
        sd.insert("key", "value");

        sd.remove("key");

        FOUNDATION_EXPECT_FALSE(sd.exist("key"));
    }

    FOUNDATION_TEST_CASE(Remove_GivenStdStringKeyOfExistingItem_RemovesItem)
    {
        StringDictionary sd;
        sd.insert("key", "value");

        sd.remove(string("key"));

        FOUNDATION_EXPECT_FALSE(sd.exist("key"));
    }

    FOUNDATION_TEST_CASE(Remove_GivenCStringKeyOfNonExistingItem_DoesNothing)
    {
        StringDictionary sd;

        sd.remove("key");
    }
}

FOUNDATION_TEST_SUITE(Foundation_Utility_DictionaryDictionary)
{
    FOUNDATION_TEST_CASE(Remove_GivenCStringKeyOfNonExistingItem_DoesNothing)
    {
        DictionaryDictionary dd;

        dd.remove("key");
    }
}

FOUNDATION_TEST_SUITE(Foundation_Utility_Dictionary)
{
    FOUNDATION_TEST_CASE(Constructor_ConstructsEmptyDictionary)
    {
        Dictionary dic;

        FOUNDATION_EXPECT_EQ(0, dic.size());
        FOUNDATION_EXPECT_TRUE(dic.empty());
    }

    FOUNDATION_TEST_CASE(CopyConstructor_GivenSourceDictionaryWithOneStringItem_CopiesStringItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        Dictionary copy(dic);

        FOUNDATION_EXPECT_EQ("value", copy.get<string>("key"));
    }

    FOUNDATION_TEST_CASE(CopyConstructor_GivenSourceDictionaryWithOneDictionaryItem_CopiesDictionaryItem)
    {
        Dictionary child;
        child.insert("key", "value");

        Dictionary dic;
        dic.insert("child", child);

        Dictionary copy(dic);

        FOUNDATION_EXPECT_EQ("value", copy.dictionary("child").get<string>("key"));
    }

    FOUNDATION_TEST_CASE(AssignmentOperator_GivenSourceDictionaryWithOneStringItem_CopiesStringItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        Dictionary other;
        other = dic;

        FOUNDATION_EXPECT_EQ("value", other.get<string>("key"));
    }

    FOUNDATION_TEST_CASE(AssignmentOperator_GivenSourceDictionaryWithOneDictionaryItem_CopiesDictionaryItem)
    {
        Dictionary child;
        child.insert("key", "value");

        Dictionary dic;
        dic.insert("child", child);

        Dictionary other;
        other = dic;

        FOUNDATION_EXPECT_EQ("value", other.dictionary("child").get<string>("key"));
    }

    FOUNDATION_TEST_CASE(Clear_GivenDictionaryWithOneStringItem_RemovesItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        dic.clear();

        FOUNDATION_EXPECT_EQ(0, dic.size());
        FOUNDATION_EXPECT_TRUE(dic.empty());
    }

    FOUNDATION_TEST_CASE(Clear_GivenDictionaryWithOneDictionaryItem_RemovesItem)
    {
        Dictionary dic;
        dic.insert("key", Dictionary());

        dic.clear();

        FOUNDATION_EXPECT_EQ(0, dic.size());
        FOUNDATION_EXPECT_TRUE(dic.empty());
    }

    FOUNDATION_TEST_CASE(Insert_GivenCStringKeyAndCStringValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", "value");

        FOUNDATION_EXPECT_EQ(1, dic.size());
        FOUNDATION_EXPECT_FALSE(dic.empty());
        FOUNDATION_EXPECT_EQ("value", dic.get<string>("key"));
    }

    FOUNDATION_TEST_CASE(Insert_GivenCStringKeyAndStdStringValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", string("value"));

        FOUNDATION_EXPECT_EQ(1, dic.size());
        FOUNDATION_EXPECT_FALSE(dic.empty());
        FOUNDATION_EXPECT_EQ("value", dic.get<string>("key"));
    }

    FOUNDATION_TEST_CASE(Insert_GivenCStringKeyAndIntegerValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", 42);

        FOUNDATION_EXPECT_EQ(1, dic.size());
        FOUNDATION_EXPECT_FALSE(dic.empty());
        FOUNDATION_EXPECT_EQ(42, dic.get<int>("key"));
    }

    FOUNDATION_TEST_CASE(Insert_GivenStdStringKeyAndCStringValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert(string("key"), "value");

        FOUNDATION_EXPECT_EQ(1, dic.size());
        FOUNDATION_EXPECT_FALSE(dic.empty());
        FOUNDATION_EXPECT_EQ("value", dic.get<string>("key"));
    }

    FOUNDATION_TEST_CASE(Insert_GivenStdStringKeyAndIntegerValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert(string("key"), 42);

        FOUNDATION_EXPECT_EQ(1, dic.size());
        FOUNDATION_EXPECT_FALSE(dic.empty());
        FOUNDATION_EXPECT_EQ(42, dic.get<int>("key"));
    }

    FOUNDATION_TEST_CASE(GetAsInt_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryItemNotFound)
    {
        Dictionary dic;

        FOUNDATION_EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const int item = dic.get<int>("key");
        });
    }

    FOUNDATION_TEST_CASE(GetAsInt_GivenStdStringKeyOfNonExistingItem_ThrowsExceptionDictionaryItemNotFound)
    {
        Dictionary dic;

        FOUNDATION_EXPECT_EXCEPTION(ExceptionDictionaryItemNotFound,
        {
            const int item = dic.get<int>(string("key"));
        });
    }
}
