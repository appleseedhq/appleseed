
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
#include "foundation/containers/dictionary.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_StringDictionary)
{
    TEST_CASE(EqualityOperator_GivenTwoEmptyDictionaries_ReturnsTrue)
    {
        StringDictionary sd1;
        StringDictionary sd2;

        EXPECT_TRUE(sd1 == sd2);
        EXPECT_FALSE(sd1 != sd2);
    }

    TEST_CASE(EqualityOperator_GivenDictionariesOfDifferentSize_ReturnsFalse)
    {
        StringDictionary sd1;
        sd1.insert("key", "value");

        StringDictionary sd2;

        EXPECT_FALSE(sd1 == sd2);
        EXPECT_TRUE(sd1 != sd2);
    }

    TEST_CASE(EqualityOperator_GivenDictionariesWithSingleItemWithSameKeyButDifferentValues_ReturnsFalse)
    {
        StringDictionary sd1;
        sd1.insert("key", "value1");

        StringDictionary sd2;
        sd2.insert("key", "value2");

        EXPECT_FALSE(sd1 == sd2);
        EXPECT_TRUE(sd1 != sd2);
    }

    TEST_CASE(EqualityOperator_GivenDictionariesWithSingleItemWithDifferentKeysButSameValue_ReturnsFalse)
    {
        StringDictionary sd1;
        sd1.insert("key1", "value");

        StringDictionary sd2;
        sd2.insert("key2", "value");

        EXPECT_FALSE(sd1 == sd2);
        EXPECT_TRUE(sd1 != sd2);
    }

    TEST_CASE(EqualityOperator_GivenDictionariesWithSingleIdenticalItem_ReturnsTrue)
    {
        StringDictionary sd1;
        sd1.insert("key", "value");

        StringDictionary sd2;
        sd2.insert("key", "value");

        EXPECT_TRUE(sd1 == sd2);
        EXPECT_FALSE(sd1 != sd2);
    }

    TEST_CASE(Insert_ReturnsThisPointer)
    {
        StringDictionary sd;

        const StringDictionary* result = &sd.insert("key", "value");

        EXPECT_EQ(&sd, result);
    }

    TEST_CASE(Get_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryKeyNotFound)
    {
        StringDictionary sd;

        EXPECT_EXCEPTION(ExceptionDictionaryKeyNotFound,
        {
            APPLESEED_UNUSED const char* item = sd.get("key");
        });
    }

    TEST_CASE(GetAsInt_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryKeyNotFound)
    {
        StringDictionary sd;

        EXPECT_EXCEPTION(ExceptionDictionaryKeyNotFound,
        {
            APPLESEED_UNUSED const int item = sd.get<int>("key");
        });
    }

    TEST_CASE(Remove_GivenCStringKeyOfExistingItem_RemovesItem)
    {
        StringDictionary sd;
        sd.insert("key", "value");

        sd.remove("key");

        EXPECT_FALSE(sd.exist("key"));
    }

    TEST_CASE(Remove_GivenCStringKeyOfNonExistingItem_DoesNothing)
    {
        StringDictionary sd;

        sd.remove("key");
    }

    TEST_CASE(Remove_ReturnsThisPointer)
    {
        StringDictionary sd;

        const StringDictionary* result = &sd.remove("key");

        EXPECT_EQ(&sd, result);
    }
}

TEST_SUITE(Foundation_Utility_DictionaryDictionary)
{
    TEST_CASE(Insert_ReturnsThisPointer)
    {
        DictionaryDictionary dd;

        const DictionaryDictionary* result = &dd.insert("key", Dictionary());

        EXPECT_EQ(&dd, result);
    }

    TEST_CASE(Remove_GivenCStringKeyOfNonExistingItem_DoesNothing)
    {
        DictionaryDictionary dd;

        dd.remove("key");
    }

    TEST_CASE(Remove_ReturnsThisPointer)
    {
        DictionaryDictionary dd;

        const DictionaryDictionary* result = &dd.remove("key");

        EXPECT_EQ(&dd, result);
    }
}

TEST_SUITE(Foundation_Utility_Dictionary)
{
    TEST_CASE(Constructor_ConstructsEmptyDictionary)
    {
        Dictionary dic;

        EXPECT_EQ(0, dic.size());
        EXPECT_TRUE(dic.empty());
    }

    TEST_CASE(CopyConstructor_GivenSourceDictionaryWithOneStringItem_CopiesStringItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        Dictionary copy(dic);

        EXPECT_EQ("value", copy.get<std::string>("key"));
    }

    TEST_CASE(CopyConstructor_GivenSourceDictionaryWithOneDictionaryItem_CopiesDictionaryItem)
    {
        Dictionary child;
        child.insert("key", "value");

        Dictionary dic;
        dic.insert("child", child);

        Dictionary copy(dic);

        EXPECT_EQ("value", copy.dictionary("child").get<std::string>("key"));
    }

    TEST_CASE(AssignmentOperator_GivenSourceDictionaryWithOneStringItem_CopiesStringItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        Dictionary other;
        other = dic;

        EXPECT_EQ("value", other.get<std::string>("key"));
    }

    TEST_CASE(AssignmentOperator_GivenSourceDictionaryWithOneDictionaryItem_CopiesDictionaryItem)
    {
        Dictionary child;
        child.insert("key", "value");

        Dictionary dic;
        dic.insert("child", child);

        Dictionary other;
        other = dic;

        EXPECT_EQ("value", other.dictionary("child").get<std::string>("key"));
    }

    TEST_CASE(Clear_GivenDictionaryWithOneStringItem_RemovesItem)
    {
        Dictionary dic;
        dic.insert("key", "value");

        dic.clear();

        EXPECT_EQ(0, dic.size());
        EXPECT_TRUE(dic.empty());
    }

    TEST_CASE(Clear_GivenDictionaryWithOneDictionaryItem_RemovesItem)
    {
        Dictionary dic;
        dic.insert("key", Dictionary());

        dic.clear();

        EXPECT_EQ(0, dic.size());
        EXPECT_TRUE(dic.empty());
    }

    TEST_CASE(Insert_GivenCStringKeyAndCStringValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", "value");

        EXPECT_EQ(1, dic.size());
        EXPECT_FALSE(dic.empty());
        EXPECT_EQ("value", dic.get<std::string>("key"));
    }

    TEST_CASE(Insert_GivenCStringKeyAndStdStringValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", std::string("value"));

        EXPECT_EQ(1, dic.size());
        EXPECT_FALSE(dic.empty());
        EXPECT_EQ("value", dic.get<std::string>("key"));
    }

    TEST_CASE(Insert_GivenCStringKeyAndIntegerValue_InsertsValue)
    {
        Dictionary dic;

        dic.insert("key", 42);

        EXPECT_EQ(1, dic.size());
        EXPECT_FALSE(dic.empty());
        EXPECT_EQ(42, dic.get<int>("key"));
    }

    TEST_CASE(Insert_GivenDictionary_ReturnsThisPointer)
    {
        Dictionary dic;

        const Dictionary* result = &dic.insert("key", Dictionary());

        EXPECT_EQ(&dic, result);
    }

    TEST_CASE(Insert_GivenCString_ReturnsThisPointer)
    {
        Dictionary dic;

        const Dictionary* result = &dic.insert("key", "value");

        EXPECT_EQ(&dic, result);
    }

    TEST_CASE(GetAsInt_GivenCStringKeyOfNonExistingItem_ThrowsExceptionDictionaryKeyNotFound)
    {
        Dictionary dic;

        EXPECT_EXCEPTION(ExceptionDictionaryKeyNotFound,
        {
            APPLESEED_UNUSED const int item = dic.get<int>("key");
        });
    }

    TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithDifferentNames_InsertsDestIntIntoSource)
    {
        Dictionary dst;
        dst.insert("A", 1);

        Dictionary src;
        src.insert("B", 2);

        dst.merge(src);

        EXPECT_EQ(2, dst.size());
        EXPECT_EQ(1, dst.get<int>("A"));
        EXPECT_EQ(2, dst.get<int>("B"));
    }

    TEST_CASE(Merge_GivenOneIntInSourceAndOneIntInDestWithSameNames_OverwritesDestValueWithSourceValue)
    {
        Dictionary dst;
        dst.insert("A", 1);

        Dictionary src;
        src.insert("A", 2);

        dst.merge(src);

        EXPECT_EQ(1, dst.size());
        EXPECT_EQ(2, dst.get<int>("A"));
    }

    TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithDifferentNames_MergesDestDicIntoSource)
    {
        Dictionary dst_child;
        dst_child.insert("AA", 1);

        Dictionary dst;
        dst.dictionaries().insert("A", dst_child);

        Dictionary src_child;
        src_child.insert("BB", 2);

        Dictionary src;
        src.dictionaries().insert("B", src_child);

        dst.merge(src);

        EXPECT_EQ(2, dst.size());
        EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        EXPECT_EQ(2, dst.dictionary("B").get<int>("BB"));
    }

    TEST_CASE(Merge_GivenOneDicInSourceAndOneDicInDestWithSameNames_MergesDicContents)
    {
        Dictionary dst_child;
        dst_child.insert("AA", 1);

        Dictionary dst;
        dst.dictionaries().insert("A", dst_child);

        Dictionary src_child;
        src_child.insert("BB", 2);

        Dictionary src;
        src.dictionaries().insert("A", src_child);

        dst.merge(src);

        EXPECT_EQ(1, dst.size());
        EXPECT_EQ(1, dst.dictionary("A").get<int>("AA"));
        EXPECT_EQ(2, dst.dictionary("A").get<int>("BB"));
    }
}
