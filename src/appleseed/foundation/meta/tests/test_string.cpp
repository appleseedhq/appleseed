
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
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/arch.h"
#include "foundation/string/string.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_String_String)
{
    TEST_CASE(ToString_GivenZeroAsInt_ReturnsCorrespondingString)
    {
        const int n = 0;
        EXPECT_EQ("0", to_string(n));
    }

    TEST_CASE(ToString_GivenBoolValues_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("true", to_string(true));
        EXPECT_EQ("false", to_string(false));
    }

    TEST_CASE(ToString_GivenInt8Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::int8_t>(0));
        EXPECT_EQ("42", to_string<std::int8_t>(42));
        EXPECT_EQ("-1", to_string<std::int8_t>(-1));
    }

    TEST_CASE(ToString_GivenInt16Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::int16_t>(0));
        EXPECT_EQ("42", to_string<std::int16_t>(42));
        EXPECT_EQ("-1", to_string<std::int16_t>(-1));
    }

    TEST_CASE(ToString_GivenInt32Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::int32_t>(0));
        EXPECT_EQ("42", to_string<std::int32_t>(42));
        EXPECT_EQ("-1", to_string<std::int32_t>(-1));
    }

    TEST_CASE(ToString_GivenInt64Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::int64_t>(0));
        EXPECT_EQ("42", to_string<std::int64_t>(42));
        EXPECT_EQ("-1", to_string<std::int64_t>(-1));
    }

    TEST_CASE(ToString_GivenUInt8Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::uint8_t>(0));
        EXPECT_EQ("42", to_string<std::uint8_t>(42));
    }

    TEST_CASE(ToString_GivenUInt16Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::uint16_t>(0));
        EXPECT_EQ("42", to_string<std::uint16_t>(42));
    }

    TEST_CASE(ToString_GivenUInt32Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::uint32_t>(0));
        EXPECT_EQ("42", to_string<std::uint32_t>(42));
    }

    TEST_CASE(ToString_GivenUInt64Values_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("0", to_string<std::uint64_t>(0));
        EXPECT_EQ("42", to_string<std::uint64_t>(42));
    }

#if defined APPLESEED_ARCH32
    #define ZERO_PTR            0x00000000
    #define ZERO_PTR_STR        "<null>"
    #define DEADBEEF_PTR        0xDEADBEEF
    #define DEADBEEF_PTR_STR    "0xDEADBEEF"
#elif defined APPLESEED_ARCH64
    #define ZERO_PTR            0x0000000000000000
    #define ZERO_PTR_STR        "<null>"
    #define DEADBEEF_PTR        0xDEADBEEFDEAFBABE
    #define DEADBEEF_PTR_STR    "0xDEADBEEFDEAFBABE"
#else
    #error Cannot determine machine architecture.
#endif

    TEST_CASE(ToString_GivenNullptrValues_ReturnsCorrespondingStrings)
    {
        EXPECT_EQ("<null>", to_string(nullptr));
    }

    TEST_CASE(ToString_GivenNonNullVoidPointer_ReturnsCorrespondingString)
    {
        void* ptr = reinterpret_cast<void*>(DEADBEEF_PTR);
        EXPECT_EQ(DEADBEEF_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNonNullConstVoidPointer_ReturnsCorrespondingString)
    {
        const void* ptr = reinterpret_cast<const void*>(DEADBEEF_PTR);
        EXPECT_EQ(DEADBEEF_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNullVoidPointer_ReturnsCorrespondingString)
    {
        void* ptr = ZERO_PTR;
        EXPECT_EQ(ZERO_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNullConstVoidPointer_ReturnsCorrespondingString)
    {
        const void* ptr = ZERO_PTR;
        EXPECT_EQ(ZERO_PTR_STR, to_string(ptr));
    }

    struct Foo { int dummy; };

    TEST_CASE(ToString_GivenNonNullClassPointer_ReturnsCorrespondingString)
    {
        Foo* ptr = reinterpret_cast<Foo*>(DEADBEEF_PTR);
        EXPECT_EQ(DEADBEEF_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNonNullConstClassPointer_ReturnsCorrespondingString)
    {
        const Foo* ptr = reinterpret_cast<const Foo*>(DEADBEEF_PTR);
        EXPECT_EQ(DEADBEEF_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNullClassPointer_ReturnsCorrespondingString)
    {
        Foo* ptr = ZERO_PTR;
        EXPECT_EQ(ZERO_PTR_STR, to_string(ptr));
    }

    TEST_CASE(ToString_GivenNullConstClassPointer_ReturnsCorrespondingString)
    {
        const Foo* ptr = ZERO_PTR;
        EXPECT_EQ(ZERO_PTR_STR, to_string(ptr));
    }

#undef ZERO_PTR
#undef ZERO_PTR_STR
#undef DEADBEEF_PTR
#undef DEADBEEF_PTR_STR

    TEST_CASE(ToString_GivenNonNullCString_ReturnsCorrespondingString)
    {
        char s[] = "bunny";
        EXPECT_EQ("bunny", to_string(s));
    }

    TEST_CASE(ToString_GivenNonNullConstCString_ReturnsCorrespondingString)
    {
        const char* s = "bunny";
        EXPECT_EQ("bunny", to_string(s));
    }

    TEST_CASE(ToString_GivenNullCString_ReturnsCorrespondingString)
    {
        char* s = 0;
        EXPECT_EQ("<null>", to_string(s));
    }

    TEST_CASE(ToString_GivenNullConstCString_ReturnsCorrespondingString)
    {
        const char* s = 0;
        EXPECT_EQ("<null>", to_string(s));
    }

    TEST_CASE(ToString_GivenEmptyArray_ReturnsEmptyString)
    {
        const int array[3] = { 1, 2, 3 };
        EXPECT_EQ("", to_string(array, 0));
    }

    TEST_CASE(ToString_GivenEmptyArrayWithCustomSeparator_ReturnsEmptyString)
    {
        const int array[3] = { 1, 2, 3 };
        EXPECT_EQ("", to_string(array, 0, ";"));
    }

    TEST_CASE(ToString_GivenNonEmptyArray_ReturnsCorrespondingString)
    {
        const int array[3] = { 1, 2, 3 };
        EXPECT_EQ("1 2 3", to_string(array, 3));
    }

    TEST_CASE(ToString_GivenNonEmptyArrayWithCustomSeparator_ReturnsCorrespondingString)
    {
        const int array[3] = { 1, 2, 3 };
        EXPECT_EQ("1;2;3", to_string(array, 3, ";"));
    }

    TEST_CASE(FromString_GivenEmptyString_ThrowsExceptionStringConversionError)
    {
        EXPECT_EXCEPTION(ExceptionStringConversionError,
        {
            from_string<int>("");
        });
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingInt8Values)
    {
        EXPECT_EQ(0, from_string<std::int8_t>("0"));
        EXPECT_EQ(42, from_string<std::int8_t>("42"));
        EXPECT_EQ(-1, from_string<std::int8_t>("-1"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingInt16Values)
    {
        EXPECT_EQ(0, from_string<std::int16_t>("0"));
        EXPECT_EQ(42, from_string<std::int16_t>("42"));
        EXPECT_EQ(-1, from_string<std::int16_t>("-1"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingInt32Values)
    {
        EXPECT_EQ(0, from_string<std::int32_t>("0"));
        EXPECT_EQ(42, from_string<std::int32_t>("42"));
        EXPECT_EQ(-1, from_string<std::int32_t>("-1"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingInt64Values)
    {
        EXPECT_EQ(0, from_string<std::int64_t>("0"));
        EXPECT_EQ(42, from_string<std::int64_t>("42"));
        EXPECT_EQ(-1, from_string<std::int64_t>("-1"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingUInt8Values)
    {
        EXPECT_EQ(0, from_string<std::uint8_t>("0"));
        EXPECT_EQ(42, from_string<std::uint8_t>("42"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingUInt16Values)
    {
        EXPECT_EQ(0, from_string<std::uint16_t>("0"));
        EXPECT_EQ(42, from_string<std::uint16_t>("42"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingUInt32Values)
    {
        EXPECT_EQ(0, from_string<std::uint32_t>("0"));
        EXPECT_EQ(42, from_string<std::uint32_t>("42"));
    }

    TEST_CASE(FromString_GivenStrings_ReturnsCorrespondingUInt64Values)
    {
        EXPECT_EQ(0, from_string<std::uint64_t>("0"));
        EXPECT_EQ(42, from_string<std::uint64_t>("42"));
    }

    TEST_CASE(FromString_GivenIntegerPrecededBySpace_IgnoresSpaceAndReturnsIntegerValue)
    {
        EXPECT_EQ(42, from_string<int>(" 42"));
    }

    TEST_CASE(FromString_GivenIntegerFollowedBySpace_ThrowsExceptionStringConversionError)
    {
        EXPECT_EXCEPTION(ExceptionStringConversionError,
        {
            from_string<int>("42 ");
        });
    }

    TEST_CASE(StrcmpNocase)
    {
        EXPECT_EQ( 0, strcmp_nocase("seal", "SEAL"));
        EXPECT_EQ( 0, strcmp_nocase("HeLLo", "hEllO"));
        EXPECT_EQ( 0, strcmp_nocase("", ""));

        EXPECT_EQ(-1, strcmp_nocase("a", "b"));
        EXPECT_EQ(-1, strcmp_nocase("A", "b"));
        EXPECT_EQ(-1, strcmp_nocase("a", "B"));
        EXPECT_EQ(-1, strcmp_nocase("A", "B"));
        EXPECT_EQ(-1, strcmp_nocase("an", "another"));
        EXPECT_EQ(-1, strcmp_nocase("", "hello"));

        EXPECT_EQ(+1, strcmp_nocase("b", "a"));
        EXPECT_EQ(+1, strcmp_nocase("B", "a"));
        EXPECT_EQ(+1, strcmp_nocase("b", "A"));
        EXPECT_EQ(+1, strcmp_nocase("B", "A"));
        EXPECT_EQ(+1, strcmp_nocase("another", "an"));
        EXPECT_EQ(+1, strcmp_nocase("hello", ""));
    }

    TEST_CASE(TrimLeftHandlesEmptyString)
    {
        EXPECT_EQ("", trim_left(""));
    }

    TEST_CASE(TrimLeftHandlesBlankStrings)
    {
        EXPECT_EQ("", trim_left(" "));
        EXPECT_EQ("", trim_left(" \t\n\v\f\r "));
    }

    TEST_CASE(TrimLeftHandlesRealStrings)
    {
        EXPECT_EQ("hello   ", trim_left("hello   "));
        EXPECT_EQ("hello", trim_left(" \t\n\v\f\r hello"));
        EXPECT_EQ("hello   ", trim_left(" \t\n\v\f\r hello   "));
    }

    TEST_CASE(TrimRightHandlesEmptyString)
    {
        EXPECT_EQ(trim_right(""), "");
    }

    TEST_CASE(TrimRightHandlesBlankStrings)
    {
        EXPECT_EQ(trim_right(" "), "");
        EXPECT_EQ(trim_right(" \t\n\v\f\r "), "");
    }

    TEST_CASE(TrimRightHandlesRealStrings)
    {
        EXPECT_EQ("   hello", trim_right("   hello"));
        EXPECT_EQ("hello", trim_right("hello \t\n\v\f\r "));
        EXPECT_EQ("   hello", trim_right("   hello \t\n\v\f\r "));
    }

    TEST_CASE(TrimBothHandlesEmptyString)
    {
        EXPECT_EQ("", trim_both(""));
    }

    TEST_CASE(TrimBothHandlesBlankStrings)
    {
        EXPECT_EQ("", trim_both(" "));
        EXPECT_EQ("", trim_both(" \t\n\v\f\r "));
    }

    TEST_CASE(TrimBothHandlesRealStrings)
    {
        EXPECT_EQ("hello", trim_both("hello \t\n\v\f\r "));
        EXPECT_EQ("hello", trim_both(" \t\n\v\f\r hello"));
        EXPECT_EQ("hello", trim_both(" \t\n\v\f\r hello \t\n\v\f\r "));
    }

    TEST_CASE(StartsWith)
    {
        EXPECT_FALSE(starts_with("", "h"));
        EXPECT_FALSE(starts_with("world", "he"));
        EXPECT_FALSE(starts_with("world hello", "he"));
        EXPECT_TRUE(starts_with("hello", "he"));
        EXPECT_TRUE(starts_with("hello", "hello"));
    }

    TEST_CASE(EndsWith)
    {
        EXPECT_FALSE(ends_with("", "ld"));
        EXPECT_FALSE(ends_with("hello", "ld"));
        EXPECT_FALSE(ends_with("world hello", "ld"));
        EXPECT_TRUE(ends_with("world", "ld"));
        EXPECT_TRUE(ends_with("world", "world"));
    }

    std::vector<std::string> tokenize_wrapper(
        const std::string&   s,
        const std::string&   delimiters)
    {
        std::vector<std::string> vec;
        tokenize(s, delimiters, vec);
        return vec;
    }

    TEST_CASE(TestTokenize)
    {
        EXPECT_EQ(std::vector<std::string>(), tokenize_wrapper("", "\n"));

        EXPECT_EQ(std::vector<std::string>(), tokenize_wrapper("\n", "\n"));
        EXPECT_EQ(std::vector<std::string>(), tokenize_wrapper("\n\n", "\n"));
        EXPECT_EQ(std::vector<std::string>(), tokenize_wrapper("\n\n\n", "\n"));

        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("hello", "\n"));
        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("hello\n", "\n"));
        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("hello\n\n", "\n"));
        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("\nhello", "\n"));
        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("\n\nhello", "\n"));
        EXPECT_EQ(make_vector("hello"), tokenize_wrapper("\n\nhello\n\n", "\n"));

        EXPECT_EQ(make_vector("hello", "world"), tokenize_wrapper("hello\nworld", "\n"));
        EXPECT_EQ(make_vector("hello", "world"), tokenize_wrapper("hello\n\nworld", "\n"));

        EXPECT_EQ(make_vector("hello", "world", "bunny"), tokenize_wrapper("helloXworld\nbunny", "X\n"));

        EXPECT_EQ(make_vector("1", "2", "3"), tokenize_wrapper("  [1, 2 ,3 ] ", ",[] "));
    }

    std::vector<std::string> split_wrapper(
        const std::string&   s,
        const std::string&   delimiters)
    {
        std::vector<std::string> vec;
        split(s, delimiters, vec);
        return vec;
    }

    TEST_CASE(TestSplit)
    {
        EXPECT_EQ(make_vector(""), split_wrapper("", "\n"));

        EXPECT_EQ(make_vector("", ""), split_wrapper("\n", "\n"));
        EXPECT_EQ(make_vector("", "", ""), split_wrapper("\n\n", "\n"));
        EXPECT_EQ(make_vector("", "", "", ""), split_wrapper("\n\n\n", "\n"));

        EXPECT_EQ(make_vector("hello"), split_wrapper("hello", "\n"));
        EXPECT_EQ(make_vector("hello", ""), split_wrapper("hello\n", "\n"));
        EXPECT_EQ(make_vector("hello", "", ""), split_wrapper("hello\n\n", "\n"));

        EXPECT_EQ(make_vector("", "hello"), split_wrapper("\nhello", "\n"));
        EXPECT_EQ(make_vector("", "hello", ""), split_wrapper("\nhello\n", "\n"));
        EXPECT_EQ(make_vector("", "hello", "", ""), split_wrapper("\nhello\n\n", "\n"));

        EXPECT_EQ(make_vector("", "", "hello"), split_wrapper("\n\nhello", "\n"));
        EXPECT_EQ(make_vector("", "", "hello", ""), split_wrapper("\n\nhello\n", "\n"));
        EXPECT_EQ(make_vector("", "", "hello", "", ""), split_wrapper("\n\nhello\n\n", "\n"));
    }

    TEST_CASE(Replace_GivenEmptyString_ReturnsEmptyString)
    {
        const std::string result = replace("", "aa", "bbb");

        EXPECT_EQ("", result);
    }

    TEST_CASE(Replace_GivenNonMatchingString_ReturnsInputString)
    {
        const std::string result = replace("xyz", "aa", "bbb");

        EXPECT_EQ("xyz", result);
    }

    TEST_CASE(Replace_GivenPartiallyMatchingString_ReturnsInputString)
    {
        const std::string result = replace("axyz", "aa", "bbb");

        EXPECT_EQ("axyz", result);
    }

    TEST_CASE(Replace_GivenStringWithSingleMatch_ReplacesMatch)
    {
        const std::string result = replace("xyaaz", "aa", "bbb");

        EXPECT_EQ("xybbbz", result);
    }

    TEST_CASE(Replace_GivenStringWithMultipleMatches_ReplacesMatches)
    {
        const std::string result = replace("xaayaaz", "aa", "bbb");

        EXPECT_EQ("xbbbybbbz", result);
    }

    TEST_CASE(Replace_GivenStringWithMatchAtTheBeginning_ReplacesMatch)
    {
        const std::string result = replace("aaxyz", "aa", "bbb");

        EXPECT_EQ("bbbxyz", result);
    }

    TEST_CASE(Replace_GivenStringWithMatchAtTheEnd_ReplacesMatch)
    {
        const std::string result = replace("xyzaa", "aa", "bbb");

        EXPECT_EQ("xyzbbb", result);
    }

    TEST_CASE(Format_GivenStringWithoutPlaceholders_ReturnsString)
    {
        const std::string result = format("hello", "world");

        EXPECT_EQ("hello", result);
    }

    TEST_CASE(Format_GivenStringWithSinglePlaceholder_ReturnsFormattedString)
    {
        const std::string result = format("hello {0}", "world");

        EXPECT_EQ("hello world", result);
    }

    TEST_CASE(Format_GivenStringWithSinglePlaceholderRepeatedTwice_ReturnsFormattedString)
    {
        const std::string result = format("{0} hello {0}", "yay");

        EXPECT_EQ("yay hello yay", result);
    }

    TEST_CASE(Format_GivenStringWithFourPlaceholders_ReturnsFormattedString)
    {
        const std::string result = format("{3} {2} {1} {0} {1} {2} {3}", "a", "b", "c", "d");

        EXPECT_EQ("d c b a b c d", result);
    }

    TEST_CASE(GetNumberedStringMaxValue_GivenEmptyPattern_ReturnsZero)
    {
        const size_t max_value = get_numbered_string_max_value("");

        EXPECT_EQ(0, max_value);
    }

    TEST_CASE(GetNumberedStringMaxValue_GivenPatternWithoutHashes_ReturnsZero)
    {
        const size_t max_value = get_numbered_string_max_value("hello");

        EXPECT_EQ(0, max_value);
    }

    TEST_CASE(GetNumberedStringMaxValue_GivenPatternWithThreeHashesAtTheBeginning_Returns999)
    {
        const size_t max_value = get_numbered_string_max_value("###hello");

        EXPECT_EQ(999, max_value);
    }

    TEST_CASE(GetNumberedStringMaxValue_GivenPatternWithThreeHashesInTheMiddle_Returns999)
    {
        const size_t max_value = get_numbered_string_max_value("he###llo");

        EXPECT_EQ(999, max_value);
    }

    TEST_CASE(GetNumberedStringMaxValue_GivenPatternWithThreeHashesAtTheEnd_Returns999)
    {
        const size_t max_value = get_numbered_string_max_value("hello###");

        EXPECT_EQ(999, max_value);
    }

    TEST_CASE(GetNumberedString_GivenEmptyPattern_ReturnsEmptyString)
    {
        const std::string result = get_numbered_string("", 12);

        EXPECT_EQ("", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithoutHashes_ReturnsPatternUnmodified)
    {
        const std::string result = get_numbered_string("hello", 12);

        EXPECT_EQ("hello", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithSingleHashAtTheBeginning_GivenSingleDigitValue_ReplacesHashByValue)
    {
        const std::string result = get_numbered_string("#hello", 5);

        EXPECT_EQ("5hello", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithSingleHashInTheMiddle_GivenSingleDigitValue_ReplacesHashByValue)
    {
        const std::string result = get_numbered_string("he#llo", 5);

        EXPECT_EQ("he5llo", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithSingleHashAtTheEnd_GivenSingleDigitValue_ReplacesHashByValue)
    {
        const std::string result = get_numbered_string("hello#", 5);

        EXPECT_EQ("hello5", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithThreeHashes_GivenSingleDigitValue_ReplacesHashesByValue)
    {
        const std::string result = get_numbered_string("hel###lo", 5);

        EXPECT_EQ("hel005lo", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithThreeHashes_GivenTwoDigitsValue_ReplacesHashesByValue)
    {
        const std::string result = get_numbered_string("hello###", 12);

        EXPECT_EQ("hello012", result);
    }

    TEST_CASE(GetNumberedString_GivenPatternWithThreeHashes_GivenFourDigitsValue_ReplacesHashesByValue)
    {
        const std::string result = get_numbered_string("hello###", 1234);

        EXPECT_EQ("hello1234", result);
    }

    TEST_CASE(ReplaceSpecialXMLCharacters_GivenEmptyString_ReturnsEmptyString)
    {
        const std::string result = replace_special_xml_characters("");

        EXPECT_EQ("", result);
    }

    TEST_CASE(ReplaceSpecialXMLCharacters_GivenStringWithAmpersand_ReplacesAmpersandByEntity)
    {
        const std::string result = replace_special_xml_characters("aa&bb");

        EXPECT_EQ("aa&amp;bb", result);
    }

    TEST_CASE(ReplaceSpecialXMLCharacters_GivenStringWithQuoteThenAmpersand_ReplacesQuoteAndAmpersandByEntities)
    {
        const std::string result = replace_special_xml_characters("aa\"&bb");

        EXPECT_EQ("aa&quot;&amp;bb", result);
    }

    TEST_CASE(ReplaceSpecialXMLCharacters_GivenStringWithXMLEntity_ReplacesAmpersandByEntity)
    {
        const std::string result = replace_special_xml_characters("aa&amp;bb");

        EXPECT_EQ("aa&amp;amp;bb", result);
    }

    bool fast_strtod_ok(const char* str)
    {
        const double ref = strtod(str, 0);
        const double val = fast_strtod(str, 0);

        return feq(ref, val, 1.0e-14);
    }

    TEST_CASE(FastStrtod)
    {
        EXPECT_TRUE(fast_strtod_ok("1"));
        EXPECT_TRUE(fast_strtod_ok("+1"));
        EXPECT_TRUE(fast_strtod_ok("-1"));

        EXPECT_TRUE(fast_strtod_ok("1."));
        EXPECT_TRUE(fast_strtod_ok("+1."));
        EXPECT_TRUE(fast_strtod_ok("-1."));

        EXPECT_TRUE(fast_strtod_ok(".1"));
        EXPECT_TRUE(fast_strtod_ok("+.1"));
        EXPECT_TRUE(fast_strtod_ok("-.1"));

        EXPECT_TRUE(fast_strtod_ok("1.0"));
        EXPECT_TRUE(fast_strtod_ok("+1.0"));
        EXPECT_TRUE(fast_strtod_ok("-1.0"));

        EXPECT_TRUE(fast_strtod_ok("1e8"));
        EXPECT_TRUE(fast_strtod_ok("1e-8"));
        EXPECT_TRUE(fast_strtod_ok("1e+8"));
        EXPECT_TRUE(fast_strtod_ok("1E8"));
        EXPECT_TRUE(fast_strtod_ok("1E-8"));
        EXPECT_TRUE(fast_strtod_ok("1E+8"));
        EXPECT_TRUE(fast_strtod_ok("+1e8"));
        EXPECT_TRUE(fast_strtod_ok("+1e-8"));
        EXPECT_TRUE(fast_strtod_ok("+1e+8"));
        EXPECT_TRUE(fast_strtod_ok("+1E8"));
        EXPECT_TRUE(fast_strtod_ok("+1E-8"));
        EXPECT_TRUE(fast_strtod_ok("+1E+8"));
        EXPECT_TRUE(fast_strtod_ok("-1e8"));
        EXPECT_TRUE(fast_strtod_ok("-1e-8"));
        EXPECT_TRUE(fast_strtod_ok("-1e+8"));
        EXPECT_TRUE(fast_strtod_ok("-1E8"));
        EXPECT_TRUE(fast_strtod_ok("-1E-8"));
        EXPECT_TRUE(fast_strtod_ok("-1E+8"));

        EXPECT_TRUE(fast_strtod_ok("1.e8"));
        EXPECT_TRUE(fast_strtod_ok("1.e-8"));
        EXPECT_TRUE(fast_strtod_ok("1.e+8"));
        EXPECT_TRUE(fast_strtod_ok("1.E8"));
        EXPECT_TRUE(fast_strtod_ok("1.E-8"));
        EXPECT_TRUE(fast_strtod_ok("1.E+8"));
        EXPECT_TRUE(fast_strtod_ok("+1.e8"));
        EXPECT_TRUE(fast_strtod_ok("+1.e-8"));
        EXPECT_TRUE(fast_strtod_ok("+1.e+8"));
        EXPECT_TRUE(fast_strtod_ok("+1.E8"));
        EXPECT_TRUE(fast_strtod_ok("+1.E-8"));
        EXPECT_TRUE(fast_strtod_ok("+1.E+8"));
        EXPECT_TRUE(fast_strtod_ok("-1.e8"));
        EXPECT_TRUE(fast_strtod_ok("-1.e-8"));
        EXPECT_TRUE(fast_strtod_ok("-1.e+8"));
        EXPECT_TRUE(fast_strtod_ok("-1.E8"));
        EXPECT_TRUE(fast_strtod_ok("-1.E-8"));
        EXPECT_TRUE(fast_strtod_ok("-1.E+8"));

        EXPECT_TRUE(fast_strtod_ok(".1e8"));
        EXPECT_TRUE(fast_strtod_ok(".1e-8"));
        EXPECT_TRUE(fast_strtod_ok(".1e+8"));
        EXPECT_TRUE(fast_strtod_ok(".1E8"));
        EXPECT_TRUE(fast_strtod_ok(".1E-8"));
        EXPECT_TRUE(fast_strtod_ok(".1E+8"));
        EXPECT_TRUE(fast_strtod_ok("+.1e8"));
        EXPECT_TRUE(fast_strtod_ok("+.1e-8"));
        EXPECT_TRUE(fast_strtod_ok("+.1e+8"));
        EXPECT_TRUE(fast_strtod_ok("+.1E8"));
        EXPECT_TRUE(fast_strtod_ok("+.1E-8"));
        EXPECT_TRUE(fast_strtod_ok("+.1E+8"));
        EXPECT_TRUE(fast_strtod_ok("-.1e8"));
        EXPECT_TRUE(fast_strtod_ok("-.1e-8"));
        EXPECT_TRUE(fast_strtod_ok("-.1e+8"));
        EXPECT_TRUE(fast_strtod_ok("-.1E8"));
        EXPECT_TRUE(fast_strtod_ok("-.1E-8"));
        EXPECT_TRUE(fast_strtod_ok("-.1E+8"));

        EXPECT_TRUE(fast_strtod_ok("1.2e8"));
        EXPECT_TRUE(fast_strtod_ok("1.2e-8"));
        EXPECT_TRUE(fast_strtod_ok("1.2e+8"));
        EXPECT_TRUE(fast_strtod_ok("1.2E8"));
        EXPECT_TRUE(fast_strtod_ok("1.2E-8"));
        EXPECT_TRUE(fast_strtod_ok("1.2E+8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2e8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2e-8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2e+8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2E8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2E-8"));
        EXPECT_TRUE(fast_strtod_ok("+1.2E+8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2e8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2e-8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2e+8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2E8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2E-8"));
        EXPECT_TRUE(fast_strtod_ok("-1.2E+8"));
    }

    TEST_CASE(TestFastStrtodPrecision)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 1000; ++i)
        {
            const double mantissa = rand_double1(rng, -1.0, 1.0);
            const int exponent = rand_int1(rng, -300, 300);
            const double value = std::ldexp(mantissa, exponent);

            char str[1000];
            sprintf(str, "%.16e", value);

            EXPECT_TRUE(fast_strtod_ok(str));
        }
    }

    TEST_CASE(MakeSafeFilename_GivenSafeFilename_ReturnsFilenameUnchanged)
    {
        const std::string result = make_safe_filename("hello-world_42.txt");

        EXPECT_EQ("hello-world_42.txt", result);
    }

    TEST_CASE(MakeSafeFilename_GivenUnsafeFilename_ReturnsSafeFilename)
    {
        const std::string result = make_safe_filename("hello/world !.txt");

        EXPECT_EQ("hello_world__.txt", result);
    }

    TEST_CASE(Capitalize_GivenEmptyString_ReturnsEmptyString)
    {
        const std::string result = capitalize("");

        EXPECT_EQ("", result);
    }

    TEST_CASE(Capitalize_GivenBlankString_ReturnsInputString)
    {
        const std::string result = capitalize(" ");

        EXPECT_EQ(" ", result);
    }

    TEST_CASE(Capitalize_GivenSingleWord_ReturnsCapitalizedWord)
    {
        const std::string result = capitalize("aB");

        EXPECT_EQ("Ab", result);
    }

    TEST_CASE(Capitalize_GivenTwoWords_ReturnsCapitalizedWords)
    {
        const std::string result = capitalize("aB c");

        EXPECT_EQ("Ab C", result);
    }

    TEST_CASE(Capitalize_GivenSingleWordSurroundedByBlanks_ReturnsInputStringWithCapitalizedWord)
    {
        const std::string result = capitalize(" heLLo ");

        EXPECT_EQ(" Hello ", result);
    }

    TEST_CASE(Capitalize_GivenTwoWordsSurroundedByBlanks_ReturnsInputStringWithCapitalizedWords)
    {
        const std::string result = capitalize(" hello CUTE carrot ");

        EXPECT_EQ(" Hello Cute Carrot ", result);
    }

    TEST_CASE(TestPrettyUInt)
    {
        EXPECT_EQ("0", pretty_uint(0));
        EXPECT_EQ("1", pretty_uint(1));
        EXPECT_EQ("10", pretty_uint(10));
        EXPECT_EQ("100", pretty_uint(100));
        EXPECT_EQ("1,000", pretty_uint(1000));
        EXPECT_EQ("10,000", pretty_uint(10000));
        EXPECT_EQ("100,000", pretty_uint(100000));
        EXPECT_EQ("1,000,000", pretty_uint(1000000));
    }

    TEST_CASE(PrettyIntHandlesPositiveValues)
    {
        EXPECT_EQ("0", pretty_int(0));
        EXPECT_EQ("1", pretty_int(1));
        EXPECT_EQ("10", pretty_int(10));
        EXPECT_EQ("100", pretty_int(100));
        EXPECT_EQ("1,000", pretty_int(1000));
        EXPECT_EQ("10,000", pretty_int(10000));
        EXPECT_EQ("100,000", pretty_int(100000));
        EXPECT_EQ("1,000,000", pretty_int(1000000));
    }

    TEST_CASE(PrettyIntHandlesNegativeValues)
    {
        EXPECT_EQ("0", pretty_int(-0));
        EXPECT_EQ("-1", pretty_int(-1));
        EXPECT_EQ("-10", pretty_int(-10));
        EXPECT_EQ("-100", pretty_int(-100));
        EXPECT_EQ("-1,000", pretty_int(-1000));
        EXPECT_EQ("-10,000", pretty_int(-10000));
        EXPECT_EQ("-100,000", pretty_int(-100000));
        EXPECT_EQ("-1,000,000", pretty_int(-1000000));
    }

    TEST_CASE(PrettyScalarHandlesPositiveValues)
    {
        EXPECT_EQ("0.0", pretty_scalar(0));
        EXPECT_EQ("1.0", pretty_scalar(1));
        EXPECT_EQ("3.1", pretty_scalar(3.1));
        EXPECT_EQ("3.1", pretty_scalar(3.14));
        EXPECT_EQ("3.14", pretty_scalar(3.14, 2));
        EXPECT_EQ("3.142", pretty_scalar(3.1415, 3));
    }

    TEST_CASE(PrettyScalarHandlesNegativeValues)
    {
        EXPECT_EQ("0.0", pretty_scalar(-0));
        EXPECT_EQ("-1.0", pretty_scalar(-1));
        EXPECT_EQ("-3.1", pretty_scalar(-3.1));
        EXPECT_EQ("-3.1", pretty_scalar(-3.14));
        EXPECT_EQ("-3.14", pretty_scalar(-3.14, 2));
        EXPECT_EQ("-3.142", pretty_scalar(-3.1415, 3));
    }

    TEST_CASE(TestPrettyTime)
    {
        const double S = 1.0;
        const double M = 60.0 * S;
        const double H = 60.0 * M;
        const double D = 24.0 * H;
        const double W = 7.0  * D;

        EXPECT_EQ("0 ms", pretty_time(0.0));
        EXPECT_EQ("1 ms", pretty_time(0.001));
        EXPECT_EQ("500 ms", pretty_time(0.5));
        EXPECT_EQ("999 ms", pretty_time(0.999));

        EXPECT_EQ("1.0 second", pretty_time(1 * S));
        EXPECT_EQ("1.5 second", pretty_time(1 * S + 0.500));
        EXPECT_EQ("1.001 second", pretty_time(1 * S + 0.001, 3));

        EXPECT_EQ("1 minute 0.0 second", pretty_time(1 * M));
        EXPECT_EQ("2 minutes 0.0 second", pretty_time(2 * M));
        EXPECT_EQ("2 minutes 30.0 seconds", pretty_time(2 * M + 30 * S));
        EXPECT_EQ("2 minutes 30.001 seconds", pretty_time(2 * M + 30 * S + 0.001, 3));

        EXPECT_EQ("1 hour 0 minute 0.0 second", pretty_time(1 * H));
        EXPECT_EQ("2 hours 0 minute 0.0 second", pretty_time(2 * H));
        EXPECT_EQ("2 hours 30 minutes 0.0 second", pretty_time(2 * H + 30 * M));
        EXPECT_EQ("2 hours 30 minutes 15.0 seconds", pretty_time(2 * H + 30 * M + 15 * S));
        EXPECT_EQ("2 hours 30 minutes 15.001 seconds", pretty_time(2 * H + 30 * M + 15 * S + 0.001, 3));

        EXPECT_EQ("1 day 0 hour 0 minute 0.0 second", pretty_time(1 * D));
        EXPECT_EQ("2 days 0 hour 0 minute 0.0 second", pretty_time(2 * D));
        EXPECT_EQ("2 days 12 hours 0 minute 0.0 second", pretty_time(2 * D + 12 * H));
        EXPECT_EQ("2 days 12 hours 30 minutes 0.0 second", pretty_time(2 * D + 12 * H + 30 * M));
        EXPECT_EQ("2 days 12 hours 30 minutes 15.0 seconds", pretty_time(2 * D + 12 * H + 30 * M + 15 * S));
        EXPECT_EQ("2 days 12 hours 30 minutes 15.001 seconds", pretty_time(2 * D + 12 * H + 30 * M + 15 * S + 0.001, 3));

        EXPECT_EQ("1 week 0 day 0 hour 0 minute 0.0 second", pretty_time(1 * W));
        EXPECT_EQ("2 weeks 0 day 0 hour 0 minute 0.0 second", pretty_time(2 * W));
        EXPECT_EQ("2 weeks 3 days 0 hour 0 minute 0.0 second", pretty_time(2 * W + 3 * D));
        EXPECT_EQ("2 weeks 3 days 12 hours 0 minute 0.0 second", pretty_time(2 * W + 3 * D + 12 * H));
        EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 0.0 second", pretty_time(2 * W + 3 * D + 12 * H + 30 * M));
        EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 15.0 seconds", pretty_time(2 * W + 3 * D + 12 * H + 30 * M + 15 * S));
        EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 15.001 seconds", pretty_time(2 * W + 3 * D + 12 * H + 30 * M + 15 * S + 0.001, 3));
    }

    TEST_CASE(TestPrettyTimeWithPrecisionSetToZero)
    {
        EXPECT_EQ("1 second", pretty_time(1.001, 0));
        EXPECT_EQ("1 second", pretty_time(1.999, 0));
        EXPECT_EQ("1 minute 59 seconds", pretty_time(119.9, 0));
    }

    TEST_CASE(TestPrettyTimeWithTimeValueSmallerThanOneSecond)
    {
        EXPECT_EQ("10 ms", pretty_time(0.01, 0));
        EXPECT_EQ("10 ms", pretty_time(0.01, 3));
        EXPECT_EQ("10.0 ms", pretty_time(0.01, 4));
    }
}
