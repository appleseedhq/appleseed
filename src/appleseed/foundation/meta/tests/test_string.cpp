
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
#include "foundation/platform/types.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>
#include <vector>

FOUNDATION_TEST_SUITE(Foundation_Utility_String)
{
    using namespace foundation;
    using namespace std;

    FOUNDATION_TEST_CASE(ToStringHandlesEmptyArray)
    {
        const int array[3] = { 1, 2, 3 };
        FOUNDATION_EXPECT_EQ("", to_string(array, 0));
    }

    FOUNDATION_TEST_CASE(ToStringHandlesEmptyArrayWithCustomSeparator)
    {
        const int array[3] = { 1, 2, 3 };
        FOUNDATION_EXPECT_EQ("", to_string(array, 0, "x"));
    }

    FOUNDATION_TEST_CASE(ToStringHandlesNonEmptyArray)
    {
        const int array[3] = { 1, 2, 3 };
        FOUNDATION_EXPECT_EQ("1 2 3", to_string(array, 3));
    }

    FOUNDATION_TEST_CASE(ToStringHandlesNonEmptyArrayWithCustomSeparator)
    {
        const int array[3] = { 1, 2, 3 };
        FOUNDATION_EXPECT_EQ("1x2x3", to_string(array, 3, "x"));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullInteger_ReturnsCorrespondingString)
    {
        const int n = 0;
        FOUNDATION_EXPECT_EQ("0", to_string(n));
    }

    FOUNDATION_TEST_CASE(Int8ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<int8>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<int8>(42));
        FOUNDATION_EXPECT_EQ("-1", to_string<int8>(-1));
    }

    FOUNDATION_TEST_CASE(Int16ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<int16>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<int16>(42));
        FOUNDATION_EXPECT_EQ("-1", to_string<int16>(-1));
    }

    FOUNDATION_TEST_CASE(Int32ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<int32>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<int32>(42));
        FOUNDATION_EXPECT_EQ("-1", to_string<int32>(-1));
    }

    FOUNDATION_TEST_CASE(Int64ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<int64>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<int64>(42));
        FOUNDATION_EXPECT_EQ("-1", to_string<int64>(-1));
    }

    FOUNDATION_TEST_CASE(UInt8ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<uint8>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<uint8>(42));
    }

    FOUNDATION_TEST_CASE(UInt16ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<uint16>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<uint16>(42));
    }

    FOUNDATION_TEST_CASE(UInt32ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<uint32>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<uint32>(42));
    }

    FOUNDATION_TEST_CASE(UInt64ToString)
    {
        FOUNDATION_EXPECT_EQ("0", to_string<uint64>(0));
        FOUNDATION_EXPECT_EQ("42", to_string<uint64>(42));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNonNullVoidPointer_ReturnsCorrespondingString)
    {
        void* ptr = reinterpret_cast<void*>(0xDEADBEEF);
        FOUNDATION_EXPECT_EQ("0xDEADBEEF", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNonNullConstVoidPointer_ReturnsCorrespondingString)
    {
        const void* ptr = reinterpret_cast<const void*>(0xDEADBEEF);
        FOUNDATION_EXPECT_EQ("0xDEADBEEF", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullVoidPointer_ReturnsCorrespondingString)
    {
        void* ptr = 0;
        FOUNDATION_EXPECT_EQ("0x00000000", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullConstVoidPointer_ReturnsCorrespondingString)
    {
        const void* ptr = 0;
        FOUNDATION_EXPECT_EQ("0x00000000", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNonNullCString_ReturnsCorrespondingString)
    {
        char* s = "bunny";
        FOUNDATION_EXPECT_EQ("bunny", to_string(s));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNonNullConstCString_ReturnsCorrespondingString)
    {
        const char* s = "bunny";
        FOUNDATION_EXPECT_EQ("bunny", to_string(s));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullCString_ReturnsCorrespondingString)
    {
        char* s = 0;
        FOUNDATION_EXPECT_EQ("<null>", to_string(s));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullConstCString_ReturnsCorrespondingString)
    {
        const char* s = 0;
        FOUNDATION_EXPECT_EQ("<null>", to_string(s));
    }

    struct Foo { int dummy; };

    FOUNDATION_TEST_CASE(ToString_GivenNonNullClassPointer_ReturnsCorrespondingString)
    {
        Foo* ptr = reinterpret_cast<Foo*>(0xDEADBEEF);
        FOUNDATION_EXPECT_EQ("0xDEADBEEF", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNonNullConstClassPointer_ReturnsCorrespondingString)
    {
        const Foo* ptr = reinterpret_cast<const Foo*>(0xDEADBEEF);
        FOUNDATION_EXPECT_EQ("0xDEADBEEF", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullClassPointer_ReturnsCorrespondingString)
    {
        Foo* ptr = 0;
        FOUNDATION_EXPECT_EQ("0x00000000", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(ToString_GivenNullConstClassPointer_ReturnsCorrespondingString)
    {
        const Foo* ptr = 0;
        FOUNDATION_EXPECT_EQ("0x00000000", to_string(ptr));
    }

    FOUNDATION_TEST_CASE(StringToInt8)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<int8>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<int8>("42"));
        FOUNDATION_EXPECT_EQ(-1, from_string<int8>("-1"));
    }

    FOUNDATION_TEST_CASE(StringToInt16)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<int16>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<int16>("42"));
        FOUNDATION_EXPECT_EQ(-1, from_string<int16>("-1"));
    }

    FOUNDATION_TEST_CASE(StringToInt32)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<int32>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<int32>("42"));
        FOUNDATION_EXPECT_EQ(-1, from_string<int32>("-1"));
    }

    FOUNDATION_TEST_CASE(StringToInt64)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<int64>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<int64>("42"));
        FOUNDATION_EXPECT_EQ(-1, from_string<int64>("-1"));
    }

    FOUNDATION_TEST_CASE(StringToUInt8)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<uint8>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<uint8>("42"));
    }

    FOUNDATION_TEST_CASE(StringToUInt16)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<uint16>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<uint16>("42"));
    }

    FOUNDATION_TEST_CASE(StringToUInt32)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<uint32>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<uint32>("42"));
    }

    FOUNDATION_TEST_CASE(StringToUInt64)
    {
        FOUNDATION_EXPECT_EQ(0, from_string<uint64>("0"));
        FOUNDATION_EXPECT_EQ(42, from_string<uint64>("42"));
    }

    FOUNDATION_TEST_CASE(StrcmpNoCaseHandlesEmptyString)
    {
        FOUNDATION_EXPECT_EQ(0, strcmp_nocase("", ""));
    }

    FOUNDATION_TEST_CASE(TestStrCmpNoCase)
    {
        FOUNDATION_EXPECT_EQ(0, strcmp_nocase("seal", "SEAL"));
        FOUNDATION_EXPECT_EQ(0, strcmp_nocase("HeLLo", "hEllO"));

        FOUNDATION_EXPECT_EQ(-1, strcmp_nocase("a", "b"));
        FOUNDATION_EXPECT_EQ(-1, strcmp_nocase("A", "b"));
        FOUNDATION_EXPECT_EQ(-1, strcmp_nocase("a", "B"));
        FOUNDATION_EXPECT_EQ(-1, strcmp_nocase("A", "B"));

        FOUNDATION_EXPECT_EQ(1, strcmp_nocase("b", "a"));
        FOUNDATION_EXPECT_EQ(1, strcmp_nocase("B", "a"));
        FOUNDATION_EXPECT_EQ(1, strcmp_nocase("b", "A"));
        FOUNDATION_EXPECT_EQ(1, strcmp_nocase("B", "A"));
    }

    FOUNDATION_TEST_CASE(TrimLeftHandlesEmptyString)
    {
        FOUNDATION_EXPECT_EQ("", trim_left(""));
    }

    FOUNDATION_TEST_CASE(TrimLeftHandlesBlankStrings)
    {
        FOUNDATION_EXPECT_EQ("", trim_left(" "));
        FOUNDATION_EXPECT_EQ("", trim_left(" \t\n\v\f\r "));
    }

    FOUNDATION_TEST_CASE(TrimLeftHandlesRealStrings)
    {
        FOUNDATION_EXPECT_EQ("hello   ", trim_left("hello   "));
        FOUNDATION_EXPECT_EQ("hello", trim_left(" \t\n\v\f\r hello"));
        FOUNDATION_EXPECT_EQ("hello   ", trim_left(" \t\n\v\f\r hello   "));
    }

    FOUNDATION_TEST_CASE(TrimRightHandlesEmptyString)
    {
        FOUNDATION_EXPECT_EQ(trim_right(""), "");
    }

    FOUNDATION_TEST_CASE(TrimRightHandlesBlankStrings)
    {
        FOUNDATION_EXPECT_EQ(trim_right(" "), "");
        FOUNDATION_EXPECT_EQ(trim_right(" \t\n\v\f\r "), "");
    }

    FOUNDATION_TEST_CASE(TrimRightHandlesRealStrings)
    {
        FOUNDATION_EXPECT_EQ("   hello", trim_right("   hello"));
        FOUNDATION_EXPECT_EQ("hello", trim_right("hello \t\n\v\f\r "));
        FOUNDATION_EXPECT_EQ("   hello", trim_right("   hello \t\n\v\f\r "));
    }

    FOUNDATION_TEST_CASE(TrimBothHandlesEmptyString)
    {
        FOUNDATION_EXPECT_EQ("", trim_both(""));
    }

    FOUNDATION_TEST_CASE(TrimBothHandlesBlankStrings)
    {
        FOUNDATION_EXPECT_EQ("", trim_both(" "));
        FOUNDATION_EXPECT_EQ("", trim_both(" \t\n\v\f\r "));
    }

    FOUNDATION_TEST_CASE(TrimBothHandlesRealStrings)
    {
        FOUNDATION_EXPECT_EQ("hello", trim_both("hello \t\n\v\f\r "));
        FOUNDATION_EXPECT_EQ("hello", trim_both(" \t\n\v\f\r hello"));
        FOUNDATION_EXPECT_EQ("hello", trim_both(" \t\n\v\f\r hello \t\n\v\f\r "));
    }

    vector<string> tokenize_wrapper(
        const string&   s,
        const string&   delimiters)
    {
        vector<string> vec;
        tokenize(s, delimiters, vec);
        return vec;
    }

    FOUNDATION_TEST_CASE(TestTokenize)
    {
        FOUNDATION_EXPECT_EQ(vector<string>(), tokenize_wrapper("", "\n"));

        FOUNDATION_EXPECT_EQ(vector<string>(), tokenize_wrapper("\n", "\n"));
        FOUNDATION_EXPECT_EQ(vector<string>(), tokenize_wrapper("\n\n", "\n"));
        FOUNDATION_EXPECT_EQ(vector<string>(), tokenize_wrapper("\n\n\n", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("hello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("hello\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("hello\n\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("\nhello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("\n\nhello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), tokenize_wrapper("\n\nhello\n\n", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(2, "hello", "world"), tokenize_wrapper("hello\nworld", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(2, "hello", "world"), tokenize_wrapper("hello\n\nworld", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(3, "hello", "world", "bunny"), tokenize_wrapper("helloXworld\nbunny", "X\n"));
    }

    vector<string> split_wrapper(
        const string&   s,
        const string&   delimiters)
    {
        vector<string> vec;
        split(s, delimiters, vec);
        return vec;
    }

    FOUNDATION_TEST_CASE(TestSplit)
    {
        FOUNDATION_EXPECT_EQ(vector<string>(), split_wrapper("", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(1, ""), split_wrapper("\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(2, "", ""), split_wrapper("\n\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(3, "", "", ""), split_wrapper("\n\n\n", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), split_wrapper("hello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(1, "hello"), split_wrapper("hello\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(2, "hello", ""), split_wrapper("hello\n\n", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(2, "", "hello"), split_wrapper("\nhello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(2, "", "hello"), split_wrapper("\nhello\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(3, "", "hello", ""), split_wrapper("\nhello\n\n", "\n"));

        FOUNDATION_EXPECT_EQ(make_vector(3, "", "", "hello"), split_wrapper("\n\nhello", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(3, "", "", "hello"), split_wrapper("\n\nhello\n", "\n"));
        FOUNDATION_EXPECT_EQ(make_vector(4, "", "", "hello", ""), split_wrapper("\n\nhello\n\n", "\n"));
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenEmptyString_ReturnsEmptyString)
    {
        const string result = capitalize("");

        FOUNDATION_EXPECT_EQ("", result);
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenBlankString_ReturnsInputString)
    {
        const string result = capitalize(" ");

        FOUNDATION_EXPECT_EQ(" ", result);
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenSingleWord_ReturnsCapitalizedWord)
    {
        const string result = capitalize("aB");

        FOUNDATION_EXPECT_EQ("Ab", result);
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenTwoWords_ReturnsCapitalizedWords)
    {
        const string result = capitalize("aB c");

        FOUNDATION_EXPECT_EQ("Ab C", result);
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenSingleWordSurroundedByBlanks_ReturnsInputStringWithCapitalizedWord)
    {
        const string result = capitalize(" heLLo ");

        FOUNDATION_EXPECT_EQ(" Hello ", result);
    }

    FOUNDATION_TEST_CASE(Capitalize_GivenTwoWordsSurroundedByBlanks_ReturnsInputStringWithCapitalizedWords)
    {
        const string result = capitalize(" hello CUTE carrot ");

        FOUNDATION_EXPECT_EQ(" Hello Cute Carrot ", result);
    }

    FOUNDATION_TEST_CASE(TestPrettyUInt)
    {
        FOUNDATION_EXPECT_EQ("0", pretty_uint(0));
        FOUNDATION_EXPECT_EQ("1", pretty_uint(1));
        FOUNDATION_EXPECT_EQ("10", pretty_uint(10));
        FOUNDATION_EXPECT_EQ("100", pretty_uint(100));
        FOUNDATION_EXPECT_EQ("1,000", pretty_uint(1000));
        FOUNDATION_EXPECT_EQ("10,000", pretty_uint(10000));
        FOUNDATION_EXPECT_EQ("100,000", pretty_uint(100000));
        FOUNDATION_EXPECT_EQ("1,000,000", pretty_uint(1000000));
    }

    FOUNDATION_TEST_CASE(PrettyIntHandlesPositiveValues)
    {
        FOUNDATION_EXPECT_EQ("0", pretty_int(0));
        FOUNDATION_EXPECT_EQ("1", pretty_int(1));
        FOUNDATION_EXPECT_EQ("10", pretty_int(10));
        FOUNDATION_EXPECT_EQ("100", pretty_int(100));
        FOUNDATION_EXPECT_EQ("1,000", pretty_int(1000));
        FOUNDATION_EXPECT_EQ("10,000", pretty_int(10000));
        FOUNDATION_EXPECT_EQ("100,000", pretty_int(100000));
        FOUNDATION_EXPECT_EQ("1,000,000", pretty_int(1000000));
    }

    FOUNDATION_TEST_CASE(PrettyIntHandlesNegativeValues)
    {
        FOUNDATION_EXPECT_EQ("0", pretty_int(-0));
        FOUNDATION_EXPECT_EQ("-1", pretty_int(-1));
        FOUNDATION_EXPECT_EQ("-10", pretty_int(-10));
        FOUNDATION_EXPECT_EQ("-100", pretty_int(-100));
        FOUNDATION_EXPECT_EQ("-1,000", pretty_int(-1000));
        FOUNDATION_EXPECT_EQ("-10,000", pretty_int(-10000));
        FOUNDATION_EXPECT_EQ("-100,000", pretty_int(-100000));
        FOUNDATION_EXPECT_EQ("-1,000,000", pretty_int(-1000000));
    }

    FOUNDATION_TEST_CASE(PrettyScalarHandlesPositiveValues)
    {
        FOUNDATION_EXPECT_EQ("0.0", pretty_scalar(0));
        FOUNDATION_EXPECT_EQ("1.0", pretty_scalar(1));
        FOUNDATION_EXPECT_EQ("3.1", pretty_scalar(3.1));
        FOUNDATION_EXPECT_EQ("3.1", pretty_scalar(3.14));
        FOUNDATION_EXPECT_EQ("3.14", pretty_scalar(3.14, 2));
        FOUNDATION_EXPECT_EQ("3.142", pretty_scalar(3.1415, 3));
    }

    FOUNDATION_TEST_CASE(PrettyScalarHandlesNegativeValues)
    {
        FOUNDATION_EXPECT_EQ("0.0", pretty_scalar(-0));
        FOUNDATION_EXPECT_EQ("-1.0", pretty_scalar(-1));
        FOUNDATION_EXPECT_EQ("-3.1", pretty_scalar(-3.1));
        FOUNDATION_EXPECT_EQ("-3.1", pretty_scalar(-3.14));
        FOUNDATION_EXPECT_EQ("-3.14", pretty_scalar(-3.14, 2));
        FOUNDATION_EXPECT_EQ("-3.142", pretty_scalar(-3.1415, 3));
    }

    FOUNDATION_TEST_CASE(TestPrettyTime)
    {
        const double S = 1.0;
        const double M = 60.0 * S;
        const double H = 60.0 * M;
        const double D = 24.0 * H;
        const double W = 7.0  * D;

        FOUNDATION_EXPECT_EQ("0 ms", pretty_time(0.0));
        FOUNDATION_EXPECT_EQ("1 ms", pretty_time(0.001));
        FOUNDATION_EXPECT_EQ("500 ms", pretty_time(0.5));
        FOUNDATION_EXPECT_EQ("999 ms", pretty_time(0.999));

        FOUNDATION_EXPECT_EQ("1.0 second", pretty_time(1 * S));
        FOUNDATION_EXPECT_EQ("1.5 second", pretty_time(1 * S + 0.500));
        FOUNDATION_EXPECT_EQ("1.001 second", pretty_time(1 * S + 0.001, 3));

        FOUNDATION_EXPECT_EQ("1 minute 0.0 second", pretty_time(1 * M));
        FOUNDATION_EXPECT_EQ("2 minutes 0.0 second", pretty_time(2 * M));
        FOUNDATION_EXPECT_EQ("2 minutes 30.0 seconds", pretty_time(2 * M + 30 * S));
        FOUNDATION_EXPECT_EQ("2 minutes 30.001 seconds", pretty_time(2 * M + 30 * S + 0.001, 3));

        FOUNDATION_EXPECT_EQ("1 hour 0 minute 0.0 second", pretty_time(1 * H));
        FOUNDATION_EXPECT_EQ("2 hours 0 minute 0.0 second", pretty_time(2 * H));
        FOUNDATION_EXPECT_EQ("2 hours 30 minutes 0.0 second", pretty_time(2 * H + 30 * M));
        FOUNDATION_EXPECT_EQ("2 hours 30 minutes 15.0 seconds", pretty_time(2 * H + 30 * M + 15 * S));
        FOUNDATION_EXPECT_EQ("2 hours 30 minutes 15.001 seconds", pretty_time(2 * H + 30 * M + 15 * S + 0.001, 3));

        FOUNDATION_EXPECT_EQ("1 day 0 hour 0 minute 0.0 second", pretty_time(1 * D));
        FOUNDATION_EXPECT_EQ("2 days 0 hour 0 minute 0.0 second", pretty_time(2 * D));
        FOUNDATION_EXPECT_EQ("2 days 12 hours 0 minute 0.0 second", pretty_time(2 * D + 12 * H));
        FOUNDATION_EXPECT_EQ("2 days 12 hours 30 minutes 0.0 second", pretty_time(2 * D + 12 * H + 30 * M));
        FOUNDATION_EXPECT_EQ("2 days 12 hours 30 minutes 15.0 seconds", pretty_time(2 * D + 12 * H + 30 * M + 15 * S));
        FOUNDATION_EXPECT_EQ("2 days 12 hours 30 minutes 15.001 seconds", pretty_time(2 * D + 12 * H + 30 * M + 15 * S + 0.001, 3));

        FOUNDATION_EXPECT_EQ("1 week 0 day 0 hour 0 minute 0.0 second", pretty_time(1 * W));
        FOUNDATION_EXPECT_EQ("2 weeks 0 day 0 hour 0 minute 0.0 second", pretty_time(2 * W));
        FOUNDATION_EXPECT_EQ("2 weeks 3 days 0 hour 0 minute 0.0 second", pretty_time(2 * W + 3 * D));
        FOUNDATION_EXPECT_EQ("2 weeks 3 days 12 hours 0 minute 0.0 second", pretty_time(2 * W + 3 * D + 12 * H));
        FOUNDATION_EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 0.0 second", pretty_time(2 * W + 3 * D + 12 * H + 30 * M));
        FOUNDATION_EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 15.0 seconds", pretty_time(2 * W + 3 * D + 12 * H + 30 * M + 15 * S));
        FOUNDATION_EXPECT_EQ("2 weeks 3 days 12 hours 30 minutes 15.001 seconds", pretty_time(2 * W + 3 * D + 12 * H + 30 * M + 15 * S + 0.001, 3));
    }

    FOUNDATION_TEST_CASE(TestPrettyTimeWithPrecisionSetToZero)
    {
        FOUNDATION_EXPECT_EQ("1 second", pretty_time(1.001, 0));
        FOUNDATION_EXPECT_EQ("1 second", pretty_time(1.999, 0));
        FOUNDATION_EXPECT_EQ("1 minute 59 seconds", pretty_time(119.9, 0));
    }

    FOUNDATION_TEST_CASE(TestPrettyTimeWithTimeValueSmallerThanOneSecond)
    {
        FOUNDATION_EXPECT_EQ("10 ms", pretty_time(0.01, 0));
        FOUNDATION_EXPECT_EQ("10 ms", pretty_time(0.01, 3));
        FOUNDATION_EXPECT_EQ("10.0 ms", pretty_time(0.01, 4));
    }
}
