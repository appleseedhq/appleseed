
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "foundation/string/string.h"
#include "foundation/utility/iesparser.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <fstream>
#include <iostream>
#include <sstream>

using namespace foundation;

TEST_SUITE(Foundation_Utility_Iesparser)
{
    TEST_CASE(ReadLine_IgnoreEmptyLines)
    {
        IESParser parser;

        std::istringstream input_stream(" \n\t\t\r\n    \nline1\n  \tline2  ");
        parser.m_ignore_empty_lines = true;
        parser.reset(input_stream);
        EXPECT_EQ("line1", parser.m_line);
        EXPECT_EQ(4, parser.m_line_counter);
        parser.read_trimmed_line(input_stream);
        EXPECT_EQ("line2", parser.m_line);
        EXPECT_EQ(5, parser.m_line_counter);
    }

    TEST_CASE(ReadLine_DoNotIgnoreEmptyLines)
    {
        IESParser parser;

        std::istringstream input_stream(" \tline1\n \t \nline2");
        parser.m_ignore_empty_lines = false;
        parser.reset(input_stream);
        EXPECT_EQ("line1", parser.m_line);
        EXPECT_EQ(1, parser.m_line_counter);
        parser.read_trimmed_line(input_stream);
        EXPECT_EQ("", parser.m_line);
        EXPECT_EQ(2, parser.m_line_counter);
        parser.read_trimmed_line(input_stream);
        EXPECT_EQ("line2", parser.m_line);
        EXPECT_EQ(3, parser.m_line_counter);
    }

    TEST_CASE(CheckEmpty)
    {
        IESParser parser;
        std::istringstream input_stream(" \tline1\n \t \nline2");
        parser.m_ignore_empty_lines = false;
        parser.reset(input_stream);
        parser.check_empty(input_stream);
        parser.read_trimmed_line(input_stream);
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.check_empty(input_stream)); // Empty line
        parser.read_trimmed_line(input_stream);
        parser.check_empty(input_stream);
        parser.read_trimmed_line(input_stream);
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.check_empty(input_stream)); // EOF
    }

    TEST_CASE(ParseFormat)
    {
        IESParser parser;

        {
            std::istringstream input_stream("IESNA91\n");
            parser.reset(input_stream);
            parser.parse_format_version(input_stream);
            EXPECT_EQ(IESParser::Format1991, parser.m_format);
        }

        {
            std::istringstream input_stream("IESNA:LM-63-1995\n");
            parser.reset(input_stream);
            parser.parse_format_version(input_stream);
            EXPECT_EQ(IESParser::Format1995, parser.m_format);
        }

        {
            std::istringstream input_stream("IESNA:LM-63-2002\n");
            parser.reset(input_stream);
            parser.parse_format_version(input_stream);
            EXPECT_EQ(IESParser::Format2002, parser.m_format);
        }

        {
            std::istringstream input_stream("Some string\n");
            parser.reset(input_stream);
            parser.parse_format_version(input_stream);
            EXPECT_EQ(IESParser::Format1986, parser.m_format);
        }
    }

    TEST_CASE(ParseKeywords)
    {
        IESParser parser;

        std::istringstream input_stream(
            "[KEYWORD] Value\n"
            "1986 standard allows any text in keywords section\n"
            "TILT=NONE\n"
            "[MANUFAC] Manufac is required by 1991 and 2002\n"
            "[TEST]\t   \tTest is required by 1991 and 2002\n"
            "   [ISSUEDATE] Issue date is required by 2002   \n"
            "[TESTLAB] Testlab is required by 2002\r\n"
            "\t[_USER]  User       \n"
            "\t[MORE]   More       \n"
            "\t[MORE]   More       \n"
            "[BAD] Bad"
            "Not a [keyword] - value pair"
            "[KEYWORD] Duplicate value\n");
        parser.reset(input_stream);

        parser.m_format = IESParser::Format1986;
        parser.parse_keywords_and_tilt(input_stream);

        parser.m_format = IESParser::Format1995;
        parser.check_required_keywords();

        parser.m_format = IESParser::Format1991;
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.check_required_keywords());
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        parser.check_required_keywords();

        parser.m_format = IESParser::Format2002;
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.check_required_keywords());
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        parser.check_required_keywords();

        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);
        parser.parse_keyword_line(parser.m_line);
        parser.read_trimmed_line(input_stream);

        EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse_keyword_line(parser.m_line));
        parser.read_trimmed_line(input_stream); // Bad keyword
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse_keyword_line(parser.m_line));
        parser.read_trimmed_line(input_stream); // Not a keyword-value pair
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse_keyword_line(parser.m_line));
        parser.read_trimmed_line(input_stream); // EOF
        EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse_keyword_line(parser.m_line));

        EXPECT_EQ("Value", parser.m_keywords_dictionary.at("KEYWORD"));
        EXPECT_EQ("Manufac is required by 1991 and 2002", parser.m_keywords_dictionary.at("MANUFAC"));
        EXPECT_EQ("User\nMore\nMore", parser.m_keywords_dictionary.at("_USER"));
        EXPECT_EXCEPTION(std::out_of_range, parser.m_keywords_dictionary.at("BAD"));
        EXPECT_EXCEPTION(std::out_of_range, parser.m_keywords_dictionary.at("MORE"));
    }

    TEST_CASE(ParseToVector_Empty)
    {
        IESParser parser;
        {
            std::istringstream input_stream("");
            parser.reset(input_stream);
            std::vector<int> result = parser.parse_to_vector<int>(input_stream, 0);
            EXPECT_EQ(std::vector<int>(), result);
        }
        {
            std::istringstream input_stream("test");
            parser.reset(input_stream);
            std::vector<double> result = parser.parse_to_vector<double>(input_stream, 0);
            EXPECT_EQ(parser.m_line, "test");
            EXPECT_EQ(std::vector<double>(), result);
        }
    }

    TEST_CASE(ParseToVector_Good)
    {
        IESParser parser;
        {
            std::istringstream input_stream("\n-0 1    2  3 \n\t4");
            parser.reset(input_stream);
            std::vector<int> result = parser.parse_to_vector<int>(input_stream, 5);
            ASSERT_EQ(5, result.size());
            for (int i = 0; i < 5; ++i)
                EXPECT_FEQ(i, result[i]);
        }
        {
            std::istringstream input_stream("\n1e-1\n\n\n0.2\n\n0.3 0.4\n\n\n0.5\n\n\n");
            parser.reset(input_stream);
            std::vector<double> result = parser.parse_to_vector<double>(input_stream, 5);
            ASSERT_EQ(5, result.size());
            for (int i = 0; i < 5; ++i)
                EXPECT_FEQ((i + 1) * 0.1, result[i]);
        }
        {
            std::istringstream input_stream("0");
            parser.reset(input_stream);
            std::vector<int> result = parser.parse_to_vector<int>(input_stream, 1);
            ASSERT_EQ(1, result.size());
            EXPECT_EQ(0, result[0]);
        }
    }

    TEST_CASE(ParseToVector_Bad)
    {
        IESParser parser;
        {
            std::istringstream input_stream("0 1 2 3");
            parser.reset(input_stream);
            EXPECT_EXCEPTION(IESParser::ParsingException,
                parser.parse_to_vector<int>(input_stream, 5));
        }
        {
            std::istringstream input_stream("0 1 2 3 \n 4 5");
            parser.reset(input_stream);
            EXPECT_EXCEPTION(IESParser::ParsingException,
                parser.parse_to_vector<int>(input_stream, 5));
        }
        {
            std::istringstream input_stream("0 1 2.0 3 4");
            parser.reset(input_stream);
            EXPECT_EXCEPTION(ExceptionStringConversionError,
                parser.parse_to_vector<int>(input_stream, 5));
        }
        {
            std::istringstream input_stream("0.0\n1.0\n+ 3.0\n4.0");
            parser.reset(input_stream);
            EXPECT_EXCEPTION(ExceptionStringConversionError,
                parser.parse_to_vector<double>(input_stream, 4));
        }
    }

    TEST_CASE(Parse_GoodExamples)
    {
        IESParser parser;
        {
            std::ifstream input_stream("unit tests/inputs/test_iesparser_1.ies");
            parser.parse(input_stream);
            EXPECT_EQ(IESParser::PhotometricTypeC, parser.get_photometric_type());
            EXPECT_EQ(IESParser::SymmetricHalvesX, parser.get_symmetry());
            EXPECT_EQ("LTL29483P14", parser.get_keyword_value("TEST"));
            EXPECT_EQ(170, parser.get_input_watts());
            EXPECT_FEQ(0.184397039525045, parser.get_candela_multiplier());
            EXPECT_EQ(73, parser.get_vertical_angles().size());
            EXPECT_FEQ(7.5, parser.get_vertical_angles()[3]);
            EXPECT_EQ(73, parser.get_number_of_horizontal_angles());
            EXPECT_FEQ(102.5, parser.get_horizontal_angles()[41]);
            EXPECT_FEQ(177.5, parser.get_horizontal_angles()[71]);
            EXPECT_FEQ(180.0, parser.get_horizontal_angles()[72]);
            EXPECT_FEQ(4086.0, parser.get_candela_values()[0][0]);
            EXPECT_FEQ(0.0, parser.get_candela_values()[72][72]);
        }
        {
            std::ifstream input_stream("unit tests/inputs/test_iesparser_2.ies");
            parser.parse(input_stream);
            EXPECT_EQ(IESParser::PhotometricTypeB, parser.get_photometric_type());
            EXPECT_EQ(IESParser::SymmetricHalvesX, parser.get_symmetry());
            EXPECT_EQ("6 X 6", parser.get_keyword_value("DISTRIBUTION"));
            EXPECT_EQ("0 , 0", parser.get_keyword_value("_LAMPPOSITION"));
            EXPECT_FEQ(1.0, parser.get_candela_multiplier());
            EXPECT_FEQ(0.0, parser.get_vertical_angles()[22]);
            EXPECT_FEQ(3.0, parser.get_horizontal_angles()[2]);
            EXPECT_FEQ(80.0, parser.get_candela_values()[0][3]);
            EXPECT_FEQ(1.0, parser.get_candela_values()[22][44]);
        }
        {
            std::ifstream input_stream("unit tests/inputs/test_iesparser_3.ies");
            parser.parse(input_stream);
        }
    }
    TEST_CASE(Parse_BadExamples)
    {
        IESParser parser;
        {
            std::ifstream input_stream("unit tests/inputs/test_iesparser_4.ies");
            EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse(input_stream));
        }
        {
            std::ifstream input_stream("unit tests/inputs/test_iesparser_5.ies");
            EXPECT_EXCEPTION(IESParser::ParsingException, parser.parse(input_stream));
        }
    }
}
