
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
#include "foundation/utility/preprocessor.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_Preprocessor)
{
    TEST_CASE(Process_GivenEmptyString_ReturnsEmptyString)
    {
        const std::string InputText = "";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenPlainString_ReturnsInputString)
    {
        const std::string InputText = "hello world";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenPlainMultilineString_ReturnsInputString)
    {
        const std::string InputText =
            "hello\n"
            "world";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenPlainStringWithLeadingEOL_ReturnsInputString)
    {
        const std::string InputText = "\nhello world";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenPlainStringWithTrailingEOL_ReturnsInputString)
    {
        const std::string InputText = "hello world\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenStringWithEOLOnly_ReturnsInputString)
    {
        const std::string InputText = "\n\n\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(InputText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_IfdefWithUndefinedSymbol_SkipsIfdefSection)
    {
        const std::string InputText =
            "#ifdef X\n"
            "ignore\n"
            "#endif\n"
            "more\n";

        const std::string ExpectedText = "more\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_IfdefWithPredefinedSymbol_KeepsIfdefSection)
    {
        const std::string InputText =
            "#ifdef X\n"
            "keep\n"
            "#endif\n"
            "more\n";

        const std::string ExpectedText =
            "keep\n"
            "more\n";

        Preprocessor preprocessor;
        preprocessor.define_symbol("X");
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_IfdefWithDefinedSymbol_KeepsIfdefSection)
    {
        const std::string InputText =
            "#define X\n"
            "#ifdef X\n"
            "keep\n"
            "#endif\n"
            "more\n";

        const std::string ExpectedText =
            "keep\n"
            "more\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_IfdefWithValuedSymbol_KeepsIfdefSection)
    {
        const std::string InputText =
            "#define X 42\n"
            "#ifdef X\n"
            "keep\n"
            "#endif\n"
            "more\n";

        const std::string ExpectedText =
            "keep\n"
            "more\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_SymbolDefinitionGuardedByIfDefWithFalseCondition_SymbolIsNotDefined)
    {
        const std::string InputText =
            "#ifdef X\n"
            "#define Y 42\n"
            "#endif\n"
            "Y";

        const std::string ExpectedText = "Y";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_UnknownKeyword_GeneratesError)
    {
        const std::string InputText = "#stuff X Y";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_FALSE(preprocessor.succeeded());
        EXPECT_EQ(std::string("Unknown directive: #stuff"), preprocessor.get_error_message());
        EXPECT_EQ(1, preprocessor.get_error_location());
    }

    TEST_CASE(Process_MissingEndIf_GeneratesError)
    {
        const std::string InputText =
            "#ifdef X\n"
            "keep\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_FALSE(preprocessor.succeeded());
        EXPECT_EQ(std::string("Expected directive: #endif"), preprocessor.get_error_message());
        EXPECT_EQ(3, preprocessor.get_error_location());
    }

    TEST_CASE(Process_GivenSymbolsDefinitions_SubstitutesSymbolsWithValues)
    {
        const std::string InputText =
            "#define X 42\n"
            "#define Y bun\n"
            "foo X bar X Y Y\n";

        const std::string ExpectedText =
            "foo 42 bar 42 bun bun\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenSymbolDefinition_OnySubstitutesSymbolsSurroundedByDelimiters)
    {
        const std::string InputText =
            "#define X 42\n"
            "X fooX\n";

        const std::string ExpectedText =
            "42 fooX\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }

    TEST_CASE(Process_GivenSymbolDefinitionUsingAnotherSymbol_SubstitutesInChain)
    {
        const std::string InputText =
            "#define X 42\n"
            "#define Y X\n"
            "Y\n";

        const std::string ExpectedText =
            "42\n";

        Preprocessor preprocessor;
        preprocessor.process(InputText.c_str());

        ASSERT_TRUE(preprocessor.succeeded());
        EXPECT_EQ(ExpectedText, preprocessor.get_processed_text());
    }
}
