
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
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif
#include "foundation/containers/dictionary.h"
#include "foundation/log/log.h"
#include "foundation/utility/settings.h"
#include "foundation/utility/test.h"
#include "foundation/utility/testutils.h"

// Standard headers.
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_SettingsFileReader)
{
    struct Fixture
    {
        Logger                  m_logger;
        SettingsFileReader      m_reader;
        Dictionary              m_dictionary;

        Fixture()
          : m_reader(m_logger)
        {
        }

        bool read(const char* filename)
        {
            return
                m_reader.read(
                    filename,
                    "../../../schemas/settings.xsd",    // path relative to input file
                    m_dictionary);
        }
    };

    TEST_CASE_F(Read_GivenEmptySettingsFile_ReturnsEmptyDictionary, Fixture)
    {
        const bool succeeded = read("unit tests/inputs/test_settings_emptysettingsfile.xml");
        ASSERT_TRUE(succeeded);

        EXPECT_TRUE(m_dictionary.empty());
    }

    TEST_CASE_F(Read_GivenSettingsFileWithTwoScalarParameters_ReturnsDictionaryWithTwoScalarParameters, Fixture)
    {
        const bool succeeded = read("unit tests/inputs/test_settings_settingsfilewithtwoscalarparameters.xml");
        ASSERT_TRUE(succeeded);

        ASSERT_EQ(2, m_dictionary.strings().size());

        EXPECT_EQ(42, m_dictionary.get<int>("x"));
        EXPECT_EQ("foo", m_dictionary.get<std::string>("y"));
    }

    TEST_CASE_F(Read_GivenSettingsFileWithTwoDictionaryParameters_ReturnsDictionaryWithTwoDictionaryParameters, Fixture)
    {
        const bool succeeded = read("unit tests/inputs/test_settings_settingsfilewithtwodictionaryparameters.xml");
        ASSERT_TRUE(succeeded);

        ASSERT_EQ(0, m_dictionary.strings().size());
        ASSERT_EQ(2, m_dictionary.dictionaries().size());

        const Dictionary& sub1 = m_dictionary.dictionaries().get("sub1");
        EXPECT_EQ(42, sub1.get<int>("x"));
        EXPECT_EQ("foo", sub1.get<std::string>("y"));

        const Dictionary& sub2 = m_dictionary.dictionaries().get("sub2");
        EXPECT_EQ("aa", sub2.get<std::string>("a"));
        EXPECT_EQ("bb", sub2.get<std::string>("b"));
    }

    TEST_CASE_F(Read_GivenSettingsFileWithNewlinesInParameters_ReturnsDictionaryWithNewlinesInParameters, Fixture)
    {
        const bool succeeded = read("unit tests/inputs/test_settings_settingsfilewithnewlinesinparameters.xml");
        ASSERT_TRUE(succeeded);

        ASSERT_EQ(2, m_dictionary.strings().size());
        ASSERT_EQ(0, m_dictionary.dictionaries().size());

        EXPECT_EQ("aa", m_dictionary.get<std::string>("a"));
        EXPECT_EQ("bb\nbb\nbb", m_dictionary.get<std::string>("b"));
    }

    TEST_CASE_F(Read_GivenSettingsFileNameWithUTF8Encoding_ReturnsSuccess, Fixture)
    {
#ifdef _WIN32
        if (!does_windows_support_utf8_code_page())
        {
            // todo: if the unit testing infrastructure made a logger available, we would issue a warning here.
            return;
        }
#endif

        const bool succeeded = read(u8"unit tests/inputs/test_settings_utf8_\u00e2\u00e9\u00ef\u00f4\u00f9.xml");

        EXPECT_TRUE(succeeded);
    }
}

TEST_SUITE(Foundation_Utility_SettingsFileWriter)
{
    TEST_CASE(Write_GivenEmptyDictionary_WriteEmptySettingsFile)
    {
        const Dictionary dictionary;

        SettingsFileWriter writer;
        writer.write("unit tests/outputs/test_settings_emptysettingsfile.xml", dictionary);

        const bool identical =
            compare_text_files(
                "unit tests/inputs/test_settings_emptysettingsfile.xml",
                "unit tests/outputs/test_settings_emptysettingsfile.xml");

        EXPECT_TRUE(identical);
    }

    TEST_CASE(Write_GivenDictionaryWithTwoScalarParameters_WritesSettingsFileWithTwoScalarParameters)
    {
        Dictionary dictionary;
        dictionary.insert("x", 42);
        dictionary.insert("y", "foo");

        SettingsFileWriter writer;
        writer.write("unit tests/outputs/test_settings_settingsfilewithtwoscalarparameters.xml", dictionary);

        const bool identical =
            compare_text_files(
                "unit tests/inputs/test_settings_settingsfilewithtwoscalarparameters.xml",
                "unit tests/outputs/test_settings_settingsfilewithtwoscalarparameters.xml");

        EXPECT_TRUE(identical);
    }

    TEST_CASE(Write_GivenDictionaryWithTwoDictionaryParameters_WritesSettingsFileWithTwoDictionaryParameters)
    {
        Dictionary sub1;
        sub1.insert("x", 42);
        sub1.insert("y", "foo");

        Dictionary sub2;
        sub2.insert("a", "aa");
        sub2.insert("b", "bb");

        Dictionary dictionary;
        dictionary.insert("sub1", sub1);
        dictionary.insert("sub2", sub2);

        SettingsFileWriter writer;
        writer.write("unit tests/outputs/test_settings_settingsfilewithtwodictionaryparameters.xml", dictionary);

        const bool identical =
            compare_text_files(
                "unit tests/inputs/test_settings_settingsfilewithtwodictionaryparameters.xml",
                "unit tests/outputs/test_settings_settingsfilewithtwodictionaryparameters.xml");

        EXPECT_TRUE(identical);
    }

    TEST_CASE(Write_GivenDictionaryWithNewlinesInParameters_WriteSettingsFileWithNewlinesInParameters)
    {
        Dictionary dictionary;
        dictionary.insert("a", "aa");
        dictionary.insert("b", "bb\nbb\nbb");

        SettingsFileWriter writer;
        writer.write("unit tests/outputs/test_settings_settingsfilewithnewlinesinparameters.xml", dictionary);

        const bool identical =
            compare_text_files(
                "unit tests/inputs/test_settings_settingsfilewithnewlinesinparameters.xml",
                "unit tests/outputs/test_settings_settingsfilewithnewlinesinparameters.xml");

        EXPECT_TRUE(identical);
    }

    TEST_CASE(Write_GivenSettingsFileNameWithUTF8Encoding_ReturnsSuccess)
    {
#ifdef _WIN32
        if (!does_windows_support_utf8_code_page())
        {
            // todo: if the unit testing infrastructure made a logger available, we would issue a warning here.
            return;
        }
#endif

        const Dictionary dictionary;

        SettingsFileWriter writer;
        const bool succeeded =
            writer.write(u8"unit tests/outputs/test_settings_utf8_\u00e2\u00e9\u00ef\u00f4\u00f9.xml", dictionary);

        EXPECT_TRUE(succeeded);
    }
}
