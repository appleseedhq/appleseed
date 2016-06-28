
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstdlib>
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Utility_SearchPaths)
{
    void set_environment_var(const char* name, const char* value)
    {
#if defined _WIN32
        _putenv_s(name, value);
#else
        setenv(name, value, 1);
#endif
    }

    static const char* TestEnvironmentName = "APPLESEED_TEST_SEARCHPATH_SEARCHPATH";

    TEST_CASE(InitializeFromEmptyEnvironmentVariable)
    {
        const char* TestEnvironmentValue = "";

        set_environment_var(TestEnvironmentName, TestEnvironmentValue);
        SearchPaths searchpaths(TestEnvironmentName, SearchPaths::environment_path_separator());
        EXPECT_EQ(TestEnvironmentValue, searchpaths.to_string(SearchPaths::environment_path_separator()));
    }

    TEST_CASE(InitializeFromEnvironmentVariableUnixSeparator)
    {
        const char* TestEnvironmentValue = "/tmp:/usr/tmp:/var/local/tmp";
        const char TestSeparator = ':';

        set_environment_var(TestEnvironmentName, TestEnvironmentValue);
        SearchPaths searchpaths(TestEnvironmentName, TestSeparator);
        EXPECT_EQ(TestEnvironmentValue, searchpaths.to_string(TestSeparator));
    }

    TEST_CASE(InitializeFromEnvironmentVariableWindowsSeparator)
    {
        const char* TestEnvironmentValue = "/tmp;/usr/tmp;/var/local/tmp";
        const char TestSeparator = ';';

        set_environment_var(TestEnvironmentName, TestEnvironmentValue);
        SearchPaths searchpaths(TestEnvironmentName, TestSeparator);
        EXPECT_EQ(TestEnvironmentValue, searchpaths.to_string(TestSeparator));
    }

    TEST_CASE(SearchPathReset)
    {
        const char* TestEnvironmentValue = "/tmp:/usr/tmp:/var/local/tmp";
        const char* TestRootPath = "/some/root/path";
        const char TestSeparator = ':';

        set_environment_var(TestEnvironmentName, TestEnvironmentValue);
        SearchPaths searchpaths(TestEnvironmentName, TestSeparator);
        searchpaths.set_root_path(TestRootPath);
        searchpaths.push_back("/home/username/appleseed");
        searchpaths.reset();

        string ExpectedResult(TestRootPath);
        ExpectedResult += string(&TestSeparator, 1);
        ExpectedResult += TestEnvironmentValue;
        EXPECT_EQ(ExpectedResult, searchpaths.to_string(TestSeparator));
    }

    TEST_CASE(SearchPathSplitAndPushBack)
    {
        const char* TestPaths = "/tmp:/usr/tmp:/var/local/tmp";
        const char TestSeparator = ':';

        SearchPaths searchpaths;
        searchpaths.split_and_push_back(TestPaths, TestSeparator);
        EXPECT_EQ(TestPaths, searchpaths.to_string(TestSeparator));
    }
}
