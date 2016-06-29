
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
        set_environment_var(TestEnvironmentName, "");

        const SearchPaths searchpaths(TestEnvironmentName, ':');

        const string result = searchpaths.to_string(':');
        EXPECT_EQ("", result);
    }

    TEST_CASE(InitializeFromEnvironmentVariable_UnixSeparator)
    {
        set_environment_var(TestEnvironmentName, "/tmp:/usr/tmp:/var/local/tmp");

        const SearchPaths searchpaths(TestEnvironmentName, ':');

        const string result = searchpaths.to_string(':');
        EXPECT_EQ("/tmp:/usr/tmp:/var/local/tmp", result);
    }

    TEST_CASE(InitializeFromEnvironmentVariable_WindowsSeparator)
    {
        set_environment_var(TestEnvironmentName, "/tmp;/usr/tmp;/var/local/tmp");

        const SearchPaths searchpaths(TestEnvironmentName, ';');

        const string result = searchpaths.to_string(';');
        EXPECT_EQ("/tmp;/usr/tmp;/var/local/tmp", result);
    }

    TEST_CASE(Reset)
    {
        set_environment_var(TestEnvironmentName, "/tmp:/usr/tmp:/var/local/tmp");

        SearchPaths searchpaths(TestEnvironmentName, ':');
        searchpaths.set_root_path("/some/root/path");
        searchpaths.push_back("/home/username/appleseed");
        searchpaths.reset();

        const string result = searchpaths.to_string(':');
        EXPECT_EQ("/some/root/path:/tmp:/usr/tmp:/var/local/tmp", result);
    }

    TEST_CASE(SplitAndPushBack)
    {
        SearchPaths searchpaths;
        searchpaths.split_and_push_back("/tmp:/usr/tmp:/var/local/tmp", ':');

        const string result = searchpaths.to_string(':');
        EXPECT_EQ("/tmp:/usr/tmp:/var/local/tmp", result);
    }
}
