
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cstdlib>
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Utility_SearchPaths)
{
    static const char* TestEnvVarName = "APPLESEED_TEST_SEARCHPATH";

#ifdef _WIN32

    void set_environment_var(const char* name, const char* value)
    {
#ifndef NDEBUG
        const errno_t result =
#endif
            _putenv_s(name, value);
        assert(result == 0);
    }

    TEST_CASE(Constructor_EmptyEnvironmentVariable)
    {
        set_environment_var(TestEnvVarName, "");

        const SearchPaths search_paths(TestEnvVarName, ';');

        const APIString result = search_paths.to_string(';');
        EXPECT_EQ("", to_string(result));
    }

    TEST_CASE(Constructor_NonEmptyEnvironmentVariable)
    {
        set_environment_var(TestEnvVarName, "C:\\Windows\\System32;C:\\Windows;C:\\Program Files");

        const SearchPaths search_paths(TestEnvVarName, ';');

        const APIString result = search_paths.to_string(';');
        EXPECT_EQ("C:\\Windows\\System32;C:\\Windows;C:\\Program Files", to_string(result));
    }

    TEST_CASE(ClearExplicitPaths)
    {
        set_environment_var(TestEnvVarName, "C:\\Windows\\System32;C:\\Windows;C:\\Program Files");

        SearchPaths search_paths(TestEnvVarName, ';');
        search_paths.set_root_path("C:\\Some\\Root\\Path");
        search_paths.push_back_explicit_path("C:\\Users\\UserName\\appleseed");

        search_paths.clear_explicit_paths();

        const APIString result = search_paths.to_string(';');
        EXPECT_EQ("C:\\Some\\Root\\Path;C:\\Windows\\System32;C:\\Windows;C:\\Program Files", to_string(result));
    }

    TEST_CASE(ToString)
    {
        SearchPaths search_paths;
        search_paths.push_back_explicit_path("C:\\Windows\\System32");
        search_paths.push_back_explicit_path("C:\\Windows");
        search_paths.push_back_explicit_path("C:\\Program Files");

        const APIString result = search_paths.to_string(';');

        EXPECT_EQ("C:\\Windows\\System32;C:\\Windows;C:\\Program Files", to_string(result));
    }

    TEST_CASE(ToStringReversed)
    {
        SearchPaths search_paths;
        search_paths.push_back_explicit_path("C:\\Windows\\System32");
        search_paths.push_back_explicit_path("C:\\Windows");
        search_paths.push_back_explicit_path("C:\\Program Files");

        const APIString result = search_paths.to_string_reversed(';');

        EXPECT_EQ("C:\\Program Files;C:\\Windows;C:\\Windows\\System32", to_string(result));
    }

#else

    void set_environment_var(const char* name, const char* value)
    {
#ifndef NDEBUG
        const int result =
#endif
            setenv(name, value, 1);
        assert(result == 0);
    }

    TEST_CASE(Constructor_EmptyEnvironmentVariable)
    {
        set_environment_var(TestEnvVarName, "");

        const SearchPaths search_paths(TestEnvVarName, ':');

        const APIString result = search_paths.to_string(':');
        EXPECT_EQ("", to_string(result));
    }

    TEST_CASE(Constructor_NonEmptyEnvironmentVariable)
    {
        set_environment_var(TestEnvVarName, "/tmp:/usr/tmp:/var/local/tmp");

        const SearchPaths search_paths(TestEnvVarName, ':');

        const APIString result = search_paths.to_string(':');
        EXPECT_EQ("/tmp:/usr/tmp:/var/local/tmp", to_string(result));
    }

    TEST_CASE(ClearExplicitPaths)
    {
        set_environment_var(TestEnvVarName, "/tmp:/usr/tmp:/var/local/tmp");

        SearchPaths search_paths(TestEnvVarName, ':');
        search_paths.set_root_path("/some/root/path");
        search_paths.push_back_explicit_path("/home/username/appleseed");

        search_paths.clear_explicit_paths();

        const APIString result = search_paths.to_string(':');
        EXPECT_EQ("/some/root/path:/tmp:/usr/tmp:/var/local/tmp", to_string(result));
    }

    TEST_CASE(ToString)
    {
        SearchPaths search_paths;
        search_paths.push_back_explicit_path("/tmp");
        search_paths.push_back_explicit_path("/usr/tmp");
        search_paths.push_back_explicit_path("/var/local/tmp");

        const APIString result = search_paths.to_string(':');

        EXPECT_EQ("/tmp:/usr/tmp:/var/local/tmp", to_string(result));
    }

    TEST_CASE(ToStringReversed)
    {
        SearchPaths search_paths;
        search_paths.push_back_explicit_path("/tmp");
        search_paths.push_back_explicit_path("/usr/tmp");
        search_paths.push_back_explicit_path("/var/local/tmp");

        const APIString result = search_paths.to_string_reversed(':');

        EXPECT_EQ("/var/local/tmp:/usr/tmp:/tmp", to_string(result));
    }

#endif
}
