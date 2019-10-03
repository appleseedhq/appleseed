
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
#include "foundation/platform/path.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cassert>
#include <cstdio>
#include <string>

using namespace foundation;
namespace bf = boost::filesystem;

TEST_SUITE(Foundation_Platform_Path)
{
    TEST_CASE(GetExecutablePath_GivenRunningApplication_ReturnsNonEmptyString)
    {
        const std::string executable_path = get_executable_path();

        EXPECT_FALSE(executable_path.empty());
    }

    TEST_CASE(GetExecutableDirectory_GivenRunningApplication_ReturnsNonEmptyString)
    {
        const std::string executable_dir = get_executable_directory();

        EXPECT_FALSE(executable_dir.empty());
    }

    TEST_CASE(HasExtension_GivenEmptyPath_ReturnsFalse)
    {
        EXPECT_FALSE(has_extension(""));
    }

    TEST_CASE(HasExtension_GivenPathWithoutDot_ReturnsFalse)
    {
        EXPECT_FALSE(has_extension("foo"));
    }

    TEST_CASE(HasExtension_GivenEmptyEndingWithOneDot_ReturnsFalse)
    {
        EXPECT_FALSE(has_extension("foo."));
    }

    TEST_CASE(HasExtension_GivenEmptyEndingWithTwoDots_ReturnsFalse)
    {
        EXPECT_FALSE(has_extension("foo.."));
    }

    TEST_CASE(HasExtension_GivenPathWithExtension_ReturnsTrue)
    {
        EXPECT_TRUE(has_extension("foo.ext"));
    }

#ifdef _WIN32

    TEST_CASE(SplitPaths_GivenTwoEmptyPathes_ReturnsEmptyPath)
    {
        const bf::path p1("");
        const bf::path p2("");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("", common.make_preferred().string());
        EXPECT_EQ("", r1.make_preferred().string());
        EXPECT_EQ("", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenOneNonEmptyPathAndOneEmptyPath_ReturnsEmptyPath)
    {
        const bf::path p1("C:\\foo");
        const bf::path p2("");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("", common.make_preferred().string());
        EXPECT_EQ("C:\\foo", r1.make_preferred().string());
        EXPECT_EQ("", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenOneEmptyPathAndOneNonEmptyPath_ReturnsEmptyPath)
    {
        const bf::path p1("");
        const bf::path p2("C:\\foo");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("", common.make_preferred().string());
        EXPECT_EQ("", r1.make_preferred().string());
        EXPECT_EQ("C:\\foo", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenIdenticalPathsButDifferentFilenames_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar\\file1.ext");
        const bf::path p2("C:\\foo\\bar\\file2.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo\\bar", common.make_preferred().string());
        EXPECT_EQ("file1.ext", r1.make_preferred().string());
        EXPECT_EQ("file2.ext", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenIdenticalPathsAndFilenamesWithExtensions_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar\\file.ext");
        const bf::path p2("C:\\foo\\bar\\file.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo\\bar", common.make_preferred().string());
        EXPECT_EQ("file.ext", r1.make_preferred().string());
        EXPECT_EQ("file.ext", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenIdenticalPathsAndFilenamesWithoutExtensions_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar\\file");
        const bf::path p2("C:\\foo\\bar\\file");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo\\bar", common.make_preferred().string());
        EXPECT_EQ("file", r1.make_preferred().string());
        EXPECT_EQ("file", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenDifferentDriveLetters_ReturnsEmptyPath)
    {
        const bf::path p1("C:\\foo\\bar\\file.ext");
        const bf::path p2("D:\\foo\\bar\\file.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("", common.make_preferred().string());
        EXPECT_EQ("C:\\foo\\bar\\file.ext", r1.make_preferred().string());
        EXPECT_EQ("D:\\foo\\bar\\file.ext", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenPathsWithCommonPrefix_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar\\file.ext");
        const bf::path p2("C:\\foo\\bob\\file.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo", common.make_preferred().string());
        EXPECT_EQ("bar\\file.ext", r1.make_preferred().string());
        EXPECT_EQ("bob\\file.ext", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenPathsWithCommonPrefixWithExtensionsInDirectoryNames_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar.baz\\file.txt");
        const bf::path p2("C:\\foo\\bar.qux\\file.txt");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo", common.make_preferred().string());
        EXPECT_EQ("bar.baz\\file.txt", r1.make_preferred().string());
        EXPECT_EQ("bar.qux\\file.txt", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenPathsOfDifferentLengthWithCommonPrefix1_ReturnsCommonPath)
    {
        const bf::path p1("C:\\foo\\bar\\file.ext");
        const bf::path p2("C:\\foo\\file.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo", common.make_preferred().string());
        EXPECT_EQ("bar\\file.ext", r1.make_preferred().string());
        EXPECT_EQ("file.ext", r2.make_preferred().string());
    }

    TEST_CASE(SplitPaths_GivenPathsOfDifferentLengthWithCommonPrefix2)
    {
        const bf::path p1("C:\\foo\\file.ext");
        const bf::path p2("C:\\foo\\bar\\file.ext");

        bf::path common, r1, r2;
        split_paths(p1, p2, common, r1, r2);

        EXPECT_EQ("C:\\foo", common.make_preferred().string());
        EXPECT_EQ("file.ext", r1.make_preferred().string());
        EXPECT_EQ("bar\\file.ext", r2.make_preferred().string());
    }

#endif

    struct FindNextAvailablePathFixture
    {
        const bf::path m_base_output;

        FindNextAvailablePathFixture()
          : m_base_output(bf::absolute("unit tests/outputs/test_path/"))
        {
            bf::remove_all(m_base_output);
            bf::create_directory(m_base_output);
        }

        static void create_file(const bf::path& filepath)
        {
            FILE* f = fopen(filepath.string().c_str(), "w");
            assert(f);
            fclose(f);
        }
    };

    TEST_CASE_F(FindNextAvailablePath_GivenEmptyDirectory_ReturnsFileNumber1, FindNextAvailablePathFixture)
    {
        EXPECT_EQ("test1.txt", find_next_available_path(m_base_output / "test#.txt").filename().string());
    }

    TEST_CASE_F(FindNextAvailablePath_GivenFileNumber1Exists_ReturnsFileNumber2, FindNextAvailablePathFixture)
    {
        create_file(m_base_output / "test1.txt");

        EXPECT_EQ("test2.txt", find_next_available_path(m_base_output / "test#.txt").filename().string());
    }

    TEST_CASE_F(FindNextAvailablePath_GivenFilesNumber1To9Exist_ReturnsFileNumber1, FindNextAvailablePathFixture)
    {
        create_file(m_base_output / "test1.txt");
        create_file(m_base_output / "test2.txt");
        create_file(m_base_output / "test3.txt");
        create_file(m_base_output / "test4.txt");
        create_file(m_base_output / "test5.txt");
        create_file(m_base_output / "test6.txt");
        create_file(m_base_output / "test7.txt");
        create_file(m_base_output / "test8.txt");
        create_file(m_base_output / "test9.txt");

        EXPECT_EQ("test1.txt", find_next_available_path(m_base_output / "test#.txt").filename().string());
    }
}
