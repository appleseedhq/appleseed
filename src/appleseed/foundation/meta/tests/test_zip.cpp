
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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
#include "foundation/utility/countof.h"
#include "foundation/utility/test.h"
#include "foundation/utility/zip.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <algorithm>
#include <set>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

TEST_SUITE(Foundation_Utility_Zip)
{
    TEST_CASE(Unzip)
    {
        const string TargetDirectory = "unit tests/outputs/test_zip/";

        try
        {
            ASSERT_FALSE(bf::exists(TargetDirectory));

            unzip("unit tests/inputs/test_zip_validzipfile.zip", TargetDirectory);

            EXPECT_TRUE(bf::exists(TargetDirectory));
            EXPECT_FALSE(bf::is_empty(TargetDirectory));

            const string ExpectedFiles[] =
            {
                "subfolder/a.txt",
                "subfolder/b.txt",
                "c.txt",
                "d.png"
            };

            const set<string> actual_files = recursive_ls(TargetDirectory);

            for (size_t i = 0; i < countof(ExpectedFiles); ++i)
                EXPECT_EQ(1, actual_files.count(ExpectedFiles[i]));

            bf::remove_all(TargetDirectory);
        }
        catch (const exception& e)
        {
            bf::remove_all(TargetDirectory);
            throw e;
        }
    }

    TEST_CASE(ZipUnzipRoundtrip)
    {
        const string InitialDirectory = "unit tests/inputs/test_zip";
        const string TargetZip = "unit tests/outputs/test_zip.zip";
        const string TargetDirectory = "unit tests/outputs/test_zip";

        try
        {
            ASSERT_TRUE(bf::exists(InitialDirectory));
            ASSERT_FALSE(bf::exists(TargetZip));
            ASSERT_FALSE(bf::exists(TargetDirectory));

            zip(TargetZip, InitialDirectory);
            unzip(TargetZip, TargetDirectory);

            const set<string> expected_files = recursive_ls(InitialDirectory);
            const set<string> actual_files = recursive_ls(TargetDirectory);

            ASSERT_EQ(expected_files.size(), actual_files.size());

            for (set<string>::iterator it = actual_files.begin(); it != actual_files.end(); ++it)
                EXPECT_EQ(1, expected_files.count(*it));

            bf::remove(TargetZip);
            bf::remove_all(TargetDirectory);
        }
        catch (const exception& e)
        {
            bf::remove(TargetZip);
            bf::remove_all(TargetDirectory);
            throw e;
        }
    }

    TEST_CASE(IsZipFile_GivenValidZipFile_ReturnsTrue)
    {
        EXPECT_TRUE(is_zip_file("unit tests/inputs/test_zip_validzipfile.zip"));
    }

    TEST_CASE(IsZipFile_GivenInvalidZipFile_ReturnsFalse)
    {
        EXPECT_FALSE(is_zip_file("unit tests/inputs/test_zip_invalidzipfile.zip"));
    }

    TEST_CASE(GetFilenamesWithExtensionFromZip)
    {
        const vector<string> files =
            get_filenames_with_extension_from_zip(
                "unit tests/inputs/test_zip_validzipfile.zip",
                ".txt");

        ASSERT_EQ(3, files.size());

        EXPECT_TRUE(find(files.begin(), files.end(), "subfolder/a.txt") != files.end());
        EXPECT_TRUE(find(files.begin(), files.end(), "subfolder/b.txt") != files.end());
        EXPECT_TRUE(find(files.begin(), files.end(), "c.txt")           != files.end());
    }
}
