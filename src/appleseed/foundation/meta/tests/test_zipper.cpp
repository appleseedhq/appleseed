
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
#include "foundation/utility/test.h"
#include "foundation/utility/zipper.h"

// Boost headers
#include "boost/filesystem.hpp"

// Standard headers
#include <set>
#include <vector>

using namespace foundation;
using namespace std;
namespace bf = boost::filesystem;

TEST_SUITE(Foundation_Utility_Unzipper)
{
    const string valid_project = "unit tests/inputs/test_packed_project_valid.appleseedz";
    const string invalid_project = "unit tests/inputs/test_packed_project_invalid.appleseedz";

    TEST_CASE(UnzipTest) 
    {
        const string unpacked_dir = valid_project + ".unpacked";

        try 
        {
            unzip(valid_project, unpacked_dir);

            EXPECT_TRUE(bf::exists(unpacked_dir));
            EXPECT_FALSE(bf::is_empty(unpacked_dir));

            const string expected_files[] = 
            {
                "01 - lambertiannrdf - arealight.appleseed",
                "geometry/sphere.obj",
                "geometry/Box002.binarymesh",
                "geometry/GeoSphere001.binarymesh",
                "geometry/dirpole reference sphere.obj",
                "geometry/Box001.binarymesh",
                "geometry/plane.obj",
                "geometry/Sphere002.binarymesh",
                "geometry/Plane002.binarymesh",
                "geometry/cube.obj",
                "geometry/Plane001.binarymesh"
            };

            const set<string> actual_files = recursive_ls(unpacked_dir);

            for (size_t i = 0; i < 11; ++i) 
            {
                EXPECT_EQ(1, actual_files.count(expected_files[i]));
            }

            bf::remove_all(unpacked_dir);
        } 
        catch (exception e) 
        {
            bf::remove_all(unpacked_dir);
            throw e;
        }
    }

    TEST_CASE(IsZipFileSuccess)
    {
        EXPECT_TRUE(is_zip_file(valid_project.c_str()));
    }

    TEST_CASE(IsZipFileFailure)
    {
        EXPECT_FALSE(is_zip_file("unit tests/inputs/test_projectfilereader_configurationblocks.appleseed"));
    }

    TEST_CASE(FilesnamesWithExtensionOneFile)
    {
        const string extension = ".appleseed";
        const vector<string> appleseed_files = get_filenames_with_extension_from_zip(valid_project, extension);

        ASSERT_EQ(1, appleseed_files.size());
        EXPECT_EQ("01 - lambertiannrdf - arealight.appleseed", appleseed_files[0]);
    }

    TEST_CASE(FilesnamesWithExtensionSeveralFiles) 
    {
        const string extension = ".appleseed";
        const vector<string> appleseed_files = get_filenames_with_extension_from_zip(invalid_project, extension);

        EXPECT_EQ(4, appleseed_files.size());
    }
}
