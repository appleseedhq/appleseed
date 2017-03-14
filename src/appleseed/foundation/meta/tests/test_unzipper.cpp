
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
#include "foundation/utility/unzipper.h"

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
    string valid_project = "unit tests/inputs/test_packed_project_valid.appleseedz";
    string invalid_project = "unit tests/inputs/test_packed_project_invalid.appleseedz";

    set<string> rec_ls(bf::path dir) 
    {
        set<string> files;

        // A default constructed directory_iterator acts as the end iterator
        bf::directory_iterator end_iter;
        for (bf::directory_iterator dir_itr(dir); dir_itr != end_iter; ++dir_itr) 
        {
            bf::path current_path = dir_itr->path();

            if (bf::is_directory(current_path))
            {
                set<string> files_in_subdir = rec_ls(current_path);
                files.insert(files_in_subdir.begin(), files_in_subdir.end());
            }
            else
                files.insert(current_path.filename().string());
        }

        return files;
    }

    TEST_CASE(UnzipTest) {
        string unpacked_dir = valid_project + ".unpacked";

        try 
        {
            unzip(valid_project, unpacked_dir);

            EXPECT_EQ(bf::exists(bf::path(unpacked_dir)), true);
            EXPECT_EQ(bf::is_empty(bf::path(unpacked_dir)), false);

            string expected_files[] = 
            {
                "01 - lambertiannrdf - arealight.appleseed",
                "sphere.obj",
                "Box002.binarymesh",
                "GeoSphere001.binarymesh",
                "dirpole reference sphere.obj",
                "Box001.binarymesh",
                "plane.obj",
                "Sphere002.binarymesh",
                "Plane002.binarymesh",
                "cube.obj",
                "Plane001.binarymesh"
            };

            set<string> actual_files = rec_ls(bf::path(unpacked_dir));

            for (int i = 0; i < 11; ++i) 
            {
                EXPECT_TRUE(actual_files.count(expected_files[i]));
            }

            bf::remove_all(bf::path(unpacked_dir));
        } 
        catch (exception e) 
        {
            bf::remove_all(bf::path(unpacked_dir));
            throw e;
        }
    }

    TEST_CASE(FilesnamesWithExtensionTest) 
    {
        string extension = ".appleseed";

        vector<string> appleseed_files_4 = get_filenames_with_extension_from_zip(invalid_project, extension);
        vector<string> appleseed_files_1 = get_filenames_with_extension_from_zip(valid_project, extension);

        EXPECT_EQ(appleseed_files_4.size(), 4);
        EXPECT_EQ(appleseed_files_1.size(), 1);

        EXPECT_EQ(appleseed_files_1[0], "01 - lambertiannrdf - arealight.appleseed");
    }
}
