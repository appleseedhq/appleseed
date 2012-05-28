
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

TEST_SUITE(Foundation_Platform_Path)
{
    using namespace foundation;
    using namespace std;

    TEST_CASE(GetExecutablePath_GivenRunningApplication_ReturnsNonEmptyString)
    {
        const string executable_path = Path::get_executable_path();

        EXPECT_FALSE(executable_path.empty());
    }

    TEST_CASE(GetExecutableDirectory_GivenRunningApplication_ReturnsNonEmptyString)
    {
        const string executable_dir = Path::get_executable_directory();

        EXPECT_FALSE(executable_dir.empty());
    }
}

TEST_SUITE(Boost_Filesystem_Path)
{
    using namespace boost;

    TEST_CASE(Constructor_GivenPathWithFilenameEndingWithDot_ConstructsValidPath)
    {
        const filesystem::path path("/directory/filename.");

        EXPECT_EQ("filename.", path.filename());
        EXPECT_EQ("filename", path.stem());
        EXPECT_EQ(".", path.extension());
    }

    TEST_CASE(Constructor_GivenPathWithDirectoryEndingWithDot_ConstructsValidPath)
    {
        const filesystem::path path("/directory./");
        const filesystem::path parent = path.parent_path();

        EXPECT_EQ("directory.", parent.filename());
        EXPECT_EQ("directory", parent.stem());
        EXPECT_EQ(".", parent.extension());
    }
}
