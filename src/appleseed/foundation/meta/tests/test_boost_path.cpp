
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
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <string>

namespace bf = boost::filesystem;

TEST_SUITE(Boost_Path)
{
    TEST_CASE(Constructor_GivenPathWithFilenameEndingWithDot_ConstructsValidPath)
    {
        const bf::path path("/directory/filename.");

        EXPECT_EQ("filename.", path.filename());
        EXPECT_EQ("filename", path.stem());
        EXPECT_EQ(".", path.extension());
    }

    TEST_CASE(Constructor_GivenPathWithDirectoryEndingWithDot_ConstructsValidPath)
    {
        const bf::path path("/directory./");
        const bf::path parent = path.parent_path();

        EXPECT_EQ("directory.", parent.filename());
        EXPECT_EQ("directory", parent.stem());
        EXPECT_EQ(".", parent.extension());
    }

    TEST_CASE(String_GivenPathWithSlashes)
    {
        const std::string s = bf::path("dir/file.txt").string();
    }

    TEST_CASE(String_GivenPathWithBackSlashes)
    {
        const std::string s = bf::path("dir\\file.txt").string();
    }

    TEST_CASE(Native_GivenPathWithSlashes)
    {
        const bf::path::string_type s = bf::path("dir/file.txt").native();
    }

    TEST_CASE(Native_GivenPathWithBackSlashes)
    {
        const bf::path::string_type s = bf::path("dir\\file.txt").native();
    }

    TEST_CASE(GenericString_GivenPathWithSlashes)
    {
        const std::string s = bf::path("dir/file.txt").generic_string();
    }

    TEST_CASE(GenericString_GivenPathWithBackSlashes)
    {
        const std::string s = bf::path("dir\\file.txt").generic_string();
    }

    TEST_CASE(MakePreferred_GivenPathWithSlashes)
    {
        const std::string s = bf::path("dir/file.txt").make_preferred().string();
    }

    TEST_CASE(MakePreferred_GivenPathWithBackSlashes)
    {
        const std::string s = bf::path("dir\\file.txt").make_preferred().string();
    }
}
