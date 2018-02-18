
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/seexpr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Utility_SeExpr_SeExprFilePathExtractor)
{
    TEST_CASE(ExtractPaths_GivenEmptyExpression_ReturnsZeroPath)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::PathCollection paths;

        extractor.extract_paths("", paths);

        EXPECT_EQ(0, paths.size());
    }

    TEST_CASE(ExtractPaths_GivenSimpleTextureExpression_ReturnsOnePath)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::PathCollection paths;

        extractor.extract_paths("texture(\"/path/to/file.exr\", $u, $v)", paths);

        ASSERT_EQ(1, paths.size());
        EXPECT_EQ("/path/to/file.exr", paths[0]);
    }

    TEST_CASE(ExtractPaths_GivenSimpleTextureExpressionWithExtraWhitespaces_ReturnsOnePath)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::PathCollection paths;

        extractor.extract_paths("texture( \"/path/to/file.exr\", $u, $v )", paths);

        ASSERT_EQ(1, paths.size());
        EXPECT_EQ("/path/to/file.exr", paths[0]);
    }

    TEST_CASE(ExtractPaths_GivenTextureExpressionEmbeddedInExpression_ReturnsOnePath)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::PathCollection paths;

        extractor.extract_paths("2*(1-texture( \"/path/to/file.exr\", $u, $v ))", paths);

        ASSERT_EQ(1, paths.size());
        EXPECT_EQ("/path/to/file.exr", paths[0]);
    }

    TEST_CASE(ExtractPaths_GivenTwoTextureExpressions_ReturnsTwoPaths)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::PathCollection paths;

        extractor.extract_paths("texture(\"/path/to/file1.exr\", $u, $v) * texture(\"/path/to/file2.exr\", $u, $v)", paths);

        ASSERT_EQ(2, paths.size());
        EXPECT_EQ("/path/to/file1.exr", paths[0]);
        EXPECT_EQ("/path/to/file2.exr", paths[1]);
    }

    TEST_CASE(ReplacePaths_GivenSimpleTextureExpression_ReplacesPath)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::MappingCollection mappings;
        mappings["/path/to/file.exr"] = "/bruce/lee.exr";

        const string result =
            extractor.replace_paths("texture(\"/path/to/file.exr\", $u, $v)", mappings);

        ASSERT_EQ("texture(\"/bruce/lee.exr\", $u, $v)", result);
    }

    TEST_CASE(ReplacePaths_GivenTwoTextureExpressionWithSamePaths_ReplacesBothPaths)
    {
        SeExprFilePathExtractor extractor;
        SeExprFilePathExtractor::MappingCollection mappings;
        mappings["/path/to/file.exr"] = "/bruce/lee.exr";

        const string result =
            extractor.replace_paths("texture(\"/path/to/file.exr\", $u, $v) * texture(\"/path/to/file.exr\", $u, $v)", mappings);

        ASSERT_EQ("texture(\"/bruce/lee.exr\", $u, $v) * texture(\"/bruce/lee.exr\", $u, $v)", result);
    }
}
