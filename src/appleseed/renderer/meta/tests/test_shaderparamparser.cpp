

//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/shadergroup/shaderparamparser.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Modeling_ShaderParamParser)
{
    TEST_CASE(ShaderParamParser3Values)
    {
        {
            ShaderParamParser parser("color 1.0");
            EXPECT_EQ(parser.param_type(), OSLParamTypeColor);
            float r, g, b;
            parser.parse_three_values<float>(r, g, b, true);
            EXPECT_EQ(r, 1.0f);
            EXPECT_EQ(g, 1.0f);
            EXPECT_EQ(b, 1.0f);
        }
        {
            ShaderParamParser parser("color 1.0 0.5 0.0");
            EXPECT_EQ(parser.param_type(), OSLParamTypeColor);
            float r, g, b;
            parser.parse_three_values<float>(r, g, b, true);
            EXPECT_EQ(r, 1.0f);
            EXPECT_EQ(g, 0.5f);
            EXPECT_EQ(b, 0.0f);
        }
    }

    TEST_CASE(ShaderParamParser1Value)
    {
        ShaderParamParser parser("float 1.0");
        EXPECT_EQ(parser.param_type(), OSLParamTypeFloat);
        EXPECT_EQ(parser.parse_one_value<float>(), 1.0f);
    }

    TEST_CASE(ShaderParamParserString)
    {
        ShaderParamParser parser("string test_string");
        EXPECT_EQ(parser.parse_string_value(), "test_string");
    }

    TEST_CASE(ShaderParamParserUnknownType)
    {
        EXPECT_EXCEPTION(ExceptionOSLParamParseError,
        {
            ShaderParamParser( "unknown-type 77");
        });
    }

    TEST_CASE(ShaderParamParserInvalidSyntax)
    {
        {
            ShaderParamParser parser("color 1.0 0.5");
            float r, g, b;

            EXPECT_EXCEPTION(ExceptionOSLParamParseError,
            {
                parser.parse_three_values<float>(r, g, b, true);
            });
        }

        {
            ShaderParamParser parser("vector 77.0 33.2");
            float x, y, z;

            EXPECT_EXCEPTION(ExceptionOSLParamParseError,
            {
                parser.parse_three_values<float>(x, y, z);
            });
        }

        {
            ShaderParamParser parser("float 1.0 0.5");
            float r, g, b;

            EXPECT_EXCEPTION(ExceptionOSLParamParseError,
            {
                parser.parse_one_value<float>();
            });
        }

        {
            ShaderParamParser parser( "string a b");

            EXPECT_EXCEPTION(ExceptionOSLParamParseError,
            {
                parser.parse_string_value();
            });
        }
    }
}
