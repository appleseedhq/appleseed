
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
            EXPECT_EQ(OSLParamTypeColor, parser.param_type());
            float r, g, b;
            parser.parse_three_values<float>(r, g, b, true);
            EXPECT_EQ(1.0f, r);
            EXPECT_EQ(1.0f, g);
            EXPECT_EQ(1.0f, b);
        }
        {
            ShaderParamParser parser("color 1.0 0.5 0.0");
            EXPECT_EQ(OSLParamTypeColor, parser.param_type());
            float r, g, b;
            parser.parse_three_values<float>(r, g, b, true);
            EXPECT_EQ(1.0f, r);
            EXPECT_EQ(0.5f, g);
            EXPECT_EQ(0.0f, b);
        }
    }

    TEST_CASE(ShaderParamParser1Value)
    {
        ShaderParamParser parser("float 1.0");
        EXPECT_EQ(OSLParamTypeFloat, parser.param_type());
        EXPECT_EQ(1.0f, parser.parse_one_value<float>());
    }

    TEST_CASE(ShaderParamParserString)
    {
        ShaderParamParser parser("string test_string");
        EXPECT_EQ("test_string", parser.parse_string_value());
    }

    TEST_CASE(ShaderParamParserEmptyString)
    {
        {
            ShaderParamParser parser("string");
            EXPECT_EQ("", parser.parse_string_value());
        }

        {
            ShaderParamParser parser("string ");
            EXPECT_EQ("", parser.parse_string_value());
        }

        {
            ShaderParamParser parser("string      ");
            EXPECT_EQ("", parser.parse_string_value());
        }
    }

    TEST_CASE(ShaderParamParserStringWithSpaces)
    {
        ShaderParamParser parser("string c:\\Some Windows Filename.tiff");
        EXPECT_EQ("c:\\Some Windows Filename.tiff", parser.parse_string_value());
    }

    TEST_CASE(ShaderParamParserFloatArray)
    {
        ShaderParamParser parser("float[] 1.0 2.0 3.0 4.0 5.0 6.0 7.0");
        EXPECT_EQ(OSLParamTypeFloatArray, parser.param_type());
        std::vector<float> values;
        parser.parse_float_array(values);
        EXPECT_EQ(7, values.size());
    }

    TEST_CASE(ShaderParamParserIntArray)
    {
        ShaderParamParser parser("int[] 1 2 3 4 5 6 7");
        EXPECT_EQ(OSLParamTypeIntArray, parser.param_type());
        std::vector<int> values;
        parser.parse_int_array(values);
        EXPECT_EQ(7, values.size());
    }

    TEST_CASE(ShaderParamParserEmptyFloatArray)
    {
        ShaderParamParser parser("float[] ");
        std::vector<float> values;

        EXPECT_EXCEPTION(ExceptionOSLParamParseError,
        {
            parser.parse_float_array(values);
        });
    }

    TEST_CASE(ShaderParamParserColorArray)
    {
        ShaderParamParser parser("color[] 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0");
        EXPECT_EQ(OSLParamTypeColorArray, parser.param_type());
        std::vector<float> values;
        parser.parse_float3_array(values);
        EXPECT_EQ(9, values.size());
    }

    TEST_CASE(ShaderParamParserVectorArrayWrongLenght)
    {
        ShaderParamParser parser("vector[] 1.0 2.0 3.0 4.0 5.0");
        std::vector<float> values;

        EXPECT_EXCEPTION(ExceptionOSLParamParseError,
        {
            parser.parse_float3_array(values);
        });
    }

    TEST_CASE(ShaderParamParserUnknownType)
    {
        EXPECT_EXCEPTION(ExceptionOSLParamParseError,
        {
            ShaderParamParser("unknown-type 77");
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

            EXPECT_EXCEPTION(ExceptionOSLParamParseError,
            {
                parser.parse_one_value<float>();
            });
        }
    }
}
