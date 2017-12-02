
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/pixel.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

// OpenEXR headers.
#include "foundation/platform/_beginexrheaders.h"
#include "OpenEXR/half.h"
#include "foundation/platform/_endexrheaders.h"

using namespace foundation;

TEST_SUITE(Foundation_Image_Pixel)
{
    TEST_CASE(ConvertToFormat_HalfToUInt32)
    {
        const half input = 1.0f;

        uint32 output;
        Pixel::convert_to_format(
            &input, &input + 1,
            1,
            PixelFormatUInt32,
            &output,
            1);

        EXPECT_EQ(4294967295UL, output);
    }

    TEST_CASE(ConvertToFormat_FloatToUInt32)
    {
        const float input = 1.0f;

        uint32 output;
        Pixel::convert_to_format(
            &input, &input + 1,
            1,
            PixelFormatUInt32,
            &output,
            1);

        EXPECT_EQ(4294967295UL, output);
    }

    TEST_CASE(ConvertFromFormat_HalfToUInt32)
    {
        const half input = 1.0f;

        uint32 output;
        Pixel::convert_from_format(
            PixelFormatHalf,
            &input, &input + 1,
            1,
            &output,
            1);

        EXPECT_EQ(4294967295UL, output);
    }

    TEST_CASE(ConvertFromFormat_FloatToUInt32)
    {
        const float input = 1.0f;

        uint32 output;
        Pixel::convert_from_format(
            PixelFormatFloat,
            &input, &input + 1,
            1,
            &output,
            1);

        EXPECT_EQ(4294967295UL, output);
    }
}
