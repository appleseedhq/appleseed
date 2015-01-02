
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Image_Image)
{
    TEST_CASE(Constructor_CreatesBlankImage)
    {
        Image image(2, 1, 1, 1, 3, PixelFormatFloat);

        Color3f c00; image.tile(0, 0).get_pixel(0, 0, c00);
        Color3f c10; image.tile(1, 0).get_pixel(0, 0, c10);

        EXPECT_EQ(Color3f(0.0), c00);
        EXPECT_EQ(Color3f(0.0), c10);
    }

    TEST_CASE(CopyConstructor_GivenVirginSourceImage_CreatesBlankImage)
    {
        Image source(2, 1, 1, 1, 3, PixelFormatFloat);
        Image copy(source);

        Color3f c00; copy.tile(0, 0).get_pixel(0, 0, c00);
        Color3f c10; copy.tile(1, 0).get_pixel(0, 0, c10);

        EXPECT_EQ(Color3f(0.0), c00);
        EXPECT_EQ(Color3f(0.0), c10);
    }

    TEST_CASE(Clear_Given4x4ImageWith2x2Tiles_FillsImageWithGivenValue)
    {
        const Color3f Expected(42.0f);

        Image image(4, 4, 2, 2, 3, PixelFormatFloat);
        image.clear(Expected);

        const CanvasProperties& props = image.properties();

        for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
            {
                const Tile& tile = image.tile(tx, ty);
                const size_t tile_width = tile.get_width();
                const size_t tile_height = tile.get_height();

                for (size_t y = 0; y < tile_height; ++y)
                {
                    for (size_t x = 0; x < tile_width; ++x)
                    {
                        Color3f value;
                        tile.get_pixel(x, y, value);

                        EXPECT_EQ(Expected, value);
                    }
                }
            }
        }
    }
}
