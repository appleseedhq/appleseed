
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

FOUNDATION_TEST_SUITE(Foundation_Image_Tile)
{
    const PixelFormat TilePixelFormat = PixelFormatFloat;
    const size_t TileWidth = 32;
    const size_t TileHeight = 32;
    const size_t TileChannels = 3;

    struct FixtureTile
    {
        Tile tile;

        FixtureTile()
          : tile(
                TileWidth,
                TileHeight,
                TileChannels,
                TilePixelFormat)
        {
        }
    };

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestProperties, FixtureTile)
    {
        FOUNDATION_EXPECT_EQ(TilePixelFormat, tile.get_pixel_format());
        FOUNDATION_EXPECT_EQ(TileWidth, tile.get_width());
        FOUNDATION_EXPECT_EQ(TileHeight, tile.get_height());
        FOUNDATION_EXPECT_EQ(TileChannels, tile.get_channel_count());
        FOUNDATION_EXPECT_EQ(TileWidth * TileHeight, tile.get_pixel_count());
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestSetAndGetPixel, FixtureTile)
    {
        const Color3f PixelColor(0.3f, 0.5f, 0.7f);
        tile.set_pixel(7, 9, PixelColor);

        Color3f c;
        tile.get_pixel(7, 9, c);

        FOUNDATION_EXPECT_FEQ(PixelColor, c);
    }

    FOUNDATION_TEST_CASE_WITH_FIXTURE(TestClear, FixtureTile)
    {
        const Color3f ClearColor(0.2f, 0.4f, 0.6f);
        tile.clear(ClearColor);

        Color3f c1, c2;
        tile.get_pixel(0, 0, c1);
        tile.get_pixel(TileWidth - 1, TileHeight - 1, c2);

        FOUNDATION_EXPECT_FEQ(ClearColor, c1);
        FOUNDATION_EXPECT_FEQ(ClearColor, c2);
    }
}
