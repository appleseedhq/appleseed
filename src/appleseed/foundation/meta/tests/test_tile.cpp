
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Image_Tile)
{
    const PixelFormat TilePixelFormat = PixelFormatFloat;
    const size_t TileWidth = 2;
    const size_t TileHeight = 2;
    const size_t TileChannels = 3;

    struct Fixture
    {
        Tile m_tile;

        Fixture()
          : m_tile(TileWidth, TileHeight, TileChannels, TilePixelFormat)
        {
        }
    };

    TEST_CASE_F(TestProperties, Fixture)
    {
        EXPECT_EQ(TilePixelFormat, m_tile.get_pixel_format());
        EXPECT_EQ(TileWidth, m_tile.get_width());
        EXPECT_EQ(TileHeight, m_tile.get_height());
        EXPECT_EQ(TileChannels, m_tile.get_channel_count());
        EXPECT_EQ(TileWidth * TileHeight, m_tile.get_pixel_count());
    }

    TEST_CASE_F(TestCopyConstructor, Fixture)
    {
        const Color3f ClearColor(0.2f, 0.4f, 0.6f);
        m_tile.clear(ClearColor);

        const Tile copy(m_tile);

        Color3f c;
        copy.get_pixel(TileWidth - 1, TileHeight - 1, c);
        EXPECT_FEQ(ClearColor, c);
    }

    TEST_CASE_F(TestSetAndGetPixel, Fixture)
    {
        const Color3f PixelColor(0.3f, 0.5f, 0.7f);
        m_tile.set_pixel(0, 0, PixelColor);

        Color3f c;
        m_tile.get_pixel(0, 0, c);
        EXPECT_FEQ(PixelColor, c);
    }

    TEST_CASE_F(TestClear, Fixture)
    {
        const Color3f ClearColor(0.2f, 0.4f, 0.6f);
        m_tile.clear(ClearColor);

        Color3f c1;
        m_tile.get_pixel(0, 0, c1);
        EXPECT_FEQ(ClearColor, c1);

        Color3f c2;
        m_tile.get_pixel(TileWidth - 1, TileHeight - 1, c2);
        EXPECT_FEQ(ClearColor, c2);
    }
}
