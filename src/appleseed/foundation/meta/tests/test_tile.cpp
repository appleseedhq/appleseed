
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
#include "foundation/image/color.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Image_Tile)
{
    struct Fixture
    {
        Tile m_tile;

        Fixture()
          : m_tile(2, 3, 4, PixelFormatFloat)
        {
        }
    };

    TEST_CASE_F(Properties, Fixture)
    {
        EXPECT_EQ(PixelFormatFloat, m_tile.get_pixel_format());
        EXPECT_EQ(2, m_tile.get_width());
        EXPECT_EQ(3, m_tile.get_height());
        EXPECT_EQ(4, m_tile.get_channel_count());
        EXPECT_EQ(6, m_tile.get_pixel_count());
        EXPECT_EQ(2 * 3 * 4 * 4, m_tile.get_size());
    }

    TEST_CASE_F(CopyConstructor, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.1f, 0.2f, 0.3f, 0.4f));
        m_tile.set_pixel(1, 0, Color4f(0.5f, 0.6f, 0.7f, 0.8f));
        m_tile.set_pixel(0, 1, Color4f(0.9f, 1.0f, 1.1f, 1.2f));
        m_tile.set_pixel(1, 1, Color4f(1.3f, 1.4f, 1.5f, 1.6f));
        m_tile.set_pixel(0, 2, Color4f(1.7f, 1.8f, 1.9f, 2.0f));
        m_tile.set_pixel(1, 2, Color4f(2.1f, 2.2f, 2.3f, 2.4f));

        const Tile copy(m_tile);

        Color4f c;
        copy.get_pixel(0, 0, c); EXPECT_EQ(Color4f(0.1f, 0.2f, 0.3f, 0.4f), c);
        copy.get_pixel(1, 0, c); EXPECT_EQ(Color4f(0.5f, 0.6f, 0.7f, 0.8f), c);
        copy.get_pixel(0, 1, c); EXPECT_EQ(Color4f(0.9f, 1.0f, 1.1f, 1.2f), c);
        copy.get_pixel(1, 1, c); EXPECT_EQ(Color4f(1.3f, 1.4f, 1.5f, 1.6f), c);
        copy.get_pixel(0, 2, c); EXPECT_EQ(Color4f(1.7f, 1.8f, 1.9f, 2.0f), c);
        copy.get_pixel(1, 2, c); EXPECT_EQ(Color4f(2.1f, 2.2f, 2.3f, 2.4f), c);
    }

    TEST_CASE_F(SetPixelGetPixel_FullPixel, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.3f, 0.5f, 0.7f, 0.9f));

        Color4f c;
        m_tile.get_pixel(0, 0, c);
        EXPECT_EQ(Color4f(0.3f, 0.5f, 0.7f, 0.9f), c);
    }

    TEST_CASE_F(SetPixel_PartialPixel_Array, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.3f, 0.5f, 0.7f, 0.9f));

        const float Components[3] = { 0.2f, 0.4f, 0.6f };
        m_tile.set_pixel(0, 0, Components, 3);

        Color4f c;
        m_tile.get_pixel(0, 0, c);
        EXPECT_EQ(Color4f(0.2f, 0.4f, 0.6f, 0.9f), c);
    }

    TEST_CASE_F(SetPixel_PartialPixel_Color, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.3f, 0.5f, 0.7f, 0.9f));

        m_tile.set_pixel(0, 0, Color3f(0.2f, 0.4f, 0.6f));

        Color4f c;
        m_tile.get_pixel(0, 0, c);
        EXPECT_EQ(Color4f(0.2f, 0.4f, 0.6f, 0.9f), c);
    }

    TEST_CASE_F(GetPixel_PartialPixel_Array, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.3f, 0.5f, 0.7f, 0.9f));

        float components[3];
        m_tile.get_pixel(0, 0, components, 3);

        EXPECT_EQ(0.3f, components[0]);
        EXPECT_EQ(0.5f, components[1]);
        EXPECT_EQ(0.7f, components[2]);
    }

    TEST_CASE_F(GetPixel_PartialPixel_Color, Fixture)
    {
        m_tile.set_pixel(0, 0, Color4f(0.3f, 0.5f, 0.7f, 0.9f));

        Color3f c;
        m_tile.get_pixel(0, 0, c);

        EXPECT_EQ(Color3f(0.3f, 0.5f, 0.7f), c);
    }

    TEST_CASE_F(Clear, Fixture)
    {
        const Color4f ClearColor(0.2f, 0.4f, 0.6f, 0.8f);
        m_tile.clear(ClearColor);

        Color4f c;
        m_tile.get_pixel(0, 0, c); EXPECT_EQ(ClearColor, c);
        m_tile.get_pixel(1, 0, c); EXPECT_EQ(ClearColor, c);
        m_tile.get_pixel(0, 1, c); EXPECT_EQ(ClearColor, c);
        m_tile.get_pixel(1, 1, c); EXPECT_EQ(ClearColor, c);
        m_tile.get_pixel(0, 2, c); EXPECT_EQ(ClearColor, c);
        m_tile.get_pixel(1, 2, c); EXPECT_EQ(ClearColor, c);
    }
}
