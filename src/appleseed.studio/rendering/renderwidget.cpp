
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

// Interface header.
#include "renderwidget.h"

// appleseed.studio headers.
#include "utility/qtiostreamop.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/nativedrawing.h"
#include "foundation/image/tile.h"
#include "foundation/utility/test.h"

// Qt headers.
#include <Qt>

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// RenderWidget class implementation.
//

namespace
{
    const int RefreshRate = 15;     // hertz
}

// Constructor.
RenderWidget::RenderWidget(
    const int       width,
    const int       height,
    QWidget*        parent)
  : QWidget(parent)
  , m_image(width, height, QImage::Format_RGB888)
{
    setFocusPolicy(Qt::StrongFocus);

    setFixedWidth(width);
    setFixedHeight(height);

    setAutoFillBackground(false);
    setAttribute(Qt::WA_OpaquePaintEvent, true);

    clear(Color4f(0.0f));
}

namespace
{
    int color_float_to_int(const float x)
    {
        return min(truncate<int>(saturate(x) * 256.0f), 255);
    }

    QColor make_qcolor(const Color4f& color)
    {
        return QColor(
            color_float_to_int(color[0]),
            color_float_to_int(color[1]),
            color_float_to_int(color[2]),
            color_float_to_int(color[3]));
    }

    FOUNDATION_TEST_SUITE(Studio_Rendering_RenderWidget_Details)
    {
        FOUNDATION_TEST_CASE(ColorFloatToInt_GivenMinusOne_ReturnsZero)
        {
            const int result = color_float_to_int(-1.0f);
            FOUNDATION_EXPECT_EQ(0, result);
        }

        FOUNDATION_TEST_CASE(ColorFloatToInt_GivenZero_ReturnsZero)
        {
            const int result = color_float_to_int(0.0f);
            FOUNDATION_EXPECT_EQ(0, result);
        }

        FOUNDATION_TEST_CASE(ColorFloatToInt_GivenOne_Returns255)
        {
            const int result = color_float_to_int(1.0f);
            FOUNDATION_EXPECT_EQ(255, result);
        }

        FOUNDATION_TEST_CASE(ColorFloatToInt_GivenTwo_Returns255)
        {
            const int result = color_float_to_int(2.0f);
            FOUNDATION_EXPECT_EQ(255, result);
        }

        FOUNDATION_TEST_CASE(MakeQColor_GivenColor4f_ReturnsQColor)
        {
            const QColor color = make_qcolor(Color4f(0.25f, 0.5f, 0.75f, 1.0f));
            FOUNDATION_EXPECT_EQ(QColor(64, 128, 192, 255), color);
        }
    }
}

void RenderWidget::clear(const Color4f& color)
{
    m_image_mutex.lock();

    m_image.fill(make_qcolor(color).rgba());

    m_image_mutex.unlock();
}

namespace
{
    inline uint8* get_image_pointer(
        QImage&         image,
        const size_t    x,
        const size_t    y)
    {
        uint8* scanline = static_cast<uint8*>(image.scanLine(y));
        return scanline + x * image.depth() / 8;
    }
}

void RenderWidget::highlight_region(
    const size_t    x,
    const size_t    y,
    const size_t    width,
    const size_t    height)
{
    m_image_mutex.lock();

    // Retrieve destination image information.
    const size_t dest_width = static_cast<size_t>(m_image.width());
    const size_t dest_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Clipping is not supported.
    assert(x < dest_width);
    assert(y < dest_height);
    assert(x + width <= dest_width);
    assert(y + height <= dest_height);

    // Get a pointer to the first destination pixel.
    uint8* dest = get_image_pointer(m_image, x, y);

    // Clear the tile in black.
    const uint8 Black[3] = { 0, 0, 0 };
    NativeDrawing::clear(
        dest,
        width,
        height,
        dest_stride,
        Black,
        sizeof(Black));

    // Draw a white rectangle around the tile.
    const uint8 White[3] = { 255, 255, 255 };
    NativeDrawing::draw_hline(
        dest,
        width,
        White,
        sizeof(White));
    NativeDrawing::draw_hline(
        dest + (height - 1) * dest_stride,
        width,
        White,
        sizeof(White));
    NativeDrawing::draw_vline(
        dest,
        dest_stride,
        height,
        White,
        sizeof(White));
    NativeDrawing::draw_vline(
        dest + (width - 1) * sizeof(White),
        dest_stride,
        height,
        White,
        sizeof(White));

    m_image_mutex.unlock();
}

void RenderWidget::blit_tile(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    m_image_mutex.lock();

    blit_tile_no_lock(frame, tile_x, tile_y);

    m_image_mutex.unlock();
}

void RenderWidget::blit_frame(
    const Frame&    frame)
{
    const CanvasProperties& frame_props = frame.properties();

    Tile float_tile_storage(
        frame_props.m_tile_width,
        frame_props.m_tile_height,
        frame_props.m_channel_count,
        PixelFormatFloat);

    Tile uint8_tile_storage(
        frame_props.m_tile_width,
        frame_props.m_tile_height,
        frame_props.m_channel_count,
        PixelFormatUInt8);

    m_image_mutex.lock();

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            blit_tile_no_lock(
                frame,
                tx,
                ty,
                float_tile_storage.get_storage(),
                uint8_tile_storage.get_storage());
        }
    }

    m_image_mutex.unlock();
}

void RenderWidget::blit_tile_no_lock(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y,
    uint8*          float_tile_storage,
    uint8*          uint8_tile_storage)
{
    // Retrieve the source tile.
    const Tile& tile = frame.tile(tile_x, tile_y);

    // Convert the tile to 32-bit floating point.
    Tile fp_rgb_tile(
        tile,
        PixelFormatFloat,
        float_tile_storage);

    // Transform the tile to the color space of the frame.
    frame.transform_tile_to_frame_color_space(fp_rgb_tile);

    // Convert the tile to 8-bit RGB for display.
    static const size_t ShuffleTable[] = { 0, 1, 2, Pixel::SkipChannel };
    const Tile uint8_rgb_tile(
        fp_rgb_tile,
        PixelFormatUInt8,
        ShuffleTable,
        uint8_tile_storage);

    // Retrieve destination image information.
    const size_t dest_width = static_cast<size_t>(m_image.width());
    const size_t dest_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Compute the coordinates of the first destination pixel.
    const CanvasProperties& frame_props = frame.properties();
    const size_t x = tile_x * frame_props.m_tile_width;
    const size_t y = tile_y * frame_props.m_tile_height;

    // Clipping is not supported.
    assert(x < dest_width);
    assert(y < dest_height);
    assert(x + tile.get_width() <= dest_width);
    assert(y + tile.get_height() <= dest_height);

    // Get a pointer to the first destination pixel.
    uint8* dest = get_image_pointer(m_image, x, y);

    // Blit the tile to the destination image.
    NativeDrawing::blit(dest, dest_stride, uint8_rgb_tile);
}

void RenderWidget::paintEvent(QPaintEvent* event)
{
    m_image_mutex.lock();

    m_painter.begin(this);
    m_painter.drawImage(rect(), m_image);
    m_painter.end();

    m_image_mutex.unlock();
}

}   // namespace studio
}   // namespace appleseed
