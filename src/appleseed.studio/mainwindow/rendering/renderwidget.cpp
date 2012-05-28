
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "utility/interop.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/nativedrawing.h"
#include "foundation/image/tile.h"

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

void RenderWidget::clear(const Color4f& color)
{
    m_image_mutex.lock();
    m_image.fill(color_to_qcolor(color).rgba());
    m_image_mutex.unlock();
}

namespace
{
    inline uint8* get_image_pointer(
        QImage&         image,
        const size_t    x,
        const size_t    y)
    {
        uint8* scanline = static_cast<uint8*>(image.scanLine(static_cast<int>(y)));
        return scanline + x * image.depth() / 8;
    }

    void draw_bracket(
        uint8*          dest,
        const size_t    dest_width,
        const size_t    dest_height,
        const size_t    dest_stride,
        const size_t    bracket_extent,
        const uint8*    pixel,
        const size_t    pixel_size)
    {
        const int w = static_cast<int>(min(bracket_extent, dest_width));
        const int h = static_cast<int>(min(bracket_extent, dest_height));

        const size_t right = (dest_width - 1) * pixel_size;
        const size_t bottom = (dest_height - 1) * dest_stride;

        // Top-left corner.
        NativeDrawing::draw_hline(dest, w, pixel, pixel_size);
        NativeDrawing::draw_vline(dest, dest_stride, h, pixel, pixel_size);

        // Top-right corner.
        NativeDrawing::draw_hline(dest + right, -w, pixel, pixel_size);
        NativeDrawing::draw_vline(dest + right, dest_stride, h, pixel, pixel_size);

        // Bottom-left corner.
        NativeDrawing::draw_hline(dest + bottom, w, pixel, pixel_size);
        NativeDrawing::draw_vline(dest + bottom, dest_stride, -h, pixel, pixel_size);

        // Bottom-right corner.
        NativeDrawing::draw_hline(dest + bottom + right, -w, pixel, pixel_size);
        NativeDrawing::draw_vline(dest + bottom + right, dest_stride, -h, pixel, pixel_size);
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
    const size_t image_width = static_cast<size_t>(m_image.width());
    const size_t image_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + width <= image_width);
    assert(y + height <= image_height);

    // Get a pointer to the first destination pixel.
    uint8* dest = get_image_pointer(m_image, x, y);

/*
    // Clear the tile.
    const uint8 ClearColor[3] = { 0, 0, 0 };
    NativeDrawing::clear(
        dest,
        width,
        height,
        dest_stride,
        ClearColor,
        sizeof(ClearColor));
*/

    // Draw a bracket around the tile.
    const uint8 BracketColor[3] = { 255, 255, 255 };
    const size_t BracketExtent = 5;
    draw_bracket(
        dest,
        width,
        height,
        dest_stride,
        BracketExtent,
        BracketColor,
        sizeof(BracketColor));

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
    const CanvasProperties& frame_props = frame.image().properties();

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
    const Tile& tile = frame.image().tile(tile_x, tile_y);

    // Convert the tile to 32-bit floating point.
    Tile fp_rgb_tile(
        tile,
        PixelFormatFloat,
        float_tile_storage);

    // Transform the tile to the color space of the frame.
    frame.transform_to_output_color_space(fp_rgb_tile);

    // Convert the tile to 8-bit RGB for display.
    static const size_t ShuffleTable[] = { 0, 1, 2, Pixel::SkipChannel };
    const Tile uint8_rgb_tile(
        fp_rgb_tile,
        PixelFormatUInt8,
        ShuffleTable,
        uint8_tile_storage);

    // Retrieve destination image information.
    const size_t image_width = static_cast<size_t>(m_image.width());
    const size_t image_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Compute the coordinates of the first destination pixel.
    const CanvasProperties& frame_props = frame.image().properties();
    const size_t x = tile_x * frame_props.m_tile_width;
    const size_t y = tile_y * frame_props.m_tile_height;

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + tile.get_width() <= image_width);
    assert(y + tile.get_height() <= image_height);

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
