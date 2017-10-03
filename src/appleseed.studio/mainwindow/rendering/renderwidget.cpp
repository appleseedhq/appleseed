
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

// Interface header.
#include "renderwidget.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/nativedrawing.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Qt headers.
#include <QColor>
#include <QMutexLocker>
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
    const size_t            width,
    const size_t            height,
    OCIO::ConstConfigRcPtr  ocio_config,
    QWidget*                parent)
  : QWidget(parent)
  , m_mutex(QMutex::Recursive)
  , m_ocio_config(ocio_config)
{
    setFocusPolicy(Qt::StrongFocus);
    setAutoFillBackground(false);
    setAttribute(Qt::WA_OpaquePaintEvent, true);

    resize(width, height);

    const char* display_name = m_ocio_config->getDefaultDisplay();
    const char* default_transform = m_ocio_config->getDefaultView(display_name);
    slot_display_transform_changed(default_transform);
}

QImage RenderWidget::get_image_copy() const
{
    QMutexLocker locker(&m_mutex);

    return m_image.copy();
}

void RenderWidget::resize(
    const size_t    width,
    const size_t    height)
{
    QMutexLocker locker(&m_mutex);

    setFixedWidth(static_cast<int>(width));
    setFixedHeight(static_cast<int>(height));

    m_image =
        QImage(
            static_cast<int>(width),
            static_cast<int>(height),
            QImage::Format_RGB888);

    clear();
}

void RenderWidget::clear()
{
    QMutexLocker locker(&m_mutex);

    m_image.fill(QColor(0, 0, 0));
    m_image_storage.reset();
}

namespace
{
    inline uint8* get_image_pointer(QImage& image)
    {
        return static_cast<uint8*>(image.scanLine(0));
    }

    inline uint8* get_image_pointer(
        QImage&         image,
        const size_t    x,
        const size_t    y)
    {
        uint8* scanline = static_cast<uint8*>(image.scanLine(static_cast<int>(y)));
        return scanline + x * image.depth() / 8;
    }
}

void RenderWidget::start_render()
{
    // Clear the image storage.
    if (m_image_storage.get())
        m_image_storage->clear(Color4f(0.0f));
}

void RenderWidget::multiply(const float multiplier)
{
    QMutexLocker locker(&m_mutex);

    assert(multiplier >= 0.0f && multiplier <= 1.0f);

    const size_t image_width = static_cast<size_t>(m_image.width());
    const size_t image_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    uint8* dest = get_image_pointer(m_image);

    for (size_t y = 0; y < image_height; ++y)
    {
        uint8* row = dest + y * dest_stride;

        for (size_t x = 0; x < image_width * 3; ++x)
            row[x] = truncate<uint8>(row[x] * multiplier);
    }
}

namespace
{
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

void RenderWidget::highlight_tile(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    QMutexLocker locker(&m_mutex);

    // Retrieve tile bounds.
    const Image& frame_image = frame.image();
    const CanvasProperties& frame_props = frame_image.properties();
    const size_t x = tile_x * frame_props.m_tile_width;
    const size_t y = tile_y * frame_props.m_tile_height;
    const Tile& tile = frame_image.tile(tile_x, tile_y);
    const size_t width = tile.get_width();
    const size_t height = tile.get_height();

    // Retrieve destination image information.
    APPLESEED_UNUSED const size_t image_width = static_cast<size_t>(m_image.width());
    APPLESEED_UNUSED const size_t image_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + width <= image_width);
    assert(y + height <= image_height);

    // Get a pointer to the first destination pixel.
    uint8* dest = get_image_pointer(m_image, x, y);

    // Draw a bracket around the tile.
    const size_t BracketExtent = 5;
    const uint8 BracketColor[3] = { 255, 255, 255 };
    draw_bracket(
        dest,
        width,
        height,
        dest_stride,
        BracketExtent,
        BracketColor,
        sizeof(BracketColor));
}

void RenderWidget::blit_tile(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    QMutexLocker locker(&m_mutex);

    allocate_working_storage(frame.image().properties());

    blit_tile_no_lock(frame, tile_x, tile_y);
    update_tile_no_lock(tile_x, tile_y);
}

void RenderWidget::blit_frame(const Frame& frame)
{
    QMutexLocker locker(&m_mutex);

    const CanvasProperties& frame_props = frame.image().properties();

    allocate_working_storage(frame_props);

    for (size_t y = 0; y < frame_props.m_tile_count_y; ++y)
    {
        for (size_t x = 0; x < frame_props.m_tile_count_x; ++x)
        {
            blit_tile_no_lock(frame, x, y);
            update_tile_no_lock(x, y);
        }
    }
}

void RenderWidget::slot_display_transform_changed(const QString& transform)
{
    {
        QMutexLocker locker(&m_mutex);

        OCIO::DisplayTransformRcPtr transform_ptr = OCIO::DisplayTransform::Create();
        transform_ptr->setInputColorSpaceName(OCIO::ROLE_SCENE_LINEAR);
        transform_ptr->setDisplay(m_ocio_config->getDefaultDisplay());
        transform_ptr->setView(transform.toStdString().c_str());

        OCIO::ConstContextRcPtr context = m_ocio_config->getCurrentContext();
        m_ocio_processor = m_ocio_config->getProcessor(context, transform_ptr, OCIO::TRANSFORM_DIR_FORWARD);

        if (m_image_storage.get())
        {
            const CanvasProperties& frame_props = m_image_storage->properties();
            for (size_t y = 0; y < frame_props.m_tile_count_y; ++y)
            {
                for (size_t x = 0; x < frame_props.m_tile_count_x; ++x)
                    update_tile_no_lock(x, y);
            }
        }
    }

    update();
}

namespace
{
    bool is_compatible(const Tile& tile, const CanvasProperties& props)
    {
        return
            tile.get_width() == props.m_tile_width &&
            tile.get_height() == props.m_tile_height &&
            tile.get_channel_count() == props.m_channel_count;
    }

    bool is_compatible(const Image& image, const CanvasProperties& props)
    {
        const CanvasProperties& image_props = image.properties();
        return
            image_props.m_tile_width == props.m_tile_width &&
            image_props.m_tile_height == props.m_tile_height &&
            image_props.m_channel_count == props.m_channel_count;
    }
}

void RenderWidget::allocate_working_storage(const CanvasProperties& frame_props)
{
    if (!m_image_storage.get() || !is_compatible(*m_image_storage.get(), frame_props))
    {
        m_image_storage.reset(
            new Image(
                frame_props.m_canvas_width,
                frame_props.m_canvas_height,
                frame_props.m_tile_width,
                frame_props.m_tile_height,
                frame_props.m_channel_count,
                PixelFormatFloat));
    }

    if (!m_float_tile_storage.get() || !is_compatible(*m_float_tile_storage.get(), frame_props))
    {
        m_float_tile_storage.reset(
            new Tile(
                frame_props.m_tile_width,
                frame_props.m_tile_height,
                frame_props.m_channel_count,
                PixelFormatFloat));
    }

    if (!m_uint8_tile_storage.get() || !is_compatible(*m_uint8_tile_storage.get(), frame_props))
    {
        m_uint8_tile_storage.reset(
            new Tile(
                frame_props.m_tile_width,
                frame_props.m_tile_height,
                frame_props.m_channel_count,
                PixelFormatUInt8));
    }
}

void RenderWidget::blit_tile_no_lock(
    const Frame&    frame,
    const size_t    tile_x,
    const size_t    tile_y)
{
    // Retrieve the source tile.
    const Tile& src_tile = frame.image().tile(tile_x, tile_y);

    // Retrieve the dest tile and copy the pixels.
    Tile& dst_tile = m_image_storage->tile(tile_x, tile_y);
    dst_tile.copy(src_tile);
}

void RenderWidget::update_tile_no_lock(const size_t tile_x, const size_t tile_y)
{
    // Retrieve the source tile.
    const Tile& src_tile = m_image_storage->tile(tile_x, tile_y);

    // Copy the tile.
    Tile float_tile(
        src_tile,
        PixelFormatFloat,
        m_float_tile_storage->get_storage());

    // Apply OCIO transform.
    OCIO::PackedImageDesc image_desc(
        reinterpret_cast<float*>(float_tile.get_storage()),
        static_cast<long>(float_tile.get_width()),
        static_cast<long>(float_tile.get_height()),
        static_cast<long>(float_tile.get_channel_count()));
    m_ocio_processor->apply(image_desc);

    // Convert the tile to 8-bit RGB for display.
    static const size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
    Tile uint8_rgb_tile(
        float_tile,
        PixelFormatUInt8,
        shuffle_table,
        m_uint8_tile_storage->get_storage());

    // Retrieve destination image information.
    APPLESEED_UNUSED const size_t image_width = static_cast<size_t>(m_image.width());
    APPLESEED_UNUSED const size_t image_height = static_cast<size_t>(m_image.height());
    const size_t dest_stride = static_cast<size_t>(m_image.bytesPerLine());

    // Compute the coordinates of the first destination pixel.
    const CanvasProperties& frame_props = m_image_storage->properties();
    const size_t x = tile_x * frame_props.m_tile_width;
    const size_t y = tile_y * frame_props.m_tile_height;

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + src_tile.get_width() <= image_width);
    assert(y + src_tile.get_height() <= image_height);

    // Get a pointer to the first destination pixel.
    uint8* dest = get_image_pointer(m_image, x, y);

    // Blit the tile to the destination image.
    NativeDrawing::blit(dest, dest_stride, uint8_rgb_tile);
}

void RenderWidget::paintEvent(QPaintEvent* event)
{
    QMutexLocker locker(&m_mutex);

    m_painter.begin(this);
    m_painter.drawImage(rect(), m_image);
    m_painter.end();
}

}   // namespace studio
}   // namespace appleseed
