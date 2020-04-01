
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

// Interface header.
#include "renderlayer.h"

// appleseed.renderer headers.
#include "renderer/api/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/nativedrawing.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"
#include "utility/gl.h"

// Qt headers.
#include <QColor>
#include <QDragEnterEvent>
#include <QDragMoveEvent>
#include <QDropEvent>
#include <QMimeData>
#include <QMutexLocker>
#include <QOpenGLFunctions_4_1_Core>
#include <QOpenGLTexture>
#include <QRect>
#include <Qt>

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdint>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// RenderLayer class implementation.
//

RenderLayer::RenderLayer(
    const std::size_t       width,
    const std::size_t       height,
    OCIO::ConstConfigRcPtr  ocio_config,
    QWidget*                parent)
  : QWidget(parent)
  , m_mutex(QMutex::Recursive)
  , m_ocio_config(ocio_config)
  , m_gl_initialized(false)
  , m_refresh_gl_texture(false)
{
    setFocusPolicy(Qt::StrongFocus);
    setFixedWidth(static_cast<int>(width));
    setFixedHeight(static_cast<int>(height));
    setAutoFillBackground(false);
    setAttribute(Qt::WA_OpaquePaintEvent, true);

    m_gl_tex = new QOpenGLTexture(QOpenGLTexture::Target2D);

    m_image =
        QImage(
            static_cast<int>(width),
            static_cast<int>(height),
            QImage::Format_RGB888);

    clear();

    const char* display_name = m_ocio_config->getDefaultDisplay();
    const char* default_transform = m_ocio_config->getDefaultView(display_name);
    set_display_transform(default_transform);

    setAcceptDrops(true);
}

void RenderLayer::draw(GLuint empty_vao, bool paths_display_active)
{
    QMutexLocker locker(&m_mutex);

    if (m_refresh_gl_texture)
    {
        m_gl_tex->destroy();
        m_gl_tex->setMinMagFilters(QOpenGLTexture::Linear, QOpenGLTexture::Nearest);
        m_gl_tex->setData(m_image, QOpenGLTexture::MipMapGeneration::DontGenerateMipMaps);
        m_refresh_gl_texture = false;
    }

    m_gl->glUseProgram(m_shader_program);

    GLfloat mult = paths_display_active ? 0.6 : 1.0;
    m_gl->glUniform1f(m_mult_loc, mult);

    m_gl->glActiveTexture(GL_TEXTURE0);
    m_gl_tex->bind();
    m_gl->glDisable(GL_DEPTH_TEST);
    m_gl->glDepthMask(GL_FALSE);
    m_gl->glBindVertexArray(empty_vao);
    m_gl->glDrawArrays(GL_TRIANGLES, 0, 3);
    m_gl->glDepthMask(GL_TRUE);
}

void RenderLayer::init_gl(QSurfaceFormat format)
{
    if (!m_gl)
    {
        RENDERER_LOG_ERROR("Attempted to initialize GL without first setting GL functions");
        return;
    }

    auto vertex_shader = load_gl_shader("fullscreen_tri.vert");
    auto fragment_shader = load_gl_shader("final_render.frag");

    m_shader_program = create_shader_program(
        m_gl,
        &vertex_shader,
        &fragment_shader);

    m_mult_loc = m_gl->glGetUniformLocation(m_shader_program, "u_mult");

    m_gl_initialized = true;
}

void RenderLayer::set_gl_functions(QOpenGLFunctions_4_1_Core* functions)
{
    m_gl = functions;
}

QImage RenderLayer::capture()
{
    QMutexLocker locker(&m_mutex);

    return m_image.copy();
}

void RenderLayer::darken()
{
    multiply(0.2f);
}

void RenderLayer::clear()
{
    QMutexLocker locker(&m_mutex);

    m_image.fill(QColor(0, 0, 0));
    m_image_storage.reset();

    m_refresh_gl_texture = true;
}

namespace
{
    inline std::uint8_t* get_image_pointer(QImage& image)
    {
        return static_cast<std::uint8_t*>(image.scanLine(0));
    }

    inline std::uint8_t* get_image_pointer(
        QImage&             image,
        const std::size_t   x,
        const std::size_t   y)
    {
        std::uint8_t* scanline = static_cast<std::uint8_t*>(image.scanLine(static_cast<int>(y)));
        return scanline + x * image.depth() / 8;
    }
}

void RenderLayer::start_render()
{
    // Clear the image storage.
    if (m_image_storage)
        m_image_storage->clear(Color4f(0.0f));
}

void RenderLayer::multiply(const float multiplier)
{
    QMutexLocker locker(&m_mutex);

    assert(multiplier >= 0.0f && multiplier <= 1.0f);

    const auto image_width = static_cast<std::size_t>(m_image.width());
    const auto image_height = static_cast<std::size_t>(m_image.height());
    const auto dest_stride = static_cast<std::size_t>(m_image.bytesPerLine());

    std::uint8_t* dest = get_image_pointer(m_image);

    for (std::size_t y = 0; y < image_height; ++y)
    {
        std::uint8_t* row = dest + y * dest_stride;

        for (std::size_t x = 0; x < image_width * 3; ++x)
            row[x] = truncate<std::uint8_t>(row[x] * multiplier);
    }

    m_refresh_gl_texture = true;
}

namespace
{
    void draw_bracket(
        std::uint8_t*           dest,
        const std::size_t       dest_width,
        const std::size_t       dest_height,
        const std::size_t       dest_stride,
        const std::size_t       bracket_extent,
        const std::uint8_t*     pixel,
        const std::size_t       pixel_size)
    {
        const int w = static_cast<int>(std::min(bracket_extent, dest_width));
        const int h = static_cast<int>(std::min(bracket_extent, dest_height));

        const std::size_t right = (dest_width - 1) * pixel_size;
        const std::size_t bottom = (dest_height - 1) * dest_stride;

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

void RenderLayer::highlight_tile(
    const Frame&        frame,
    const std::size_t   tile_x,
    const std::size_t   tile_y,
    const std::size_t   thread_index,
    const std::size_t   thread_count)
{
    QMutexLocker locker(&m_mutex);

    // Retrieve tile bounds.
    const Image& frame_image = frame.image();
    const CanvasProperties& frame_props = frame_image.properties();
    const std::size_t x = tile_x * frame_props.m_tile_width;
    const std::size_t y = tile_y * frame_props.m_tile_height;
    const Tile& tile = frame_image.tile(tile_x, tile_y);
    const std::size_t width = tile.get_width();
    const std::size_t height = tile.get_height();

    // Retrieve destination image information.
    APPLESEED_UNUSED const auto image_width = static_cast<std::size_t>(m_image.width());
    APPLESEED_UNUSED const auto image_height = static_cast<std::size_t>(m_image.height());
    const auto dest_stride = static_cast<std::size_t>(m_image.bytesPerLine());

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + width <= image_width);
    assert(y + height <= image_height);

    // Get a pointer to the first destination pixel.
    std::uint8_t* dest = get_image_pointer(m_image, x, y);

    // Choose one color per thread (see https://www.shadertoy.com/view/wlKXDm).
    std::uint8_t BracketColor[3];
    float t = TwoPi<float>() * (thread_index / static_cast<float>(thread_count));
    float cos_t = std::cos(t);
    float sin_t = std::sin(t);
    BracketColor[0] = 128 + static_cast<std::uint8_t>(127.5f * cos_t);
    BracketColor[1] = 128 + static_cast<std::uint8_t>(127.5f * (-0.5f * cos_t - 0.866f * sin_t));
    BracketColor[2] = 128 + static_cast<std::uint8_t>(127.5f * (-0.5f * cos_t + 0.866f * sin_t));

    // Draw a bracket around the tile.
    const std::size_t BracketExtent = 5;
    draw_bracket(
        dest,
        width,
        height,
        dest_stride,
        BracketExtent,
        BracketColor,
        sizeof(BracketColor));

    m_refresh_gl_texture = true;
}

void RenderLayer::blit_tile(
    const Frame&        frame,
    const std::size_t   tile_x,
    const std::size_t   tile_y)
{
    QMutexLocker locker(&m_mutex);

    allocate_working_storage(frame.image().properties());

    blit_tile_no_lock(frame, tile_x, tile_y);
    update_tile_no_lock(tile_x, tile_y);

    m_refresh_gl_texture = true;
}

void RenderLayer::blit_frame(const Frame& frame)
{
    QMutexLocker locker(&m_mutex);

    const CanvasProperties& frame_props = frame.image().properties();

    allocate_working_storage(frame_props);

    for (std::size_t y = 0; y < frame_props.m_tile_count_y; ++y)
    {
        for (std::size_t x = 0; x < frame_props.m_tile_count_x; ++x)
        {
            blit_tile_no_lock(frame, x, y);
            update_tile_no_lock(x, y);
        }
    }

    m_refresh_gl_texture = true;
}

void RenderLayer::set_display_transform(const QString& transform)
{
    QMutexLocker locker(&m_mutex);

    OCIO::DisplayTransformRcPtr transform_ptr = OCIO::DisplayTransform::Create();
    transform_ptr->setInputColorSpaceName(OCIO::ROLE_SCENE_LINEAR);
    transform_ptr->setDisplay(m_ocio_config->getDefaultDisplay());
    transform_ptr->setView(transform.toStdString().c_str());

    OCIO::ConstContextRcPtr context = m_ocio_config->getCurrentContext();
    m_ocio_processor = m_ocio_config->getProcessor(context, transform_ptr, OCIO::TRANSFORM_DIR_FORWARD);

    if (m_image_storage)
    {
        const CanvasProperties& frame_props = m_image_storage->properties();
        for (std::size_t y = 0; y < frame_props.m_tile_count_y; ++y)
        {
            for (std::size_t x = 0; x < frame_props.m_tile_count_x; ++x)
                update_tile_no_lock(x, y);
        }
    }
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

void RenderLayer::allocate_working_storage(const CanvasProperties& frame_props)
{
    if (!m_image_storage || !is_compatible(*m_image_storage, frame_props))
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

    if (!m_float_tile_storage || !is_compatible(*m_float_tile_storage, frame_props))
    {
        m_float_tile_storage.reset(
            new Tile(
                frame_props.m_tile_width,
                frame_props.m_tile_height,
                frame_props.m_channel_count,
                PixelFormatFloat));
    }

    if (!m_uint8_tile_storage || !is_compatible(*m_uint8_tile_storage, frame_props))
    {
        m_uint8_tile_storage.reset(
            new Tile(
                frame_props.m_tile_width,
                frame_props.m_tile_height,
                frame_props.m_channel_count,
                PixelFormatUInt8));
    }
}

void RenderLayer::blit_tile_no_lock(
    const Frame&        frame,
    const std::size_t   tile_x,
    const std::size_t   tile_y)
{
    // Retrieve the source tile.
    const Tile& src_tile = frame.image().tile(tile_x, tile_y);

    // Retrieve the dest tile and copy the pixels.
    Tile& dst_tile = m_image_storage->tile(tile_x, tile_y);
    dst_tile.copy_from(src_tile);
}

void RenderLayer::update_tile_no_lock(const std::size_t tile_x, const std::size_t tile_y)
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
    static const std::size_t shuffle_table[4] = { 0, 1, 2, Pixel::SkipChannel };
    Tile uint8_rgb_tile(
        float_tile,
        PixelFormatUInt8,
        shuffle_table,
        m_uint8_tile_storage->get_storage());

    // Retrieve destination image information.
    APPLESEED_UNUSED const auto image_width = static_cast<std::size_t>(m_image.width());
    APPLESEED_UNUSED const auto image_height = static_cast<std::size_t>(m_image.height());
    const auto dest_stride = static_cast<std::size_t>(m_image.bytesPerLine());

    // Compute the coordinates of the first destination pixel.
    const CanvasProperties& frame_props = m_image_storage->properties();
    const std::size_t x = tile_x * frame_props.m_tile_width;
    const std::size_t y = tile_y * frame_props.m_tile_height;

    // Clipping is not supported.
    assert(x < image_width);
    assert(y < image_height);
    assert(x + src_tile.get_width() <= image_width);
    assert(y + src_tile.get_height() <= image_height);

    // Get a pointer to the first destination pixel.
    std::uint8_t* dest = get_image_pointer(m_image, x, y);

    // Blit the tile to the destination image.
    NativeDrawing::blit(dest, dest_stride, uint8_rgb_tile);
}

}   // namespace studio
}   // namespace appleseed
