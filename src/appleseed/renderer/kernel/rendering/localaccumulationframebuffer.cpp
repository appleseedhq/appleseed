
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
#include "localaccumulationframebuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/sample.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cassert>
#include <algorithm>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

// If defined, draw each pixel with a shade of gray proportional to the number of samples it contains.
#undef DEBUG_DISPLAY_SAMPLE_COUNT

LocalAccumulationFramebuffer::LocalAccumulationFramebuffer(
    const size_t    width,
    const size_t    height)
  : AccumulationFramebuffer(width, height)
{
    // todo: change to static_assert<>.
    assert(sizeof(AccumulationPixel) == 5 * sizeof(float));

    const size_t MinSize = 32;

    size_t level_width = width;
    size_t level_height = height;

    do
    {
        m_levels.push_back(
            new Tile(
                max(level_width, MinSize),
                max(level_height, MinSize),
                4 + 1,
                PixelFormatFloat));

        m_set_pixels.push_back(0);

        level_width /= 2;
        level_height /= 2;
    }
    while (level_width > MinSize && level_height > MinSize);

    clear();
}

LocalAccumulationFramebuffer::~LocalAccumulationFramebuffer()
{
    for (size_t i = 0; i < m_levels.size(); ++i)
        delete m_levels[i];
}

void LocalAccumulationFramebuffer::clear()
{
    Spinlock::ScopedLock lock(m_spinlock);

    AccumulationFramebuffer::clear_no_lock();

    for (size_t level = 0; level < m_levels.size(); ++level)
    {
        Tile* tile = m_levels[level];

        AccumulationPixel* pixel = reinterpret_cast<AccumulationPixel*>(tile->pixel(0));
        const size_t pixel_count = tile->get_pixel_count();

        for (size_t i = 0; i < pixel_count; ++i)
        {
            pixel[i].m_color.set(0.0f);
            pixel[i].m_count = 0;
        }

        m_set_pixels[level] = 0;
    }

    m_current_level = m_levels.size() - 1;
}

void LocalAccumulationFramebuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    Spinlock::ScopedLock lock(m_spinlock);

    const Sample* RESTRICT sample_ptr = samples;
    const Sample* RESTRICT sample_end = samples + sample_count;

    if (m_current_level == 0)
    {
        Tile* tile = m_levels[0];

        const double fb_width = static_cast<double>(tile->get_width());
        const double fb_height = static_cast<double>(tile->get_height());

        while (sample_ptr < sample_end)
        {
            const double fx = sample_ptr->m_position.x * fb_width;
            const double fy = sample_ptr->m_position.y * fb_height;
            const size_t x = truncate<size_t>(fx);
            const size_t y = truncate<size_t>(fy);

            AccumulationPixel* pixel =
                reinterpret_cast<AccumulationPixel*>(tile->pixel(x, y));

            pixel->m_color += sample_ptr->m_color;
            pixel->m_count += 1;

            if (pixel->m_count == 1)
                ++m_set_pixels[0];

            ++sample_ptr;
        }
    }
    else
    {
        while (sample_ptr < sample_end)
        {
            for (size_t level = 0; level <= m_current_level; ++level)
            {
                Tile* tile = m_levels[level];

                const double fx = sample_ptr->m_position.x * tile->get_width();
                const double fy = sample_ptr->m_position.y * tile->get_height();
                const size_t x = truncate<size_t>(fx);
                const size_t y = truncate<size_t>(fy);

                AccumulationPixel* pixel =
                    reinterpret_cast<AccumulationPixel*>(tile->pixel(x, y));

                pixel->m_color += sample_ptr->m_color;
                pixel->m_count += 1;

                if (pixel->m_count == 1)
                {
                    if (++m_set_pixels[level] == tile->get_pixel_count())
                    {
                        --m_current_level;
                        break;
                    }
                }
            }

            ++sample_ptr;
        }
    }

    m_sample_count += sample_count;
}

void LocalAccumulationFramebuffer::develop_to_frame(Frame& frame) const
{
    Image& image = frame.image();
    const CanvasProperties& frame_props = image.properties();

    assert(frame_props.m_canvas_width == m_width);
    assert(frame_props.m_canvas_height == m_height);
    assert(frame_props.m_channel_count == 4);

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            Tile& tile = image.tile(tx, ty);

            const size_t origin_x = tx * frame_props.m_tile_width;
            const size_t origin_y = ty * frame_props.m_tile_height;

            develop_to_tile(tile, origin_x, origin_y, tx, ty);
        }
    }
}

void LocalAccumulationFramebuffer::develop_to_tile(
    Tile&           tile,
    const size_t    origin_x,
    const size_t    origin_y,
    const size_t    tile_x,
    const size_t    tile_y) const
{
    const size_t display_level =
        m_current_level > 0 || m_set_pixels[0] < m_pixel_count
            ? min(m_current_level + 1, m_levels.size() - 1)
            : 0;

    const Tile* src_tile = m_levels[display_level];

    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
#ifdef DEBUG_DISPLAY_SAMPLE_COUNT

            const AccumulationPixel* pixel =
                reinterpret_cast<const AccumulationPixel*>(
                    m_tile->pixel(origin_x + x, origin_y + y));

            const float c = saturate(static_cast<float>(pixel->m_count) / 20.0f);

            tile.set_pixel(x, y, Color4f(c, c, c, 1.0f));

#else

            const size_t src_x = (origin_x + x) * src_tile->get_width() / m_width;
            const size_t src_y = (origin_y + y) * src_tile->get_height() / m_height;

            const AccumulationPixel* pixel =
                reinterpret_cast<const AccumulationPixel*>(
                    src_tile->pixel(src_x, src_y));

            const Color4f color =
                pixel->m_count > 0
                    ? pixel->m_color / static_cast<float>(pixel->m_count)
                    : Color4f(0.0f);

            tile.set_pixel(x, y, color);

#endif
        }
    }
}

}   // namespace renderer
