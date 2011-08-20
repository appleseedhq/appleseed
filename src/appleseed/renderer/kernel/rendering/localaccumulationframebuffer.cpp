
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

    m_tile.reset(
        new Tile(
            m_width,
            m_height,
            4 + 1,
            PixelFormatFloat));

    clear();
}

void LocalAccumulationFramebuffer::clear()
{
    Spinlock::ScopedLock lock(m_spinlock);

    AccumulationFramebuffer::clear_no_lock();

    AccumulationPixel* pixel =
        reinterpret_cast<AccumulationPixel*>(m_tile->pixel(0));

    for (size_t i = 0; i < m_pixel_count; ++i)
    {
        pixel[i].m_color.set(0.0f);
        pixel[i].m_count = 0;
    }
}

void LocalAccumulationFramebuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    Spinlock::ScopedLock lock(m_spinlock);

    const double fb_width = static_cast<double>(m_width);
    const double fb_height = static_cast<double>(m_height);

    const Sample* RESTRICT sample_ptr = samples;
    const Sample* RESTRICT sample_end = samples + sample_count;

    while (sample_ptr < sample_end)
    {
        const double fx = sample_ptr->m_position.x * fb_width;
        const double fy = sample_ptr->m_position.y * fb_height;

        const size_t x = truncate<size_t>(fx);
        const size_t y = truncate<size_t>(fy);

        add_pixel(x, y, sample_ptr->m_color);

        ++sample_ptr;
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

            const size_t x = tx * frame_props.m_tile_width;
            const size_t y = ty * frame_props.m_tile_height;

            develop_to_tile(tile, x, y, tx, ty);
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

            const Color4f color =
                get_pixel(
                    origin_x + x,
                    origin_y + y);

            tile.set_pixel(x, y, color);

#endif
        }
    }
}

}   // namespace renderer
