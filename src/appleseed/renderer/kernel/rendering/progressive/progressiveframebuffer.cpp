
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
#include "progressiveframebuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/progressive/sample.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// ProgressiveFrameBuffer class implementation.
//

// Constructor.
ProgressiveFrameBuffer::ProgressiveFrameBuffer(
    const size_t    width,
    const size_t    height)
  : m_fb(width, height)
{
    allocate_mipmaps();

    clear();
}

// Destructor.
ProgressiveFrameBuffer::~ProgressiveFrameBuffer()
{
    deallocate_mipmaps();
}

void ProgressiveFrameBuffer::clear()
{
    mutex::scoped_lock lock(m_fb_mutex);

    m_fb.clear();

   m_sample_count = 0;

    m_timer_frequency = m_timer.frequency();

    m_last_time = m_timer.read();
    m_last_sample_count = 0;
}

void ProgressiveFrameBuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    mutex::scoped_lock lock(m_fb_mutex);

    for (size_t i = 0; i < sample_count; ++i)
    {
        const Sample& sample = samples[i];
        store_sample(sample);
    }

    m_sample_count += sample_count;
}

void ProgressiveFrameBuffer::render_to_frame(Frame& frame)
{
    mutex::scoped_lock lock(m_fb_mutex);

    const bool Resample = false;

    if (!Resample || m_fb.is_complete())
    {
        copy_to_frame(m_fb, frame);
    }
    else
    {
        render_to_frame_resample(frame);
    }

    print_statistics(frame);
}

void ProgressiveFrameBuffer::copy_to_frame(
    const AccumulationFrameBuffer&  fb,
    Frame&                          frame)
{
    const CanvasProperties& frame_props = frame.properties();

    assert(frame_props.m_canvas_width == fb.get_width());
    assert(frame_props.m_canvas_height == fb.get_height());
    assert(frame_props.m_channel_count == 4);

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            Tile& tile = frame.tile(tx, ty);

            const size_t x = tx * frame_props.m_tile_width;
            const size_t y = ty * frame_props.m_tile_height;

            copy_to_frame_tile(fb, tile, x, y, tx, ty);
        }
    }
}

void ProgressiveFrameBuffer::copy_to_frame_tile(
    const AccumulationFrameBuffer&  fb,
    Tile&                           tile,
    const size_t                    origin_x,
    const size_t                    origin_y,
    const size_t                    tile_x,
    const size_t                    tile_y)
{
    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
            const Color4f color =
                fb.get_pixel(
                    origin_x + x,
                    origin_y + y);

            tile.set_pixel(x, y, color);
        }
    }
}

void ProgressiveFrameBuffer::allocate_mipmaps()
{
    size_t width = m_fb.get_width();
    size_t height = m_fb.get_height();

    while (width && height)
    {
        m_mipmaps.push_back(
            new AccumulationFrameBuffer(
                max<size_t>(width, 1),
                max<size_t>(height, 1)));

        width /= 2;
        height /= 2;
    }
}

void ProgressiveFrameBuffer::deallocate_mipmaps()
{
    for (size_t i = 0; i < m_mipmaps.size(); ++i)
        delete m_mipmaps[i];

    m_mipmaps.clear();
}

inline void ProgressiveFrameBuffer::store_sample(const Sample& sample)
{
    const double fx = (0.5 + sample.m_position.x) * m_fb.get_width();
    const double fy = (0.5 - sample.m_position.y) * m_fb.get_height();

    const size_t x = truncate<size_t>(fx);
    const size_t y = truncate<size_t>(fy);

    m_fb.add_pixel(x, y, sample.m_color);
}

void ProgressiveFrameBuffer::render_to_frame_resample(Frame& frame) const
{
    AccumulationFrameBuffer::copy(m_fb, *m_mipmaps[0]);

    recursive_resample(0);

    copy_to_frame(*m_mipmaps[0], frame);
}

namespace
{
    void downsample(
        const AccumulationFrameBuffer&  source,
        AccumulationFrameBuffer&        destination)
    {
        const size_t dest_width = destination.get_width();
        const size_t dest_height = destination.get_height();

        for (size_t y = 0; y < dest_height; ++y)
        {
            for (size_t x = 0; x < dest_width; ++x)
            {
                const size_t sx0 = x * 2;
                const size_t sy0 = y * 2;
                const size_t sx1 = sx0 + 1;
                const size_t sy1 = sy0 + 1;

                size_t count = 0;

                Color4f c00; count += source.get_pixel(sx0, sy0, c00);
                Color4f c01; count += source.get_pixel(sx0, sy1, c01);
                Color4f c10; count += source.get_pixel(sx1, sy0, c10);
                Color4f c11; count += source.get_pixel(sx1, sy1, c11);

                if (count > 0)
                {
                    const Color4f avg = (c00 + c01 + c10 + c11) / static_cast<float>(count);
                    destination.set_pixel(x, y, avg);
                }
            }
        }
    }

    template <bool PreserveSetPixels>
    void upsample(
        const AccumulationFrameBuffer&  source,
        AccumulationFrameBuffer&        destination)
    {
        const size_t dest_width = destination.get_width();
        const size_t dest_height = destination.get_height();

        const float rcp_dest_width = 1.0f / dest_width;
        const float rcp_dest_height = 1.0f / dest_height;

        for (size_t y = 0; y < dest_height; ++y)
        {
            for (size_t x = 0; x < dest_width; ++x)
            {
                if (!PreserveSetPixels || !destination.is_set(x, y))
                {
                    const float normalized_x = static_cast<float>(x) * rcp_dest_width;
                    const float normalized_y = static_cast<float>(y) * rcp_dest_height;

                    const Color4f c =
                        source.get_pixel_bilinear(
                            normalized_x,
                            normalized_y);

                    destination.set_pixel(x, y, c);
                }
            }
        }
    }
}

void ProgressiveFrameBuffer::recursive_resample(const size_t level) const
{
    AccumulationFrameBuffer& current = *m_mipmaps[level];
    AccumulationFrameBuffer& next = *m_mipmaps[level + 1];

    next.clear();

    downsample(current, next);

    if (!next.is_complete() && (next.get_width() > 1 || next.get_height() > 1))
        recursive_resample(level + 1);

    upsample<true>(next, current);
}

void ProgressiveFrameBuffer::print_statistics(const Frame& frame)
{
    const uint64 time = m_timer.read();
    const uint64 elapsed_ticks = time - m_last_time;
    const double elapsed_seconds = static_cast<double>(elapsed_ticks) / m_timer_frequency;

    if (elapsed_seconds >= 1.0)
    {
        const size_t rendered_samples = m_sample_count - m_last_sample_count;

        const double average_luminance = frame.compute_average_luminance();

        RENDERER_LOG_INFO(
            "%s samples in the progressive framebuffer (%s samples/pixel, %s samples/second, "
            "average luminance is %s)",
            pretty_uint(m_sample_count).c_str(),
            pretty_ratio(m_sample_count, m_fb.get_pixel_count()).c_str(),
            pretty_ratio(static_cast<double>(rendered_samples), elapsed_seconds).c_str(),
            pretty_scalar(average_luminance, 6).c_str());

        m_last_sample_count = m_sample_count;
        m_last_time = time;
    }
}

}   // namespace renderer
