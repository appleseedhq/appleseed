
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "globalaccumulationframebuffer.h"

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
#include <algorithm>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

GlobalAccumulationFramebuffer::GlobalAccumulationFramebuffer(
    const size_t    width,
    const size_t    height)
  : AccumulationFramebuffer(width, height)
{
    m_tile.reset(
        new Tile(
            m_width,
            m_height,
            3,
            PixelFormatFloat));

    clear();
}

void GlobalAccumulationFramebuffer::clear()
{
    mutex::scoped_lock lock(m_mutex);

    AccumulationFramebuffer::clear_no_lock();

    m_tile->clear(Color3f(0.0));
}

void GlobalAccumulationFramebuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    mutex::scoped_lock lock(m_mutex);

    const double fw = static_cast<double>(m_width);
    const double fh = static_cast<double>(m_height);
    const size_t max_x = m_width - 1;
    const size_t max_y = m_height - 1;

    const Sample* RESTRICT sample_ptr = samples;
    const Sample* RESTRICT sample_end = samples + sample_count;

    while (sample_ptr < sample_end)
    {
        const double fx = sample_ptr->m_position.x * fw;
        const double fy = sample_ptr->m_position.y * fh;

        const size_t x = min(truncate<size_t>(fx), max_x);
        const size_t y = min(truncate<size_t>(fy), max_y);

        add_pixel(x, y, sample_ptr->m_color.rgb());

        ++sample_ptr;
    }
}

void GlobalAccumulationFramebuffer::increment_sample_count(const uint64 delta_sample_count)
{
    mutex::scoped_lock lock(m_mutex);

    m_sample_count += delta_sample_count;
}

void GlobalAccumulationFramebuffer::develop_to_frame(Frame& frame) const
{
    Image& image = frame.image();
    const CanvasProperties& frame_props = image.properties();

    assert(frame_props.m_canvas_width == m_width);
    assert(frame_props.m_canvas_height == m_height);
    assert(frame_props.m_channel_count == 4);

    const float scale = 1.0f / m_sample_count;

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            Tile& tile = image.tile(tx, ty);

            const size_t x = tx * frame_props.m_tile_width;
            const size_t y = ty * frame_props.m_tile_height;

            develop_to_tile(tile, x, y, tx, ty, scale);
        }
    }
}

void GlobalAccumulationFramebuffer::develop_to_tile(
    Tile&           tile,
    const size_t    origin_x,
    const size_t    origin_y,
    const size_t    tile_x,
    const size_t    tile_y,
    const float     scale) const
{
    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    for (size_t y = 0; y < tile_height; ++y)
    {
        for (size_t x = 0; x < tile_width; ++x)
        {
            Color4f color;
            
            color.rgb() =
                get_pixel(
                    origin_x + x,
                    origin_y + y);

            color.rgb() *= scale;

            color.a = 1.0f;

            tile.set_pixel(x, y, color);
        }
    }
}

}   // namespace renderer
