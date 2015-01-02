
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "globalsampleaccumulationbuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/sample.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/platform/thread.h"

using namespace foundation;
using namespace std;

namespace renderer
{

GlobalSampleAccumulationBuffer::GlobalSampleAccumulationBuffer(
    const size_t    width,
    const size_t    height,
    const Filter2f& filter)
  : m_fb(width, height, 3, filter)
  , m_filter_rcp_norm_factor(1.0f / compute_normalization_factor(filter))
{
}

void GlobalSampleAccumulationBuffer::clear()
{
    boost::mutex::scoped_lock lock(m_mutex);

    SampleAccumulationBuffer::clear_no_lock();

    m_fb.clear();
}

void GlobalSampleAccumulationBuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    boost::mutex::scoped_lock lock(m_mutex);

    const float fw = static_cast<float>(m_fb.get_width());
    const float fh = static_cast<float>(m_fb.get_height());
    const Sample* sample_end = samples + sample_count;

    for (const Sample* sample_ptr = samples; sample_ptr < sample_end; ++sample_ptr)
    {
        const float fx = sample_ptr->m_position.x * fw;
        const float fy = sample_ptr->m_position.y * fh;

        Color3f value(sample_ptr->m_values);
        value *= m_filter_rcp_norm_factor;

        m_fb.add(fx, fy, &value[0]);
    }
}

void GlobalSampleAccumulationBuffer::develop_to_frame(Frame& frame)
{
    boost::mutex::scoped_lock lock(m_mutex);

    Image& image = frame.image();
    const CanvasProperties& frame_props = image.properties();

    assert(frame_props.m_canvas_width == m_fb.get_width());
    assert(frame_props.m_canvas_height == m_fb.get_height());
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

void GlobalSampleAccumulationBuffer::increment_sample_count(const uint64 delta_sample_count)
{
    boost::mutex::scoped_lock lock(m_mutex);

    m_sample_count += delta_sample_count;
}

void GlobalSampleAccumulationBuffer::develop_to_tile(
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
            const float* ptr = m_fb.pixel(origin_x + x, origin_y + y);

            Color4f color(ptr[1], ptr[2], ptr[3], 1.0f);
            color.rgb() *= scale;

            tile.set_pixel(x, y, color);
        }
    }
}

}   // namespace renderer
