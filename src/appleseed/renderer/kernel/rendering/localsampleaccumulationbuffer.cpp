
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "localsampleaccumulationbuffer.h"

// appleseed.renderer headers.
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/filteredtile.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// LocalSampleAccumulationBuffer class implementation.
//
// The algorithm for progressive display deserves some explanations.  Here is how it works:
//
//   When the accumulation buffer is constructed, we create a stack of framebuffers of
//   decreasing resolution, much like a mipmap pyramid: each level of this pyramid is a
//   quarter of the resolution of the previous one (half the resolution in each dimension).
//   We don't actually go down all the way to the 1x1 level; instead we stop when we reach
//   a resolution that we consider provides a good balance between speed and usefulness.
//
//   At render-time, the store_samples() method pushes the individual samples through this
//   pyramid.  Samples are stored starting at the highest resolution level and up to what
//   we call the "active level", that is, the coarsest level of the pyramid that we're still
//   pushing samples to and the level that is displayed.  As soon as a level contains enough
//   samples, it becomes the new active level.
//

LocalSampleAccumulationBuffer::LocalSampleAccumulationBuffer(
    const size_t    width,
    const size_t    height,
    const Filter2f& filter)
{
    const size_t MinSize = 32;

    size_t level_width = width;
    size_t level_height = height;

    while (true)
    {
        m_levels.push_back(
            new FilteredTile(
                level_width,
                level_height,
                5,
                filter));

        if (level_width <= MinSize * 2 || level_height <= MinSize * 2)
            break;

        if (level_width > MinSize)
            level_width = max(level_width / 2, MinSize);

        if (level_height > MinSize)
            level_height = max(level_height / 2, MinSize);
    }

    m_remaining_pixels = new atomic<int32>[m_levels.size()];

    clear();
}

LocalSampleAccumulationBuffer::~LocalSampleAccumulationBuffer()
{
    delete[] m_remaining_pixels;

    for (size_t i = 0, e = m_levels.size(); i < e; ++i)
        delete m_levels[i];
}

void LocalSampleAccumulationBuffer::clear()
{
    // Request exclusive access.
    unique_lock<shared_mutex> lock(m_mutex);

    m_sample_count = 0;

    for (size_t i = 0, e = m_levels.size(); i < e; ++i)
    {
        m_levels[i]->clear();

        m_remaining_pixels[i] =
            static_cast<int32>(m_levels[i]->get_pixel_count());
    }

    m_active_level = static_cast<uint32>(m_levels.size() - 1);
}

void LocalSampleAccumulationBuffer::store_samples(
    const size_t    sample_count,
    const Sample    samples[])
{
    {
        // Request non-exclusive access.
        shared_lock<shared_mutex> lock(m_mutex);

        // Store samples at every level between the highest resolution level (index 0) to the active level.
        const Sample* sample_end = samples + sample_count;
        for (uint32 i = 0, e = m_active_level; i <= e; ++i)
        {
            FilteredTile* level = m_levels[i];
            const float level_width = static_cast<float>(level->get_width());
            const float level_height = static_cast<float>(level->get_height());

            for (const Sample* s = samples; s < sample_end; ++s)
            {
                const float fx = s->m_position.x * level_width;
                const float fy = s->m_position.y * level_height;
                level->add(fx, fy, s->m_values);
            }
        }
    }

    m_sample_count += sample_count;

    // Potentially update the new active level if we're not already at the highest resolution level.
    if (m_active_level > 0)
    {
        // Update pixel counters for all levels up to the active level.
        const int32 n = static_cast<int32>(sample_count);
        for (uint32 i = 0, e = m_active_level; i <= e; ++i)
            m_remaining_pixels[i].fetch_sub(n);

        while (true)
        {
            uint32 cur_active_level = m_active_level;
            uint32 new_active_level = cur_active_level;
            for (uint32 i = 0, e = cur_active_level; i < e; ++i)
            {
                if (m_remaining_pixels[i] <= 0)
                {
                    new_active_level = i;
                    break;
                }
            }

            if (new_active_level == cur_active_level)
                break;

            assert(new_active_level < cur_active_level);

            //if (m_active_level.compare_exchange_strong(cur_active_level, new_active_level))
            m_active_level.compare_exchange_strong(cur_active_level, new_active_level);

            break;
        }
    }
}

namespace
{
    void develop_to_tile(
        Tile&                   color_tile,
        Tile&                   depth_tile,
        const size_t            image_width,
        const size_t            image_height,
        const FilteredTile&     level,
        const size_t            origin_x,
        const size_t            origin_y,
        const AABB2u&           crop_window,
        const bool              undo_premultiplied_alpha)
    {
        const AABB2u r =
            AABB2u::intersect(
                AABB2u(
                    Vector2u(origin_x, origin_y),
                    Vector2u(origin_x + color_tile.get_width() - 1, origin_y + color_tile.get_height() - 1)),
                crop_window);

        if (undo_premultiplied_alpha)
        {
            for (size_t iy = r.min.y; iy <= r.max.y; ++iy)
            {
                const size_t level_width = level.get_width();
                const size_t src_y = (iy * level.get_height() / image_height) * level_width;
                const size_t dest_y = (iy - origin_y) * color_tile.get_width() - origin_x;

                for (size_t ix = r.min.x; ix <= r.max.x; ++ix)
                {
                    Color<float, 5> values;

                    level.get_pixel(
                        src_y + ix * level_width / image_width,
                        &values[0]);

                    const float rcp_alpha = values[3] == 0.0f ? 0.0f : 1.0f / values[3];
                    values[0] *= rcp_alpha;
                    values[1] *= rcp_alpha;
                    values[2] *= rcp_alpha;

                    color_tile.set_pixel<float>(dest_y + ix, &values[0]);
                    depth_tile.set_component(dest_y + ix, 0, values[4]);
                }
            }
        }
        else
        {
            for (size_t iy = r.min.y; iy <= r.max.y; ++iy)
            {
                const size_t level_width = level.get_width();
                const size_t src_y = (iy * level.get_height() / image_height) * level_width;
                const size_t dest_y = (iy - origin_y) * color_tile.get_width() - origin_x;

                for (size_t ix = r.min.x; ix <= r.max.x; ++ix)
                {
                    Color<float, 5> values;

                    level.get_pixel(
                        src_y + ix * level_width / image_width,
                        &values[0]);

                    color_tile.set_pixel<float>(dest_y + ix, &values[0]);
                    depth_tile.set_component(dest_y + ix, 0, values[4]);
                }
            }
        }
    }
}

void LocalSampleAccumulationBuffer::develop_to_frame(Frame& frame)
{
    // Request exclusive access.
    unique_lock<shared_mutex> lock(m_mutex);

    Image& color_image = frame.image();
    Image& depth_image = frame.aov_images().get_image(0);

    const CanvasProperties& frame_props = color_image.properties();
    assert(frame_props.m_canvas_width == m_levels[0]->get_width());
    assert(frame_props.m_canvas_height == m_levels[0]->get_height());
    assert(frame_props.m_channel_count == 4);

    const AABB2u& crop_window = frame.get_crop_window();
    const bool undo_premultiplied_alpha = !frame.is_premultiplied_alpha();

    const FilteredTile& level = *m_levels[m_active_level];

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            const size_t origin_x = tx * frame_props.m_tile_width;
            const size_t origin_y = ty * frame_props.m_tile_height;

            Tile& color_tile = color_image.tile(tx, ty);
            Tile& depth_tile = depth_image.tile(tx, ty);

            develop_to_tile(
                color_tile,
                depth_tile,
                frame_props.m_canvas_width,
                frame_props.m_canvas_height,
                level,
                origin_x, origin_y,
                crop_window,
                undo_premultiplied_alpha);
        }
    }
}

}   // namespace renderer
