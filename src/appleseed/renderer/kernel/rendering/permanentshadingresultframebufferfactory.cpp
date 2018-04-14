
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
#include "permanentshadingresultframebufferfactory.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"

using namespace foundation;

namespace renderer
{

void PermanentShadingResultFrameBufferFactory::release()
{
    delete this;
}

PermanentShadingResultFrameBufferFactory::PermanentShadingResultFrameBufferFactory(
    const Frame&                frame)
{
    const size_t tile_count_x = frame.image().properties().m_tile_count_x;
    const size_t tile_count_y = frame.image().properties().m_tile_count_y;

    m_framebuffers.resize(tile_count_x * tile_count_y, nullptr);
}

PermanentShadingResultFrameBufferFactory::~PermanentShadingResultFrameBufferFactory()
{
    for (size_t i = 0; i < m_framebuffers.size(); ++i)
        delete m_framebuffers[i];
}

ShadingResultFrameBuffer* PermanentShadingResultFrameBufferFactory::create(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    const size_t                tile_level,
    const AABB2u&               tile_bbox)
{
    size_t ax, ay;

    if (tile_level > 0)
    {
        ax = tile_x / 2;
        ay = tile_y / 2;
    }
    else
    {
        ax = tile_x;
        ay = tile_y;
    }

    const size_t tile_count_x = frame.image().properties().m_tile_count_x;
    const size_t index = ay * tile_count_x + ax;

    const Tile& tile = frame.image().tile(tile_x, tile_y, tile_level);

    if (m_framebuffers[index] == nullptr)
    {
        const int tile_origin_x = static_cast<int>(frame.image().properties().m_tile_width * tile_x);
        const int tile_origin_y = static_cast<int>(frame.image().properties().m_tile_height * tile_y);

        AABB2i base_tile_bbox;
        base_tile_bbox.min.x = tile_origin_x;
        base_tile_bbox.min.y = tile_origin_y;
        base_tile_bbox.max.x = tile_origin_x + static_cast<int>(frame.image().properties().m_tile_width) - 1;
        base_tile_bbox.max.y = tile_origin_y + static_cast<int>(frame.image().properties().m_tile_height) - 1;
        base_tile_bbox = AABB2i::intersect(base_tile_bbox, AABB2i(frame.get_crop_window()));

        base_tile_bbox.min.x -= tile_origin_x;
        base_tile_bbox.min.y -= tile_origin_y;
        base_tile_bbox.max.x -= tile_origin_x;
        base_tile_bbox.max.y -= tile_origin_y;

        m_framebuffers[index] =
            new ShadingResultFrameBuffer(
                tile.get_width() * (1 + tile_level),
                tile.get_height() * (1 + tile_level),
                frame.aov_images().size(),
                base_tile_bbox,
                frame.get_filter());

        m_framebuffers[index]->clear();
    }

    if (tile_level > 0)
    {
        int x = tile_x % 2;
        int y = tile_y % 2;

        int i = 2 * x + y;

        if (m_framebuffers[index]->m_sub_tiles == nullptr)
        {
            m_framebuffers[index]->m_sub_tiles = new Tile*[4];

            for (int i = 0; i < 4; ++i)
                m_framebuffers[index]->m_sub_tiles[i] = nullptr;
        }

        if (m_framebuffers[index]->m_sub_tiles[i] == nullptr)
        {
            m_framebuffers[index]->m_sub_tiles[i] =
                new ShadingResultFrameBuffer(
                    tile.get_width(),
                    tile.get_height(),
                    frame.aov_images().size(),
                    tile_bbox,
                    frame.get_filter());

            ((ShadingResultFrameBuffer*)(m_framebuffers[index]->m_sub_tiles[i]))->clear();
        }

        return (ShadingResultFrameBuffer*)m_framebuffers[index]->m_sub_tiles[i];
    }

    return m_framebuffers[index];
}

void PermanentShadingResultFrameBufferFactory::destroy(
    ShadingResultFrameBuffer*   framebuffer)
{
}

}   // namespace renderer
