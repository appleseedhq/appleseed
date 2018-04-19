
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
    const size_t root_tile_x = tile_level == 0 ? tile_x : tile_x / 2;
    const size_t root_tile_y = tile_level == 0 ? tile_y : tile_y / 2;

    const size_t tile_count_x = frame.image().properties().m_tile_count_x;
    const size_t index = root_tile_y * tile_count_x + root_tile_x;

    size_t tile_width = frame.image().properties().m_tile_width;
    size_t tile_height = frame.image().properties().m_tile_height;
    
    if (m_framebuffers[index] == nullptr)
    {
        const int tile_origin_x = static_cast<int>(tile_width * root_tile_x);
        const int tile_origin_y = static_cast<int>(tile_height * root_tile_y);

        AABB2i root_tile_bbox;
        root_tile_bbox.min.x = tile_origin_x;
        root_tile_bbox.min.y = tile_origin_y;
        root_tile_bbox.max.x = tile_origin_x + static_cast<int>(tile_width) - 1;
        root_tile_bbox.max.y = tile_origin_y + static_cast<int>(tile_height) - 1;
        root_tile_bbox = AABB2i::intersect(root_tile_bbox, AABB2i(frame.get_crop_window()));

        root_tile_bbox.min.x -= tile_origin_x;
        root_tile_bbox.min.y -= tile_origin_y;
        root_tile_bbox.max.x -= tile_origin_x;
        root_tile_bbox.max.y -= tile_origin_y;

        m_framebuffers[index] =
            new ShadingResultFrameBuffer(
                tile_width,
                tile_height,
                frame.aov_images().size(),
                root_tile_bbox,
                frame.get_filter());

        m_framebuffers[index]->clear();
    }

    if (tile_level > 0)
    {
        const int x = tile_x % 2;
        const int y = tile_y % 2;
        const int sub_tile_index = x + 2 * y;

        return (ShadingResultFrameBuffer*)m_framebuffers[index]->m_sub_tiles[sub_tile_index];
    }

    return m_framebuffers[index];
}

void PermanentShadingResultFrameBufferFactory::destroy(
    ShadingResultFrameBuffer*   framebuffer)
{
}

}   // namespace renderer
