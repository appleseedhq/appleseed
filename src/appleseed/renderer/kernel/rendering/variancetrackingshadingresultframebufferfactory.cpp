
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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
#include "variancetrackingshadingresultframebufferfactory.h"

// appleseed.renderer headers.
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/variancetrackingshadingresultframebuffer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/tile.h"

using namespace foundation;

namespace renderer
{

void VarianceTrackingShadingResultFrameBufferFactory::release()
{
    delete this;
}

VarianceTrackingShadingResultFrameBufferFactory::VarianceTrackingShadingResultFrameBufferFactory(
    const Frame&                frame)
{
    const size_t tile_count_x = frame.image().properties().m_tile_count_x;
    const size_t tile_count_y = frame.image().properties().m_tile_count_y;

    m_framebuffers.resize(tile_count_x * tile_count_y, nullptr);
}

VarianceTrackingShadingResultFrameBufferFactory::~VarianceTrackingShadingResultFrameBufferFactory()
{
    for (size_t i = 0; i < m_framebuffers.size(); ++i)
        delete m_framebuffers[i];
}

ShadingResultFrameBuffer* VarianceTrackingShadingResultFrameBufferFactory::create(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    const AABB2u&               tile_bbox)
{
    const size_t tile_count_x = frame.image().properties().m_tile_count_x;
    const size_t index = tile_y * tile_count_x + tile_x;

    if (m_framebuffers[index] == nullptr)
    {
        const Tile& tile = frame.image().tile(tile_x, tile_y);

        m_framebuffers[index] =
            new VarianceTrackingShadingResultFrameBuffer(
                tile.get_width(),
                tile.get_height(),
                frame.aov_images().size(),
                tile_bbox);

        m_framebuffers[index]->clear();
    }

    return m_framebuffers[index];
}

void VarianceTrackingShadingResultFrameBufferFactory::destroy(
    ShadingResultFrameBuffer*   framebuffer)
{
}

void VarianceTrackingShadingResultFrameBufferFactory::clear()
{
    for (auto framebuffer : m_framebuffers)
    {
        if (framebuffer != nullptr)
            framebuffer->clear();
    }
}

size_t VarianceTrackingShadingResultFrameBufferFactory::get_total_channel_count(
    const size_t                aov_count) const
{
    return VarianceTrackingShadingResultFrameBuffer::get_total_channel_count(
        aov_count);
}

float VarianceTrackingShadingResultFrameBufferFactory::estimator_variance() const
{
    float variance = 0.0f;
    size_t buffer_count = 0;

    for (const auto framebuffer : m_framebuffers)
    {
        if (framebuffer == nullptr)
            continue;
            
        variance += framebuffer->estimator_variance();
        ++buffer_count;
    }

    return buffer_count == 0 ? 0 : variance / buffer_count;
}

float VarianceTrackingShadingResultFrameBufferFactory::estimator_variance_to_image(
    Image&                      image) const
{
    const size_t tile_count_x = image.properties().m_tile_count_x;
    const size_t tile_count_y = image.properties().m_tile_count_y;
    
    float variance = 0.0f;
    size_t buffer_count = 0;

    for (size_t tile_y = 0; tile_y < tile_count_y; ++tile_y)
        for (size_t tile_x = 0; tile_x < tile_count_x; ++tile_x)
        {
            Tile &tile = image.tile(tile_x, tile_y);
            const size_t index = tile_y * tile_count_x + tile_x;

            const VarianceTrackingShadingResultFrameBuffer* framebuffer =
                m_framebuffers[index];

            if (framebuffer == nullptr)
                continue;

            variance += framebuffer->estimator_variance_to_tile(tile);
            ++buffer_count;
        }

    return buffer_count == 0 ? 0 : variance / buffer_count;
}

}   // namespace renderer
