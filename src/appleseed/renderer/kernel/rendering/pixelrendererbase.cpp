
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
#include "pixelrendererbase.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/aov/aovaccumulator.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// PixelRendererBase class implementation.
//

PixelRendererBase::PixelRendererBase()
  : m_invalid_pixel_count(0)
  , m_invalid_sample_count(0)
{
}

void PixelRendererBase::on_tile_begin(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    Tile&                       tile,
    TileStack&                  aov_tiles)
{
}

void PixelRendererBase::on_tile_end(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    Tile&                       tile,
    TileStack&                  aov_tiles)
{
}

void PixelRendererBase::on_pixel_begin(
    const Frame&                frame,
    const Vector2i&             pi,
    const Vector2i&             pt,
    const AABB2i&               tile_bbox,
    AOVAccumulatorContainer&    aov_accumulators)
{
    m_invalid_sample_count = 0;

    aov_accumulators.on_pixel_begin(pi);
}

void PixelRendererBase::on_pixel_end(
    const Frame&                frame,
    const Vector2i&             pi,
    const Vector2i&             pt,
    const AABB2i&               tile_bbox,
    AOVAccumulatorContainer&    aov_accumulators)
{
    aov_accumulators.on_pixel_end(pi);

    if (m_invalid_sample_count > 0)
    {
        static const size_t MaxWarningsPerThread = 5;

        ++m_invalid_pixel_count;

        if (m_invalid_pixel_count <= MaxWarningsPerThread)
        {
            RENDERER_LOG_WARNING(
                "%s sample%s at pixel (%d, %d) had NaN, negative or infinite components and %s ignored.",
                pretty_uint(m_invalid_sample_count).c_str(),
                m_invalid_sample_count > 1 ? "s" : "",
                pi.x, pi.y,
                m_invalid_sample_count > 1 ? "were" : "was");
        }
        else if (m_invalid_pixel_count == MaxWarningsPerThread + 1)
        {
            RENDERER_LOG_WARNING("more invalid samples found, omitting warning messages for brevity.");
        }
    }
}

void PixelRendererBase::signal_invalid_sample()
{
    ++m_invalid_sample_count;
}

}   // namespace renderer
