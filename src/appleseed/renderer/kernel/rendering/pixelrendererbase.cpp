
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
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/otherwise.h"

using namespace foundation;

namespace renderer
{

//
// PixelRendererBase class implementation.
//

const uint8 NoState = 0;
const uint8 InvalidSample = 1;
const uint8 CorrectSample = 2;

PixelRendererBase::PixelRendererBase(
    const Frame&        frame,
    const size_t        thread_index,
    const ParamArray&   params)
  : m_params(params)
  , m_invalid_pixel_count(0)
  , m_invalid_sample_aov_index(~size_t(0))
{
    if (m_params.m_diagnostics)
    {
        m_invalid_sample_aov_index = frame.create_extra_aov_image("invalid_samples");

        if (m_invalid_sample_aov_index == ~size_t(0) && thread_index == 0)
        {
            RENDERER_LOG_WARNING(
                "could not create invalid samples aov, maximum number of aovs (" FMT_SIZE_T ") reached.",
                MaxAOVCount);
        }
    }
}

bool PixelRendererBase::are_diagnostics_enabled() const
{
    return m_params.m_diagnostics;
}

void PixelRendererBase::on_tile_begin(
    const Frame&            frame,
    Tile&                   tile,
    TileStack&              aov_tiles)
{
    if (m_invalid_sample_aov_index != ~size_t(0))
    {
        m_invalid_sample_diagnostic.reset(
            new Tile(tile.get_width(), tile.get_height(), 1, PixelFormatUInt8));
    }
}

void PixelRendererBase::on_tile_end(
    const Frame&            frame,
    Tile&                   tile,
    TileStack&              aov_tiles)
{
    if (m_invalid_sample_aov_index != ~size_t(0))
    {
        const size_t width = tile.get_width();
        const size_t height = tile.get_height();

        for (size_t y = 0; y < height; ++y)
        {
            for (size_t x = 0; x < width; ++x)
            {
                Color<uint8, 1> sample_state;
                m_invalid_sample_diagnostic->get_pixel(x, y, sample_state);

                Color4f color;

                switch (sample_state[0])
                {
                  case NoState:
                    color = Color4f(1.0f, 0.0f, 0.0f, 1.0f);
                    break;

                  case InvalidSample:
                    color = Color4f(1.0f, 0.0f, 1.0f, 1.0f);
                    break;

                  case CorrectSample:
                    tile.get_pixel(x, y, color);
                    color.rgb().set(0.2f * luminance(color.rgb()));     // 20% of luminance
                    color.a = 1.0f;
                    break;

                  assert_otherwise;
                }

                aov_tiles.set_pixel(x, y, m_invalid_sample_aov_index, color);
            }
        }
    }
}

void PixelRendererBase::on_pixel_begin(
    const Vector2i&             pi,
    const Vector2i&             pt,
    const AABB2i&               tile_bbox,
    AOVAccumulatorContainer&    aov_accumulators)
{
    m_invalid_sample_count = 0;
    aov_accumulators.on_pixel_begin(pi);
}

void PixelRendererBase::on_pixel_end(
    const Vector2i&             pi,
    const Vector2i&             pt,
    const AABB2i&               tile_bbox,
    AOVAccumulatorContainer&    aov_accumulators)
{
    aov_accumulators.on_pixel_end(pi);

    // todo: mark pixel as faulty in the diagnostic map.

    if (m_invalid_sample_count > 0)
    {
        ++m_invalid_pixel_count;

        const size_t MaxWarningsPerThread = 5;

        if (m_invalid_pixel_count <= MaxWarningsPerThread)
        {
            RENDERER_LOG_WARNING(
                FMT_SIZE_T " sample%s at pixel (%d, %d) had nan, negative or infinite components and %s ignored.",
                m_invalid_sample_count,
                m_invalid_sample_count > 1 ? "s" : "",
                pi.x, pi.y,
                m_invalid_sample_count > 1 ? "were" : "was");
        }
        else if (m_invalid_pixel_count == MaxWarningsPerThread + 1)
        {
            RENDERER_LOG_WARNING("more invalid samples found, omitting warning messages for brevity.");
        }
    }

    if (m_params.m_diagnostics && tile_bbox.contains(pt))
    {
        m_invalid_sample_diagnostic->set_pixel(pt.x, pt.y,
            m_invalid_sample_count > 0 ? &InvalidSample : &CorrectSample);
    }
}

void PixelRendererBase::signal_invalid_sample()
{
    ++m_invalid_sample_count;
}


//
// PixelRendererBaseFactory class implementation.
//

Dictionary PixelRendererBaseFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "enable_diagnostics",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Enable Diagnostics")
            .insert("help", "Enable pixel renderer diagnostics"));

    return metadata;
}

}   // namespace renderer
