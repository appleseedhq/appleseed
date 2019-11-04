
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
#include "debugtilerenderer.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstdint>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Debug tile renderer.
    //

    class DebugTileRenderer
      : public ITileRenderer
    {
      public:
        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
        }

        void render_tile(
            const Frame&            frame,
            const size_t            tile_x,
            const size_t            tile_y,
            const std::uint32_t     pass_hash,
            IAbortSwitch&           abort_switch) override
        {
            Image& image = frame.image();

            assert(tile_x < image.properties().m_tile_count_x);
            assert(tile_y < image.properties().m_tile_count_y);

            Tile& tile = image.tile(tile_x, tile_y);
            const size_t tile_width = tile.get_width();
            const size_t tile_height = tile.get_height();
            const size_t max_x = tile_width - 1;
            const size_t max_y = tile_height - 1;

            // Draw a pixel-sized checkerboard inside the tile.
            for (size_t y = 0; y < tile_height; ++y)
            {
                for (size_t x = 0; x < tile_width; ++x)
                {
                    const float gray = 0.6f + ((x + y) & 1) * 0.2f;
                    const Color4f pixel_color(gray, gray, gray, 1.0f);
                    tile.set_pixel(x, y, pixel_color);
                }
            }

            // Color the corners of the tile.
            tile.set_pixel(0,     0,     Color4f(1.0f, 0.0f, 0.0f, 1.0f));      // top left pixel is red
            tile.set_pixel(max_x, 0,     Color4f(0.0f, 1.0f, 0.0f, 1.0f));      // top right pixel is green
            tile.set_pixel(0,     max_y, Color4f(1.0f, 1.0f, 1.0f, 1.0f));      // bottom left pixel is white
            tile.set_pixel(max_x, max_y, Color4f(0.0f, 0.0f, 1.0f, 1.0f));      // bottom right pixel is blue
        }

        StatisticsVector get_statistics() const override
        {
            return StatisticsVector();
        }
    };
}


//
// DebugTileRendererFactory class implementation.
//

void DebugTileRendererFactory::release()
{
    delete this;
}

ITileRenderer* DebugTileRendererFactory::create(
    const size_t    thread_index)
{
    return new DebugTileRenderer();
}

}   // namespace renderer
