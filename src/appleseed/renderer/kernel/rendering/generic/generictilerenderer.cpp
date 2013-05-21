
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "generictilerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/math/ordering.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/breakpoint.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Generic tile renderer.
    //

#ifndef NDEBUG

    // Define this symbol to break execution into the debugger
    // when a specific pixel is about to be rendered.
    // #define DEBUG_BREAK_AT_PIXEL Vector2u(0, 0)

#endif

    class GenericTileRenderer
      : public ITileRenderer
    {
      public:
        GenericTileRenderer(
            const Frame&                frame,
            IPixelRendererFactory*      factory,
            const ParamArray&           params,
            const bool                  primary)
          : m_params(params)
          , m_pixel_renderer(factory->create(primary))
        {
            compute_tile_margins(frame, primary);
            compute_pixel_ordering(frame);
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void render_tile(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            AbortSwitch&                abort_switch) OVERRIDE
        {
            // Retrieve frame properties.
            const CanvasProperties& frame_properties = frame.image().properties();
            assert(tile_x < frame_properties.m_tile_count_x);
            assert(tile_y < frame_properties.m_tile_count_y);

            // Retrieve tile properties.
            Tile& tile = frame.image().tile(tile_x, tile_y);
            TileStack aov_tiles = frame.aov_images().tiles(tile_x, tile_y);
            const size_t aov_count = frame.aov_images().size();
            const size_t tile_origin_x = frame_properties.m_tile_width * tile_x;
            const size_t tile_origin_y = frame_properties.m_tile_height * tile_y;
            const size_t tile_width = tile.get_width();
            const size_t tile_height = tile.get_height();

            // Compute the bounding box in image space of the pixels that need to be rendered.
            AABB2u tile_bbox;
            tile_bbox.min.x = tile_origin_x;
            tile_bbox.min.y = tile_origin_y;
            tile_bbox.max.x = tile_origin_x + tile_width - 1;
            tile_bbox.max.y = tile_origin_y + tile_height - 1;
            if (m_params.m_crop)
                tile_bbox = AABB2u::intersect(tile_bbox, m_params.m_crop_window);
            if (!tile_bbox.is_valid())
                return;

            // Transform the bounding box to tile space.
            tile_bbox.min.x -= tile_origin_x;
            tile_bbox.min.y -= tile_origin_y;
            tile_bbox.max.x -= tile_origin_x;
            tile_bbox.max.y -= tile_origin_y;

            // Inform the pixel renderer that we are about to render a tile.
            m_pixel_renderer->on_tile_begin(frame, tile, aov_tiles);

            // Allocate the framebuffer into which we will accumulate the samples.
            ShadingResultFrameBuffer framebuffer(
                tile_width,
                tile_height,
                aov_count,
                tile_bbox,
                frame.get_filter());
            framebuffer.clear();

            // Seed the RNG with the tile index.
            m_rng = SamplingContext::RNGType(
                static_cast<uint32>(
                    tile_y * frame_properties.m_tile_count_x + tile_x));

            // Loop over tile pixels.
            const size_t tile_pixel_count = m_pixel_ordering.size();
            for (size_t i = 0; i < tile_pixel_count; ++i)
            {
                // Cancel any work done on this tile if rendering is aborted.
                if (abort_switch.is_aborted())
                    return;

                // Retrieve the coordinates of the pixel in the padded tile.
                const int tx = m_pixel_ordering[i].x;
                const int ty = m_pixel_ordering[i].y;

                // Skip pixels outside of the padded tile.
                if (tx >= static_cast<int>(tile_width + m_margin_width) ||
                    ty >= static_cast<int>(tile_height + m_margin_height))
                    continue;

                // Compute the coordinates of the pixel in the padded image.
                const int ix = static_cast<int>(tile_origin_x) + tx;
                const int iy = static_cast<int>(tile_origin_y) + ty;

#ifdef DEBUG_BREAK_AT_PIXEL

                // Break in the debugger when this pixel is reached.
                if (Vector2u(ix, iy) == DEBUG_BREAK_AT_PIXEL)
                    BREAKPOINT();

#endif

                // If cropping is enabled, skip pixels outside the crop window.
                if (m_params.m_crop && !m_params.m_crop_window.contains(Vector2u(ix, iy)))
                    continue;

                // Render this pixel.
                m_pixel_renderer->render_pixel(
                    frame,
                    tile,
                    aov_tiles,
                    tile_bbox,
                    ix, iy,
                    tx, ty,
                    m_rng,
                    framebuffer);
            }

            // Develop the framebuffer to the tile.
            if (frame.is_premultiplied_alpha())
                framebuffer.develop_to_tile_premult_alpha(tile, aov_tiles);
            else framebuffer.develop_to_tile_straight_alpha(tile, aov_tiles);

            // Inform the pixel renderer that we are done rendering the tile.
            m_pixel_renderer->on_tile_end(frame, tile, aov_tiles);
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return m_pixel_renderer->get_statistics();
        }

      protected:
        struct Parameters
        {
            bool    m_crop;
            AABB2u  m_crop_window;

            explicit Parameters(const ParamArray& params)
            {
                // Retrieve crop window parameter.
                m_crop = params.strings().exist("crop_window");
                if (m_crop)
                {
                    m_crop_window =
                        params.get_required<AABB2u>(
                            "crop_window",
                            AABB2u(Vector2u(0, 0), Vector2u(65535, 65535)));
                }
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<IPixelRenderer>    m_pixel_renderer;
        int                                 m_margin_width;
        int                                 m_margin_height;
        vector<Vector<int16, 2> >           m_pixel_ordering;
        SamplingContext::RNGType            m_rng;

        void compute_tile_margins(const Frame& frame, const bool primary)
        {
            m_margin_width = truncate<int>(ceil(frame.get_filter().get_xradius() - 0.5));
            m_margin_height = truncate<int>(ceil(frame.get_filter().get_yradius() - 0.5));

            const CanvasProperties& properties = frame.image().properties();
            const size_t padded_tile_width = properties.m_tile_width + 2 * m_margin_width;
            const size_t padded_tile_height = properties.m_tile_height + 2 * m_margin_height;
            const size_t padded_pixel_count = padded_tile_width * padded_tile_height;
            const size_t pixel_count = properties.m_tile_width * properties.m_tile_height;
            const size_t overhead_pixel_count = padded_pixel_count - pixel_count;
            const double wasted_effort = static_cast<double>(overhead_pixel_count) / pixel_count * 100.0;
            const double MaxWastedEffort = 15.0;    // percents

            if (primary)
            {
                RENDERER_LOG(
                    wasted_effort > MaxWastedEffort ? LogMessage::Warning : LogMessage::Info,
                    "rendering effort wasted by tile borders: %s (tile dimensions: %sx%s, tile margins: %sx%s)",
                    pretty_percent(overhead_pixel_count, pixel_count).c_str(),
                    pretty_uint(properties.m_tile_width).c_str(),
                    pretty_uint(properties.m_tile_height).c_str(),
                    pretty_uint(2 * m_margin_width).c_str(),
                    pretty_uint(2 * m_margin_height).c_str());
            }
        }

        void compute_pixel_ordering(const Frame& frame)
        {
            // Compute the dimensions in pixels of the padded tile.
            const CanvasProperties& properties = frame.image().properties();
            const size_t padded_tile_width = properties.m_tile_width + 2 * m_margin_width;
            const size_t padded_tile_height = properties.m_tile_height + 2 * m_margin_height;
            const size_t pixel_count = padded_tile_width * padded_tile_height;

            // Generate the pixel ordering inside the padded tile.
            vector<size_t> ordering;
            ordering.reserve(pixel_count);
            hilbert_ordering(ordering, padded_tile_width, padded_tile_height);
            assert(ordering.size() == pixel_count);

            // Convert the pixel ordering to (x, y) representation.
            m_pixel_ordering.resize(pixel_count);
            for (size_t i = 0; i < pixel_count; ++i)
            {
                const size_t x = ordering[i] % padded_tile_width;
                const size_t y = ordering[i] / padded_tile_width;
                assert(x < padded_tile_width);
                assert(y < padded_tile_height);
                m_pixel_ordering[i].x = static_cast<int16>(x) - m_margin_width;
                m_pixel_ordering[i].y = static_cast<int16>(y) - m_margin_height;
            }
        }
    };
}


//
// GenericTileRendererFactory class implementation.
//

GenericTileRendererFactory::GenericTileRendererFactory(
    const Frame&                frame,
    IPixelRendererFactory*      factory,
    const ParamArray&           params)
  : m_frame(frame)
  , m_factory(factory)
  , m_params(params)
{
}

void GenericTileRendererFactory::release()
{
    delete this;
}

ITileRenderer* GenericTileRendererFactory::create(const bool primary)
{
    return new GenericTileRenderer(m_frame, m_factory, m_params, primary);
}

ITileRenderer* GenericTileRendererFactory::create(
    const Frame&                frame,
    IPixelRendererFactory*      factory,
    const ParamArray&           params,
    const bool                  primary)
{
    return new GenericTileRenderer(frame, factory, params, primary);
}

}   // namespace renderer
