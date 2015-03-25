
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
#include "generictilerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/math/hash.h"
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
    // #define DEBUG_BREAK_AT_PIXEL Vector2i(0, 0)

#endif

    class GenericTileRenderer
      : public ITileRenderer
    {
      public:
        GenericTileRenderer(
            const Frame&                        frame,
            IPixelRendererFactory*              pixel_renderer_factory,
            IShadingResultFrameBufferFactory*   framebuffer_factory,
            const ParamArray&                   params,
            const size_t                        thread_index)
          : m_pixel_renderer(pixel_renderer_factory->create(thread_index))
          , m_framebuffer_factory(framebuffer_factory)
        {
            compute_tile_margins(frame, thread_index == 0);
            compute_pixel_ordering(frame);
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void render_tile(
            const Frame&    frame,
            const size_t    tile_x,
            const size_t    tile_y,
            const size_t    pass_hash,
            IAbortSwitch&   abort_switch) APPLESEED_OVERRIDE
        {
            // Retrieve frame properties.
            const CanvasProperties& frame_properties = frame.image().properties();
            assert(tile_x < frame_properties.m_tile_count_x);
            assert(tile_y < frame_properties.m_tile_count_y);

            // Retrieve tile properties.
            Tile& tile = frame.image().tile(tile_x, tile_y);
            TileStack aov_tiles = frame.aov_images().tiles(tile_x, tile_y);
            const size_t tile_origin_x = frame_properties.m_tile_width * tile_x;
            const size_t tile_origin_y = frame_properties.m_tile_height * tile_y;

            // Compute the image space bounding box of the pixels to render.
            AABB2u tile_bbox;
            tile_bbox.min.x = tile_origin_x;
            tile_bbox.min.y = tile_origin_y;
            tile_bbox.max.x = tile_origin_x + tile.get_width() - 1;
            tile_bbox.max.y = tile_origin_y + tile.get_height() - 1;
            tile_bbox = AABB2u::intersect(tile_bbox, frame.get_crop_window());
            if (!tile_bbox.is_valid())
                return;

            // Transform the bounding box to local (tile) space.
            tile_bbox.min.x -= tile_origin_x;
            tile_bbox.min.y -= tile_origin_y;
            tile_bbox.max.x -= tile_origin_x;
            tile_bbox.max.y -= tile_origin_y;

            // Inform the pixel renderer that we are about to render a tile.
            m_pixel_renderer->on_tile_begin(frame, tile, aov_tiles);

            // Create the framebuffer into which we will accumulate the samples.
            ShadingResultFrameBuffer* framebuffer =
                m_framebuffer_factory->create(
                    frame,
                    tile_x,
                    tile_y,
                    tile_bbox);
            assert(framebuffer);

            // Seed the RNG with the tile index.
            m_rng = SamplingContext::RNGType(
                hash_uint32(
                    static_cast<uint32>(pass_hash + tile_y * frame_properties.m_tile_count_x + tile_x)));

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

                // Skip pixels outside the intersection of the padded tile and the crop window.
                if (tx < static_cast<int>(tile_bbox.min.x) - m_margin_width  ||
                    ty < static_cast<int>(tile_bbox.min.y) - m_margin_height ||
                    tx > static_cast<int>(tile_bbox.max.x) + m_margin_width  ||
                    ty > static_cast<int>(tile_bbox.max.y) + m_margin_height)
                    continue;

                // Create a pixel context that identifies the pixel currently being rendered.
                const PixelContext pixel_context(
                    static_cast<int>(tile_origin_x) + tx,
                    static_cast<int>(tile_origin_y) + ty);

#ifdef DEBUG_BREAK_AT_PIXEL

                // Break in the debugger when this pixel is reached.
                if (pixel_context.get_pixel_coordinates() == DEBUG_BREAK_AT_PIXEL)
                    BREAKPOINT();

#endif

                // Render this pixel.
                m_pixel_renderer->render_pixel(
                    frame,
                    tile,
                    aov_tiles,
                    AABB2i(tile_bbox),
                    pixel_context,
                    pass_hash,
                    tx, ty,
                    m_rng,
                    *framebuffer);
            }

            // Develop the framebuffer to the tile.
            if (frame.is_premultiplied_alpha())
                framebuffer->develop_to_tile_premult_alpha(tile, aov_tiles);
            else framebuffer->develop_to_tile_straight_alpha(tile, aov_tiles);

            // Release the framebuffer.
            m_framebuffer_factory->destroy(framebuffer);

            // Inform the pixel renderer that we are done rendering the tile.
            m_pixel_renderer->on_tile_end(frame, tile, aov_tiles);
        }

        virtual StatisticsVector get_statistics() const APPLESEED_OVERRIDE
        {
            return m_pixel_renderer->get_statistics();
        }

      protected:
        auto_release_ptr<IPixelRenderer>    m_pixel_renderer;
        IShadingResultFrameBufferFactory*   m_framebuffer_factory;
        int                                 m_margin_width;
        int                                 m_margin_height;
        vector<Vector<int16, 2> >           m_pixel_ordering;
        SamplingContext::RNGType            m_rng;

        void compute_tile_margins(const Frame& frame, const bool primary)
        {
            m_margin_width = truncate<int>(ceil(frame.get_filter().get_xradius() - 0.5f));
            m_margin_height = truncate<int>(ceil(frame.get_filter().get_yradius() - 0.5f));

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
                    "rendering effort wasted by tile borders: %s (tile dimensions: %s x %s, tile margins: %s x %s)",
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

            // Convert the pixel ordering to a 2D representation.
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
    const Frame&                        frame,
    IPixelRendererFactory*              pixel_renderer_factory,
    IShadingResultFrameBufferFactory*   framebuffer_factory,
    const ParamArray&                   params)
  : m_frame(frame)
  , m_pixel_renderer_factory(pixel_renderer_factory)
  , m_framebuffer_factory(framebuffer_factory)
  , m_params(params)
{
}

void GenericTileRendererFactory::release()
{
    delete this;
}

ITileRenderer* GenericTileRendererFactory::create(
    const size_t    thread_index)
{
    return
        new GenericTileRenderer(
            m_frame,
            m_pixel_renderer_factory,
            m_framebuffer_factory,
            m_params,
            thread_index);
}

}   // namespace renderer
