
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
#include "generictilerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/hash/hash.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/ordering.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/arch.h"
#include "foundation/platform/debugger.h"
#include "foundation/string/string.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

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
          , m_aov_accumulators(frame)
          , m_framebuffer_factory(framebuffer_factory)
        {
            compute_pixel_ordering(frame);
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            m_pixel_renderer->print_settings();
        }

        void render_tile(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            const std::uint32_t                 pass_hash,
            IAbortSwitch&                       abort_switch) override
        {
            // Retrieve frame properties.
            const CanvasProperties& frame_properties = frame.image().properties();
            assert(tile_x < frame_properties.m_tile_count_x);
            assert(tile_y < frame_properties.m_tile_count_y);

            // Retrieve tile properties.
            Tile& tile = frame.image().tile(tile_x, tile_y);
            TileStack aov_tiles = frame.aov_images().tiles(tile_x, tile_y);
            const int tile_origin_x = static_cast<int>(frame_properties.m_tile_width * tile_x);
            const int tile_origin_y = static_cast<int>(frame_properties.m_tile_height * tile_y);
            const int tile_width = static_cast<int>(tile.get_width());
            const int tile_height = static_cast<int>(tile.get_height());

            // Compute the tile space bounding box of the pixels to render.
            const AABB2i tile_bbox =
                compute_tile_space_bbox(
                    tile_origin_x,
                    tile_origin_y,
                    tile_width,
                    tile_height,
                    frame.get_crop_window());
            if (!tile_bbox.is_valid())
                return;

            // Inform the pixel renderer that we are about to render a tile.
            m_pixel_renderer->on_tile_begin(
                frame,
                tile_x,
                tile_y,
                tile,
                aov_tiles);

            // Inform the AOV accumulators that we are about to render a tile.
            m_aov_accumulators.on_tile_begin(
                frame,
                tile_x,
                tile_y,
                m_pixel_renderer->get_max_samples_per_pixel());

            // Create the framebuffer into which we will accumulate the samples.
            ShadingResultFrameBuffer* framebuffer =
                m_framebuffer_factory->create(
                    frame,
                    tile_x,
                    tile_y,
                    tile_bbox);
            assert(framebuffer);

            // Loop over tile pixels.
            for (size_t i = 0, e = m_pixel_ordering.size(); i < e; ++i)
            {
                // Cancel any work done on this tile if rendering is aborted.
                if (abort_switch.is_aborted())
                    return;

                // Retrieve the coordinates of the pixel in the tile.
                // todo: switch to Vector2u now that we no longer have pixels outside tiles.
                const Vector2i pt(m_pixel_ordering[i].x, m_pixel_ordering[i].y);

                // Skip pixels outside the intersection of the tile and the crop window.
                if (!tile_bbox.contains(pt))
                    continue;

                const Vector2i pi(tile_origin_x + pt.x, tile_origin_y + pt.y);

#ifdef DEBUG_BREAK_AT_PIXEL

                // Break in the debugger when this pixel is reached.
                if (pi == DEBUG_BREAK_AT_PIXEL)
                    BREAKPOINT();

#endif

                // Render this pixel.
                m_pixel_renderer->render_pixel(
                    frame,
                    tile,
                    aov_tiles,
                    tile_bbox,
                    pass_hash,
                    pi,
                    pt,
                    m_aov_accumulators,
                    *framebuffer);
            }

            // Develop the framebuffer to the tile.
            framebuffer->develop_to_tile(tile, aov_tiles);

            // Release the framebuffer.
            m_framebuffer_factory->destroy(framebuffer);

            // Inform the AOV accumulators that we are done rendering a tile.
            m_aov_accumulators.on_tile_end(frame, tile_x, tile_y);

            // Inform the pixel renderer that we are done rendering the tile.
            m_pixel_renderer->on_tile_end(
                frame,
                tile_x,
                tile_y,
                tile,
                aov_tiles);
        }

        StatisticsVector get_statistics() const override
        {
            return m_pixel_renderer->get_statistics();
        }

      protected:
        auto_release_ptr<IPixelRenderer>        m_pixel_renderer;
        AOVAccumulatorContainer                 m_aov_accumulators;
        IShadingResultFrameBufferFactory*       m_framebuffer_factory;
        std::vector<Vector<std::int16_t, 2>>    m_pixel_ordering;

        void compute_pixel_ordering(const Frame& frame)
        {
            // Compute the dimensions in pixels of the tile.
            const CanvasProperties& properties = frame.image().properties();
            const size_t tile_width = properties.m_tile_width;
            const size_t tile_height = properties.m_tile_height;
            const size_t pixel_count = tile_width * tile_height;

            // Generate the pixel ordering inside the tile.
            std::vector<size_t> ordering;
            ordering.reserve(pixel_count);
            hilbert_ordering(ordering, tile_width, tile_height);
            assert(ordering.size() == pixel_count);

            // Convert the pixel ordering to a 2D representation.
            m_pixel_ordering.resize(pixel_count);
            for (size_t i = 0; i < pixel_count; ++i)
            {
                const size_t x = ordering[i] % tile_width;
                const size_t y = ordering[i] / tile_width;
                assert(x < tile_width);
                assert(y < tile_height);
                m_pixel_ordering[i].x = static_cast<std::int16_t>(x);
                m_pixel_ordering[i].y = static_cast<std::int16_t>(y);
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
