
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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
#include "adaptivetilerenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/ishadingresultframebufferfactory.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/math/ordering.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/arch.h"
#include "foundation/platform/debugger.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <deque>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Reference:
//
// A Hierarchical Automatic Stopping Condition for Monte Carlo Global Illumination.
// https://jo.dreggn.org/home/2009_stopping.pdf
//

namespace
{

    // Minimum allowed size for a block of pixel.
    const size_t BlockMinAllowedSize = 8;
    // Maximum allowed size for a block of pixel. A low value may introduce artifacts.
    const size_t BlockMaxAllowedSize = 40;
    // Minimum allowed size for a block of pixel before splitting.
    const size_t BlockSplittingThreshold = BlockMinAllowedSize * 2;
    // Threshold used to warn the user if blocks don't converge.
    const int BlockConvergenceWarningThreshold = 70;


    //
    // A block of pixel used for adaptive sampling.
    //

    struct PixelBlock
    {
        // The bounding box of the block, including tile's margin.
        AABB2i          m_surface;
        // Samples/pixel made in this block.
        size_t          m_spp;
        // Noise amount of the pixel block.
        float           m_block_error;
        bool            m_converged;

        enum class Axis
        {
            Horizontal,
            Vertical,
        };

        // Orientation of the block.
        Axis            m_main_axis;

        explicit PixelBlock(
            const AABB2i&       surface)
          : m_surface(surface)
          , m_spp(0)
          , m_block_error(0)
          , m_converged(false)
        {
            assert(m_surface.is_valid());

            if (m_surface.extent(0) >= m_surface.extent(1))
                m_main_axis = Axis::Horizontal; // block is wider
            else
                m_main_axis = Axis::Vertical; // block is taller
        }
    };


    //
    // Adaptive tile renderer.
    //

#ifndef NDEBUG

    // Define this symbol to break execution into the debugger
    // when a specific pixel is about to be rendered.
    // #define DEBUG_BREAK_AT_PIXEL Vector2i(0, 0)

#endif

    class AdaptiveTileRenderer
      : public ITileRenderer
    {
      public:
        AdaptiveTileRenderer(
            const Frame&                        frame,
            ISampleRendererFactory*             sample_renderer_factory,
            IShadingResultFrameBufferFactory*   framebuffer_factory,
            const ParamArray&                   params,
            const size_t                        thread_index)
          : m_aov_accumulators(frame)
          , m_framebuffer_factory(framebuffer_factory)
          , m_params(params)
          , m_invalid_sample_count(0)
          , m_sample_aov_tile(nullptr)
          , m_sample_renderer(sample_renderer_factory->create(thread_index))
          , m_total_pixel(0)
          , m_total_pixel_converged(0)
        {
            compute_tile_margins(frame, thread_index == 0);

            m_sample_aov_index = frame.aovs().get_index("pixel_sample_count");

            if (m_params.m_adaptiveness == 0.0f && thread_index == 0)
            {
                RENDERER_LOG_WARNING("adaptiveness is set to 0, use the uniform renderer for better performance");
            }
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "adaptive tile renderer settings:\n"
                "  min samples                   %s\n"
                "  max samples                   %s\n"
                "  noise threshold               %f\n"
                "  adaptiveness                  %f",
                pretty_uint(m_params.m_min_samples).c_str(),
                pretty_uint(m_params.m_max_samples).c_str(),
                m_params.m_noise_threshold,
                m_params.m_adaptiveness);

            RENDERER_LOG_DEBUG("adaptive tile renderer splitting threshold: %f",
                m_params.m_splitting_threshold);

            m_sample_renderer->print_settings();
        }

        void render_tile(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            const size_t                        pass_hash,
            IAbortSwitch&                       abort_switch) override
        {
            const size_t frame_width = frame.image().properties().m_canvas_width;
            const size_t aov_count = frame.aov_images().size();

            // Retrieve frame properties.
            const CanvasProperties& frame_properties = frame.image().properties();
            assert(tile_x < frame_properties.m_tile_count_x);
            assert(tile_y < frame_properties.m_tile_count_y);

            // Retrieve tile properties.
            Tile& tile = frame.image().tile(tile_x, tile_y);
            TileStack aov_tiles = frame.aov_images().tiles(tile_x, tile_y);
            const int tile_origin_x = static_cast<int>(frame_properties.m_tile_width * tile_x);
            const int tile_origin_y = static_cast<int>(frame_properties.m_tile_height * tile_y);

            // Compute the image space bounding box of the pixels to render.
            AABB2i tile_bbox;
            tile_bbox.min.x = tile_origin_x;
            tile_bbox.min.y = tile_origin_y;
            tile_bbox.max.x = tile_origin_x + static_cast<int>(tile.get_width()) - 1;
            tile_bbox.max.y = tile_origin_y + static_cast<int>(tile.get_height()) - 1;
            tile_bbox = AABB2i::intersect(tile_bbox, AABB2i(frame.get_crop_window()));
            if (!tile_bbox.is_valid())
                return;

            // Transform the bounding box to local (tile) space.
            tile_bbox.min.x -= tile_origin_x;
            tile_bbox.min.y -= tile_origin_y;
            tile_bbox.max.x -= tile_origin_x;
            tile_bbox.max.y -= tile_origin_y;

            // Pad the bounding box with tile margins.
            AABB2i padded_tile_bbox;
            padded_tile_bbox.min.x = tile_bbox.min.x - m_margin_width;
            padded_tile_bbox.min.y = tile_bbox.min.y - m_margin_height;
            padded_tile_bbox.max.x = tile_bbox.max.x + m_margin_width;
            padded_tile_bbox.max.y = tile_bbox.max.y + m_margin_height;

            // Inform the pixel renderer that we are about to render a tile.
            on_tile_begin(frame, tile_x, tile_y, tile, aov_tiles);

            // Inform the AOV accumulators that we are about to render a tile.
            m_aov_accumulators.on_tile_begin(
                frame,
                tile_x,
                tile_y,
                m_params.m_max_samples);

            // Create the framebuffer into which we will accumulate the samples.
            ShadingResultFrameBuffer* framebuffer =
                m_framebuffer_factory->create(
                    frame,
                    tile_x,
                    tile_y,
                    tile_bbox);
            assert(framebuffer);

            // Create the buffer into which we will accumulate every second samples.
            ShadingResultFrameBuffer* second_framebuffer = nullptr;

            // If rendering multiple passes, the permanent buffer factory will return
            // the same buffer so we must create a new one.
            second_framebuffer = new ShadingResultFrameBuffer(
                tile.get_width(),
                tile.get_height(),
                frame.aov_images().size(),
                tile_bbox,
                frame.get_filter());

            if (m_params.m_passes > 1)
                second_framebuffer->copy_from(*framebuffer);
            else
                second_framebuffer->clear();

            assert(second_framebuffer);

            const size_t pixel_count = framebuffer->get_width() * framebuffer->get_height();

            // Blocks rendering.
            deque<PixelBlock> rendering_blocks;
            vector<PixelBlock> finished_blocks;

            // Initially split blocks so that no block is larger than `BlockMaxAllowedSize`.
            create_rendering_blocks(rendering_blocks, padded_tile_bbox, framebuffer->get_crop_window());

            // First uniform pass based on adaptiveness parameter.
            if (m_params.m_adaptiveness < 1.0f)
            {
                for (size_t i = 0; i < rendering_blocks.size(); ++i)
                {
                    PixelBlock& pb = rendering_blocks.front();
                    rendering_blocks.pop_front();

                    // First batch contains `max_samples` * `1 - adaptiveness`
                    const size_t batch_size = static_cast<size_t>(
                        m_params.m_max_samples * (1.0f - m_params.m_adaptiveness));

                    // Draw samples.
                    sample_pixel_block(
                            pb,
                            abort_switch,
                            batch_size,
                            framebuffer,
                            second_framebuffer,
                            tile_origin_x,
                            tile_origin_y,
                            frame,
                            frame_width,
                            pass_hash,
                            aov_count);

                    rendering_blocks.push_back(pb);
                }
            }

            while (true)
            {
                // Check if image is converged or rendering was aborted.
                if (abort_switch.is_aborted() || rendering_blocks.size() < 1)
                {
                    finished_blocks.insert(finished_blocks.end(), rendering_blocks.begin(), rendering_blocks.end());
                    break;
                }

                // Sample the block in front of the deque.
                PixelBlock& pb = rendering_blocks.front();
                rendering_blocks.pop_front();

                // Each batch contains 'min' samples.
                assert(m_params.m_max_samples > pb.m_spp);
                const size_t remaining_samples = m_params.m_max_samples - pb.m_spp;
                const size_t batch_size = min(m_params.m_min_samples, remaining_samples);

                if (remaining_samples < 1)
                {
                    finished_blocks.push_back(pb);
                    continue;
                }

                // Draw samples.
                sample_pixel_block(
                    pb,
                    abort_switch,
                    batch_size,
                    framebuffer,
                    second_framebuffer,
                    tile_origin_x,
                    tile_origin_y,
                    frame,
                    frame_width,
                    pass_hash,
                    aov_count);

                // Check if this was the last batch.
                if (remaining_samples - batch_size < 1)
                {
                    finished_blocks.push_back(pb);
                }
                else
                {
                    const AABB2u& block_image_bb = AABB2i::intersect(framebuffer->get_crop_window(), pb.m_surface);

                    // Evaluate block's noise amount.
                    pb.m_block_error = FilteredTile::compute_tile_variance(
                        block_image_bb,
                        framebuffer,
                        second_framebuffer);

                    // Decide if the blocks needs to be splitted, sampled or if it has converged.
                    if (pb.m_block_error <= m_params.m_noise_threshold)
                    {
                        pb.m_converged = true;
                        finished_blocks.push_back(pb);
                    }
                    else if (pb.m_block_error <= m_params.m_splitting_threshold)
                    {
                        if (pb.m_main_axis == PixelBlock::Axis::Horizontal
                                && block_image_bb.extent(0) >= BlockSplittingThreshold)
                        {
                            split_pixel_block(
                                pb,
                                rendering_blocks,
                                static_cast<int>(block_image_bb.min.x)
                                + static_cast<int>(block_image_bb.extent(0) * 0.5f - 0.5f));
                        }
                        else if (pb.m_main_axis == PixelBlock::Axis::Vertical
                                && block_image_bb.extent(1) >= BlockSplittingThreshold)
                        {
                            split_pixel_block(
                                pb,
                                rendering_blocks,
                                static_cast<int>(block_image_bb.min.y)
                                + static_cast<int>(block_image_bb.extent(1) * 0.5f - 0.5f));
                        }
                        else
                        {
                            rendering_blocks.push_front(pb);
                        }
                    }
                    else
                    {
                        // Block's variance is too high and it needs to be sampled.
                        rendering_blocks.push_front(pb);
                    }
                }
            }

            // Rendering finished, fill diagnostic AOVs and update statistics.
            size_t tile_converged_pixel = 0;

            for (size_t i = 0, n = finished_blocks.size(); i < n; ++i)
            {
                const PixelBlock& pb = finished_blocks[i];
                const AABB2u& pb_image_aabb = AABB2i::intersect(framebuffer->get_crop_window(), pb.m_surface);
                const size_t pb_pixel_count = static_cast<size_t>(pb_image_aabb.volume());

                // Update statistics.
                m_spp.insert(pb.m_spp, pb_pixel_count);

                if (pb.m_converged)
                    tile_converged_pixel += pb_pixel_count;

                if (m_sample_aov_tile == nullptr)
                    continue;

                for (size_t y = pb_image_aabb.min.y; y <= pb_image_aabb.max.y; ++y)
                {
                    for (size_t x = pb_image_aabb.min.x; x <= pb_image_aabb.max.x; ++x)
                    {
                        // Retrieve the coordinates of the pixel in the padded tile.
                        const Vector2u pt(x, y);

                        Color3f value(static_cast<float>(pb.m_spp), 0.0f, 0.0f);
                        m_sample_aov_tile->set_pixel(pt.x, pt.y, value);
                    }
                }
            }

            m_total_pixel += pixel_count;
            m_total_pixel_converged += tile_converged_pixel;

            // Warn the user if adaptive sampling wasn't efficient on this tile.
            if (static_cast<float>(tile_converged_pixel) / static_cast<float>(pixel_count) < BlockConvergenceWarningThreshold / 100.0f)
            {
                RENDERER_LOG_WARNING(
                    "%s of pixels have converged, be sure to increase the maximum number of samples "
                    "or decrease the noise threshold for better performance",
                    pretty_percent(tile_converged_pixel, pixel_count, 1).c_str());
            }

            // Develop the framebuffer to the tile.
            framebuffer->develop_to_tile(tile, aov_tiles);

            // Release the framebuffer.
            m_framebuffer_factory->destroy(framebuffer);

            // Delete the accumulation buffer.
            delete second_framebuffer;

            // Inform the AOV accumulators that we are done rendering a tile.
            m_aov_accumulators.on_tile_end(frame, tile_x, tile_y);

            // Inform the pixel renderer that we are done rendering the tile.
            on_tile_end(frame, tile_x, tile_y, tile, aov_tiles);
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;

            // How many samples per pixel were made.
            stats.insert("samples/pixel", m_spp);
            // Converged pixels over total pixels.
            stats.insert_percent("convergence rate", m_total_pixel_converged, m_total_pixel);

            StatisticsVector vec;
            vec.insert("adaptive tile renderer statistics", stats);
            vec.merge(m_sample_renderer->get_statistics());

            return vec;
        }

      private:
        struct Parameters
        {
            const SamplingContext::Mode     m_sampling_mode;
            const size_t                    m_min_samples;
            const size_t                    m_max_samples;
            const float                     m_noise_threshold;
            const float                     m_splitting_threshold;
            const float                     m_adaptiveness;
            const size_t                    m_passes;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
              , m_min_samples(params.get_required<size_t>("min_samples", 16))
              , m_max_samples(params.get_required<size_t>("max_samples", 256))
              , m_noise_threshold(params.get_required<float>("noise_threshold", 5.0f))
              , m_splitting_threshold(m_noise_threshold * 256.0f)
              , m_adaptiveness(params.get_optional<float>("adaptiveness", 0.9f))
              , m_passes(params.get_optional<size_t>("passes", 1))
            {
            }
        };

        AOVAccumulatorContainer                 m_aov_accumulators;
        IShadingResultFrameBufferFactory*       m_framebuffer_factory;
        int                                     m_margin_width;
        int                                     m_margin_height;
        const Parameters                        m_params;
        size_t                                  m_sample_aov_index;
        size_t                                  m_invalid_sample_count;
        Tile*                                   m_sample_aov_tile;
        auto_release_ptr<ISampleRenderer>       m_sample_renderer;

        // Members used for statistics.
        Population<uint64>                      m_spp;
        size_t                                  m_total_pixel;
        size_t                                  m_total_pixel_converged;

        void compute_tile_margins(
            const Frame&                        frame,
            const bool                          primary)
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

        void on_tile_begin(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            Tile&                               tile,
            TileStack&                          aov_tiles)
        {
            if (m_sample_aov_index != ~size_t(0))
                m_sample_aov_tile = &frame.aovs().get_by_index(m_sample_aov_index)->get_image().tile(tile_x, tile_y);
        }

        void on_tile_end(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            Tile&                               tile,
            TileStack&                          aov_tiles)
        {
            m_sample_aov_tile = nullptr;
        }

        void on_pixel_begin(
            const Frame&                        frame,
            const Vector2i&                     pi,
            const Vector2i&                     pt)
        {
            m_invalid_sample_count = 0;
            m_aov_accumulators.on_pixel_begin(pi);
        }

        void on_pixel_end(
            const Frame&                        frame,
            const Vector2i&                     pi,
            const Vector2i&                     pt)
        {
            static const size_t MaxWarningsPerThread = 2;

            m_aov_accumulators.on_pixel_end(pi);

            // Warns the user for bad pixels.
            if (m_invalid_sample_count > 0)
            {
                // We can't store the number of error per pixel because of adaptive rendering.
                if (m_invalid_sample_count <= MaxWarningsPerThread)
                {
                    RENDERER_LOG_WARNING(
                        "%s sample%s at pixel (%d, %d) had nan, negative or infinite components and %s ignored.",
                        pretty_uint(m_invalid_sample_count).c_str(),
                        m_invalid_sample_count > 1 ? "s" : "",
                        pi.x, pi.y,
                        m_invalid_sample_count > 1 ? "were" : "was");
                }
                else if (m_invalid_sample_count == MaxWarningsPerThread + 1)
                {
                    RENDERER_LOG_WARNING("more invalid samples found, omitting warning messages for brevity.");
                }
            }
        }

        void create_rendering_blocks(
            deque<PixelBlock>&                  rendering_blocks,
            const AABB2i&                       padded_tile_bbox,
            const AABB2u&                       frame_bbox)
        {
            deque<PixelBlock> initial_blocks(1, PixelBlock(padded_tile_bbox));

            while (initial_blocks.size() > 0)
            {
                PixelBlock& pb = initial_blocks.front();
                initial_blocks.pop_front();

                const AABB2u& block_image_bb = AABB2i::intersect(frame_bbox, pb.m_surface);

                if (block_image_bb.extent(0) <= BlockMaxAllowedSize
                    && block_image_bb.extent(1) <= BlockMaxAllowedSize)
                {
                    rendering_blocks.push_front(pb);
                    continue;
                }

                // Split the block if it's too big.
                if (pb.m_main_axis == PixelBlock::Axis::Horizontal
                        && block_image_bb.extent(0) >= BlockSplittingThreshold)
                {
                    split_pixel_block(
                        pb,
                        initial_blocks,
                        static_cast<int>(block_image_bb.min.x)
                        + static_cast<int>(block_image_bb.extent(0) * 0.5f - 0.5f));
                }
                else if (pb.m_main_axis == PixelBlock::Axis::Vertical
                        && block_image_bb.extent(1) >= BlockSplittingThreshold)
                {
                    split_pixel_block(
                        pb,
                        initial_blocks,
                        static_cast<int>(block_image_bb.min.y)
                        + static_cast<int>(block_image_bb.extent(1) * 0.5f - 0.5f));
                }
            }
        }

        void sample_pixel_block(
            PixelBlock&                         pb,
            IAbortSwitch&                       abort_switch,
            const size_t                        batch_size,
            ShadingResultFrameBuffer*           framebuffer,
            ShadingResultFrameBuffer*           second_framebuffer,
            const int                           tile_origin_x,
            const int                           tile_origin_y,
            const Frame&                        frame,
            const size_t                        frame_width,
            const size_t                        pass_hash,
            const size_t                        aov_count)
        {
            // Loop over block's pixels.
            for (int y = pb.m_surface.min.y; y <= pb.m_surface.max.y; ++y)
            {
                for (int x = pb.m_surface.min.x; x <= pb.m_surface.max.x; ++x)
                {
                    // Cancel any work done on this tile if rendering is aborted.
                    if (abort_switch.is_aborted())
                        return;

                    // Retrieve the coordinates of the pixel in the padded tile.
                    const Vector2i pt(x, y);

                    // Skip pixels outside the intersection of the padded tile and the crop window.
                    if (!pb.m_surface.contains(pt))
                        continue;

                    const Vector2i pi(tile_origin_x + pt.x, tile_origin_y + pt.y);

#ifdef DEBUG_BREAK_AT_PIXEL

                    // Break in the debugger when this pixel is reached.
                    if (pi == DEBUG_BREAK_AT_PIXEL)
                        BREAKPOINT();

#endif

                    const size_t pixel_index = pi.y * frame_width + pi.x;
                    const size_t instance = hash_uint32(static_cast<uint32>(pass_hash + pixel_index +
                        (pb.m_spp * frame_width * frame.image().properties().m_canvas_height)));

                    // Render this pixel.
                    sample_pixel(
                        frame,
                        pi,
                        pt,
                        framebuffer,
                        second_framebuffer,
                        pass_hash,
                        instance,
                        batch_size,
                        aov_count);
                }
            }

            pb.m_spp += batch_size;
        }

        void sample_pixel(
            const Frame&                        frame,
            const Vector2i&                     pi,
            const Vector2i&                     pt,
            ShadingResultFrameBuffer*           framebuffer,
            ShadingResultFrameBuffer*           second_framebuffer,
            const size_t                        pass_hash,
            const size_t                        instance,
            const size_t                        batch_size,
            const size_t                        aov_count)
        {
            on_pixel_begin(frame, pi, pt);

            SamplingContext::RNGType rng(pass_hash, instance);
            SamplingContext sampling_context(
                rng,
                m_params.m_sampling_mode,
                2,                          // number of dimensions
                0,                          // number of samples -- unknown
                instance);                  // initial instance number

            bool second = false;

            for (size_t j = 0; j < batch_size; ++j)
            {
                // Generate a uniform sample in [0,1)^2.
                const Vector2d s = sampling_context.next2<Vector2d>();

                // Compute the sample position in NDC.
                const Vector2d sample_position = frame.get_sample_position(pi.x + s.x, pi.y + s.y);

                // Create a pixel context that identifies the pixel and sample currently being rendered.
                const PixelContext pixel_context(pi, sample_position);

                // Render the sample.
                ShadingResult shading_result(aov_count);
                SamplingContext child_sampling_context(sampling_context);
                m_sample_renderer->render_sample(
                    child_sampling_context,
                    pixel_context,
                    sample_position,
                    m_aov_accumulators,
                    shading_result);

                // Ignore invalid samples.
                if (!shading_result.is_valid())
                {
                    ++m_invalid_sample_count;
                    continue;
                }

                // Merge the sample into the scratch framebuffer.
                framebuffer->add(
                    static_cast<float>(pt.x + s.x),
                    static_cast<float>(pt.y + s.y),
                    shading_result);

                if (second)
                {
                    second_framebuffer->add(
                            static_cast<float>(pt.x + s.x),
                            static_cast<float>(pt.y + s.y),
                            shading_result);
                }

                second = !second;
            }

            on_pixel_end(frame, pi, pt);
        }

        // Split the given block in two.
        void split_pixel_block(
            PixelBlock&                         pb,
            deque<PixelBlock>&                  blocks,
            const int&                          splitting_point) const
        {
            AABB2i f_half = pb.m_surface, s_half = pb.m_surface;

            if (pb.m_main_axis == PixelBlock::Axis::Horizontal)
            {
                assert(pb.m_surface.min.x < splitting_point);
                assert(pb.m_surface.max.x > splitting_point);
                // Split horizontaly.
                f_half.max.x = splitting_point;
                s_half.min.x = splitting_point + 1;
            }
            else
            {
                assert(pb.m_surface.min.y < splitting_point);
                assert(pb.m_surface.max.y > splitting_point);
                // Split verticaly.
                f_half.max.y = splitting_point;
                s_half.min.y = splitting_point + 1;
            }

            assert(f_half.is_valid());
            assert(s_half.is_valid());

            PixelBlock f_block(f_half), s_block(s_half);

            // If the block is on the border of the tile, one pixel will be missed on each axis.
            assert(f_block.m_surface.extent(0) >= BlockMinAllowedSize && f_block.m_surface.extent(1) >= BlockMinAllowedSize);
            assert(s_block.m_surface.extent(0) >= BlockMinAllowedSize && s_block.m_surface.extent(1) >= BlockMinAllowedSize);

            f_block.m_spp = s_block.m_spp = pb.m_spp;

            blocks.push_front(s_block);
            blocks.push_front(f_block);
        }

        static Color4f colorize_samples(
            const float                         value)
        {
            static const Color4f Blue(0.0f, 0.0f, 1.0f, 1.0f);
            static const Color4f Orange(1.0f, 0.6f, 0.0f, 1.0f);
            return lerp(Blue, Orange, saturate(value));
        }
    };
}


//
// AdaptiveTileRendererFactory class implementation.
//

AdaptiveTileRendererFactory::AdaptiveTileRendererFactory(
    const Frame&                        frame,
    ISampleRendererFactory*             sample_renderer_factory,
    IShadingResultFrameBufferFactory*   framebuffer_factory,
    const ParamArray&                   params)
  : m_frame(frame)
  , m_sample_renderer_factory(sample_renderer_factory)
  , m_framebuffer_factory(framebuffer_factory)
  , m_params(params)
{
}

void AdaptiveTileRendererFactory::release()
{
    delete this;
}

ITileRenderer* AdaptiveTileRendererFactory::create(
    const size_t    thread_index)
{
    return
        new AdaptiveTileRenderer(
            m_frame,
            m_sample_renderer_factory,
            m_framebuffer_factory,
            m_params,
            thread_index);
}

Dictionary AdaptiveTileRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "min_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("label", "Min Samples")
            .insert("help", "Minimum number of anti-aliasing samples"));

    metadata.dictionaries().insert(
        "max_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "256")
            .insert("label", "Max Samples")
            .insert("help", "Maximum number of anti-aliasing samples"));

    metadata.dictionaries().insert(
        "noise_threshold",
        Dictionary()
            .insert("type", "float")
            .insert("default", "5.0")
            .insert("label", "Noise Threshold")
            .insert("help", "Rendering stop threshold"));

    metadata.dictionaries().insert(
        "adaptiveness",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.9")
            .insert("label", "Adaptiveness")
            .insert("help", "Quantity of samples generated adaptively"));

    return metadata;
}

}   // namespace renderer
