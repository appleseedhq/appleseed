
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
#include "renderer/modeling/aov/pixelsamplecountaov.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/filtersamplingtable.h"
#include "foundation/math/ordering.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/arch.h"
#include "foundation/platform/debugger.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstdint>
#include <deque>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;

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

    // Minimum allowed size for a block of pixels.
    const size_t BlockMinAllowedSize = 8;

    // Maximum allowed size for a block of pixels. A low value may introduce artifacts.
    const size_t BlockMaxAllowedSize = 40;

    // Minimum allowed size for a block of pixels before splitting.
    const size_t BlockSplittingThreshold = BlockMinAllowedSize * 2;

    // Threshold used to warn the user if blocks don't converge.
    const float BlockConvergenceWarningThreshold = 0.5f;


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

        explicit PixelBlock(const AABB2i& surface)
          : m_surface(surface)
          , m_spp(0)
          , m_block_error(0.0f)
          , m_converged(false)
        {
            assert(m_surface.is_valid());
            m_main_axis =
                m_surface.extent(0) >= m_surface.extent(1)
                    ? Axis::Horizontal  // block is wider
                    : Axis::Vertical;   // block is taller
        }
    };


    //
    // Calculation of noise level inside a block or a pixel.
    //

    // Compute the variance of a weighted pixel given the value of the same pixel with 2 different samples count.
    // The first given pixel `main` contains N samples.
    // The second given pixel `second` contains N/2 samples (samples included in `second` are also in `main`).
    float compute_weighted_pixel_variance(
        const float*            main,
        const float*            second)
    {
        // Get weights.
        const float main_weight = *main++;
        const float rcp_main_weight = main_weight == 0.0f ? 0.0f : 1.0f / main_weight;
        const float second_weight = *second++;
        const float rcp_second_weight = second_weight == 0.0f ? 0.0f : 1.0f / second_weight;

        // Get colors and assign weights.
        Color4f main_color(main[0], main[1], main[2], main[3]);
        main_color *= rcp_main_weight;

        Color4f second_color(second[0], second[1], second[2], second[3]);
        second_color *= rcp_second_weight;

        const float rgb = std::abs(main_color.r) + std::abs(main_color.g) + std::abs(main_color.b);

        if (rgb == 0.0f)
            return 0.0f;

        // Compute variance.
        return
            fast_rcp_sqrt(rgb) * (
                std::abs(main_color.r - second_color.r) +
                std::abs(main_color.g - second_color.g) +
                std::abs(main_color.b - second_color.b));
    }

    // Compute the variance of the tile `main` for pixels in the bounding box `bb`.
    // A second tile `second` is used which contains half of the samples of `main`.
    float compute_tile_variance(
        const AABB2u&           bb,
        const AccumulatorTile*  main,
        const AccumulatorTile*  second)
    {
        float error = 0.0f;

        assert(main->get_crop_window() == second->get_crop_window());
        assert(main->get_crop_window().contains(bb.min));
        assert(main->get_crop_window().contains(bb.max));

        // Loop over block pixels.
        for (size_t y = bb.min.y; y <= bb.max.y; ++y)
        {
            for (size_t x = bb.min.x; x <= bb.max.x; ++x)
            {
                const float* main_ptr = main->pixel(x, y);
                const float* second_ptr = second->pixel(x, y);

                error = std::max(error, compute_weighted_pixel_variance(main_ptr, second_ptr));
            }
        }

        return error;
    }


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
          , m_variation_aov_tile(nullptr)
          , m_sample_renderer(sample_renderer_factory->create(thread_index))
          , m_total_pixel_count(0)
          , m_total_converged_pixel_count(0)
        {
            m_sample_aov_index = frame.aovs().get_index("pixel_sample_count");
            m_variation_aov_index = frame.aovs().get_index("pixel_variation");

            // Send sampling parameters to the sample count AOV.
            if (m_sample_aov_index != ~size_t(0))
            {
                PixelSampleCountAOV* sample_aov =
                    static_cast<PixelSampleCountAOV*>(
                        frame.aovs().get_by_index(m_sample_aov_index));

                // The AOV takes care of normalizing values depending on sampling parameters.
                sample_aov->set_normalization_range(
                    m_params.m_min_samples,
                    m_params.m_max_samples * m_params.m_pass_count);
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
                "  batch size                    %s\n"
                "  min samples                   %s\n"
                "  max samples                   %s\n"
                "  noise threshold               %f",
                pretty_uint(m_params.m_batch_size).c_str(),
                pretty_uint(m_params.m_min_samples).c_str(),
                m_params.m_max_samples > 0 ? pretty_uint(m_params.m_max_samples).c_str() : "unlimited",
                m_params.m_noise_threshold);

            RENDERER_LOG_DEBUG("adaptive tile renderer splitting threshold: %f",
                m_params.m_splitting_threshold);

            m_sample_renderer->print_settings();
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
            const size_t aov_count = frame.aov_images().size();

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
            // If rendering multiple passes, the permanent buffer factory will return
            // the same buffer so we must create a new one.
            std::unique_ptr<ShadingResultFrameBuffer> second_framebuffer(
                new ShadingResultFrameBuffer(
                    tile.get_width(),
                    tile.get_height(),
                    frame.aov_images().size(),
                    tile_bbox));

            if (m_params.m_pass_count > 1)
                second_framebuffer->copy_from(*framebuffer);
            else second_framebuffer->clear();

            const size_t tile_pixel_count = framebuffer->get_width() * framebuffer->get_height();

            // Blocks rendering.
            std::deque<PixelBlock> rendering_blocks;
            std::vector<PixelBlock> finished_blocks;

            // Initially split blocks so that no block is larger than `BlockMaxAllowedSize`.
            create_rendering_blocks(rendering_blocks, tile_bbox, framebuffer->get_crop_window());

            // First uniform pass based on adaptiveness parameter.
            if (m_params.m_min_samples > 0)
            {
                const size_t batch_size =
                    m_params.m_max_samples > 0
                        ? std::min(m_params.m_min_samples, m_params.m_max_samples)
                        : m_params.m_min_samples;

                std::deque<PixelBlock> blocks = rendering_blocks;
                rendering_blocks.clear();

                while (!blocks.empty())
                {
                    PixelBlock pb = blocks.front();
                    blocks.pop_front();

                    // Draw samples.
                    sample_pixel_block(
                        pb,
                        abort_switch,
                        batch_size,
                        framebuffer,
                        second_framebuffer.get(),
                        tile_origin_x,
                        tile_origin_y,
                        frame,
                        frame_properties.m_canvas_width,
                        frame_properties.m_canvas_height,
                        pass_hash,
                        aov_count);

                    rendering_blocks.push_back(pb);
                }
            }

            while (true)
            {
                // Check if image is converged or rendering was aborted.
                if (abort_switch.is_aborted() || rendering_blocks.empty())
                {
                    finished_blocks.insert(finished_blocks.end(), rendering_blocks.begin(), rendering_blocks.end());

                    if (rendering_blocks.empty())
                        break;
                    else
                        return;
                }

                // Sample the block in front of the deque.
                PixelBlock pb = rendering_blocks.front();
                rendering_blocks.pop_front();

                // Each batch contains 'min' samples.
                assert(pb.m_spp <= m_params.m_max_samples || m_params.m_max_samples == 0);
                const size_t batch_size =
                    m_params.m_max_samples > 0
                        ? std::min(m_params.m_batch_size, m_params.m_max_samples - pb.m_spp)
                        : m_params.m_batch_size;

                if (batch_size == 0)
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
                    second_framebuffer.get(),
                    tile_origin_x,
                    tile_origin_y,
                    frame,
                    frame_properties.m_canvas_width,
                    frame_properties.m_canvas_height,
                    pass_hash,
                    aov_count);

                const AABB2u block_image_bb = AABB2i::intersect(framebuffer->get_crop_window(), pb.m_surface);

                // Evaluate block's noise amount.
                pb.m_block_error =
                    compute_tile_variance(
                        block_image_bb,
                        framebuffer,
                        second_framebuffer.get());

                if (batch_size < m_params.m_batch_size)
                {
                    // It was the last batch.
                    finished_blocks.push_back(pb);
                }
                else if (pb.m_block_error <= m_params.m_noise_threshold)
                {
                    // The block has converged.
                    pb.m_converged = true;
                    finished_blocks.push_back(pb);
                }
                else if (pb.m_block_error <= m_params.m_splitting_threshold)
                {
                    // The block needs to be split.
                    if (pb.m_main_axis == PixelBlock::Axis::Horizontal &&
                        block_image_bb.extent(0) >= BlockSplittingThreshold)
                    {
                        split_pixel_block(
                            pb,
                            rendering_blocks,
                            static_cast<int>((block_image_bb.min.x + block_image_bb.max.x) / 2));
                    }
                    else if (pb.m_main_axis == PixelBlock::Axis::Vertical &&
                             block_image_bb.extent(1) >= BlockSplittingThreshold)
                    {
                        split_pixel_block(
                            pb,
                            rendering_blocks,
                            static_cast<int>((block_image_bb.min.y + block_image_bb.max.y) / 2));
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

            //
            // Rendering finished, fill diagnostic AOVs and update statistics.
            //

            size_t tile_converged_pixel_count = 0;
            float average_noise_level = 0.0f;
            const float normalizing_factor = 1.0f / m_params.m_noise_threshold;

            for (size_t i = 0, n = rendering_blocks.size(); i < n; ++i)
            {
                const PixelBlock& pb = rendering_blocks[i];
                const AABB2u pb_image_aabb = AABB2i::intersect(framebuffer->get_crop_window(), pb.m_surface);
                const size_t pb_pixel_count = pb_image_aabb.volume();

                average_noise_level += pb.m_block_error * pb_pixel_count;
            }

            for (size_t i = 0, n = finished_blocks.size(); i < n; ++i)
            {
                const PixelBlock& pb = finished_blocks[i];
                const AABB2u pb_image_aabb = AABB2i::intersect(framebuffer->get_crop_window(), pb.m_surface);
                const size_t pb_pixel_count = pb_image_aabb.volume();

                average_noise_level += pb.m_block_error * pb_pixel_count;

                // Update statistics.
                m_spp.insert(pb.m_spp, pb_pixel_count);

                if (pb.m_converged)
                    tile_converged_pixel_count += pb_pixel_count;

                if (m_sample_aov_tile == nullptr && m_variation_aov_tile == nullptr)
                    continue;

                for (size_t y = pb_image_aabb.min.y; y <= pb_image_aabb.max.y; ++y)
                {
                    for (size_t x = pb_image_aabb.min.x; x <= pb_image_aabb.max.x; ++x)
                    {
                        // Retrieve the coordinates of the pixel in the padded tile.
                        const Vector2u pt(x, y);

                        if (m_sample_aov_tile != nullptr)
                        {
                            Color3f samples;
                            m_sample_aov_tile->get_pixel(pt.x, pt.y, samples);
                            samples[0] += static_cast<float>(pb.m_spp);
                            m_sample_aov_tile->set_pixel(pt.x, pt.y, samples);
                        }

                        if (m_variation_aov_tile != nullptr)
                        {
                            Color3f variation;
                            m_variation_aov_tile->get_pixel(pt.x, pt.y, variation);
                            variation[0] += pb.m_block_error * normalizing_factor;
                            m_variation_aov_tile->set_pixel(pt.x, pt.y, variation);
                        }
                    }
                }
            }

            m_total_pixel_count += tile_pixel_count;
            m_total_converged_pixel_count += tile_converged_pixel_count;
            average_noise_level /= tile_pixel_count;

            // Print final statistics about this tile.
            const std::string converged_pixels_string = pretty_percent(tile_converged_pixel_count, tile_pixel_count, 0);
            Statistics stats;
            stats.insert(
                "pixels",
                format(
                    "total {0}  converged {1} ({2})",
                    pretty_uint(tile_pixel_count),
                    pretty_uint(tile_converged_pixel_count),
                    converged_pixels_string));
            stats.insert("samples/pixel", m_spp);
            stats.insert("average noise level", pretty_scalar(average_noise_level, 3));
            RENDERER_LOG_DEBUG(
                "tile (" FMT_SIZE_T ", " FMT_SIZE_T ") final statistics:\n%s",
                tile_x,
                tile_y,
                stats.to_string().c_str());

            // Warn the user if adaptive sampling wasn't efficient on this tile.
            if (static_cast<float>(tile_converged_pixel_count) < BlockConvergenceWarningThreshold * tile_pixel_count)
            {
                RENDERER_LOG_WARNING(
                    "convergence rate for tile (" FMT_SIZE_T ", " FMT_SIZE_T ") is %s.",
                    tile_x,
                    tile_y,
                    converged_pixels_string.c_str());
            }

            // Develop the framebuffer to the tile.
            framebuffer->develop_to_tile(tile, aov_tiles);

            // Release the framebuffer.
            m_framebuffer_factory->destroy(framebuffer);

            // Inform the AOV accumulators that we are done rendering the tile.
            m_aov_accumulators.on_tile_end(frame, tile_x, tile_y);

            // Inform the pixel renderer that we are done rendering the tile.
            on_tile_end(frame, tile_x, tile_y, tile, aov_tiles);
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;

            // How many samples per pixel were computed.
            stats.insert("samples/pixel", m_spp);

            // Converged pixels over total pixels.
            stats.insert_percent("convergence rate", m_total_converged_pixel_count, m_total_pixel_count);

            StatisticsVector vec;
            vec.insert("adaptive tile renderer statistics", stats);
            vec.merge(m_sample_renderer->get_statistics());

            return vec;
        }

      private:
        struct Parameters
        {
            const SamplingContext::Mode         m_sampling_mode;
            const size_t                        m_batch_size;
            const size_t                        m_min_samples;
            const size_t                        m_max_samples;
            const float                         m_noise_threshold;
            const float                         m_splitting_threshold;
            const size_t                        m_pass_count;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
              , m_batch_size(params.get_required<size_t>("batch_size", 16))
              , m_min_samples(params.get_required<size_t>("min_samples", 0))
              , m_max_samples(params.get_required<size_t>("max_samples", 256))
              , m_noise_threshold(params.get_required<float>("noise_threshold", 1.0f))
              , m_splitting_threshold(m_noise_threshold * 256.0f)
              , m_pass_count(params.get_optional<size_t>("passes", 1))
            {
            }
        };

        AOVAccumulatorContainer                 m_aov_accumulators;
        IShadingResultFrameBufferFactory*       m_framebuffer_factory;
        const Parameters                        m_params;
        size_t                                  m_sample_aov_index;
        size_t                                  m_variation_aov_index;
        size_t                                  m_invalid_sample_count;
        Tile*                                   m_sample_aov_tile;
        Tile*                                   m_variation_aov_tile;
        auto_release_ptr<ISampleRenderer>       m_sample_renderer;

        // Members used for statistics.
        Population<std::uint64_t>               m_spp;
        size_t                                  m_total_pixel_count;
        size_t                                  m_total_converged_pixel_count;

        void on_tile_begin(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            Tile&                               tile,
            TileStack&                          aov_tiles)
        {
            if (m_sample_aov_index != ~size_t(0))
                m_sample_aov_tile = &frame.aovs().get_by_index(m_sample_aov_index)->get_image().tile(tile_x, tile_y);

            if (m_variation_aov_index != ~size_t(0))
                m_variation_aov_tile = &frame.aovs().get_by_index(m_variation_aov_index)->get_image().tile(tile_x, tile_y);
        }

        void on_tile_end(
            const Frame&                        frame,
            const size_t                        tile_x,
            const size_t                        tile_y,
            Tile&                               tile,
            TileStack&                          aov_tiles)
        {
            m_sample_aov_tile = nullptr;
            m_variation_aov_tile = nullptr;
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
                        "%s sample%s at pixel (%d, %d) had NaN, negative or infinite components and %s ignored.",
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
            std::deque<PixelBlock>&                  rendering_blocks,
            const AABB2i&                            tile_bbox,
            const AABB2u&                            frame_bbox)
        {
            std::deque<PixelBlock> initial_blocks(1, PixelBlock(tile_bbox));

            while (!initial_blocks.empty())
            {
                PixelBlock pb = initial_blocks.front();
                initial_blocks.pop_front();

                const AABB2u block_image_bb = AABB2i::intersect(frame_bbox, pb.m_surface);

                if (block_image_bb.extent(0) <= BlockMaxAllowedSize &&
                    block_image_bb.extent(1) <= BlockMaxAllowedSize)
                {
                    rendering_blocks.push_front(pb);
                    continue;
                }

                // Split the block if it's too big.
                if (pb.m_main_axis == PixelBlock::Axis::Horizontal &&
                    block_image_bb.extent(0) >= BlockSplittingThreshold)
                {
                    split_pixel_block(
                        pb,
                        initial_blocks,
                        static_cast<int>((block_image_bb.min.x + block_image_bb.max.x) / 2));
                }
                else if (pb.m_main_axis == PixelBlock::Axis::Vertical &&
                         block_image_bb.extent(1) >= BlockSplittingThreshold)
                {
                    split_pixel_block(
                        pb,
                        initial_blocks,
                        static_cast<int>((block_image_bb.min.y + block_image_bb.max.y) / 2));
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
            const size_t                        frame_height,
            const std::uint32_t                 pass_hash,
            const size_t                        aov_count)
        {
            // Loop over the block's pixels.
            for (int y = pb.m_surface.min.y; y <= pb.m_surface.max.y; ++y)
            {
                for (int x = pb.m_surface.min.x; x <= pb.m_surface.max.x; ++x)
                {
                    // Cancel any work done on this tile if rendering is aborted.
                    if (abort_switch.is_aborted())
                        return;

                    // Retrieve the coordinates of the pixel in the tile.
                    const Vector2i pt(x, y);

                    // Skip pixels outside the intersection of the tile and the crop window.
                    if (!pb.m_surface.contains(pt))
                        continue;

                    const Vector2i pi(tile_origin_x + pt.x, tile_origin_y + pt.y);

#ifdef DEBUG_BREAK_AT_PIXEL

                    // Break in the debugger when this pixel is reached.
                    if (pi == DEBUG_BREAK_AT_PIXEL)
                        BREAKPOINT();

#endif

                    const size_t pixel_index = pi.y * frame_width + pi.x;
                    const size_t instance =
                        hash_uint32(
                            static_cast<std::uint32_t>(
                                pass_hash + pixel_index + (pb.m_spp * frame_width * frame_height)));

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
            const std::uint32_t                 pass_hash,
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

            for (size_t i = 0; i < batch_size; ++i)
            {
                // Generate a uniform sample in [0,1)^2.
                const Vector2f s = sampling_context.next2<Vector2f>();

                // Sample the pixel filter.
                const auto& filter_table = frame.get_filter_sampling_table();
                const Vector2d pf(
                    static_cast<double>(filter_table.sample(s[0]) + 0.5f),
                    static_cast<double>(filter_table.sample(s[1]) + 0.5f));

                // Compute the sample position in NDC.
                const Vector2d sample_position = frame.get_sample_position(pi.x + pf.x, pi.y + pf.y);

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
                framebuffer->add(Vector2u(pt), shading_result);

                // Only half the samples go into the second scratch framebuffer.
                if (i & 1)
                    second_framebuffer->add(Vector2u(pt), shading_result);
            }

            on_pixel_end(frame, pi, pt);
        }

        // Split the given block in two.
        void split_pixel_block(
            const PixelBlock&                        pb,
            std::deque<PixelBlock>&                  blocks,
            const int                                splitting_point) const
        {
            AABB2i f_half = pb.m_surface, s_half = pb.m_surface;

            if (pb.m_main_axis == PixelBlock::Axis::Horizontal)
            {
                // Split horizontally.
                assert(splitting_point > pb.m_surface.min.x);
                assert(splitting_point < pb.m_surface.max.x);
                f_half.max.x = splitting_point;
                s_half.min.x = splitting_point + 1;
            }
            else
            {
                // Split vertically.
                assert(splitting_point > pb.m_surface.min.y);
                assert(splitting_point < pb.m_surface.max.y);
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
    };
}


//
// AdaptiveTileRendererFactory class implementation.
//

Dictionary AdaptiveTileRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "batch_size",
        Dictionary()
            .insert("type", "int")
            .insert("default", "16")
            .insert("min", "1")
            .insert("max", "1000000")
            .insert("label", "Batch Size")
            .insert("help", "How many samples to render before each convergence estimation"));

    metadata.dictionaries().insert(
        "min_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "16")
            .insert("min", "0")
            .insert("max", "1000000")
            .insert("label", "Min Samples")
            .insert("help", "Number of uniform samples to render before adaptive sampling"));

    metadata.dictionaries().insert(
        "max_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "256")
            .insert("min", "0")
            .insert("max", "1000000")
            .insert("label", "Max Samples")
            .insert("help", "Maximum number of anti-aliasing samples (0 for unlimited)"));

    metadata.dictionaries().insert(
        "noise_threshold",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.1")
            .insert("min", "0.0001")
            .insert("max", "10000.0")
            .insert("label", "Noise Threshold")
            .insert("help", "Maximum amount of noise allowed in the image"));

    return metadata;
}

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

}   // namespace renderer
