
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/generic/pixelsampler.h"
#include "renderer/kernel/rendering/generic/sampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/generic/variationtracker.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/math/hash.h"
#include "foundation/math/minmax.h"
#include "foundation/math/ordering.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/breakpoint.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/job.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <memory>
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
    // We are using the discrete-to-continuous mapping described in:
    //
    //   A Pixel Is Not A Little Square, Technical Memo 6, Alvy Ray Smith
    //   http://alvyray.com/Memos/CG/Microsoft/6_pixel.pdf
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
            ISampleRendererFactory*     factory,
            const ParamArray&           params)
          : m_params(params)
          , m_sample_renderer(factory->create())
          , m_lighting_conditions(frame.get_lighting_conditions())
        {
            // Compute pixel filter extent.
            compute_pixel_filter_extent(frame);

            // Precompute pixel ordering.
            precompute_pixel_ordering(frame);

            // Initialize the pixel sampler.
            initialize_pixel_sampler();

            // Create diagnostic AOVs for the adaptive pixel sampler.
            if (m_params.m_sampler_type == Parameters::AdaptiveSampler &&
                m_params.m_adaptive_sampler_diagnostics)
            {
                m_variation_aov_index = frame.aov_images().get_or_append("variation", PixelFormatFloat);
                m_samples_aov_index = frame.aov_images().get_or_append("samples", PixelFormatFloat);
            }
        }

        virtual void release() override
        {
            delete this;
        }

        virtual void render_tile(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            AbortSwitch&                abort_switch) override
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

            // Allocate the buffer that will hold the samples and the filter weights.
            SampleAccumulationBuffer sample_buffer(tile_width, tile_height, aov_count);

            auto_ptr<SampleAccumulationBuffer> temp_sample_buffer;
            auto_ptr<Tile> diagnostics;

            if (m_params.m_sampler_type == Parameters::AdaptiveSampler)
            {
                // Allocate a second temporary buffer.
                temp_sample_buffer.reset(
                    new SampleAccumulationBuffer(
                        m_filter_half_width * 2 + 1,
                        m_filter_half_height * 2 + 1,
                        aov_count));

                // Allocate the tile that will hold diagnostic AOVs.
                if (m_params.m_adaptive_sampler_diagnostics)
                    diagnostics.reset(new Tile(tile_width, tile_height, 3, PixelFormatFloat));
            }

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
                if (tx >= static_cast<int>(tile_width + m_filter_half_width) ||
                    ty >= static_cast<int>(tile_height + m_filter_half_height))
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
                if (!m_params.m_crop || m_params.m_crop_window.contains(Vector2u(ix, iy)))
                {
                    // Render and accumulate samples.
                    if (m_params.m_sampler_type == Parameters::UniformSampler)
                    {
                        render_pixel_uniform(
                            frame,
                            tile_bbox,
                            ix, iy,
                            tx, ty,
                            sample_buffer);
                    }
                    else
                    {
                        render_pixel_adaptive(
                            frame,
                            tile_bbox,
                            ix, iy,
                            tx, ty,
                            sample_buffer,
                            *temp_sample_buffer.get(),
                            diagnostics.get());
                    }
                }
            }

            // Develop the accumulation buffer to the tile.
            if (frame.is_premultiplied_alpha())
                sample_buffer.develop_to_tile_premult_alpha(tile, aov_tiles);
            else sample_buffer.develop_to_tile_straight_alpha(tile, aov_tiles);

            // Merge diagnostics AOVs.
            if (m_params.m_sampler_type == Parameters::AdaptiveSampler &&
                m_params.m_adaptive_sampler_diagnostics)
            {
                for (size_t y = tile_bbox.min.y; y <= tile_bbox.max.y; ++y)
                {
                    for (size_t x = tile_bbox.min.x; x <= tile_bbox.max.x; ++x)
                    {
                        Color3f c;
                        diagnostics->get_pixel(x, y, c);
                        aov_tiles.set_pixel(x, y, m_variation_aov_index, scalar_to_color(c[0]));
                        aov_tiles.set_pixel(x, y, m_samples_aov_index, scalar_to_color(c[1]));
                    }
                }
            }
        }

        virtual StatisticsVector get_statistics() const override
        {
            return m_sample_renderer->get_statistics();
        }

      private:
        struct Parameters
        {
            typedef Filter2<double> FilterType;

            const double            m_filter_radius;
            auto_ptr<FilterType>    m_filter;

            enum SamplerType
            {
                UniformSampler,
                AdaptiveSampler
            };

            SamplerType             m_sampler_type;
            const size_t            m_min_samples;          // minimum number of samples per pixel
            const size_t            m_max_samples;          // maximum number of samples per pixel
            const float             m_max_variation;
            const bool              m_adaptive_sampler_diagnostics;

            bool                    m_crop;                 // is cropping enabled?
            AABB2u                  m_crop_window;

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_filter_radius(params.get_required<double>("filter_size", 2.0))
              , m_min_samples(params.get_required<size_t>("min_samples", 1))
              , m_max_samples(params.get_required<size_t>("max_samples", 1))
              , m_max_variation(pow(10.0f, -params.get_optional<float>("quality", 3.0f)))
              , m_adaptive_sampler_diagnostics(params.get_optional<bool>("enable_adaptive_sampler_diagnostics"))
            {
                // Retrieve filter parameter.
                const string filter_str = params.get_required<string>("filter", "mitchell");
                if (filter_str == "box")
                    m_filter.reset(new BoxFilter2<double>(m_filter_radius, m_filter_radius));
                else if (filter_str == "triangle")
                    m_filter.reset(new TriangleFilter2<double>(m_filter_radius, m_filter_radius));
                else if (filter_str == "gaussian")
                    m_filter.reset(new GaussianFilter2<double>(m_filter_radius, m_filter_radius, 8.0));
                else if (filter_str == "mitchell")
                    m_filter.reset(new MitchellFilter2<double>(m_filter_radius, m_filter_radius, 1.0/3, 1.0/3));
                else if (filter_str == "bspline")
                    m_filter.reset(new MitchellFilter2<double>(m_filter_radius, m_filter_radius, 1.0, 0.0));
                else if (filter_str == "catmull")
                    m_filter.reset(new MitchellFilter2<double>(m_filter_radius, m_filter_radius, 0.0, 0.5));
                else if (filter_str == "lanczos")
                    m_filter.reset(new LanczosFilter2<double>(m_filter_radius, m_filter_radius, 3.0));
                else
                {
                    RENDERER_LOG_ERROR(
                        "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                        filter_str.c_str(),
                        "filter",
                        "mitchell");
                    m_filter.reset(new MitchellFilter2<double>(m_filter_radius, m_filter_radius, 1.0/3, 1.0/3));
                }

                // Retrieve sampler parameter.
                const string sampler_str = params.get_required<string>("sampler", "uniform");
                if (sampler_str == "uniform")
                    m_sampler_type = UniformSampler;
                else if (sampler_str == "adaptive")
                    m_sampler_type = AdaptiveSampler;
                else
                {
                    RENDERER_LOG_ERROR(
                        "invalid value \"%s\" for parameter \"%s\", using default value \"%s\".",
                        sampler_str.c_str(),
                        "sampler",
                        "uniform");
                    m_sampler_type = UniformSampler;
                }

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
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        const LightingConditions&           m_lighting_conditions;
        SamplingContext::RNGType            m_rng;

        // Pixel filter.
        int                                 m_filter_half_width;
        int                                 m_filter_half_height;
        int                                 m_filter_width;
        int                                 m_filter_height;

        // Pixel ordering.
        vector<Vector<int16, 2> >           m_pixel_ordering;

        // Pixel sampler.
        int                                 m_sqrt_sample_count;
        PixelSampler                        m_pixel_sampler;

        // Adaptive sampler only.
        size_t                              m_variation_aov_index;
        size_t                              m_samples_aov_index;

        void compute_pixel_filter_extent(const Frame& frame)
        {
            m_filter_half_width = truncate<int>(ceil(m_params.m_filter_radius) - 1.0);
            m_filter_half_height = truncate<int>(ceil(m_params.m_filter_radius) - 1.0);
            m_filter_width = 2 * m_filter_half_width + 1;
            m_filter_height = 2 * m_filter_half_height + 1;

            const CanvasProperties& properties = frame.image().properties();
            const size_t padded_tile_width = properties.m_tile_width + 2 * m_filter_half_width;
            const size_t padded_tile_height = properties.m_tile_height + 2 * m_filter_half_height;
            const size_t padded_pixel_count = padded_tile_width * padded_tile_height;
            const size_t pixel_count = properties.m_tile_width * properties.m_tile_height;
            const size_t overhead_pixel_count = padded_pixel_count - pixel_count;
            const double wasted_effort = static_cast<double>(overhead_pixel_count) / pixel_count * 100.0;

            RENDERER_LOG(
                wasted_effort >= 10.0 ? LogMessage::Warning : LogMessage::Info,
                "rendering effort wasted by tile borders: %s (tile dimensions: %sx%s, filter dimensions: %sx%s)",
                pretty_percent(overhead_pixel_count, pixel_count).c_str(),
                pretty_uint(properties.m_tile_width).c_str(),
                pretty_uint(properties.m_tile_height).c_str(),
                pretty_uint(m_filter_width).c_str(),
                pretty_uint(m_filter_height).c_str());
        }

        void precompute_pixel_ordering(const Frame& frame)
        {
            // Compute the dimensions in pixels of the padded tile.
            const CanvasProperties& properties = frame.image().properties();
            const size_t padded_tile_width = properties.m_tile_width + 2 * m_filter_half_width;
            const size_t padded_tile_height = properties.m_tile_height + 2 * m_filter_half_height;
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
                m_pixel_ordering[i].x = static_cast<int16>(x) - m_filter_half_width;
                m_pixel_ordering[i].y = static_cast<int16>(y) - m_filter_half_height;
            }
        }

        void initialize_pixel_sampler()
        {
            m_sqrt_sample_count = round<int>(sqrt(static_cast<double>(m_params.m_max_samples)));
            m_pixel_sampler.initialize(m_sqrt_sample_count);

            RENDERER_LOG_INFO(
                "effective max subpixel grid size: %dx%d",
                m_sqrt_sample_count,
                m_sqrt_sample_count);
        }

        void render_pixel_uniform(
            const Frame&                frame,
            const AABB2u&               tile_bbox,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            SampleAccumulationBuffer&   sample_buffer)
        {
            // Find the pixels affected by this sample.
            const int minx = max(tx - m_filter_half_width,  static_cast<int>(tile_bbox.min.x));
            const int miny = max(ty - m_filter_half_height, static_cast<int>(tile_bbox.min.y));
            const int maxx = min(tx + m_filter_half_width,  static_cast<int>(tile_bbox.max.x));
            const int maxy = min(ty + m_filter_half_height, static_cast<int>(tile_bbox.max.y));

            const size_t aov_count = frame.aov_images().size();
            const int base_sx = ix * m_sqrt_sample_count;
            const int base_sy = iy * m_sqrt_sample_count;

            for (int sy = 0; sy < m_sqrt_sample_count; ++sy)
            {
                for (int sx = 0; sx < m_sqrt_sample_count; ++sx)
                {
                    // Compute the sample position in sample space and the instance number.
                    Vector2d s;
                    size_t instance;
                    m_pixel_sampler.sample(
                        base_sx + sx,
                        base_sy + sy,
                        s,
                        instance);

                    // Compute the sample position in NDC.
                    const Vector2d sample_position = frame.get_sample_position(s.x, s.y);

                    // Create a sampling context. We start with an initial dimension of 1,
                    // as this seems to give less correlation artifacts than when the
                    // initial dimension is set to 0 or 2.
                    SamplingContext sampling_context(
                        m_rng,
                        1,              // number of dimensions
                        instance,       // number of samples
                        instance);      // initial instance number

                    // Render the sample.
                    ShadingResult shading_result;
                    shading_result.m_aovs.set_size(aov_count);
                    m_sample_renderer->render_sample(
                        sampling_context,
                        sample_position,
                        shading_result);

                    // Add the contribution of this sample to the affected pixels.
                    const double dx = (s.x - ix) - (0.5 - tx);
                    const double dy = (s.y - iy) - (0.5 - ty);
                    for (int ry = miny; ry <= maxy; ++ry)
                    {
                        for (int rx = minx; rx <= maxx; ++rx)
                        {
                            const float weight =
                                static_cast<float>(m_params.m_filter->evaluate(dx - rx, dy - ry));
                            sample_buffer.add(rx, ry, shading_result, weight);
                        }
                    }
                }
            }
        }

        void render_pixel_adaptive(
            const Frame&                frame,
            const AABB2u&               tile_bbox,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            SampleAccumulationBuffer&   sample_buffer,
            SampleAccumulationBuffer&   temp_sample_buffer,
            Tile*                       diagnostics)
        {
            // Find the pixels affected by this sample.
            const int minx = max(tx - m_filter_half_width,  static_cast<int>(tile_bbox.min.x));
            const int miny = max(ty - m_filter_half_height, static_cast<int>(tile_bbox.min.y));
            const int maxx = min(tx + m_filter_half_width,  static_cast<int>(tile_bbox.max.x));
            const int maxy = min(ty + m_filter_half_height, static_cast<int>(tile_bbox.max.y));

            // Clear the temporary sample accumulation buffer.
            temp_sample_buffer.clear();

            // Create a sampling context.
            const size_t pixel_index = iy * frame.image().properties().m_canvas_width + ix;
            const size_t instance = hashint32(static_cast<uint32>(pixel_index));
            SamplingContext sampling_context(
                m_rng,
                2,                      // number of dimensions
                0,                      // number of samples
                instance);

            const size_t aov_count = frame.aov_images().size();
            VariationTracker trackers[3];

            while (true)
            {
                // Don't exceed 'max' samples in total.
                assert(trackers[0].get_size() <= m_params.m_max_samples);
                const size_t remaining_samples = m_params.m_max_samples - trackers[0].get_size();
                if (remaining_samples == 0)
                    break;

                // The first batch contains 'min' samples, each subsequent batch contains half that.
                const size_t samples_per_batch =
                    trackers[0].get_size() == 0
                        ? m_params.m_min_samples
                        : max<size_t>(m_params.m_min_samples / 2, 1);
                const size_t batch_size = min(samples_per_batch, remaining_samples);

                for (size_t i = 0; i < batch_size; ++i)
                {
                    // Generate a uniform sample in [0,1)^2.
                    const Vector2d s = sampling_context.next_vector2<2>();

                    // Compute the sample position in NDC.
                    const Vector2d sample_position =
                        frame.get_sample_position(ix + s.x, iy + s.y);

                    // Render the sample.
                    SamplingContext child_sampling_context = sampling_context.split(0, 0);
                    ShadingResult shading_result;
                    shading_result.m_aovs.set_size(aov_count);
                    m_sample_renderer->render_sample(
                        child_sampling_context,
                        sample_position,
                        shading_result);

                    // Add the contribution of this sample to the affected pixels in the temporary buffer.
                    const double dx = s.x - (0.5 - tx);
                    const double dy = s.y - (0.5 - ty);
                    for (int ry = miny; ry <= maxy; ++ry)
                    {
                        for (int rx = minx; rx <= maxx; ++rx)
                        {
                            const float weight =
                                static_cast<float>(m_params.m_filter->evaluate(dx - rx, dy - ry));
                            temp_sample_buffer.add(rx - minx, ry - miny, shading_result, weight);
                        }
                    }

                    // Update statistics for this pixel.
                    // todo: one tracker per AOV?
                    trackers[0].insert(shading_result.m_color[0]);
                    trackers[1].insert(shading_result.m_color[1]);
                    trackers[2].insert(shading_result.m_color[2]);
                }

                // Stop if the variation criterion are met.
                if (trackers[0].get_variation() <= m_params.m_max_variation &&
                    trackers[1].get_variation() <= m_params.m_max_variation &&
                    trackers[2].get_variation() <= m_params.m_max_variation)
                    break;
            }

            // Scale the samples from the temporary buffer and merge them into the accumulation buffer.
            const float rcp_sample_count = 1.0f / trackers[0].get_size();
            for (int ry = miny; ry <= maxy; ++ry)
            {
                for (int rx = minx; rx <= maxx; ++rx)
                    sample_buffer.add(temp_sample_buffer, rx - minx, ry - miny, rx, ry, rcp_sample_count);
            }

            // Output the diagnostic AOVs.
            if (m_params.m_adaptive_sampler_diagnostics &&
                tx >= static_cast<int>(tile_bbox.min.x) &&
                ty >= static_cast<int>(tile_bbox.min.y) &&
                tx <= static_cast<int>(tile_bbox.max.x) &&
                ty <= static_cast<int>(tile_bbox.max.y))
            {
                diagnostics->set_component(tx, ty, 0,
                    trackers[0].get_variation() <= m_params.m_max_variation ? 0.0f : 1.0f);

                diagnostics->set_component(tx, ty, 1,
                    m_params.m_min_samples == m_params.m_max_samples
                        ? 1.0f
                        : fit(
                            static_cast<float>(trackers[0].get_size()),
                            static_cast<float>(m_params.m_min_samples),
                            static_cast<float>(m_params.m_max_samples),
                            0.0f, 1.0f));
            }
        }

        static Color4f scalar_to_color(const float value)
        {
            return
                lerp(
                    Color4f(0.0f, 0.0f, 1.0f, 1.0f),
                    Color4f(1.0f, 0.0f, 0.0f, 1.0f),
                    saturate(value));
        }
    };
}


//
// GenericTileRendererFactory class implementation.
//

GenericTileRendererFactory::GenericTileRendererFactory(
    const Frame&                frame,
    ISampleRendererFactory*     factory,
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

ITileRenderer* GenericTileRendererFactory::create()
{
    return
        new GenericTileRenderer(
            m_frame,
            m_factory,
            m_params);
}

ITileRenderer* GenericTileRendererFactory::create(
    const Frame&                frame,
    ISampleRendererFactory*     factory,
    const ParamArray&           params)
{
    return
        new GenericTileRenderer(
            frame,
            factory,
            params);
}

}   // namespace renderer
