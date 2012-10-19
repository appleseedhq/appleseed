
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
#include "renderer/kernel/rendering/generic/developedpixelbuffer.h"
#include "renderer/kernel/rendering/generic/pixelsampler.h"
#include "renderer/kernel/rendering/generic/sampleaccumulationbuffer.h"
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
#include "foundation/math/population.h"
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
            m_filter_half_width = truncate<int>(ceil(m_params.m_filter_radius) - 1.0);
            m_filter_half_height = truncate<int>(ceil(m_params.m_filter_radius) - 1.0);
            m_filter_width = 2 * m_filter_half_width + 1;
            m_filter_height = 2 * m_filter_half_height + 1;

            // Precompute pixel filter weights.
            precompute_filter_weights();

            // Precompute pixel ordering.
            if (m_params.m_sampler_type == Parameters::UniformSampler)
                precompute_pixel_ordering(frame);

            // Initialize the pixel sampler.
            initialize_pixel_sampler();

            // Create diagnostic AOVs for the adaptive pixel sampler.
            if (m_params.m_sampler_type == Parameters::AdaptiveSampler &&
                m_params.m_adaptive_sampler_diagnostics)
            {
                m_variation_aov_index = frame.aov_images().get_or_append("variation", PixelFormatFloat);
                m_contrast_aov_index = frame.aov_images().get_or_append("contrast", PixelFormatFloat);
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
            if (m_params.m_sampler_type == Parameters::UniformSampler)
                render_tile_uniform(frame, tile_x, tile_y, abort_switch);
            else render_tile_adaptive(frame, tile_x, tile_y, abort_switch);
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
            const float             m_max_contrast;
            const double            m_max_variation;
            const bool              m_adaptive_sampler_diagnostics;

            bool                    m_crop;                 // is cropping enabled?
            AABB2u                  m_crop_window;

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_filter_radius(params.get_required<double>("filter_size", 2.0))
              , m_min_samples(params.get_required<size_t>("min_samples", 1))
              , m_max_samples(params.get_required<size_t>("max_samples", 1))
              , m_max_contrast(params.get_optional<float>("max_contrast", 1.0f / 256))
              , m_max_variation(params.get_optional<double>("max_variation", 0.15))
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
        vector<float>                       m_filter_weights;

        // Pixel ordering.
        vector<Vector<int16, 2> >           m_pixel_ordering;

        // Pixel sampler.
        size_t                              m_sqrt_sample_count;
        PixelSampler                        m_pixel_sampler;

        // Adaptive sampler only.
        size_t                              m_variation_aov_index;
        size_t                              m_contrast_aov_index;
        size_t                              m_samples_aov_index;

        void precompute_filter_weights()
        {
            m_filter_weights.resize(m_filter_width * m_filter_height);

            for (int dy = -m_filter_half_height; dy <= m_filter_half_height; ++dy)
            {
                for (int dx = -m_filter_half_width; dx <= m_filter_half_width; ++dx)
                {
                    const int x = dx + m_filter_half_width;
                    const int y = dy + m_filter_half_height;

                    m_filter_weights[y * m_filter_width + x] =
                        static_cast<float>(m_params.m_filter->evaluate(dx, dy));
                }
            }
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
            m_sqrt_sample_count = round<size_t>(sqrt(static_cast<double>(m_params.m_max_samples)));
            m_pixel_sampler.initialize(m_sqrt_sample_count);

            RENDERER_LOG_INFO(
                "effective max subpixel grid size: " FMT_SIZE_T "x" FMT_SIZE_T,
                m_sqrt_sample_count,
                m_sqrt_sample_count);
        }

        void render_tile_uniform(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            AbortSwitch&                abort_switch)
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

            // Allocate the buffer that will hold the samples and the filter weights.
            SampleAccumulationBuffer sample_buffer(tile_width, tile_height, aov_count);

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
                const int ix = tile_origin_x + tx;
                const int iy = tile_origin_y + ty;

#ifdef DEBUG_BREAK_AT_PIXEL

                // Break in the debugger when this pixel is reached.
                if (Vector2u(ix, iy) == DEBUG_BREAK_AT_PIXEL)
                    BREAKPOINT();

#endif

                // If cropping is enabled, skip pixels outside the crop window.
                if (!m_params.m_crop || m_params.m_crop_window.contains(Vector2u(ix, iy)))
                {
                    render_pixel(
                        frame,
                        ix, iy,
                        tx, ty,
                        sample_buffer);
                }
            }

            // Develop the accumulation buffer to the tile.
            if (frame.is_premultiplied_alpha())
                sample_buffer.develop_to_tile_premult_alpha(tile, aov_tiles);
            else sample_buffer.develop_to_tile_straight_alpha(tile, aov_tiles);
        }

        void render_pixel(
            const Frame&                frame,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            SampleAccumulationBuffer&   sample_buffer)
        {
            const size_t aov_count = frame.aov_images().size();
            const int base_sx = ix * m_sqrt_sample_count;
            const int base_sy = iy * m_sqrt_sample_count;
            const int buffer_width = static_cast<int>(sample_buffer.get_width());
            const int buffer_height = static_cast<int>(sample_buffer.get_height());

            for (size_t sy = 0; sy < m_sqrt_sample_count; ++sy)
            {
                for (size_t sx = 0; sx < m_sqrt_sample_count; ++sx)
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

                    // Find the pixels affected by this sample.
                    const int minx = max(tx - m_filter_half_width, 0);
                    const int miny = max(ty - m_filter_half_height, 0);
                    const int maxx = min(tx + m_filter_half_width, buffer_width - 1);
                    const int maxy = min(ty + m_filter_half_height, buffer_height - 1);

                    // Add the contribution of this sample to the affected pixels.
                    for (int ry = miny; ry <= maxy; ++ry)
                    {
                        for (int rx = minx; rx <= maxx; ++rx)
                        {
                            const int x = rx + m_filter_half_width - tx;
                            const int y = ry + m_filter_half_height - ty;
                            const float weight = m_filter_weights[y * m_filter_width + x];
                            sample_buffer.add(rx, ry, shading_result, weight);
                        }
                    }
                }
            }
        }

        void render_tile_adaptive(
            const Frame&                frame,
            const size_t                tile_x,
            const size_t                tile_y,
            AbortSwitch&                abort_switch)
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
            const int tile_width = static_cast<int>(tile.get_width());
            const int tile_height = static_cast<int>(tile.get_height());

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

            // Allocate the buffer that will hold the samples and the filter weights.
            SampleAccumulationBuffer sample_buffer(tile_width, tile_height, aov_count);

            // Allocate the buffer that will hold the fully developed pixels used to compute contrast.
            DevelopedPixelBuffer pixel_buffer(
                tile_width,
                tile_height,
                aov_count,
                tile_bbox,
                frame.get_color_space(),
                m_params.m_max_contrast);

            // Allocate the tiles that will hold diagnostic AOVs.
            auto_ptr<Tile> diagnostics;
            if (m_params.m_adaptive_sampler_diagnostics)
                diagnostics.reset(new Tile(tile_width, tile_height, 3, PixelFormatFloat));

            // Render the top row and left column of pixels.
            render_top_left_border_pixels(
                frame,
                tile_bbox,
                tile_origin_x,
                tile_origin_y,
                pixel_buffer);

            // Loop over tile pixels.
            for (int ty = -m_filter_half_height; ty < tile_height + m_filter_half_height; ++ty)
            {
                for (int tx = -m_filter_half_width; tx < tile_width + m_filter_half_width; ++tx)
                {
                    // Cancel any work done on this tile if rendering is aborted.
                    if (abort_switch.is_aborted())
                        return;

                    // Compute the coordinates of the pixel in the padded image.
                    const int ix = tile_origin_x + tx;
                    const int iy = tile_origin_y + ty;

#ifdef DEBUG_BREAK_AT_PIXEL

                    // Break in the debugger when this pixel is reached.
                    if (Vector2u(ix, iy) == DEBUG_BREAK_AT_PIXEL)
                        BREAKPOINT();

#endif

                    // If cropping is enabled, skip pixels outside the crop window.
                    if (!m_params.m_crop || m_params.m_crop_window.contains(Vector2u(ix, iy)))
                    {
                        // Render and accumulate samples.
                        render_pixel_adaptive(
                            frame,
                            ix, iy,
                            tx, ty,
                            sample_buffer,
                            pixel_buffer,
                            diagnostics.get());
                    }
                }
            }

            tile_bbox.min.x -= tile_origin_x;
            tile_bbox.min.y -= tile_origin_y;
            tile_bbox.max.x -= tile_origin_x;
            tile_bbox.max.y -= tile_origin_y;

            // Develop the accumulation buffer to the tile.
            if (frame.is_premultiplied_alpha())
                sample_buffer.develop_to_tile_premult_alpha(tile_bbox, tile, aov_tiles);
            else sample_buffer.develop_to_tile_straight_alpha(tile_bbox, tile, aov_tiles);

            // Merge diagnostics AOVs.
            if (m_params.m_adaptive_sampler_diagnostics)
            {
                for (size_t y = tile_bbox.min.y; y <= tile_bbox.max.y; ++y)
                {
                    for (size_t x = tile_bbox.min.x; x <= tile_bbox.max.x; ++x)
                    {
                        Color3f c;
                        diagnostics->get_pixel(x, y, c);
                        aov_tiles.set_pixel(x, y, m_variation_aov_index, scalar_to_color(c[0]));
                        aov_tiles.set_pixel(x, y, m_contrast_aov_index, scalar_to_color(c[1]));
                        aov_tiles.set_pixel(x, y, m_samples_aov_index, scalar_to_color(c[2]));
                    }
                }
            }
        }

        void render_top_left_border_pixels(
            const Frame&                frame,
            const AABB2u&               tile_bbox,
            const size_t                tile_origin_x,
            const size_t                tile_origin_y,
            DevelopedPixelBuffer&       pixel_buffer)
        {
            if (tile_bbox.min.y > 0)
            {
                const size_t width = tile_bbox.extent()[0] + 1;
                for (size_t tx = 0; tx < width; ++tx)
                {
                    render_border_pixel(
                        frame,
                        tile_bbox.min.x + tx,
                        tile_bbox.min.y - 1,
                        tile_bbox.min.x + tx - tile_origin_x,
                        tile_bbox.min.y - 1 - tile_origin_y,
                        pixel_buffer);
                }
            }

            if (tile_bbox.min.x > 0)
            {
                const size_t height = tile_bbox.extent()[1] + 1;
                for (size_t ty = 0; ty < height; ++ty)
                {
                    render_border_pixel(
                        frame,
                        tile_bbox.min.x - 1,
                        tile_bbox.min.y + ty,
                        tile_bbox.min.x - 1 - tile_origin_x,
                        tile_bbox.min.y + ty - tile_origin_y,
                        pixel_buffer);
                }
            }
        }

        void render_border_pixel(
            const Frame&                frame,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            DevelopedPixelBuffer&       pixel_buffer)
        {
            const size_t aov_count = frame.aov_images().size();
            const int base_sx = ix * m_sqrt_sample_count;
            const int base_sy = iy * m_sqrt_sample_count;

            Color4f pixel_color(0.0f);

            SpectrumStack pixel_aovs(frame.aov_images().size());
            pixel_aovs.set(0.0f);

            for (size_t sy = 0; sy < m_sqrt_sample_count; ++sy)
            {
                for (size_t sx = 0; sx < m_sqrt_sample_count; ++sx)
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
                    shading_result.m_aovs.set_size(pixel_aovs.size());
                    m_sample_renderer->render_sample(
                        sampling_context,
                        sample_position,
                        shading_result);

                    // Accumulate the sample.
                    assert(shading_result.m_color_space == ColorSpaceLinearRGB);
                    pixel_color[0] += shading_result.m_color[0];
                    pixel_color[1] += shading_result.m_color[1];
                    pixel_color[2] += shading_result.m_color[2];
                    pixel_color[3] += shading_result.m_alpha[0];
                    pixel_aovs += shading_result.m_aovs;            // todo: add the first 4 components only of each AOV
                }
            }

            // Finish computing the pixel values.
            const float rcp_sample_count = 1.0f / (m_sqrt_sample_count * m_sqrt_sample_count);
            pixel_color *= rcp_sample_count;
            pixel_aovs *= rcp_sample_count;

            // Set the pixel in the pixel buffer.
            pixel_buffer.set_pixel_color(tx, ty, pixel_color);
            for (size_t i = 0; i < aov_count; ++i)
            {
                const Spectrum& aov = pixel_aovs[i];
                pixel_buffer.set_pixel_aov(tx, ty, i, Color3f(aov[0], aov[1], aov[2]));
            }
        }

        void render_pixel_adaptive(
            const Frame&                frame,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            SampleAccumulationBuffer&   sample_buffer,
            DevelopedPixelBuffer&       pixel_buffer,
            Tile*                       diagnostics)
        {
            const size_t aov_count = frame.aov_images().size();
            const int buffer_width = static_cast<int>(sample_buffer.get_width());
            const int buffer_height = static_cast<int>(sample_buffer.get_height());

            // Create a sampling context.
            const size_t pixel_index = iy * frame.image().properties().m_canvas_width + ix;
            const size_t instance = hashint32(static_cast<uint32>(pixel_index));
            SamplingContext sampling_context(
                m_rng,
                2,                      // number of dimensions
                0,                      // number of samples
                instance);

            Population<float> history;

            while (true)
            {
                assert(history.get_size() <= m_params.m_max_samples);

                const size_t remaining_samples = m_params.m_max_samples - history.get_size();

                if (remaining_samples == 0)
                    break;

                const size_t batch_size = min(m_params.m_min_samples, remaining_samples);

                for (size_t i = 0; i < batch_size; ++i)
                {
                    // Generate a uniform sample in [0,1)^2.
                    const Vector2d s = sampling_context.next_vector2<2>();

                    // Compute the sample position in NDC.
                    const Vector2d sample_position =
                        frame.get_sample_position(ix + s[0], iy + s[1]);

                    // Render the sample.
                    SamplingContext child_sampling_context = sampling_context.split(0, 0);
                    ShadingResult shading_result;
                    shading_result.m_aovs.set_size(aov_count);
                    m_sample_renderer->render_sample(
                        child_sampling_context,
                        sample_position,
                        shading_result);

                    // Find the pixels affected by this sample.
                    const int minx = max(tx - m_filter_half_width, 0);
                    const int miny = max(ty - m_filter_half_height, 0);
                    const int maxx = min(tx + m_filter_half_width, buffer_width - 1);
                    const int maxy = min(ty + m_filter_half_height, buffer_height - 1);

                    // Add the contribution of this sample to the affected pixels.
                    for (int ry = miny; ry <= maxy; ++ry)
                    {
                        for (int rx = minx; rx <= maxx; ++rx)
                        {
                            const int x = rx + m_filter_half_width - tx;
                            const int y = ry + m_filter_half_height - ty;
                            const float weight = m_filter_weights[y * m_filter_width + x];
                            sample_buffer.add(rx, ry, shading_result, weight);
                        }
                    }

                    // Update statistics for this pixel.
                    // todo: one history per AOV?
                    history.insert(
                        luminance(
                            Color3f(
                                shading_result.m_color[0],
                                shading_result.m_color[1],
                                shading_result.m_color[2])));
                }

                // Pixels outside the tile are always refined.
                if (tx >= 0 && ty >= 0 && tx < buffer_width && ty < buffer_height)
                {
                    // Develop the pixel.
                    sample_buffer.develop_pixel(tx, ty, pixel_buffer);

                    if (m_params.m_adaptive_sampler_diagnostics && history.get_size() == m_params.m_min_samples)
                    {
                        // Check variation.
                        const bool pass_variation_check =
                            history.get_var() <= m_params.m_max_variation;

                        // Check contrast.
                        const bool pass_contrast_check =
                            pixel_buffer.check_contrast(ix, iy, tx, ty);

                        // Output diagnostic AOVs.
                        diagnostics->set_component(tx, ty, 0, pass_variation_check ? 0.0f : 1.0f);
                        diagnostics->set_component(tx, ty, 1, pass_contrast_check ? 0.0f : 1.0f);

                        if (pass_contrast_check && pass_variation_check)
                            break;
                    }
                    else
                    {
                        // Check variation.
                        if (history.get_var() > m_params.m_max_variation)
                            continue;

                        // Check contrast.
                        if (!pixel_buffer.check_contrast(ix, iy, tx, ty))
                            continue;

                        break;
                    }
                }
            }

            // Output the 'samples' diagnostic AOV.
            if (m_params.m_adaptive_sampler_diagnostics &&
                tx >= 0 && ty >= 0 && tx < buffer_width && ty < buffer_height)
            {
                diagnostics->set_component(tx, ty, 2,
                    m_params.m_min_samples == m_params.m_max_samples
                        ? 1.0f
                        : fit(
                            static_cast<float>(history.get_size()),
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
