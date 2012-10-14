
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
            ISampleRendererFactory*     factory,
            const ParamArray&           params)
          : m_params(params)
          , m_sample_renderer(factory->create())
          , m_frame_properties(frame.image().properties())
          , m_frame_color_space(frame.get_color_space())
          , m_lighting_conditions(frame.get_lighting_conditions())
        {
            // Retrieve frame properties.
            const CanvasProperties& properties = frame.image().properties();
            const size_t tile_pixel_count = properties.m_tile_width * properties.m_tile_height;

            // Generate pixel ordering.
            vector<size_t> ordering;
            ordering.reserve(tile_pixel_count);
            if (m_params.m_sampler_type == Parameters::AdaptiveSampler)
            {
                // A linear order allows to compute contrast with the left and top neighbors.
                linear_ordering(ordering, tile_pixel_count);
            }
            else
            {
                hilbert_ordering(
                    ordering,
                    properties.m_tile_width,
                    properties.m_tile_height);
            }
            assert(ordering.size() == tile_pixel_count);

            // Convert pixel ordering to (x, y) representation.
            m_pixel_ordering.resize(tile_pixel_count);
            for (size_t i = 0; i < tile_pixel_count; ++i)
            {
                const size_t x = ordering[i] % properties.m_tile_width;
                const size_t y = ordering[i] / properties.m_tile_width;
                assert(x < properties.m_tile_width);
                assert(y < properties.m_tile_height);
                m_pixel_ordering[i].x = static_cast<uint16>(x);
                m_pixel_ordering[i].y = static_cast<uint16>(y);
            }

            // Initialize the pixel sampler.
            m_sqrt_sample_count = round<size_t>(sqrt(static_cast<double>(m_params.m_max_samples)));
            m_rcp_sample_count = 1.0f / (m_sqrt_sample_count * m_sqrt_sample_count);
            m_pixel_sampler.initialize(m_sqrt_sample_count);
            RENDERER_LOG_INFO(
                "effective max subpixel grid size: " FMT_SIZE_T "x" FMT_SIZE_T,
                m_sqrt_sample_count,
                m_sqrt_sample_count);

            if (m_params.m_sampler_type == Parameters::AdaptiveSampler)
            {
                if (m_params.m_adaptive_sampler_diagnostics)
                {
                    m_variation_aov_index = frame.aov_images().get_or_append("variation", PixelFormatFloat);
                    m_contrast_aov_index = frame.aov_images().get_or_append("contrast", PixelFormatFloat);
                    m_samples_aov_index = frame.aov_images().get_or_append("samples", PixelFormatFloat);
                }
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
            assert(tile_x < m_frame_properties.m_tile_count_x);
            assert(tile_y < m_frame_properties.m_tile_count_y);

            // Retrieve the tile and tile information.
            Tile& tile = frame.image().tile(tile_x, tile_y);
            TileStack aov_tiles(frame.aov_images().tiles(tile_x, tile_y));
            const size_t aov_count = frame.aov_images().size();
            const size_t tile_origin_x = m_frame_properties.m_tile_width * tile_x;
            const size_t tile_origin_y = m_frame_properties.m_tile_height * tile_y;
            const size_t tile_width = tile.get_width();
            const size_t tile_height = tile.get_height();

            // Compute the bounding box of the pixels that will need to be rendered.
            AABB2u rect;
            rect.min.x = tile_origin_x;
            rect.min.y = tile_origin_y;
            rect.max.x = tile_origin_x + tile_width - 1;
            rect.max.y = tile_origin_y + tile_height - 1;
            if (m_params.m_crop)
                rect = AABB2u::intersect(rect, m_params.m_crop_window);
            if (!rect.is_valid())
                return;

            // If the adaptive sampler is selected, render the top row and left column of pixels.
            vector<Pixel> top_pixels, left_pixels;
            if (m_params.m_sampler_type == Parameters::AdaptiveSampler)
                render_neighboring_pixels(frame, rect, top_pixels, left_pixels);

            // Loop over tile pixels.
            const size_t tile_pixel_count = m_pixel_ordering.size();
            for (size_t i = 0; i < tile_pixel_count; ++i)
            {
                // Retrieve the coordinates of the pixel in the tile.
                const size_t tx = static_cast<size_t>(m_pixel_ordering[i].x);
                const size_t ty = static_cast<size_t>(m_pixel_ordering[i].y);

                // Skip pixels outside of the tile.
                if (tx >= tile_width || ty >= tile_height)
                    continue;

                // Initialize the pixel values.
                Color4f pixel_color(0.0f);
                SpectrumStack pixel_aovs(aov_count);
                pixel_aovs.set(0.0f);

                // Compute the coordinates of the pixel in the image.
                const size_t ix = tile_origin_x + tx;
                const size_t iy = tile_origin_y + ty;

#ifdef DEBUG_BREAK_AT_PIXEL

                // Break in the debugger when this pixel is reached.
                if (Vector2u(ix, iy) == DEBUG_BREAK_AT_PIXEL)
                    BREAKPOINT();

#endif

                if (!abort_switch.is_aborted())
                {
                    // If cropping is enabled, skip pixels outside the crop window.
                    if (!m_params.m_crop || m_params.m_crop_window.contains(Vector2u(ix, iy)))
                    {
                        // Render, filter and accumulate samples.
                        if (m_params.m_sampler_type == Parameters::UniformSampler)
                        {
                            render_pixel(
                                frame,
                                ix, iy,
                                pixel_color,
                                pixel_aovs);
                        }
                        else
                        {
                            render_pixel_adaptive(
                                frame,
                                ix, iy,
                                rect,
                                top_pixels,
                                left_pixels,
                                pixel_color,
                                pixel_aovs);
                        }
                    }
                }

                // Optionally undo alpha premultiplication.
                if (!frame.is_premultiplied_alpha())
                {
                    const float rcp_alpha = pixel_color[3] == 0.0f ? 0.0f : 1.0f / pixel_color[3];
                    pixel_color.rgb() *= rcp_alpha;
                    pixel_aovs *= rcp_alpha;
                }

                // Store the pixel values.
                tile.set_pixel(tx, ty, pixel_color);
                aov_tiles.set_pixel(tx, ty, pixel_aovs, pixel_color.a);
            }
        }

        virtual StatisticsVector get_statistics() const override
        {
            return m_sample_renderer->get_statistics();
        }

      private:
        struct Parameters
        {
            enum SamplerType
            {
                UniformSampler,
                AdaptiveSampler
            };

            SamplerType         m_sampler_type;
            const size_t        m_min_samples;          // minimum number of samples per pixel
            const size_t        m_max_samples;          // maximum number of samples per pixel
            const float         m_max_contrast;
            const double        m_max_variation;
            const bool          m_adaptive_sampler_diagnostics;

            bool                m_crop;                 // is cropping enabled?
            AABB2u              m_crop_window;

            // Constructor, extract parameters.
            explicit Parameters(const ParamArray& params)
              : m_min_samples(params.get_required<size_t>("min_samples", 1))
              , m_max_samples(params.get_required<size_t>("max_samples", 1))
              , m_max_contrast(params.get_optional<float>("max_contrast", 1.0f / 256))
              , m_max_variation(params.get_optional<double>("max_variation", 0.15))
              , m_adaptive_sampler_diagnostics(params.get_optional<bool>("enable_adaptive_sampler_diagnostics"))
            {
                // Retrieve sampler parameter.
                const string sampler_str = params.get_optional<string>("sampler", "uniform");
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
                            AABB2u(Vector2u(0U), Vector2u(65535U)));
                }
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        const CanvasProperties&             m_frame_properties;
        const ColorSpace                    m_frame_color_space;
        const LightingConditions&           m_lighting_conditions;
        SamplingContext::RNGType            m_rng;

        // Pixel coordinates in a tile. Max tile size is 65536 x 65536 pixels.
        typedef Vector<uint16, 2> PixelCoords;

        vector<PixelCoords>                 m_pixel_ordering;

        size_t                              m_sqrt_sample_count;
        float                               m_rcp_sample_count;
        PixelSampler                        m_pixel_sampler;

        // Adaptive sampler only.
        struct Pixel
        {
            Color4f m_color;
            Color3f m_aovs[SpectrumStack::MaxSize];
        };
        size_t                              m_variation_aov_index;
        size_t                              m_contrast_aov_index;
        size_t                              m_samples_aov_index;

        void render_pixel(
            const Frame&                frame,
            const size_t                ix,
            const size_t                iy,
            Color4f&                    pixel_color,
            SpectrumStack&              pixel_aovs)
        {
            const size_t base_sx = ix * m_sqrt_sample_count;
            const size_t base_sy = iy * m_sqrt_sample_count;

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
                    // todo: implement proper sample filtering.
                    assert(shading_result.m_color_space == ColorSpaceLinearRGB);
                    pixel_color[0] += shading_result.m_color[0];
                    pixel_color[1] += shading_result.m_color[1];
                    pixel_color[2] += shading_result.m_color[2];
                    pixel_color[3] += shading_result.m_alpha[0];
                    pixel_aovs += shading_result.m_aovs;            // todo: add the first 4 components only of each AOV
                }
            }

            // Finish computing the pixel values.
            pixel_color *= m_rcp_sample_count;
            pixel_aovs *= m_rcp_sample_count;
        }

        void render_neighboring_pixels(
            const Frame&                frame,
            const AABB2u&               rect,
            vector<Pixel>&              top_pixels,
            vector<Pixel>&              left_pixels)
        {
            const size_t width = rect.extent()[0] + 1;
            const size_t height = rect.extent()[1] + 1;
            const size_t aov_count = frame.aov_images().size();

            top_pixels.resize(width);

            if (rect.min.y > 0)
            {
                for (size_t x = 0; x < width; ++x)
                {
                    Pixel& pixel = top_pixels[x];
                    pixel.m_color.set(0.0f);

                    SpectrumStack pixel_aovs(aov_count);
                    pixel_aovs.set(0.0f);

                    render_pixel(
                        frame,
                        rect.min.x + x,
                        rect.min.y - 1,
                        pixel.m_color,
                        pixel_aovs);

                    for (size_t i = 0; i < aov_count; ++i)
                    {
                        const Spectrum& aov = pixel_aovs[i];
                        pixel.m_aovs[i] = Color3f(aov[0], aov[1], aov[2]);
                    }

                    convert_to_frame_color_space(pixel, aov_count);
                }
            }

            left_pixels.resize(height);

            if (rect.min.x > 0)
            {
                for (size_t y = 0; y < height; ++y)
                {
                    Pixel& pixel = left_pixels[y];
                    pixel.m_color.set(0.0f);

                    SpectrumStack pixel_aovs(aov_count);
                    pixel_aovs.set(0.0f);

                    render_pixel(
                        frame,
                        rect.min.x - 1,
                        rect.min.y + y,
                        pixel.m_color,
                        pixel_aovs);

                    for (size_t i = 0; i < aov_count; ++i)
                    {
                        const Spectrum& aov = pixel_aovs[i];
                        pixel.m_aovs[i] = Color3f(aov[0], aov[1], aov[2]);
                    }

                    convert_to_frame_color_space(pixel, aov_count);
                }
            }
        }

        void render_pixel_adaptive(
            const Frame&                frame,
            const size_t                ix,
            const size_t                iy,
            const AABB2u&               rect,
            vector<Pixel>&              top_pixels,
            vector<Pixel>&              left_pixels,
            Color4f&                    pixel_color,
            SpectrumStack&              pixel_aovs)
        {
            // Create a sampling context.
            const size_t pixel_index = iy * frame.image().properties().m_canvas_width + ix;
            const size_t instance = hashint32(static_cast<uint32>(pixel_index));
            SamplingContext sampling_context(
                m_rng,
                2,                      // number of dimensions
                0,                      // number of samples
                instance);

            Population<float> history;

            Pixel& top_pixel = top_pixels[ix - rect.min.x];
            Pixel& left_pixel = left_pixels[iy - rect.min.y];
            Pixel current_pixel;

            const size_t aov_count = frame.aov_images().size();

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
                    shading_result.m_aovs.set_size(pixel_aovs.size());
                    m_sample_renderer->render_sample(
                        child_sampling_context,
                        sample_position,
                        shading_result);

                    // Accumulate the sample.
                    // todo: implement proper sample filtering.
                    assert(shading_result.m_color_space == ColorSpaceLinearRGB);
                    pixel_color[0] += shading_result.m_color[0];
                    pixel_color[1] += shading_result.m_color[1];
                    pixel_color[2] += shading_result.m_color[2];
                    pixel_color[3] += shading_result.m_alpha[0];
                    pixel_aovs += shading_result.m_aovs;            // todo: add the first 4 components only of each AOV

                    // Update statistics for this pixel.
                    // todo: one history per AOV.
                    history.insert(
                        luminance(
                            Color3f(
                                shading_result.m_color[0],
                                shading_result.m_color[1],
                                shading_result.m_color[2])));
                }

                // Compute the current pixel as if it was complete.
                const float rcp_sample_count = 1.0f / history.get_size();
                current_pixel.m_color = pixel_color * rcp_sample_count;
                for (size_t i = 0; i < aov_count; ++i)
                {
                    const Spectrum& aov = pixel_aovs[i];
                    current_pixel.m_aovs[i] = Color3f(aov[0], aov[1], aov[2]);
                    current_pixel.m_aovs[i] *= rcp_sample_count;
                }
                convert_to_frame_color_space(current_pixel, aov_count);

                if (m_params.m_adaptive_sampler_diagnostics && history.get_size() == m_params.m_min_samples)
                {
                    // Check variation.
                    const bool pass_variation_check =
                        history.get_var() <= m_params.m_max_variation;

                    // Check contrast.
                    bool pass_contrast_check = true;
                    if (rect.min.y > 0 || iy > 0)
                    {
                        // Check contrast with top neighbor.
                        if (!check_contrast(current_pixel, top_pixel, m_params.m_max_contrast, aov_count))
                            pass_contrast_check = false;
                    }
                    if (rect.min.x > 0 || ix > 0)
                    {
                        // Check contrast with left neighbor.
                        if (!check_contrast(current_pixel, left_pixel, m_params.m_max_contrast, aov_count))
                            pass_contrast_check = false;
                    }

                    // Output the 'variation' diagnostic AOV.
                    set_false_color(
                        pixel_aovs,
                        m_variation_aov_index,
                        pass_variation_check ? 0.0f : 1.0f);

                    // Output the 'contrast' diagnostic AOV.
                    set_false_color(
                        pixel_aovs,
                        m_contrast_aov_index,
                        pass_contrast_check ? 0.0f : 1.0f);

                    if (pass_contrast_check && pass_variation_check)
                        break;
                }
                else
                {
                    // Check variation.
                    if (history.get_var() > m_params.m_max_variation)
                        continue;

                    // Check contrast with top neighbor.
                    if (rect.min.y > 0 || iy > 0)
                    {
                        if (!check_contrast(current_pixel, top_pixel, m_params.m_max_contrast, aov_count))
                            continue;
                    }

                    // Check contrast with left neighbor.
                    if (rect.min.x > 0 || ix > 0)
                    {
                        if (!check_contrast(current_pixel, left_pixel, m_params.m_max_contrast, aov_count))
                            continue;
                    }

                    break;
                }
            }

            // Update top and left pixels.
            copy_pixel(top_pixel, current_pixel, aov_count);
            copy_pixel(left_pixel, current_pixel, aov_count);

            // Scale diagnostic AOVs to cancel the division performed below.
            const float sample_count = static_cast<float>(history.get_size());
            if (m_params.m_adaptive_sampler_diagnostics)
            {
                pixel_aovs[m_variation_aov_index] *= sample_count;
                pixel_aovs[m_contrast_aov_index] *= sample_count;
            }

            // Finish computing the pixel values.
            const float rcp_sample_count = 1.0f / sample_count;
            pixel_color *= rcp_sample_count;
            pixel_aovs *= rcp_sample_count;

            // Output the 'samples' diagnostic AOV.
            if (m_params.m_adaptive_sampler_diagnostics)
            {
                // Sample count AOV.
                if (m_params.m_min_samples == m_params.m_max_samples)
                    set_false_color(pixel_aovs, m_samples_aov_index, 1.0f);
                else
                {
                    const float normalized_sample_count =
                        fit(
                            sample_count,
                            static_cast<float>(m_params.m_min_samples),
                            static_cast<float>(m_params.m_max_samples),
                            0.0f, 1.0f);
                    set_false_color(pixel_aovs, m_samples_aov_index, normalized_sample_count);
                }
            }
        }

        void convert_to_frame_color_space(
            Pixel&                      pixel,
            const size_t                aov_count) const
        {
            pixel.m_color.rgb() =
                transform_color(pixel.m_color.rgb(), ColorSpaceLinearRGB, m_frame_color_space);

            for (size_t i = 0; i < aov_count; ++i)
            {
                if (!is_diagnostic_aov(i))
                {
                    pixel.m_aovs[i] =
                        transform_color(pixel.m_aovs[i], ColorSpaceLinearRGB, m_frame_color_space);
                }
            }
        }

        bool check_contrast(
            const Pixel&                a,
            const Pixel&                b,
            const float                 max_contrast,
            const size_t                aov_count) const
        {
            if (!check_contrast(a.m_color, b.m_color, max_contrast))
                return false;

            for (size_t i = 0; i < aov_count; ++i)
            {
                if (!is_diagnostic_aov(i))
                {
                    if (!check_contrast(a.m_aovs[i], b.m_aovs[i], max_contrast))
                        return false;
                }
            }

            return true;
        }

        template <typename T, size_t N>
        static bool check_contrast(
            const Color<T, N>&          a,
            const Color<T, N>&          b,
            const float                 max_contrast)
        {
            for (size_t i = 0; i < N; ++i)
            {
                if (abs(a[i] - b[i]) > max_contrast)
                    return false;
            }

            return true;
        }

        static void copy_pixel(
            Pixel&                      dest,
            const Pixel&                source,
            const size_t                aov_count)
        {
            dest.m_color = source.m_color;

            for (size_t i = 0; i < aov_count; ++i)
                dest.m_aovs[i] = source.m_aovs[i];
        }

        bool is_diagnostic_aov(const size_t aov_index) const
        {
            return
                m_params.m_adaptive_sampler_diagnostics &&
                (aov_index == m_variation_aov_index ||
                 aov_index == m_contrast_aov_index ||
                 aov_index == m_samples_aov_index);
        }

        static void set_false_color(
            SpectrumStack&              pixel_aovs,
            const size_t                aov_index,
            const float                 value)
        {
            const Color3f color =
                lerp(
                    Color3f(0.0f, 0.0f, 1.0f),
                    Color3f(1.0f, 0.0f, 0.0f),
                    saturate(value));

            pixel_aovs[aov_index][0] = color[0];
            pixel_aovs[aov_index][1] = color[1];
            pixel_aovs[aov_index][2] = color[2];
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
