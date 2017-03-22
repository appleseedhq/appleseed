
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "adaptivepixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/final/variationtracker.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/minmax.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <memory>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Adaptive pixel renderer.
    //

    class AdaptivePixelRenderer
      : public PixelRendererBase
    {
      public:
        AdaptivePixelRenderer(
            const Frame&                frame,
            ISampleRendererFactory*     factory,
            const ParamArray&           params,
            const size_t                thread_index)
          : m_params(params)
          , m_sample_renderer(factory->create(thread_index))
        {
            if (m_params.m_diagnostics)
            {
                m_variation_aov_index = frame.create_extra_aov_image("variation");
                m_samples_aov_index = frame.create_extra_aov_image("samples");

                if ((thread_index == 0) && (m_variation_aov_index == size_t(~0) || m_samples_aov_index == size_t(~0)))
                {
                    RENDERER_LOG_WARNING(
                        "could not create some of the diagnostic AOVs, maximum number of AOVs (" FMT_SIZE_T ") reached.",
                        MaxAOVCount);
                }
            }
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void on_tile_begin(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) APPLESEED_OVERRIDE
        {
            m_scratch_fb_half_width = truncate<int>(ceil(frame.get_filter().get_xradius()));
            m_scratch_fb_half_height = truncate<int>(ceil(frame.get_filter().get_yradius()));

            m_scratch_fb.reset(
                new ShadingResultFrameBuffer(
                    2 * m_scratch_fb_half_width + 1,
                    2 * m_scratch_fb_half_height + 1,
                    frame.aov_images().size(),
                    frame.get_filter()));

            if (m_params.m_diagnostics)
                m_diagnostics.reset(new Tile(tile.get_width(), tile.get_height(), 2, PixelFormatFloat));
        }

        virtual void on_tile_end(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) APPLESEED_OVERRIDE
        {
            if (m_params.m_diagnostics)
            {
                const size_t width = tile.get_width();
                const size_t height = tile.get_height();

                for (size_t y = 0; y < height; ++y)
                {
                    for (size_t x = 0; x < width; ++x)
                    {
                        Color<float, 2> values;
                        m_diagnostics->get_pixel(x, y, values);

                        if (m_variation_aov_index != size_t(~0))
                            aov_tiles.set_pixel(x, y, m_variation_aov_index, scalar_to_color(values[0]));

                        if (m_samples_aov_index != size_t(~0))
                            aov_tiles.set_pixel(x, y, m_samples_aov_index, scalar_to_color(values[1]));
                    }
                }
            }
        }

        virtual void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2i&               tile_bbox,
            const size_t                pass_hash,
            const Vector2i&             pi,
            const Vector2i&             pt,
            SamplingContext::RNGType&   rng,
            ShadingResultFrameBuffer&   framebuffer) APPLESEED_OVERRIDE
        {
            const size_t aov_count = frame.aov_images().size();

            on_pixel_begin();

            m_scratch_fb->clear();

            // Create a sampling context.
            const size_t frame_width = frame.image().properties().m_canvas_width;
            const size_t instance =
                mix_uint32(
                    static_cast<uint32>(pass_hash),
                    static_cast<uint32>(pi.y * frame_width + pi.x));
            SamplingContext sampling_context(
                rng,
                m_params.m_sampling_mode,
                2,                          // number of dimensions
                0,                          // number of samples -- unknown
                instance);                  // initial instance number

            VariationTracker trackers[3];

            while (true)
            {
                trackers[0].reset_variation();
                trackers[1].reset_variation();
                trackers[2].reset_variation();

                // Don't exceed 'max' samples in total.
                assert(trackers[0].get_size() <= m_params.m_max_samples);
                const size_t remaining_samples = m_params.m_max_samples - trackers[0].get_size();
                if (remaining_samples == 0)
                    break;

                // Each batch contains 'min' samples.
                const size_t batch_size = min(m_params.m_min_samples, remaining_samples);

                for (size_t i = 0; i < batch_size; ++i)
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
                        shading_result);

                    // Ignore invalid samples.
                    if (!shading_result.is_valid_linear_rgb())
                    {
                        signal_invalid_sample();
                        continue;
                    }

                    // Merge the sample into the scratch framebuffer.
                    m_scratch_fb->add(
                        static_cast<float>(m_scratch_fb_half_width + s.x),
                        static_cast<float>(m_scratch_fb_half_height + s.y),
                        shading_result);

                    // Update statistics for this pixel.
                    // todo: variation should be computed in a user-selectable color space, typically the target color space.
                    // todo: one tracker per AOV?
                    trackers[0].insert(shading_result.m_main.m_color[0]);
                    trackers[1].insert(shading_result.m_main.m_color[1]);
                    trackers[2].insert(shading_result.m_main.m_color[2]);
                }

                // Stop if the variation criterion is met.
                if (trackers[0].get_variation() <= m_params.m_max_variation &&
                    trackers[1].get_variation() <= m_params.m_max_variation &&
                    trackers[2].get_variation() <= m_params.m_max_variation)
                    break;
            }

            // Merge the scratch framebuffer into the output framebuffer.
            const float rcp_sample_count = 1.0f / trackers[0].get_size();
            for (int y = -m_scratch_fb_half_height; y <= m_scratch_fb_half_height; ++y)
            {
                for (int x = -m_scratch_fb_half_width; x <= m_scratch_fb_half_width; ++x)
                {
                    if (tile_bbox.contains(Vector2i(pt.x + x, pt.y + y)))
                    {
                        framebuffer.merge(                  // destination
                            pt.x + x,                       // destination X
                            pt.y + y,                       // destination Y
                            *m_scratch_fb.get(),            // source
                            m_scratch_fb_half_width + x,    // source X
                            m_scratch_fb_half_height + y,   // source Y
                            rcp_sample_count);              // scaling
                    }
                }
            }

            // Store diagnostics values in the diagnostics tile.
            if (m_params.m_diagnostics && tile_bbox.contains(pt))
            {
                Color<float, 2> values;

                values[0] =
                    saturate(
                        max(
                            trackers[0].get_variation(),
                            trackers[1].get_variation(),
                            trackers[2].get_variation())
                        / m_params.m_max_variation);

                values[1] =
                    m_params.m_min_samples == m_params.m_max_samples
                        ? 1.0f
                        : fit(
                            static_cast<float>(trackers[0].get_size()),
                            static_cast<float>(m_params.m_min_samples),
                            static_cast<float>(m_params.m_max_samples),
                            0.0f, 1.0f);

                m_diagnostics->set_pixel(pt.x, pt.y, values);
            }

            on_pixel_end(pi);
        }

        virtual StatisticsVector get_statistics() const APPLESEED_OVERRIDE
        {
            return m_sample_renderer->get_statistics();
        }

      private:
        struct Parameters
        {
            const SamplingContext::Mode     m_sampling_mode;
            const size_t                    m_min_samples;
            const size_t                    m_max_samples;
            const float                     m_max_variation;
            const bool                      m_diagnostics;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
              , m_min_samples(params.get_required<size_t>("min_samples", 16))
              , m_max_samples(params.get_required<size_t>("max_samples", 256))
              , m_max_variation(pow(10.0f, -params.get_optional<float>("quality", 2.0f)))
              , m_diagnostics(params.get_optional<bool>("enable_diagnostics", false))
            {
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        size_t                              m_variation_aov_index;
        size_t                              m_samples_aov_index;
        int                                 m_scratch_fb_half_width;
        int                                 m_scratch_fb_half_height;
        auto_ptr<ShadingResultFrameBuffer>  m_scratch_fb;
        auto_ptr<Tile>                      m_diagnostics;

        static Color4f scalar_to_color(const float value)
        {
            static const Color4f Blue(0.0f, 0.0f, 1.0f, 1.0f);
            static const Color4f Red(1.0f, 0.0f, 0.0f, 1.0f);
            return lerp(Blue, Red, saturate(value));
        }
    };
}


//
// AdaptivePixelRendererFactory class implementation.
//

AdaptivePixelRendererFactory::AdaptivePixelRendererFactory(
    const Frame&                frame,
    ISampleRendererFactory*     factory,
    const ParamArray&           params)
  : m_frame(frame)
  , m_factory(factory)
  , m_params(params)
{
}

void AdaptivePixelRendererFactory::release()
{
    delete this;
}

IPixelRenderer* AdaptivePixelRendererFactory::create(
    const size_t                thread_index)
{
    return new AdaptivePixelRenderer(
        m_frame,
        m_factory,
        m_params,
        thread_index);
}

Dictionary AdaptivePixelRendererFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "min_samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "16")
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
        "quality",
        Dictionary()
            .insert("type", "float")
            .insert("default", "2.0")
            .insert("label", "Quality")
            .insert("help", "Quality factor"));

    metadata.dictionaries().insert(
        "enable_diagnostics",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Enable Diagnostics")
            .insert(
                "help",
                "Enable adaptive sampling diagnostics"));

    return metadata;
}

}   // namespace renderer
