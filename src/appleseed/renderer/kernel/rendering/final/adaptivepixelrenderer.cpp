
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
#include "adaptivepixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/aov/tilestack.h"
#include "renderer/kernel/rendering/final/variationtracker.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
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
      : public IPixelRenderer
    {
      public:
        AdaptivePixelRenderer(
            const Frame&                frame,
            ISampleRendererFactory*     factory,
            const ParamArray&           params)
          : m_params(params)
          , m_sample_renderer(factory->create())
        {
            if (m_params.m_diagnostics)
            {
                ImageStack& images = frame.aov_images();

                m_variation_aov_index = images.get("variation");
                if (m_variation_aov_index == ~0 && images.size() < SpectrumStack::MaxSize)
                    m_variation_aov_index = images.append("variation", PixelFormatFloat);

                m_samples_aov_index = images.get("samples");
                if (m_samples_aov_index == ~0 && images.size() < SpectrumStack::MaxSize)
                    m_samples_aov_index = images.append("samples", PixelFormatFloat);

                if (m_variation_aov_index == ~0 || m_samples_aov_index == ~0)
                {
                    RENDERER_LOG_WARNING(
                        "could not create some of the diagnostic AOVs, maximum number of AOVs (" FMT_SIZE_T ") reached.",
                        SpectrumStack::MaxSize);
                }
            }
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void on_tile_begin(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) OVERRIDE
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
                m_diagnostics.reset(new Tile(tile.get_width(), tile.get_height(), 4, PixelFormatFloat));
        }

        virtual void on_tile_end(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) OVERRIDE
        {
            if (m_params.m_diagnostics)
            {
                const size_t width = tile.get_width();
                const size_t height = tile.get_height();

                for (size_t y = 0; y < height; ++y)
                {
                    for (size_t x = 0; x < width; ++x)
                    {
                        Color4f values;
                        m_diagnostics->get_pixel(x, y, values);

                        if (m_variation_aov_index < ~0)
                            aov_tiles.set_pixel(x, y, m_variation_aov_index, Color4f(values[0], values[1], values[2], 1.0f));

                        if (m_samples_aov_index != ~0)
                            aov_tiles.set_pixel(x, y, m_samples_aov_index, scalar_to_color(values[1]));
                    }
                }
            }
        }

        virtual void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2u&               tile_bbox,
            const int                   ix,
            const int                   iy,
            const int                   tx,
            const int                   ty,
            ShadingResultFrameBuffer&   framebuffer) OVERRIDE
        {
            m_scratch_fb->clear();

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

                    // Merge the sample into the scratch framebuffer.
                    m_scratch_fb->add(
                        m_scratch_fb_half_width + s.x,
                        m_scratch_fb_half_height + s.y,
                        shading_result);

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

            // Merge the scratch framebuffer into the output framebuffer.
            const float rcp_sample_count = 1.0f / trackers[0].get_size();
            const size_t width = m_scratch_fb->get_width();
            const size_t height = m_scratch_fb->get_height();
            for (int y = -m_scratch_fb_half_height; y <= m_scratch_fb_half_height; ++y)
            {
                for (int x = -m_scratch_fb_half_width; x <= m_scratch_fb_half_width; ++x)
                {
                    if (tile_bbox.contains(Vector2i(tx + x, ty + y)))
                    {
                        framebuffer.merge(                  // destination
                            tx + x,                         // destination X
                            ty + y,                         // destination Y
                            *m_scratch_fb.get(),            // source
                            m_scratch_fb_half_width + x,    // source X
                            m_scratch_fb_half_height + y,   // source Y
                            rcp_sample_count);              // scaling
                    }
                }
            }

            // Store diagnostics values in the diagnostics tile.
            if (m_params.m_diagnostics && tile_bbox.contains(Vector2i(tx, ty)))
            {
                Color4f values;

                values[0] = saturate(trackers[0].get_variation() / m_params.m_max_variation);
                values[1] = saturate(trackers[1].get_variation() / m_params.m_max_variation);
                values[2] = saturate(trackers[2].get_variation() / m_params.m_max_variation);

                values[3] =
                    m_params.m_min_samples == m_params.m_max_samples
                        ? 1.0f
                        : fit(
                            static_cast<float>(trackers[0].get_size()),
                            static_cast<float>(m_params.m_min_samples),
                            static_cast<float>(m_params.m_max_samples),
                            0.0f, 1.0f);

                m_diagnostics->set_pixel(tx, ty, values);
            }
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return m_sample_renderer->get_statistics();
        }

      private:
        struct Parameters
        {
            const size_t    m_min_samples;
            const size_t    m_max_samples;
            const float     m_max_variation;
            const bool      m_diagnostics;

            explicit Parameters(const ParamArray& params)
              : m_min_samples(params.get_required<size_t>("min_samples", 1))
              , m_max_samples(params.get_required<size_t>("max_samples", 1))
              , m_max_variation(pow(10.0f, -params.get_optional<float>("quality", 3.0f)))
              , m_diagnostics(params.get_optional<bool>("enable_diagnostics"))
            {
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        SamplingContext::RNGType            m_rng;
        size_t                              m_variation_aov_index;
        size_t                              m_samples_aov_index;
        int                                 m_scratch_fb_half_width;
        int                                 m_scratch_fb_half_height;
        auto_ptr<ShadingResultFrameBuffer>  m_scratch_fb;
        auto_ptr<Tile>                      m_diagnostics;

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

IPixelRenderer* AdaptivePixelRendererFactory::create()
{
    return new AdaptivePixelRenderer(m_frame, m_factory, m_params);
}

IPixelRenderer* AdaptivePixelRendererFactory::create(
    const Frame&                frame,
    ISampleRendererFactory*     factory,
    const ParamArray&           params)
{
    return new AdaptivePixelRenderer(frame, factory, params);
}

}   // namespace renderer
