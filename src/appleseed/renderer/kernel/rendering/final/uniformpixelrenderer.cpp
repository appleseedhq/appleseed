
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
#include "uniformpixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/final/pixelsampler.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/population.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Uniform pixel renderer.
    //

    class UniformPixelRenderer
      : public PixelRendererBase
    {
      public:
        UniformPixelRenderer(
            const Frame&                frame,
            ISampleRendererFactory*     factory,
            const ParamArray&           params,
            const size_t                thread_index)
          : PixelRendererBase(frame, thread_index, params)
          , m_params(params)
          , m_sample_renderer(factory->create(thread_index))
          , m_sample_count(m_params.m_samples)
          , m_sqrt_sample_count(round<int>(sqrt(static_cast<double>(m_params.m_samples))))
        {
            if (!m_params.m_decorrelate)
            {
                m_pixel_sampler.initialize(m_sqrt_sample_count);

                if (thread_index == 0)
                {
                    if (params.get_optional<size_t>("passes", 1) > 1)
                        RENDERER_LOG_WARNING("doing multipass rendering with pixel decorrelation off.");

                    RENDERER_LOG_INFO(
                        "effective max subpixel grid size: %d x %d",
                        m_sqrt_sample_count,
                        m_sqrt_sample_count);
                }
            }
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "uniform pixel renderer settings:\n"
                "  sampling mode                 %s\n"
                "  samples                       " FMT_SIZE_T "\n"
                "  force antialiasing            %s\n"
                "  decorrelate pixels            %s\n"
                "  diagnostics                   %s",
                m_params.m_sampling_mode == SamplingContext::Mode::QMCMode ? "qmc" : "rng",
                m_params.m_samples,
                m_params.m_force_aa ? "on" : "off",
                m_params.m_decorrelate ? "on" : "off",
                are_diagnostics_enabled() ? "on" : "off");
        }

        void release() override
        {
            delete this;
        }

        void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2i&               tile_bbox,
            const size_t                pass_hash,
            const Vector2i&             pi,
            const Vector2i&             pt,
            AOVAccumulatorContainer&    aov_accumulators,
            ShadingResultFrameBuffer&   framebuffer) override
        {
            const size_t aov_count = frame.aov_images().size();

            on_pixel_begin(pi, pt, tile_bbox, aov_accumulators);

            if (m_params.m_decorrelate)
            {
                // Create a sampling context.
                const size_t frame_width = frame.image().properties().m_canvas_width;
                const size_t pixel_index = pi.y * frame_width + pi.x;
                const size_t instance = hash_uint32(static_cast<uint32>(pass_hash + pixel_index));
                SamplingContext::RNGType rng(pass_hash, instance);
                SamplingContext sampling_context(
                    rng,
                    m_params.m_sampling_mode,
                    2,                          // number of dimensions
                    0,                          // number of samples -- unknown
                    instance);                  // initial instance number

                for (size_t i = 0; i < m_sample_count; ++i)
                {
                    // Generate a uniform sample in [0,1)^2.
                    const Vector2d s =
                        m_sample_count > 1 || m_params.m_force_aa
                            ? sampling_context.next2<Vector2d>()
                            : Vector2d(0.5);

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
                        aov_accumulators,
                        shading_result);

                    // Update sampling statistics.
                    m_total_sampling_dim.insert(child_sampling_context.get_total_dimension());

                    // Merge the sample into the framebuffer.
                    if (shading_result.is_valid())
                    {
                        framebuffer.add(
                            static_cast<float>(pt.x + s.x),
                            static_cast<float>(pt.y + s.y),
                            shading_result);
                    }
                    else signal_invalid_sample();
                }
            }
            else
            {
                const size_t frame_width = frame.image().properties().m_canvas_width;
                const size_t pixel_index = pi.y * frame_width + pi.x;
                const size_t instance = hash_uint32(static_cast<uint32>(pass_hash + pixel_index));
                SamplingContext::RNGType rng(pass_hash, instance);

                const int base_sx = pi.x * m_sqrt_sample_count;
                const int base_sy = pi.y * m_sqrt_sample_count;

                for (int sy = 0; sy < m_sqrt_sample_count; ++sy)
                {
                    for (int sx = 0; sx < m_sqrt_sample_count; ++sx)
                    {
                        // Compute the sample position (in continuous image space) and the instance number.
                        Vector2d s;
                        size_t instance;
                        m_pixel_sampler.sample(base_sx + sx, base_sy + sy, s, instance);

                        // Compute the sample position in NDC.
                        const Vector2d sample_position = frame.get_sample_position(s.x, s.y);

                        // Create a pixel context that identifies the pixel and sample currently being rendered.
                        const PixelContext pixel_context(pi, sample_position);

                        // Create a sampling context. We start with an initial dimension of 1,
                        // as this seems to give less correlation artifacts than when the
                        // initial dimension is set to 0 or 2.
                        SamplingContext sampling_context(
                            rng,
                            m_params.m_sampling_mode,
                            1,                          // number of dimensions
                            instance,                   // number of samples
                            instance);                  // initial instance number -- end of sequence

                        // Render the sample.
                        ShadingResult shading_result(aov_count);
                        m_sample_renderer->render_sample(
                            sampling_context,
                            pixel_context,
                            sample_position,
                            aov_accumulators,
                            shading_result);

                        // Update sampling statistics.
                        m_total_sampling_dim.insert(sampling_context.get_total_dimension());

                        // Merge the sample into the framebuffer.
                        if (shading_result.is_valid())
                        {
                            framebuffer.add(
                                static_cast<float>(s.x - pi.x + pt.x),
                                static_cast<float>(s.y - pi.y + pt.y),
                                shading_result);
                        }
                        else signal_invalid_sample();
                    }
                }
            }

            on_pixel_end(pi, pt, tile_bbox, aov_accumulators);
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("max sampling dimension", m_total_sampling_dim);

            StatisticsVector vec;
            vec.insert("generic sample generator statistics", stats);
            vec.merge(m_sample_renderer->get_statistics());

            return vec;
        }

        size_t get_max_samples_per_pixel() const override
        {
            return m_sample_count;
        }

      private:
        struct Parameters
        {
            const SamplingContext::Mode     m_sampling_mode;
            const size_t                    m_samples;
            const bool                      m_force_aa;
            const bool                      m_decorrelate;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
              , m_samples(params.get_required<size_t>("samples", 64))
              , m_force_aa(params.get_optional<bool>("force_antialiasing", false))
              , m_decorrelate(params.get_optional<bool>("decorrelate_pixels", true))
            {
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        const size_t                        m_sample_count;
        const int                           m_sqrt_sample_count;
        PixelSampler                        m_pixel_sampler;
        Population<uint64>                  m_total_sampling_dim;
    };
}


//
// UniformPixelRendererFactory class implementation.
//

UniformPixelRendererFactory::UniformPixelRendererFactory(
    const Frame&                frame,
    ISampleRendererFactory*     factory,
    const ParamArray&           params)
  : m_frame(frame)
  , m_factory(factory)
  , m_params(params)
{
}

void UniformPixelRendererFactory::release()
{
    delete this;
}

IPixelRenderer* UniformPixelRendererFactory::create(
    const size_t                thread_index)
{
    return new UniformPixelRenderer(m_frame, m_factory, m_params, thread_index);
}

Dictionary UniformPixelRendererFactory::get_params_metadata()
{
    Dictionary metadata = PixelRendererBaseFactory::get_params_metadata();

    metadata.dictionaries().insert(
        "samples",
        Dictionary()
            .insert("type", "int")
            .insert("default", "64")
            .insert("label", "Samples")
            .insert("help", "Number of anti-aliasing samples"));

    metadata.dictionaries().insert(
        "force_antialiasing",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "false")
            .insert("label", "Force Anti-Aliasing")
            .insert(
                "help",
                "When using 1 sample/pixel and Force Anti-Aliasing is disabled, samples are placed at the center of pixels"));

    metadata.dictionaries().insert(
        "decorrelate_pixels",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "true")
            .insert("label", "Decorrelate Pixels")
            .insert(
                "help",
                "Avoid correlation patterns at the expense of slightly more sampling noise"));

    return metadata;
}

}   // namespace renderer
