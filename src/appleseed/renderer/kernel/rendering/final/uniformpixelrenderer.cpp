
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
#include "uniformpixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/rendering/final/pixelsampler.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/samplingmode.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class TileStack; }

using namespace foundation;
using namespace std;

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
            ISampleRendererFactory*     factory,
            const ParamArray&           params,
            const size_t                thread_index)
          : m_params(params)
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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual void render_pixel(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles,
            const AABB2i&               tile_bbox,
            const PixelContext&         pixel_context,
            const size_t                pass_hash,
            const int                   tx,
            const int                   ty,
            SamplingContext::RNGType&   rng,
            ShadingResultFrameBuffer&   framebuffer) APPLESEED_OVERRIDE
        {
            const int ix = pixel_context.m_ix;
            const int iy = pixel_context.m_iy;
            const size_t aov_count = frame.aov_images().size();

            if (m_params.m_decorrelate)
            {
                // Create a sampling context.
                const size_t frame_width = frame.image().properties().m_canvas_width;
                const size_t instance = hash_uint32(static_cast<uint32>(pass_hash + iy * frame_width + ix));
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
                            ? sampling_context.next_vector2<2>()
                            : Vector2d(0.5);

                    // Compute the sample position in NDC.
                    const Vector2d sample_position = frame.get_sample_position(ix + s.x, iy + s.y);

                    // Create and initialize a shading result.
                    // The main output *must* be set by the sample renderer (typically, by the surface shader).
                    ShadingResult shading_result(aov_count);
                    shading_result.set_aovs_to_transparent_black_linear_rgba();

                    // Render the sample.
                    SamplingContext child_sampling_context(sampling_context);
                    m_sample_renderer->render_sample(
                        child_sampling_context,
                        pixel_context,
                        sample_position,
                        shading_result);

                    // Merge the sample into the framebuffer.
                    if (shading_result.is_valid_linear_rgb())
                    {
                        framebuffer.add(
                            static_cast<float>(tx + s.x),
                            static_cast<float>(ty + s.y),
                            shading_result);
                    }
                    else signal_invalid_sample(ix, iy);
                }
            }
            else
            {
                const int base_sx = ix * m_sqrt_sample_count;
                const int base_sy = iy * m_sqrt_sample_count;

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
                            shading_result);

                        // Merge the sample into the framebuffer.
                        if (shading_result.is_valid_linear_rgb())
                        {
                            framebuffer.add(
                                static_cast<float>(s.x - ix + tx),
                                static_cast<float>(s.y - iy + ty),
                                shading_result);
                        }
                        else signal_invalid_sample(ix, iy);
                    }
                }
            }
        }

        virtual StatisticsVector get_statistics() const APPLESEED_OVERRIDE
        {
            return m_sample_renderer->get_statistics();
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
              , m_samples(params.get_required<size_t>("samples", 1))
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
    };
}


//
// UniformPixelRendererFactory class implementation.
//

UniformPixelRendererFactory::UniformPixelRendererFactory(
    ISampleRendererFactory*     factory,
    const ParamArray&           params)
  : m_factory(factory)
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
    return new UniformPixelRenderer(m_factory, m_params, thread_index);
}

Dictionary UniformPixelRendererFactory::get_params_metadata()
{
    Dictionary metadata;
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
            .insert("label", "Force Anti-aliasing")
            .insert(
                "help",
                "When using 1 sample/pixel and force_antialiasing is disabled, samples are placed in the middle of the pixels"));

    return metadata;
}

}   // namespace renderer
