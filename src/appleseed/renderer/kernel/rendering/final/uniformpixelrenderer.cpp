
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
#include "uniformpixelrenderer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/rendering/final/pixelsampler.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/pixelrendererbase.h"
#include "renderer/kernel/rendering/shadingresultframebuffer.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/aabb.h"
#include "foundation/math/hash.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cmath>
#include <cstddef>

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
            const bool                  primary)
          : m_params(params)
          , m_sample_renderer(factory->create(primary))
          , m_sqrt_sample_count(round<int>(sqrt(static_cast<double>(m_params.m_samples))))
          , m_sample_count(m_sqrt_sample_count * m_sqrt_sample_count)
        {
            if (!m_params.m_decorrelate)
            {
                m_pixel_sampler.initialize(m_sqrt_sample_count);

                if (primary)
                {
                    RENDERER_LOG_INFO(
                        "effective max subpixel grid size: %dx%d",
                        m_sqrt_sample_count,
                        m_sqrt_sample_count);
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
        }

        virtual void on_tile_end(
            const Frame&                frame,
            Tile&                       tile,
            TileStack&                  aov_tiles) OVERRIDE
        {
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
            SamplingContext::RNGType&   rng,
            ShadingResultFrameBuffer&   framebuffer) OVERRIDE
        {
            const size_t aov_count = frame.aov_images().size();

            if (m_params.m_decorrelate)
            {
                // Create a sampling context.
                const size_t frame_width = frame.image().properties().m_canvas_width;
                const size_t instance = hashint32(static_cast<uint32>(iy * frame_width + ix));
                SamplingContext sampling_context(
                    rng,
                    2,                  // number of dimensions
                    0,                  // number of samples -- unknown
                    instance);          // initial instance number

                for (size_t i = 0; i < m_sample_count; ++i)
                {
                    // Generate a uniform sample in [0,1)^2.
                    const Vector2d s =
                        m_sample_count == 1
                            ? Vector2d(0.5)
                            : sampling_context.next_vector2<2>();

                    // Compute the sample position in NDC.
                    const Vector2d sample_position = frame.get_sample_position(ix + s.x, iy + s.y);

                    // Render the sample.
                    SamplingContext child_sampling_context(sampling_context);
                    ShadingResult shading_result;
                    shading_result.m_aovs.set_size(aov_count);
                    m_sample_renderer->render_sample(
                        child_sampling_context,
                        sample_position,
                        shading_result);

                    // Merge the sample into the framebuffer.
                    if (shading_result.is_valid_linear_rgb())
                        framebuffer.add(tx + s.x, ty + s.y, shading_result);
                    else signal_invalid_sample();
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
                            1,              // number of dimensions
                            instance,       // number of samples
                            instance);      // initial instance number -- end of sequence

                        // Render the sample.
                        ShadingResult shading_result;
                        shading_result.m_aovs.set_size(aov_count);
                        m_sample_renderer->render_sample(
                            sampling_context,
                            sample_position,
                            shading_result);

                        // Merge the sample into the framebuffer.
                        if (shading_result.is_valid_linear_rgb())
                            framebuffer.add(s.x - ix + tx, s.y - iy + ty, shading_result);
                        else signal_invalid_sample();
                    }
                }
            }
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return m_sample_renderer->get_statistics();
        }

      private:
        struct Parameters
        {
            const size_t    m_samples;
            const bool      m_decorrelate;

            explicit Parameters(const ParamArray& params)
              : m_samples(params.get_required<size_t>("samples", 1))
              , m_decorrelate(params.get_optional<bool>("decorrelate_pixels", true))
            {
            }
        };

        const Parameters                    m_params;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        const int                           m_sqrt_sample_count;
        const size_t                        m_sample_count;
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

IPixelRenderer* UniformPixelRendererFactory::create(const bool primary)
{
    return new UniformPixelRenderer(m_factory, m_params, primary);
}

IPixelRenderer* UniformPixelRendererFactory::create(
    ISampleRendererFactory*     factory,
    const ParamArray&           params,
    const bool                  primary)
{
    return new UniformPixelRenderer(factory, params, primary);
}

}   // namespace renderer
