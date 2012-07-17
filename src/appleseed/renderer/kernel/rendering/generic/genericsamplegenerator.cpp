
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
#include "genericsamplegenerator.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/imageimportancesampler.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/localaccumulationframebuffer.h"
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/rendering/samplegeneratorbase.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <vector>

// Forward declarations.
namespace foundation    { class LightingConditions; }

using namespace foundation;
using namespace std;

#undef ADAPTIVE_IMAGE_SAMPLING

namespace renderer
{

namespace
{
    //
    // GenericSampleGenerator class implementation.
    //

    class PixelSampler
    {
      public:
        PixelSampler(
            const size_t    width,
            const size_t    height)
          : m_width(width)
          , m_height(height)
          , m_pixel_history(width * height)
        {
            const size_t pixel_count = m_pixel_history.size();
            for (size_t i = 0; i < pixel_count; ++i)
            {
                PixelHistory& history = m_pixel_history[i];
                history.m_avg = 0.0;
                history.m_s = 0.0;
                history.m_size = 0;
            }
        }

        double operator()(
            const size_t    x,
            const size_t    y) const
        {
            assert(x < m_width);
            assert(y < m_height);

            const size_t MinSamplesPerPixel = 4;
            const double VariationThreshold = 0.1;
            const double LowProbability = 1.0 / 6;
            const double HighProbability = 1.0;

            const PixelHistory& history = m_pixel_history[y * m_width + x];

            if (history.m_size < MinSamplesPerPixel)
                return HighProbability;

            if (history.m_avg == 0.0)
                return HighProbability;

            // Compute the standard deviation.
            const double deviation = sqrt(history.m_s / history.m_size);

            // Compute the coefficient of variation.
            const double variation = deviation / history.m_avg;
            assert(variation >= 0.0);

#if 0
            return variation < VariationThreshold ? LowProbability : HighProbability;
#else
            return clamp(variation, LowProbability, HighProbability);
#endif
        }

        void update_pixel_variance(
            const size_t    x,
            const size_t    y,
            const float     luminance)
        {
            assert(x < m_width);
            assert(y < m_height);
            assert(luminance >= 0.0f);

            PixelHistory& history = m_pixel_history[y * m_width + x];

            const double double_luminance = static_cast<double>(luminance);

            // Compute residual value.
            const double residual = double_luminance - history.m_avg;

            // Compute the new size of the population.
            const size_t new_size = history.m_size + 1;

            // Compute the new mean value of the population.
            const double new_avg = history.m_avg + residual / new_size;

            // Update s.
            history.m_s += residual * (double_luminance - new_avg);

            // Update the mean value of the population.
            history.m_avg = new_avg;

            // Update the size of the population.
            history.m_size = new_size;
        }

      private:
        struct PixelHistory
        {
            double  m_avg;
            double  m_s;
            size_t  m_size;
        };

        const size_t            m_width;
        const size_t            m_height;
        vector<PixelHistory>    m_pixel_history;
    };

    class GenericSampleGenerator
      : public SampleGeneratorBase
    {
      public:
        GenericSampleGenerator(
            const Frame&                    frame,
            ISampleRendererFactory*         sample_renderer_factory,
            const size_t                    generator_index,
            const size_t                    generator_count)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_frame(frame)
          , m_frame_props(frame.image().properties())
          , m_lighting_conditions(frame.get_lighting_conditions())
          , m_sample_renderer(sample_renderer_factory->create())
#ifdef ADAPTIVE_IMAGE_SAMPLING
          , m_pixel_sampler(m_frame_props.m_canvas_width, m_frame_props.m_canvas_height)
          , m_image_sampler(m_frame_props.m_canvas_width, m_frame_props.m_canvas_height, m_pixel_sampler)
          , m_sample_count(0)
#else
          , m_frame_width_next_pow2(next_power(static_cast<double>(m_frame_props.m_canvas_width), 2.0))
          , m_frame_height_next_pow3(next_power(static_cast<double>(m_frame_props.m_canvas_height), 3.0))
          , m_scale_x(m_frame_width_next_pow2 / m_frame_props.m_canvas_width)
          , m_scale_y(m_frame_height_next_pow3 / m_frame_props.m_canvas_height)
#endif
        {
        }

        virtual void release() override
        {
            delete this;
        }

        virtual void reset() override
        {
            SampleGeneratorBase::reset();
            m_rng = MersenneTwister();
        }

        virtual StatisticsVector get_statistics() const override
        {
            Statistics stats;
            stats.insert("max. samp. dim.", m_total_sampling_dim);
            stats.insert("max. samp. inst.", m_total_sampling_inst);

            StatisticsVector vec;
            vec.insert("generic sample generator statistics", stats);
            vec.merge(m_sample_renderer->get_statistics());

            return vec;
        }

      private:
        const Frame&                        m_frame;
        const CanvasProperties&             m_frame_props;
        const LightingConditions&           m_lighting_conditions;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        MersenneTwister                     m_rng;

#ifdef ADAPTIVE_IMAGE_SAMPLING
        PixelSampler                        m_pixel_sampler;
        ImageImportanceSampler<double>      m_image_sampler;
        size_t                              m_sample_count;
#else
        const double                        m_frame_width_next_pow2;
        const double                        m_frame_height_next_pow3;
        const double                        m_scale_x;
        const double                        m_scale_y;
#endif

        Population<size_t>                  m_total_sampling_dim;
        Population<size_t>                  m_total_sampling_inst;

        virtual size_t generate_samples(
            const size_t                    sequence_index,
            SampleVector&                   samples) override
        {
#ifdef ADAPTIVE_IMAGE_SAMPLING

            // Generate a uniform sample in [0,1)^2.
            const size_t Bases[2] = { 2, 3 };
            const Vector2d s = halton_sequence<double, 2>(Bases, sequence_index);

            // Choose a pixel.
            size_t pixel_x, pixel_y;
            double pixel_prob;
            m_image_sampler.sample(s, pixel_x, pixel_y, pixel_prob);

            // Create a sampling context. We start with an initial dimension of 2,
            // corresponding to the Halton sequence used for the sample positions.
            SamplingContext sampling_context(
                m_rng,
                2,                          // number of dimensions
                0,                          // number of samples
                sequence_index);            // initial instance number

            // Compute the sample position in NDC.
            sampling_context.split_in_place(2, 1);
            const Vector2d subpixel = sampling_context.next_vector2<2>();
            const Vector2d sample_position(
                (pixel_x + subpixel.x) * m_frame_props.m_rcp_canvas_width,
                (pixel_y + subpixel.y) * m_frame_props.m_rcp_canvas_height);

#else

            // Compute the sample position in NDC.
            const size_t Bases[2] = { 2, 3 };
            const Vector2d s = halton_sequence<double, 2>(Bases, sequence_index);

            // Compute the coordinates of the pixel in the larger frame.
            const size_t x = truncate<size_t>(s[0] * m_frame_width_next_pow2);
            const size_t y = truncate<size_t>(s[1] * m_frame_height_next_pow3);

            // Reject samples that fall outside the actual frame.
            if (x >= m_frame_props.m_canvas_width || y >= m_frame_props.m_canvas_height)
                return 0;

            // Transform back the pixel coordinates to NDC.
            const Vector2d sample_position(s[0] * m_scale_x, s[1] * m_scale_y);

            // Create a sampling context. We start with an initial dimension of 2,
            // corresponding to the Halton sequence used for the sample positions.
            SamplingContext sampling_context(
                m_rng,
                2,                          // number of dimensions
                sequence_index,             // number of samples
                sequence_index);            // initial instance number

#endif

            // Render the sample.
            ShadingResult shading_result;
            m_sample_renderer->render_sample(
                sampling_context,
                sample_position,
                shading_result);

            // Transform the sample to the linear RGB color space.
            shading_result.transform_to_linear_rgb(m_lighting_conditions);

            // Create a single sample.
            Sample sample;
            sample.m_position = sample_position;
            sample.m_color[0] = shading_result.m_color[0];
            sample.m_color[1] = shading_result.m_color[1];
            sample.m_color[2] = shading_result.m_color[2];
            sample.m_color[3] = shading_result.m_alpha[0];
            samples.push_back(sample);

#ifdef ADAPTIVE_IMAGE_SAMPLING

            // Update pixel variance.
            m_pixel_sampler.update_pixel_variance(
                pixel_x,
                pixel_y,
                luminance(sample.m_color.rgb()));

            // Rebuild the pixel CDF regularly.
            const size_t ImageSamplerUpdatePeriod = 256 * 1000;
            if (++m_sample_count == ImageSamplerUpdatePeriod)
            {
                RENDERER_LOG_DEBUG("rebuilding pixel cdf...");
                m_image_sampler.rebuild(m_pixel_sampler);
                m_sample_count = 0;
            }

#endif

            m_total_sampling_dim.insert(sampling_context.get_total_dimension());
            m_total_sampling_inst.insert(sampling_context.get_total_instance());

            return 1;
        }
    };
}


//
// GenericSampleGeneratorFactory class implementation.
//

GenericSampleGeneratorFactory::GenericSampleGeneratorFactory(
    const Frame&            frame,
    ISampleRendererFactory* sample_renderer_factory)
  : m_frame(frame)
  , m_sample_renderer_factory(sample_renderer_factory)
{
}

void GenericSampleGeneratorFactory::release()
{
    delete this;
}

ISampleGenerator* GenericSampleGeneratorFactory::create(
    const size_t            generator_index,
    const size_t            generator_count)
{
    return
        new GenericSampleGenerator(
            m_frame,
            m_sample_renderer_factory,
            generator_index,
            generator_count);
}

AccumulationFramebuffer* GenericSampleGeneratorFactory::create_accumulation_framebuffer(
    const size_t            canvas_width,
    const size_t            canvas_height)
{
    return
        new LocalAccumulationFramebuffer(
            canvas_width,
            canvas_height);
}

}   // namespace renderer
