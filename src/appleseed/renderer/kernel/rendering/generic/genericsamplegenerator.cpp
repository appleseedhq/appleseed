
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/aov/aovaccumulator.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/localsampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/pixelcontext.h"
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/rendering/samplegeneratorbase.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/settingsparsing.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/filtersamplingtable.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cstdint>
#include <vector>

using namespace foundation;

namespace renderer
{

namespace
{
    class GenericSampleGenerator
      : public SampleGeneratorBase
    {
      public:
        GenericSampleGenerator(
            const Frame&                    frame,
            ISampleRendererFactory*         sample_renderer_factory,
            const ParamArray&               params,
            const size_t                    generator_index,
            const size_t                    generator_count)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_params(params)
          , m_canvas_width(frame.image().properties().m_canvas_width)
          , m_canvas_height(frame.image().properties().m_canvas_height)
          , m_window_origin_x(static_cast<int>(frame.get_crop_window().min.x))
          , m_window_origin_y(static_cast<int>(frame.get_crop_window().min.y))
          , m_window_width(static_cast<int>(frame.get_crop_window().extent()[0]))
          , m_window_height(static_cast<int>(frame.get_crop_window().extent()[1]))
          , m_sample_renderer(sample_renderer_factory->create(generator_index))
          , m_window_width_next_pow2(next_power(static_cast<double>(m_window_width), 2.0))
          , m_window_height_next_pow3(next_power(static_cast<double>(m_window_height), 3.0))
          , m_filter_sampling_table(frame.get_filter_sampling_table())
        {
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
            m_sample_renderer->print_settings();
        }

        void reset() override
        {
            SampleGeneratorBase::reset();
            m_rng = SamplingContext::RNGType();
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

      private:
        struct Parameters
        {
            const SamplingContext::Mode     m_sampling_mode;

            explicit Parameters(const ParamArray& params)
              : m_sampling_mode(get_sampling_context_mode(params))
            {
            }
        };

        const Parameters                    m_params;
        const size_t                        m_canvas_width;
        const size_t                        m_canvas_height;
        const int                           m_window_origin_x;
        const int                           m_window_origin_y;
        const int                           m_window_width;
        const int                           m_window_height;
        auto_release_ptr<ISampleRenderer>   m_sample_renderer;
        SamplingContext::RNGType            m_rng;

        const double                        m_window_width_next_pow2;
        const double                        m_window_height_next_pow3;

        const FilterSamplingTable&          m_filter_sampling_table;

        Population<std::uint64_t>           m_total_sampling_dim;

        AOVAccumulatorContainer             m_aov_accumulators;

        size_t generate_samples(
            const size_t                    sequence_index,
            SampleVector&                   samples) override
        {
            // Compute the sample position in NDC.
            const size_t Bases[2] = { 2, 3 };
            const Vector2d s = halton_sequence<double, 2>(Bases, sequence_index);

            // Compute the coordinates of the pixel in the padded crop window.
            const Vector2d t(s[0] * m_window_width_next_pow2, s[1] * m_window_height_next_pow3);
            const int x = truncate<int>(t[0]);
            const int y = truncate<int>(t[1]);

            // Reject samples that fall outside the actual frame.
            if (x >= m_window_width || y >= m_window_height)
                return 0;

            // Create a sampling context. We start with an initial dimension of 2,
            // corresponding to the Halton sequence used for the sample positions.
            SamplingContext sampling_context(
                m_rng,
                m_params.m_sampling_mode,
                2,                          // number of dimensions
                sequence_index,             // number of samples
                sequence_index);            // initial instance number

            // Sample the pixel filter.
            sampling_context.split_in_place(2, 1);
            const Vector2f f = sampling_context.next2<Vector2f>();
            const Vector2d pf(
                static_cast<double>(m_filter_sampling_table.sample(f.x)) + 0.5,
                static_cast<double>(m_filter_sampling_table.sample(f.y)) + 0.5);

            // Transform the sample position back to NDC.
            const Vector2d sample_position(
                (m_window_origin_x + x + pf[0]) / m_canvas_width,
                (m_window_origin_y + y + pf[1]) / m_canvas_height);

            // Create a pixel context that identifies the pixel and sample currently being rendered.
            const PixelContext pixel_context(
                Vector2i(m_window_origin_x + x, m_window_origin_y + y),
                sample_position);

            // Render the sample.
            ShadingResult shading_result;
            m_sample_renderer->render_sample(
                sampling_context,
                pixel_context,
                sample_position,
                m_aov_accumulators,
                shading_result);

            // Update sampling statistics.
            m_total_sampling_dim.insert(sampling_context.get_total_dimension());

            // Report then ignore invalid samples.
            if (!shading_result.is_valid())
            {
                signal_invalid_sample();
                return 0;
            }

            // Create a single sample.
            Sample sample;
            sample.m_pixel_coords = pixel_context.get_pixel_coords();
            sample.m_color = shading_result.m_main;
            samples.push_back(sample);

            return 1;
        }
    };
}


//
// GenericSampleGeneratorFactory class implementation.
//

GenericSampleGeneratorFactory::GenericSampleGeneratorFactory(
    const Frame&            frame,
    ISampleRendererFactory* sample_renderer_factory,
    const ParamArray&       params)
  : m_frame(frame)
  , m_sample_renderer_factory(sample_renderer_factory)
  , m_params(params)
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
            m_params,
            generator_index,
            generator_count);
}

SampleAccumulationBuffer* GenericSampleGeneratorFactory::create_sample_accumulation_buffer()
{
    const CanvasProperties& props = m_frame.image().properties();

    return
        new LocalSampleAccumulationBuffer(
            props.m_canvas_width,
            props.m_canvas_height);
}

}   // namespace renderer
