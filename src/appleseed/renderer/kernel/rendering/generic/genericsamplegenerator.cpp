
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
#include "genericsamplegenerator.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rendering/isamplerenderer.h"
#include "renderer/kernel/rendering/localsampleaccumulationbuffer.h"
#include "renderer/kernel/rendering/sample.h"
#include "renderer/kernel/rendering/samplegeneratorbase.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/population.h"
#include "foundation/math/qmc.h"
#include "foundation/math/rng.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <vector>

// Forward declarations.
namespace foundation    { class LightingConditions; }

using namespace foundation;
using namespace std;

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
            const size_t                    generator_index,
            const size_t                    generator_count,
            const bool                      primary)
          : SampleGeneratorBase(generator_index, generator_count)
          , m_frame(frame)
          , m_frame_props(frame.image().properties())
          , m_lighting_conditions(frame.get_lighting_conditions())
          , m_sample_renderer(sample_renderer_factory->create(primary))
          , m_frame_width_next_pow2(next_power(static_cast<double>(m_frame_props.m_canvas_width), 2.0))
          , m_frame_height_next_pow3(next_power(static_cast<double>(m_frame_props.m_canvas_height), 3.0))
        {
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual void reset() OVERRIDE
        {
            SampleGeneratorBase::reset();
            m_rng = MersenneTwister();
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
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

        const double                        m_frame_width_next_pow2;
        const double                        m_frame_height_next_pow3;

        Population<uint64>                  m_total_sampling_dim;
        Population<uint64>                  m_total_sampling_inst;

        virtual size_t generate_samples(
            const size_t                    sequence_index,
            SampleVector&                   samples) OVERRIDE
        {
            // Compute the sample position in NDC.
            const size_t Bases[2] = { 2, 3 };
            const Vector2d s = halton_sequence<double, 2>(Bases, sequence_index);

            // Compute the coordinates of the pixel in the larger frame.
            const Vector2d t(s[0] * m_frame_width_next_pow2, s[1] * m_frame_height_next_pow3);
            const size_t x = truncate<size_t>(t[0]);
            const size_t y = truncate<size_t>(t[1]);

            // Reject samples that fall outside the actual frame.
            if (x >= m_frame_props.m_canvas_width || y >= m_frame_props.m_canvas_height)
                return 0;

            // Transform the sample position back to NDC. Full precision divisions are required
            // to ensure that the sample position indeed lies in the [0,1)^2 interval.
            const Vector2d sample_position(
                t[0] / m_frame_props.m_canvas_width,
                t[1] / m_frame_props.m_canvas_height);

            // Create a sampling context. We start with an initial dimension of 2,
            // corresponding to the Halton sequence used for the sample positions.
            SamplingContext sampling_context(
                m_rng,
                2,                          // number of dimensions
                sequence_index,             // number of samples
                sequence_index);            // initial instance number

            // Render the sample.
            ShadingResult shading_result;
            m_sample_renderer->render_sample(
                sampling_context,
                sample_position,
                shading_result);

            // Ignore invalid samples.
            if (!shading_result.is_valid_linear_rgb())
                return 0;

            // Create a single sample.
            Sample sample;
            sample.m_position = sample_position;
            sample.m_color[0] = shading_result.m_color[0];
            sample.m_color[1] = shading_result.m_color[1];
            sample.m_color[2] = shading_result.m_color[2];
            sample.m_color[3] = shading_result.m_alpha[0];
            samples.push_back(sample);

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
    const size_t            generator_count,
    const bool              primary)
{
    return
        new GenericSampleGenerator(
            m_frame,
            m_sample_renderer_factory,
            generator_index,
            generator_count,
            primary);
}

SampleAccumulationBuffer* GenericSampleGeneratorFactory::create_sample_accumulation_buffer()
{
    const CanvasProperties& props = m_frame.image().properties();

    return
        new LocalSampleAccumulationBuffer(
            props.m_canvas_width,
            props.m_canvas_height,
            m_frame.get_filter());
}

}   // namespace renderer
