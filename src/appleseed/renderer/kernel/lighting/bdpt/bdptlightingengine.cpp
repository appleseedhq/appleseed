
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Aytek Aman, The appleseedhq Organization
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
#include "bdptlightingengine.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/forwardlightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"

// appleseed.foundation headers.
#include "foundation/utility/statistics.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Bidirectional Path Tracing lighting engine.
    //

    class BDPTLightingEngine
      : public ILightingEngine
    {
      public:
        struct Parameters
        {
            explicit Parameters(const ParamArray& params)
            {

            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "bdpt settings:\n");
            }
        };

        BDPTLightingEngine(
            const ForwardLightSampler&  light_sampler, 
            const ParamArray&           params)
          : m_light_sampler(light_sampler)
          ,  m_params(params)
        {
        }

        void release() override
        {
            delete this;
        }

        void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance) override      // output radiance, in W.sr^-1.m^-2
        {
            PathVisitor light_path_visitor;
            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, true> light_path_tracer(     // true = adjoint
                light_path_visitor,
                volume_visitor,
                ~0,
                1,
                ~0,
                ~0,
                ~0,
                ~0,
                shading_context.get_max_iterations());
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;

            return StatisticsVector::make("bdpt statistics", stats);
        }

      private:
        const Parameters            m_params;

        const ForwardLightSampler&  m_light_sampler;

        struct PathVisitor
        {
            PathVisitor()
            {
            }
        };

        struct VolumeVisitor
        {
            VolumeVisitor()
            {
            }
        };
    };
}

BDPTLightingEngineFactory::BDPTLightingEngineFactory(
    const ForwardLightSampler&  light_sampler,
    const ParamArray&           params)
  : m_light_sampler(light_sampler)
  , m_params(params)
{
    BDPTLightingEngine::Parameters(params).print();
}

void BDPTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* BDPTLightingEngineFactory::create()
{
    return new BDPTLightingEngine(m_light_sampler, m_params);
}

Dictionary BDPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    add_common_params_metadata(metadata, true);

    return metadata;
}

}   // namespace renderer
