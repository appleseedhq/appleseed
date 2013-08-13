
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
#include "sppmlightingengine.h"

// appleseed.foundation headers.
#include "foundation/utility/statistics.h"

using namespace foundation;

namespace renderer
{

//
// Lighting engine implementing the Stochastic Progressive Photon Mapping algorithm.
//
// Reference:
//
//   Stochastic Progressive Photon Mapping
//   Toshiya Hachisuka, Henrik Wann Jensen
//   http://cs.au.dk/~toshiya/sppm.pdf
//

namespace
{
    class SPPMLightingEngine
      : public ILightingEngine
    {
      public:
        SPPMLightingEngine(
            const LightSampler&     light_sampler,
            const ParamArray&       params)
        {
        }

        virtual void release()
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,
            SpectrumStack&          aovs) OVERRIDE
        {
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return StatisticsVector();
        }
    };
}

//
// SPPMLightingEngineFactory class implementation.
//

SPPMLightingEngineFactory::SPPMLightingEngineFactory(
    const LightSampler& light_sampler,
    const ParamArray&   params)
  : m_light_sampler(light_sampler)
  , m_params(params)
{
}

void SPPMLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* SPPMLightingEngineFactory::create()
{
    return new SPPMLightingEngine(m_light_sampler, m_params);
}

}   // namespace renderer
