
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/sppm/sppmpasscallback.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>

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
            const SPPMPassCallback&     pass_callback,
            const LightSampler&         light_sampler,
            const ParamArray&           params)
          : m_pass_callback(pass_callback)
        {
        }

        virtual void release()
        {
            delete this;
        }

        virtual void compute_lighting(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            Spectrum&                   radiance,
            SpectrumStack&              aovs) OVERRIDE
        {
            radiance.set(0.0f);
            aovs.set(0.0f);

            // Retrieve the material at the shading point.
            const Material* material = shading_point.get_material();
            if (material == 0)
                return;

            // Retrieve the BSDF at the shading point.
            const BSDF* bsdf = material->get_bsdf();
            if (bsdf == 0)
                return;

            // Evaluate the BSDF's inputs.
            InputEvaluator input_evaluator(shading_context.get_texture_cache());
            bsdf->evaluate_inputs(input_evaluator, shading_point.get_uv(0));

            // Compute the outgoing direction (toward the camera).
            const Vector3d outgoing = -normalize(shading_point.get_ray().m_dir);

            float max_square_dist = 0.0f;

            // Search for the k nearest photons.
            const SPPMPhotonMap& photon_map = m_pass_callback.get_photon_map();
            knn::Answer<float> answer(100);
            knn::Query3f query(photon_map, answer);
            query.run(shading_point.get_point());

            // Loop over the nearby photons.
            const size_t answer_size = answer.size();
            for (size_t i = 0; i < answer_size; ++i)
            {
                // Retrieve the i'th photon.
                const knn::Answer<float>::Entry& photon = answer.get(i);
                const SPPMPhotonPayload& payload = photon_map.get_photon_payload(photon.m_index);

                // Evaluate the BSDF for this photon.
                Spectrum bsdf_value;
                const double bsdf_prob =
                    bsdf->evaluate(
                        input_evaluator.data(),
                        true,                                       // adjoint
                        true,                                       // multiply by |cos(incoming, normal)|
                        shading_point.get_geometric_normal(),
                        shading_point.get_shading_basis(),
                        outgoing,                                   // toward the camera
                        normalize(Vector3d(payload.m_incoming)),    // toward the light
                        BSDF::AllScatteringModes,
                        bsdf_value);
                if (bsdf_prob == 0.0)
                    continue;

                bsdf_value *= payload.m_flux;
                radiance += bsdf_value;

                if (max_square_dist < photon.m_distance)
                    max_square_dist = photon.m_distance;
            }

            radiance /= static_cast<float>(Pi * max_square_dist);
        }

        virtual StatisticsVector get_statistics() const OVERRIDE
        {
            return StatisticsVector();
        }

      private:
        const SPPMPassCallback& m_pass_callback;
    };
}

//
// SPPMLightingEngineFactory class implementation.
//

SPPMLightingEngineFactory::SPPMLightingEngineFactory(
    const SPPMPassCallback&     pass_callback,
    const LightSampler&         light_sampler,
    const ParamArray&           params)
  : m_pass_callback(pass_callback)
  , m_light_sampler(light_sampler)
  , m_params(params)
{
}

void SPPMLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* SPPMLightingEngineFactory::create()
{
    return
        new SPPMLightingEngine(
            m_pass_callback,    
            m_light_sampler,
            m_params);
}

}   // namespace renderer
