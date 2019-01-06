
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Artem Bishev, The appleseedhq Organization
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

#pragma once

// Interface header.
#include "specularbtdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/phasefunction.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace renderer
{
    //
    // Phase Function BSDF.
    //

    APPLESEED_DECLARE_INPUT_VALUES(PhaseFunctionBSDFInputValues)
    {
        Spectrum m_albedo;
    };

    class PhaseFunctionBSDF
      : public BSDF
    {
      public:
        std::unique_ptr<foundation::PhaseFunction> m_phase_function;

        PhaseFunctionBSDF(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::Volume, params)
        {
            m_inputs.declare("scattering_coefficient", InputFormatSpectralReflectance);
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return "phasefunction_bsdf";
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            assert(m_phase_function);

            if (!ScatteringMode::has_volume(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            sampling_context.split_in_place(2, 1);
            foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
            foundation::Vector3f incoming;
            const float pdf = m_phase_function->sample(-sample.m_outgoing.get_value(), s, incoming);
            sample.set_to_scattering(ScatteringMode::Volume, pdf);
            sample.m_incoming = foundation::Dual3f(incoming);

            sample.m_value.set(0.0f);
            sample.m_value.m_volume = values->m_albedo;
            sample.m_value.m_volume *= pdf;
            sample.m_value.m_beauty = sample.m_value.m_volume;
        }

        float evaluate(
            const void*                     data,
            const bool                      adjoint,
            const bool                      cosine_mult,
            const foundation::Vector3f&     geometric_normal,
            const foundation::Basis3f&      shading_basis,
            const foundation::Vector3f&     outgoing,
            const foundation::Vector3f&     incoming,
            const int                       modes,
            DirectShadingComponents&        value) const override
        {
            assert(m_phase_function);

            const InputValues* values = static_cast<const InputValues*>(data);
            float pdf = evaluate_pdf(
                data,
                adjoint,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                modes);

            value.set(0.0f);
            value.m_volume = values->m_albedo;
            value.m_volume *= pdf;
            value.m_beauty = value.m_volume;

            return pdf;
        }

        float evaluate_pdf(
            const void*                     data,
            const bool                      adjoint,
            const foundation::Vector3f&     geometric_normal,
            const foundation::Basis3f&      shading_basis,
            const foundation::Vector3f&     outgoing,
            const foundation::Vector3f&     incoming,
            const int                       modes) const override
        {
            assert(m_phase_function);

            return m_phase_function->evaluate(-outgoing, incoming);
        }

      private:
        typedef PhaseFunctionBSDFInputValues InputValues;
    };
} // namespace renderer
