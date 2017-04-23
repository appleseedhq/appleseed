
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "blinnbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Blinn BRDF.
    //

    class FresnelFun
    {
      public:
        explicit FresnelFun(const float eta)
          : m_eta(eta)
        {
        }

        void operator()(
            const Vector3f& o,
            const Vector3f& h,
            const Vector3f& n,
            Spectrum&                   value) const
        {
            float f;
            fresnel_reflectance_dielectric(
                f,
                m_eta,
                clamp(dot(o, h), -1.0f, 1.0f));

            value.set(f);
        }

      private:
        const float     m_eta;
    };

    const char* Model = "blinn_brdf";

    class BlinnBRDFImpl
      : public BSDF
    {
      public:
        BlinnBRDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();
            values->m_precomputed.m_outside_ior = shading_point.get_ray().get_current_ior();
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const Vector3f& n = sample.m_shading_basis.get_normal();
            const Vector3f& outgoing = sample.m_outgoing.get_value();
            const float cos_on = min(dot(outgoing, n), 1.0f);
            if (cos_on < 0.0f)
                return;

            const InputValues* values = static_cast<const InputValues*>(data);
            const FresnelFun f(values->m_precomputed.m_outside_ior / values->m_ior);

            MicrofacetBRDFHelper::sample(
                sampling_context,
                m_mdf,
                values->m_exponent,
                values->m_exponent,
                0.0f,
                f,
                cos_on,
                sample);
        }

        virtual float evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const FresnelFun f(values->m_precomputed.m_outside_ior / values->m_ior);

            return MicrofacetBRDFHelper::evaluate(
                m_mdf,
                values->m_exponent,
                values->m_exponent,
                0.0f,
                shading_basis,
                outgoing,
                incoming,
                f,
                cos_in,
                cos_on,
                value);
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            return MicrofacetBRDFHelper::pdf(
                m_mdf,
                values->m_exponent,
                values->m_exponent,
                0.0f,
                shading_basis,
                outgoing,
                incoming);
        }

      private:
        typedef BlinnBRDFInputValues InputValues;

        BlinnMDF m_mdf;
    };

    typedef BSDFWrapper<BlinnBRDFImpl> BlinnBRDF;
}


//
// BlinnBRDFFactory class implementation.
//

auto_release_ptr<BSDF> BlinnBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new BlinnBRDF(name, params));
}

}   // namespace renderer
