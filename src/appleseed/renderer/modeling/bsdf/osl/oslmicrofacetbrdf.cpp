
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslmicrofacetbrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacetbrdfhelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <memory>
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
    struct NoFresnel
    {
        void operator()(
            const Vector3d& o,
            const Vector3d& h,
            const Vector3d& n,
            Spectrum&       value) const
        {
            value.set(1.0f);
        }
    };


    //
    // OSL microfacet BRDF.
    //

    const char* Model = "osl_microfacet_brdf";

    class OSLMicrofacetBRDFImpl
      : public BSDF
    {
      public:
        OSLMicrofacetBRDFImpl(
            const char*         name,
            const ParamArray&   params)
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

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);
            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "beckmann",
                    make_vector("beckmann", "blinn", "ggx"),
                    context);

            if (mdf == "beckmann")
                m_mdf.reset(new BeckmannMDF<double>());
            else if (mdf == "blinn")
                m_mdf.reset(new BlinnMDF<double>());
            else if (mdf == "ggx")
                m_mdf.reset(new GGXMDF<double>());
            else
                return false;

            return true;
        }

        FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            MicrofacetBRDFHelper<double>::sample(
                sampling_context,
                *m_mdf,
                values->m_ax,
                values->m_ay,
                values->m_ax,
                values->m_ay,
                NoFresnel(),
                sample);
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            return MicrofacetBRDFHelper<double>::evaluate(
                *m_mdf,
                values->m_ax,
                values->m_ay,
                values->m_ax,
                values->m_ay,
                shading_basis,
                outgoing,
                incoming,
                modes,
                NoFresnel(),
                value);
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            return MicrofacetBRDFHelper<double>::pdf(
                *m_mdf,
                values->m_ax,
                values->m_ay,
                shading_basis,
                outgoing,
                incoming,
                modes);
        }

      private:
        typedef OSLMicrofacetBRDFInputValues InputValues;

        auto_ptr<MDF<double> >  m_mdf;
    };

    typedef BSDFWrapper<OSLMicrofacetBRDFImpl> OSLMicrofacetBRDF;
}


//
// OSLMicrofacetBRDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLMicrofacetBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLMicrofacetBRDF(name, params));
}

}   // namespace renderer
