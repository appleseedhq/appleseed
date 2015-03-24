
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
#include "oslmicrofacetbtdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
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

    //
    // OSLMicrofacet BTDF.
    //

    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //

    const char* Model = "osl_microfacet_btdf";

    class OSLMicrofacetBTDFImpl
      : public BSDF
    {
      public:
        OSLMicrofacetBTDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Transmissive, BSDFSample::Glossy, params)
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
                    make_vector("beckmann", "ggx"),
                    context);

            if (mdf == "beckmann")
                m_mdf.reset(new BeckmannMDF<double>());
            else if (mdf == "ggx")
                m_mdf.reset(new GGXMDF<double>());
            else
                return false;

            return true;
        }

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the incoming direction by sampling the MDF.
            sample.get_sampling_context().split_in_place(3, 1);
            const Vector3d s = sample.get_sampling_context().next_vector2<3>();
            const Vector3d wo = sample.get_shading_basis().transform_to_local(sample.get_outgoing_vector());
            const Vector3d m = m_mdf->sample(wo, s, values->m_ax, values->m_ay);
            const Vector3d ht = sample.get_shading_basis().transform_to_parent(m);
            const double eta = values->m_from_ior / values->m_to_ior;

            Vector3d incoming;
            if (!refract(sample.get_outgoing_vector(), ht, eta, incoming))
                return; // Ignore TIR.

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = sample.get_shading_normal();
            if (dot(incoming, n) * dot(sample.get_outgoing_vector(), n) >= 0.0)
                return;

            const double G =
                m_mdf->G(
                    sample.get_shading_basis().transform_to_local(incoming),
                    wo,
                    m,
                    values->m_ax,
                    values->m_ay);

            if (G == 0.0)
                return;

            const double D = m_mdf->D(m, values->m_ax, values->m_ay);

            sample.value().set(
                static_cast<float>(
                    refraction_term(
                        sample.get_outgoing_vector(),
                        incoming,
                        n,
                        ht,
                        values->m_from_ior,
                        values->m_to_ior,
                        D,
                        G,
                        adjoint)));

            const double ht_norm =
                norm(values->m_from_ior * sample.get_outgoing_vector() + values->m_to_ior * incoming);
            const double dwh_dwo =
                refraction_jacobian(
                    incoming,
                    values->m_to_ior,
                    ht,
                    ht_norm);

            sample.set_probability(m_mdf->pdf(wo, m, values->m_ax, values->m_ay) * dwh_dwo);
            sample.set_mode(BSDFSample::Glossy);
            sample.set_incoming(incoming);
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
            Spectrum&           value) const
        {
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = shading_basis.get_normal();
            if (dot(incoming, n) * dot(outgoing, n) >= 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            double ht_norm;
            const Vector3d ht = half_refraction_vector(
                outgoing,
                incoming,
                n,
                values->m_from_ior,
                values->m_to_ior,
                ht_norm);

            const Vector3d m = shading_basis.transform_to_local(ht);
            const Vector3d wo = shading_basis.transform_to_local(outgoing);
            const double G =
                m_mdf->G(
                    shading_basis.transform_to_local(incoming),
                    wo,
                    m,
                    values->m_ax,
                    values->m_ay);

            if (G == 0.0)
                return 0.0;

            const double D = m_mdf->D(m, values->m_ax, values->m_ay);

            value.set(
                static_cast<float>(
                    refraction_term(
                        outgoing,
                        incoming,
                        n,
                        ht,
                        values->m_from_ior,
                        values->m_to_ior,
                        D,
                        G,
                        adjoint)));

            const double dwh_dwo = refraction_jacobian(
                incoming,
                values->m_to_ior,
                ht,
                ht_norm);

            return m_mdf->pdf(wo, m, values->m_ax, values->m_ay) * dwh_dwo;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = shading_basis.get_normal();
            if (dot(incoming, n) * dot(outgoing, n) >= 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            double ht_norm;
            const Vector3d ht = half_refraction_vector(
                outgoing,
                incoming,
                n,
                values->m_from_ior,
                values->m_to_ior,
                ht_norm);

            const Vector3d m = shading_basis.transform_to_local(ht);
            const double dwh_dwo = refraction_jacobian(
                incoming,
                values->m_to_ior,
                ht,
                ht_norm);

            return m_mdf->pdf(
                shading_basis.transform_to_local(outgoing),
                m,
                values->m_ax,
                values->m_ay) * dwh_dwo;
        }

      private:
        typedef OSLMicrofacetBTDFInputValues InputValues;

        auto_ptr<MDF<double> > m_mdf;

        static Vector3d half_refraction_vector(
            const Vector3d& outgoing,
            const Vector3d& incoming,
            const Vector3d& normal,
            const double    from_ior,
            const double    to_ior,
            double&         ht_norm)
        {
            // [1] equation 16.
            Vector3d ht = -(from_ior * outgoing + to_ior * incoming);
            ht_norm = norm(ht);
            ht /= ht_norm;

            // Flip the half vector to be on the same side as the normal.
            if (dot(ht, normal) < 0.0)
                ht = -ht;

            return ht;
        }

        static double refraction_jacobian(
            const Vector3d& incoming,
            const double    to_ior,
            const Vector3d& h,
            const double    hnorm)
        {
            // [1] equation 17.
            return abs(square(to_ior) * dot(incoming, h) / square(hnorm));
        }

        static double refraction_term(
            const Vector3d& outgoing,
            const Vector3d& incoming,
            const Vector3d& n,
            const Vector3d& ht,
            const double from_ior,
            const double to_ior,
            const double D,
            const double G,
            bool adjoint)
        {
            const double cos_oh = dot(outgoing, ht);
            const double cos_ih = dot(incoming, ht);
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);

            // [1] equation 21.
            double v = abs((cos_ih * cos_oh) / (cos_in * cos_on));
            v *= square(to_ior) * D * G;
            const double denom = to_ior * cos_ih + from_ior * cos_oh;
            v /= square(denom);

            // TODO: check for a possibly missing eta^2 factor if adjoint != true.
            //if (!adjoint)
            //    v *= square(eta);

            return v;
        }
    };

    typedef BSDFWrapper<OSLMicrofacetBTDFImpl> OSLMicrofacetBTDF;
}


//
// OSLMicrofacetBTDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLMicrofacetBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLMicrofacetBTDF(name, params));
}

}   // namespace renderer
