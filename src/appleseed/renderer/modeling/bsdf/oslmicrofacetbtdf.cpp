
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/microfacet2.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    Vector3d half_refraction_vector(
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

    double refraction_jacobian(
        const Vector3d& incoming,
        const double    to_ior,
        const Vector3d& h,
        const double    hnorm)
    {
        // [1] equation 17.
        return abs(square(to_ior) * dot(incoming, h) / square(hnorm));
    }


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
          : BSDF(name, Transmissive, Glossy, params)
        {
            m_inputs.declare("ax", InputFormatScalar, "0.05");
            m_inputs.declare("ay", InputFormatScalar, "0.05");
            m_inputs.declare("from_ior", InputFormatScalar, "1.0");
            m_inputs.declare("to_ior", InputFormatScalar, "1.5");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly,
            AbortSwitch*        abort_switch) OVERRIDE
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
                m_mdf.reset(new BeckmannMDF2<double>());
            else if (mdf == "ggx")
                m_mdf.reset(new GGXMDF2<double>());
            else
                return false;

            return true;
        }

        FORCE_INLINE virtual Mode sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the incoming direction by sampling the MDF.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d m = m_mdf->sample(s, values->m_ax, values->m_ay);
            const Vector3d ht = shading_basis.transform_to_parent(m);

            if (!refract(
                    outgoing,
                    ht,
                    values->m_from_ior / values->m_to_ior,
                    incoming))
            {
                // Ignore TIR.
                return Absorption;
            }

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = shading_basis.get_normal();
            if (dot(incoming, n) * dot(outgoing, n) >= 0.0)
                return Absorption;

            const double G =
                m_mdf->G(
                    shading_basis.transform_to_local(incoming),
                    shading_basis.transform_to_local(outgoing),
                    m,
                    values->m_ax,
                    values->m_ay);

            if (G == 0.0)
                return Absorption;

            const double D = m_mdf->D(m, values->m_ax, values->m_ay);

            const double cos_oh = dot(outgoing, ht);
            const double cos_ih = dot(incoming, ht);
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
 
            // [1] equation 21.
            double v = abs((cos_ih * cos_oh) / (cos_in * cos_on));
            v *= square(values->m_to_ior) * D * G;
            const double denom = values->m_to_ior * cos_ih + values->m_from_ior * cos_oh;
            v /= square(denom);
            value.set(static_cast<float>(v));

            const double ht_norm = norm(values->m_from_ior * outgoing + values->m_to_ior * incoming);
            const double dwh_dwo = refraction_jacobian(
                incoming,
                values->m_to_ior,
                ht,
                ht_norm);

            probability = m_mdf->pdf(m, values->m_ax, values->m_ay) * dwh_dwo;
            return Glossy;
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
            if (!(modes & Glossy))
                return 0.0;

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = shading_basis.get_normal();
            if (dot(incoming, n) * dot(outgoing, n) >= 0.0)
                return Absorption;

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

            const double G =
                m_mdf->G(
                    shading_basis.transform_to_local(incoming),
                    shading_basis.transform_to_local(outgoing),
                    m,
                    values->m_ax,
                    values->m_ay);

            if (G == 0.0)
                return 0.0;

            const double D = m_mdf->D(m, values->m_ax, values->m_ay);

            const double cos_oh = dot(outgoing, ht);
            const double cos_ih = dot(incoming, ht);
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
 
            // [1] equation 21.
            double v = abs((cos_ih * cos_oh) / (cos_in * cos_on));
            v *= square(values->m_to_ior) * D * G;
            const double denom = values->m_to_ior * cos_ih + values->m_from_ior * cos_oh;
            v /= square(denom);
            value.set(static_cast<float>(v));

            const double dwh_dwo = refraction_jacobian(
                incoming,
                values->m_to_ior,
                ht,
                ht_norm);

            return m_mdf->pdf(m, values->m_ax, values->m_ay) * dwh_dwo;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            if (!(modes & Glossy))
                return 0.0;

            // If incoming and outgoing are on the same hemisphere
            // this is not a refraction.
            const Vector3d& n = shading_basis.get_normal();
            if (dot(incoming, n) * dot(outgoing, n) >= 0.0)
                return Absorption;

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

            return m_mdf->pdf(m, values->m_ax, values->m_ay) * dwh_dwo;
        }

      private:
        typedef OSLMicrofacetBTDFInputValues InputValues;

        auto_ptr<MDF<double> > m_mdf;
    };

    typedef BSDFWrapper<OSLMicrofacetBTDFImpl> OSLMicrofacetBTDF;
}


//
// OSLMicrofacetBTDFFactory class implementation.
//

const char* OSLMicrofacetBTDFFactory::get_model() const
{
    return Model;
}

const char* OSLMicrofacetBTDFFactory::get_human_readable_model() const
{
    return "OSL Microfacet BTDF";
}

DictionaryArray OSLMicrofacetBTDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Beckmann", "beckmann")
                    .insert("GGX", "ggx"))
            .insert("use", "required")
            .insert("default", "beckmann"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ax")
            .insert("label", "Alpha X")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.2"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ay")
            .insert("label", "Alpha Y")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.2"));

    metadata.push_back(
        Dictionary()
            .insert("name", "from_ior")
            .insert("label", "From IOR")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "to_ior")
            .insert("label", "To IOR")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.5"));

    return metadata;
}

auto_release_ptr<BSDF> OSLMicrofacetBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLMicrofacetBTDF(name, params));
}

}   // namespace renderer
