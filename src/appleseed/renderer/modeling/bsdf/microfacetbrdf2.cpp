
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "microfacetbrdf2.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet2.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <string>
#include <memory>

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
    
    //
    // Microfacet2 BRDF.
    //

    const char* Model = "microfacet2_brdf";

    class Microfacet2BRDFImpl
      : public BSDF
    {
      public:
        Microfacet2BRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, Glossy, params)
        {
            m_inputs.declare("ax", InputFormatScalar);
            m_inputs.declare("ay", InputFormatScalar, "0.0");
            m_inputs.declare("exponent", InputFormatScalar, "0.0");
            m_inputs.declare("ior", InputFormatScalar, "1.5");
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
                    "blinn",
                    make_vector("blinn"),
                    context);

            if (mdf == "blinn")
                m_mdf.reset(new BlinnMDF2<double>());
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
            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_on < 0.0)
                return Absorption;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the incoming direction by sampling the MDF.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            Vector3d h;
            
            double mdf_value, mdf_pdf = 0.0;
            //const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;
            //sample_mdf(glossiness, s, h, mdf_value, mdf_pdf);
            
            m_mdf->sample(s, values->m_ax, values->m_ay, values->m_exponent);
            
            if (mdf_pdf == 0.0)
                return Absorption;
            h = shading_basis.transform_to_parent(h);
            incoming = reflect(outgoing, h);
            const double cos_hn = dot(h, n);
            const double cos_oh = dot(outgoing, h);

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, n);
            if (cos_in < 0.0)
                return Absorption;

            // Compute the BRDF value.
            //const double g = evaluate_attenuation(cos_on, cos_in, cos_hn, cos_oh);
            //value = fresnel_dielectric_schlick(values->m_reflectance, cos_on, values->m_fr_multiplier);
            //value *= static_cast<float>(mdf_value * g / (4.0 * cos_on * cos_in) * values->m_reflectance_multiplier);

            // Compute the PDF value.
            probability = mdf_pdf / (4.0 * cos_oh);

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
            return 0.0;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            return 0.0;
        }

      private:
        typedef Microfacet2BRDFInputValues InputValues;
        
        std::auto_ptr<MDF<double> >     m_mdf;
        std::auto_ptr<Fresnel<double> > m_fresnel;
    };

    typedef BSDFWrapper<Microfacet2BRDFImpl> Microfacet2BRDF;
}


//
// Microfacet2BRDFFactory class implementation.
//

const char* Microfacet2BRDFFactory::get_model() const
{
    return Model;
}

const char* Microfacet2BRDFFactory::get_human_readable_model() const
{
    return "Microfacet2 BRDF";
}

DictionaryArray Microfacet2BRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    
    metadata.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Blinn", "blinn")
                    .insert("Beckmann", "beckmann")
                    .insert("Dishwasher", "dishwasher")
                    .insert("GGX", "ggx")
                    .insert("GTR", "gtr")
                    .insert("SGD", "sgd")
                    .insert("Ward", "ward"))
            .insert("use", "required")
            .insert("default", "blinn"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ax")
            .insert("label", "Alpha X")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ay")
            .insert("label", "Alpha Y")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "exponent")
            .insert("label", "Exponent")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));
    
    return metadata;
}

auto_release_ptr<BSDF> Microfacet2BRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new Microfacet2BRDF(name, params));
}

}   // namespace renderer
