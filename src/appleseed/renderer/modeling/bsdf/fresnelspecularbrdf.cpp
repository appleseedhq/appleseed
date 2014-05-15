
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
#include "fresnelspecularbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Fresnel Specular BRDF.
    //

    const char* Model = "fresnel_specular_brdf";

    class FresnelSpecularBRDFImpl
      : public BSDF
    {
      public:
        FresnelSpecularBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, Specular, params)
        {
            m_inputs.declare("eta", InputFormatScalar, "0.0");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
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
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_on < 0.0)
                return Absorption;

            // Compute the incoming direction.
            incoming = reflect(outgoing, shading_normal);
            incoming = force_above_surface(incoming, geometric_normal);

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, shading_normal);
            if (cos_in < 0.0)
                return Absorption;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            value.set( 1.0 / cos_in);

            // Multiply by fresnel term is eta != 0.0
            if (values->m_eta != 0.0)
                value *= static_cast<float>(fresnel_dielectric_reflection(cos_in, values->m_eta));

            // The probability density of the sampled direction is the Dirac delta.
            probability = DiracDelta;

            // Return the scattering mode.
            return Specular;
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
        typedef FresnelSpecularBRDFInputValues InputValues;
    };

    typedef BSDFWrapper<FresnelSpecularBRDFImpl> FresnelSpecularBRDF;
}


//
// FresnelSpecularBRDFFactory class implementation.
//

const char* FresnelSpecularBRDFFactory::get_model() const
{
    return Model;
}

const char* FresnelSpecularBRDFFactory::get_human_readable_model() const
{
    return "Fresnel Specular BRDF";
}

DictionaryArray FresnelSpecularBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "eta")
            .insert("label", "Eta")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));
    
    return metadata;
}

auto_release_ptr<BSDF> FresnelSpecularBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new FresnelSpecularBRDF(name, params));
}

}   // namespace renderer
