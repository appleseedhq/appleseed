
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "specularbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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
    // Specular BRDF.
    //

    const char* Model = "specular_brdf";

    class SpecularBRDFImpl
      : public BSDF
    {
      public:
        SpecularBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, ScatteringMode::Specular, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3d& shading_normal = sample.get_shading_normal();
            const double cos_on = dot(sample.m_outgoing.get_value(), shading_normal);
            if (cos_on < 0.0)
                return;

            // Compute the incoming direction.
            const Vector3d incoming(
                force_above_surface(
                    reflect(sample.m_outgoing.get_value(), shading_normal),
                    sample.get_geometric_normal()));

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, shading_normal);
            if (cos_in < 0.0)
                return;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            sample.m_value = values->m_reflectance;
            sample.m_value *= static_cast<float>(values->m_reflectance_multiplier / cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            sample.m_probability = DiracDelta;

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Specular;

            sample.m_incoming = Dual3d(incoming);
            sample.compute_reflected_differentials();
        }

        APPLESEED_FORCE_INLINE virtual double evaluate(
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
            return 0.0;
        }

        APPLESEED_FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            return 0.0;
        }

      private:
        typedef SpecularBRDFInputValues InputValues;
    };

    typedef BSDFWrapper<SpecularBRDFImpl> SpecularBRDF;
}


//
// SpecularBRDFFactory class implementation.
//

const char* SpecularBRDFFactory::get_model() const
{
    return Model;
}

Dictionary SpecularBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Specular BRDF");
}

DictionaryArray SpecularBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSDF> SpecularBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SpecularBRDF(name, params));
}

auto_release_ptr<BSDF> SpecularBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new SpecularBRDF(name, params));
}

}   // namespace renderer
