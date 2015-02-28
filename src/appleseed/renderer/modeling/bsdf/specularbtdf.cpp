
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "specularbtdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <algorithm>
#include <cmath>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Specular BTDF.
    //

    const char* Model = "specular_btdf";

    class SpecularBTDFImpl
      : public BSDF
    {
      public:
        SpecularBTDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Transmissive, BSDFSample::Specular, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("from_ior", InputFormatScalar);
            m_inputs.declare("to_ior", InputFormatScalar);
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d& shading_normal = sample.get_shading_normal();
            const double eta = values->m_from_ior / values->m_to_ior;
            const double cos_theta_i = dot(sample.get_outgoing_vector(), shading_normal);
            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double cos_theta_t2 = 1.0 - square(eta) * sin_theta_i2;

            Vector3d incoming;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection: compute the reflected direction and radiance.
                incoming = reflect(sample.get_outgoing_vector(), shading_normal);
                sample.value() = values->m_transmittance;
                sample.value() *= static_cast<float>(values->m_transmittance_multiplier);
            }
            else
            {
                // Compute the Fresnel reflection factor.
                const double cos_theta_t = sqrt(cos_theta_t2);
                double fresnel_reflection;
                fresnel_dielectric_unpolarized(
                    fresnel_reflection,
                    values->m_from_ior,
                    values->m_to_ior,
                    abs(cos_theta_i),
                    cos_theta_t);
                fresnel_reflection *= values->m_fresnel_multiplier;

                sample.get_sampling_context().split_in_place(1, 1);
                const double s = sample.get_sampling_context().next_double2();

                if (s < fresnel_reflection)
                {
                    // Fresnel reflection: compute the reflected direction and radiance.
                    incoming = reflect(sample.get_outgoing_vector(), shading_normal);
                    sample.value() = values->m_reflectance;
                    sample.value() *= static_cast<float>(values->m_reflectance_multiplier);
                }
                else
                {
                    // Compute the refracted direction.
                    incoming =
                        cos_theta_i > 0.0
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * sample.get_outgoing_vector()
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * sample.get_outgoing_vector();

                    // Compute the refracted radiance.
                    sample.value() = values->m_transmittance;
                    sample.value() *=
                        adjoint
                            ? static_cast<float>(values->m_transmittance_multiplier)
                            : static_cast<float>(eta * eta * values->m_transmittance_multiplier);
                }
            }

            const double cos_in = abs(dot(incoming, shading_normal));
            sample.value() /= static_cast<float>(cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            sample.set_probability(DiracDelta);

            // Set the scattering mode.
            sample.set_mode(BSDFSample::Specular);

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
        typedef SpecularBTDFInputValues InputValues;
    };

    typedef BSDFWrapper<SpecularBTDFImpl> SpecularBTDF;
}


//
// SpecularBTDFFactory class implementation.
//

const char* SpecularBTDFFactory::get_model() const
{
    return Model;
}

Dictionary SpecularBTDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Specular BTDF");
}

DictionaryArray SpecularBTDFFactory::get_input_metadata() const
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

    metadata.push_back(
        Dictionary()
            .insert("name", "transmittance")
            .insert("label", "Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "transmittance_multiplier")
            .insert("label", "Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_multiplier")
            .insert("label", "Fresnel Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "from_ior")
            .insert("label", "From Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "to_ior")
            .insert("label", "To Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.5"));

    return metadata;
}

auto_release_ptr<BSDF> SpecularBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SpecularBTDF(name, params));
}

}   // namespace renderer
