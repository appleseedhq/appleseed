
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/btdfwrapper.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

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
          : BSDF(name, Transmissive, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectrum);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("transmittance", InputFormatSpectrum);
            m_inputs.declare("transmittance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("from_ior", InputFormatScalar);
            m_inputs.declare("to_ior", InputFormatScalar);
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
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
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d& shading_normal = shading_basis.get_normal();
            const double eta = values->m_from_ior / values->m_to_ior;
            const double cos_theta_i = dot(outgoing, shading_normal);
            const double sin_theta_i2 = 1.0 - cos_theta_i * cos_theta_i;
            const double cos_theta_t2 = 1.0 - eta * eta * sin_theta_i2;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection.
                incoming = reflect(outgoing, shading_normal);
                value = values->m_transmittance;
                value *= static_cast<float>(values->m_transmittance_multiplier);
            }
            else
            {
                // Compute the Fresnel reflection factor.
                const double cos_theta_t = sqrt(cos_theta_t2);
                const Spectrum fresnel_reflection =
                    fresnel_reflection_no_polarization(
                        Spectrum(static_cast<Spectrum::ValueType>(values->m_from_ior)),
                        Spectrum(static_cast<Spectrum::ValueType>(values->m_to_ior)),
                        abs(cos_theta_i),
                        cos_theta_t);

                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();
                const float reflection_prob = min(max_value(fresnel_reflection), 1.0f);

                if (s < static_cast<double>(reflection_prob))
                {
                    // Compute the reflected direction.
                    incoming = reflect(outgoing, shading_normal);

                    // Force the reflected direction to be above the geometric surface.
                    incoming = force_above_surface(incoming, geometric_normal);

                    // Compute the reflected radiance.
                    value = values->m_reflectance;
                    value *= fresnel_reflection;
                    value *= static_cast<float>(values->m_reflectance_multiplier) / reflection_prob;
                }
                else
                {
                    // Compute the refracted direction.
                    incoming =
                        cos_theta_i > 0.0
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * outgoing
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * outgoing;

                    // Compute the refracted radiance.
                    value = values->m_transmittance;
                    value *= Spectrum(1.0f) - fresnel_reflection;
                    value *= static_cast<float>(values->m_transmittance_multiplier) / (1.0f - reflection_prob);
                }
            }

            const double cos_in = abs(dot(incoming, shading_normal));
            value /= static_cast<float>(cos_in);

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
            Spectrum&           value) const
        {
            return 0.0;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            return 0.0;
        }

      private:
        struct InputValues
        {
            Spectrum    m_reflectance;                  // specular reflectance
            Alpha       m_reflectance_alpha;            // unused
            double      m_reflectance_multiplier;       // specular reflectance multiplier
            Spectrum    m_transmittance;                // specular transmittance
            Alpha       m_transmittance_alpha;          // unused
            double      m_transmittance_multiplier;     // specular transmittance multiplier
            double      m_from_ior;                     // from this index of refraction
            double      m_to_ior;                       // to this index of refraction
        };
    };

    typedef BTDFWrapper<SpecularBTDFImpl> SpecularBTDF;
}


//
// SpecularBTDFFactory class implementation.
//

const char* SpecularBTDFFactory::get_model() const
{
    return Model;
}

const char* SpecularBTDFFactory::get_human_readable_model() const
{
    return "Specular BTDF";
}

DictionaryArray SpecularBTDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "transmittance")
            .insert("label", "Transmittance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "transmittance_multiplier")
            .insert("label", "Transmittance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "from_ior")
            .insert("label", "From Index of Refraction")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "to_ior")
            .insert("label", "To Index of Refraction")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "1.5"));

    return definitions;
}

auto_release_ptr<BSDF> SpecularBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SpecularBTDF(name, params));
}

}   // namespace renderer
