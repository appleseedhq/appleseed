
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/btdfwrapper.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"

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
          : BSDF(params)
          , m_name(name)
        {
            m_inputs.declare("reflectance", InputFormatSpectrum);
            m_inputs.declare("ior", InputFormatScalar);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual void sample(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     s,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d& shading_normal = shading_basis.get_normal();

            const bool entering = dot(shading_normal, geometric_normal) > 0.0;

            const double ior_outside = 1.0;             // todo: remove

            // Figure out the incoming and transmitted IOR.
            double ior_i, ior_t;
            if (entering)
            {
                ior_i = ior_outside;
                ior_t = values->m_ior;
            }
            else
            {
                ior_i = values->m_ior;
                ior_t = ior_outside;
            }

            const double eta = ior_i / ior_t;

            const double cos_theta_i = dot(outgoing, shading_normal);
            const double sin_theta_i2 = 1.0 - cos_theta_i * cos_theta_i;
            const double cos_theta_t2 = 1.0 - eta * eta * sin_theta_i2;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection.
                assert(!entering);
                incoming = reflect(outgoing, shading_normal);
                value = values->m_reflectance;
            }
            else
            {
                const double cos_theta_t = sqrt(cos_theta_t2);

                // Compute the Fresnel reflection factor.
                const Spectrum fresnel_reflection =
                    fresnel_reflection_no_polarization(
                        Spectrum(static_cast<Spectrum::ValueType>(ior_i)),
                        Spectrum(static_cast<Spectrum::ValueType>(ior_t)),
                        abs(cos_theta_i),
                        cos_theta_t);

                const float reflection_prob = min(max_value(fresnel_reflection), 1.0f);

                if (s[0] < static_cast<double>(reflection_prob))
                {
                    // Compute the reflected direction.
                    incoming = reflect(outgoing, shading_normal);

                    // Force the incoming direction to be above the geometric surface.
                    incoming = force_above_surface(incoming, geometric_normal);

                    // Compute the reflected radiance.
                    value = values->m_reflectance;
                    value *= fresnel_reflection;
                    value /= reflection_prob;
                }
                else
                {
                    // Compute the refracted direction.
                    incoming =
                        cos_theta_i > 0.0
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * outgoing
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * outgoing;

                    // Compute the refracted radiance.
                    value = values->m_reflectance;
                    value *= Spectrum(1.0f) - fresnel_reflection;
                    value /= 1.0f - reflection_prob;
                }
            }

            // Divide the value of the BTDF by the PDF.
            const double cos_in = abs(dot(incoming, shading_normal));
            value *= static_cast<float>(1.0 / cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            probability = DiracDelta;

            // Set the scattering mode.
            mode = Specular;
        }

        virtual void evaluate(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            value.set(0.0f);
        }

        virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            return 0.0;
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_reflectance;          // specular transmittance
            Alpha       m_reflectance_alpha;    // alpha channel of specular transmittance
            double      m_ior;                  // index of refraction
        };

        const string    m_name;
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
    return "Specular Transmission";
}

DictionaryArray SpecularBTDFFactory::get_widget_definitions() const
{
    Dictionary entity_types;
    entity_types.insert("color", "Colors");
    entity_types.insert("texture_instance", "Textures");

    DictionaryArray definitions;

    {
        Dictionary widget;
        widget.insert("name", "reflectance");
        widget.insert("label", "Reflectance");
        widget.insert("widget", "entity_picker");
        widget.insert("entity_types", entity_types);
        widget.insert("use", "required");
        widget.insert("default", "");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "ior");
        widget.insert("label", "Index of Refraction");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "1.5");
        definitions.push_back(widget);
    }

    return definitions;
}

auto_release_ptr<BSDF> SpecularBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<BSDF>(
            new SpecularBTDF(name, params));
}

}   // namespace renderer
