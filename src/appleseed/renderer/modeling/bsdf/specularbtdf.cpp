
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
#include "specularbtdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

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
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Transmissive, ScatteringMode::Specular, params)
        {
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("ior", InputFormatScalar);
            m_inputs.declare("density", InputFormatScalar, "0.0");
            m_inputs.declare("scale", InputFormatScalar, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size(
            const Assembly&         assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(InputValues), 16);
        }

        virtual void prepare_inputs(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);

            if (shading_point.is_entering())
                values->m_eta = shading_point.get_ray().get_current_ior() / values->m_ior;
            else
                values->m_eta = values->m_ior / shading_point.get_ray().get_previous_ior();
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d& shading_normal = sample.get_shading_normal();
            const double cos_theta_i = dot(sample.m_outgoing.get_value(), shading_normal);
            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double sin_theta_t2 = sin_theta_i2 * square(values->m_eta);
            const double cos_theta_t2 = 1.0 - sin_theta_t2;

            Vector3d incoming;
            bool refract_differentials = true;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection: compute the reflected direction and radiance.
                incoming = reflect(sample.m_outgoing.get_value(), shading_normal);
                sample.m_value = values->m_transmittance;
                sample.m_value *= static_cast<float>(values->m_transmittance_multiplier);
                refract_differentials = false;
            }
            else
            {
                // Compute the Fresnel reflection factor.
                const double cos_theta_t = sqrt(cos_theta_t2);
                double fresnel_reflection;
                fresnel_reflectance_dielectric(
                    fresnel_reflection,
                    1.0 / values->m_eta,
                    abs(cos_theta_i),
                    cos_theta_t);
                fresnel_reflection *= values->m_fresnel_multiplier;

                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();

                if (s < fresnel_reflection)
                {
                    // Fresnel reflection: compute the reflected direction and radiance.
                    incoming = reflect(sample.m_outgoing.get_value(), shading_normal);
                    sample.m_value = values->m_reflectance;
                    sample.m_value *= static_cast<float>(values->m_reflectance_multiplier);
                    refract_differentials = false;
                }
                else
                {
                    // Compute the refracted direction.
                    incoming =
                        cos_theta_i > 0.0
                            ? (values->m_eta * cos_theta_i - cos_theta_t) * shading_normal - values->m_eta * sample.m_outgoing.get_value()
                            : (values->m_eta * cos_theta_i + cos_theta_t) * shading_normal - values->m_eta * sample.m_outgoing.get_value();

                    // Compute the refracted radiance.
                    sample.m_value = values->m_transmittance;
                    sample.m_value *=
                        adjoint
                            ? static_cast<float>(values->m_transmittance_multiplier)
                            : static_cast<float>(square(values->m_eta) * values->m_transmittance_multiplier);
                }
            }

            // todo: we could get rid of this by not wrapping this BTDF in BSDFWrapper<>.
            const double cos_in = abs(dot(incoming, shading_normal));
            sample.m_value /= static_cast<float>(cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            sample.m_probability = DiracDelta;

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Specular;

            // Set the incoming direction.
            incoming = improve_normalization(incoming);
            sample.m_incoming = Dual3d(incoming);

            // Compute the ray differentials.
            if (refract_differentials)
                sample.compute_transmitted_differentials(values->m_eta);
            else sample.compute_reflected_differentials();
        }

        APPLESEED_FORCE_INLINE virtual double evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            return 0.0;
        }

        APPLESEED_FORCE_INLINE virtual double evaluate_pdf(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            return 0.0;
        }

        double sample_ior(
            SamplingContext&        sampling_context,
            const void*             data) const APPLESEED_OVERRIDE
        {
            return static_cast<const InputValues*>(data)->m_ior;
        }

        void compute_absorption(
            const void*             data,
            const double            distance,
            Spectrum&               absorption) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            const float d = static_cast<float>(values->m_density * values->m_scale * distance);

            absorption.resize(values->m_transmittance.size());

            for (size_t i = 0, e = absorption.size(); i < e; ++i)
            {
                //
                // Reference:
                //
                //   Beer-Lambert law:
                //   https://en.wikipedia.org/wiki/Beer%E2%80%93Lambert_law
                //

                const float a = 1.0f - static_cast<float>(values->m_transmittance[i] * values->m_transmittance_multiplier);
                const float optical_depth = a * d;
                absorption[i] = exp(-optical_depth);
            }
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
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "1.0")
            .insert("max_value", "2.5")
            .insert("use", "required")
            .insert("default", "1.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "density")
            .insert("label", "Density")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "scale")
            .insert("label", "Scale")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSDF> SpecularBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SpecularBTDF(name, params));
}

auto_release_ptr<BSDF> SpecularBTDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new SpecularBTDF(name, params));
}

}   // namespace renderer
