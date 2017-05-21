
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include <cstddef>

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
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("ior", InputFormatFloat);
            m_inputs.declare("volume_density", InputFormatFloat, "0.0");
            m_inputs.declare("volume_scale", InputFormatFloat, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();
            values->m_precomputed.m_eta =
                shading_point.is_entering()
                    ? shading_point.get_ray().get_current_ior() / values->m_ior
                    : values->m_ior / shading_point.get_ray().get_previous_ior();
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const int               modes,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_specular(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f& shading_normal = sample.m_shading_basis.get_normal();
            const float cos_theta_i = dot(sample.m_outgoing.get_value(), shading_normal);
            const float sin_theta_i2 = 1.0f - square(cos_theta_i);
            const float sin_theta_t2 = sin_theta_i2 * square(values->m_precomputed.m_eta);
            const float cos_theta_t2 = 1.0f - sin_theta_t2;

            Vector3f incoming;
            bool refract_differentials = true;

            if (cos_theta_t2 < 0.0f)
            {
                // Total internal reflection: compute the reflected direction and radiance.
                incoming = reflect(sample.m_outgoing.get_value(), shading_normal);
                sample.m_value = values->m_transmittance;
                sample.m_value *= values->m_transmittance_multiplier;
                refract_differentials = false;
            }
            else
            {
                // Compute the Fresnel reflection factor.
                const float cos_theta_t = sqrt(cos_theta_t2);
                float fresnel_reflection;
                fresnel_reflectance_dielectric(
                    fresnel_reflection,
                    1.0f / values->m_precomputed.m_eta,
                    abs(cos_theta_i),
                    cos_theta_t);
                fresnel_reflection *= values->m_fresnel_multiplier;

                sampling_context.split_in_place(1, 1);
                const float s = sampling_context.next2<float>();

                if (s < fresnel_reflection)
                {
                    // Fresnel reflection: compute the reflected direction and radiance.
                    incoming = reflect(sample.m_outgoing.get_value(), shading_normal);
                    sample.m_value = values->m_reflectance;
                    sample.m_value *= values->m_reflectance_multiplier;
                    refract_differentials = false;
                }
                else
                {
                    // Compute the refracted direction.
                    const float eta = values->m_precomputed.m_eta;
                    incoming =
                        cos_theta_i > 0.0f
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * sample.m_outgoing.get_value()
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * sample.m_outgoing.get_value();

                    // Compute the refracted radiance.
                    sample.m_value = values->m_transmittance;
                    sample.m_value *=
                        adjoint
                            ? values->m_transmittance_multiplier
                            : square(eta) * values->m_transmittance_multiplier;
                }
            }

            // todo: we could get rid of this by not wrapping this BTDF in BSDFWrapper<>.
            const float cos_in = abs(dot(incoming, shading_normal));
            sample.m_value /= cos_in;

            // The probability density of the sampled direction is the Dirac delta.
            sample.m_probability = DiracDelta;

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Specular;

            // Set the incoming direction.
            incoming = improve_normalization(incoming);
            sample.m_incoming = Dual3f(incoming);

            // Compute the ray differentials.
            if (refract_differentials)
                sample.compute_transmitted_differentials(values->m_precomputed.m_eta);
            else sample.compute_reflected_differentials();
        }

        virtual float evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            return 0.0f;
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            return 0.0f;
        }

        virtual float sample_ior(
            SamplingContext&        sampling_context,
            const void*             data) const APPLESEED_OVERRIDE
        {
            return static_cast<const InputValues*>(data)->m_ior;
        }

        void compute_absorption(
            const void*             data,
            const float             distance,
            Spectrum&               absorption) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            const float d = values->m_volume_density * values->m_volume_scale * distance;

            absorption.resize(values->m_transmittance.size());

            for (size_t i = 0, e = absorption.size(); i < e; ++i)
            {
                //
                // Reference:
                //
                //   Beer-Lambert law:
                //   https://en.wikipedia.org/wiki/Beer%E2%80%93Lambert_law
                //

                const float a = 1.0f - (values->m_transmittance[i] * values->m_transmittance_multiplier);
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
            .insert("name", "volume_density")
            .insert("label", "Volume Density")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_scale")
            .insert("label", "Volume Scale")
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
