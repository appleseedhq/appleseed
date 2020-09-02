
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Specular BTDF.
    //

    const char* Model = "specular_btdf";

    class SpecularBTDF
      : public BSDF
    {
      public:
        SpecularBTDF(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Transmissive, ScatteringMode::Specular, params)
        {
            m_inputs.declare("reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("transmittance", InputFormat::SpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("ior", InputFormat::Float);
            m_inputs.declare("volume_density", InputFormat::Float, "0.0");
            m_inputs.declare("volume_scale", InputFormat::Float, "1.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();
            values->m_precomputed.m_eta =
                shading_point.is_entering()
                    ? shading_point.get_ray().get_current_ior() / values->m_ior
                    : values->m_ior / shading_point.get_ray().get_previous_ior();
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            assert(is_normalized(local_geometry.m_geometric_normal));
            assert(is_normalized(outgoing.get_value()));

            if (!ScatteringMode::has_specular(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f& shading_normal = local_geometry.m_shading_basis.get_normal();
            const float cos_theta_i = dot(outgoing.get_value(), shading_normal);
            const float sin_theta_i2 = 1.0f - square(cos_theta_i);
            const float sin_theta_t2 = sin_theta_i2 * square(values->m_precomputed.m_eta);
            const float cos_theta_t2 = 1.0f - sin_theta_t2;

            Vector3f incoming;
            bool refract_differentials = true;

            if (cos_theta_t2 < 0.0f)
            {
                // Total internal reflection: compute the reflected direction and radiance.
                incoming = reflect(outgoing.get_value(), shading_normal);
                sample.m_value.m_glossy = values->m_transmittance;
                sample.m_value.m_glossy *= values->m_transmittance_multiplier;
                refract_differentials = false;
            }
            else
            {
                // Compute the Fresnel reflection factor.
                const float cos_theta_t = std::sqrt(cos_theta_t2);
                float fresnel_reflection;
                fresnel_reflectance_dielectric(
                    fresnel_reflection,
                    1.0f / values->m_precomputed.m_eta,
                    std::abs(cos_theta_i),
                    cos_theta_t);
                fresnel_reflection *= values->m_fresnel_multiplier;

                sampling_context.split_in_place(1, 1);
                const float s = sampling_context.next2<float>();

                if (s < fresnel_reflection)
                {
                    // Fresnel reflection: compute the reflected direction and radiance.
                    incoming = reflect(outgoing.get_value(), shading_normal);
                    sample.m_value.m_glossy = values->m_reflectance;
                    sample.m_value.m_glossy *= values->m_reflectance_multiplier;
                    refract_differentials = false;
                }
                else
                {
                    // Compute the refracted direction.
                    const float eta = values->m_precomputed.m_eta;
                    incoming =
                        cos_theta_i > 0.0f
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * outgoing.get_value()
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * outgoing.get_value();

                    // Compute the refracted radiance.
                    sample.m_value.m_glossy = values->m_transmittance;
                    sample.m_value.m_glossy *=
                        adjoint
                            ? values->m_transmittance_multiplier
                            : square(eta) * values->m_transmittance_multiplier;
                }
            }

            if (!cosine_mult)
            {
                const float cos_in = std::abs(dot(incoming, shading_normal));
                sample.m_value.m_glossy /= cos_in;
            }

            sample.m_value.m_beauty = sample.m_value.m_glossy;

            sample.set_to_scattering(ScatteringMode::Specular, DiracDelta);

            // Set the incoming direction.
            incoming = improve_normalization(incoming);
            sample.m_incoming = Dual3f(incoming);
            assert(is_normalized(sample.m_incoming.get_value(), 1.0e-5f));

            // Compute the ray differentials.
            if (refract_differentials)
            {
                sample.compute_specular_transmitted_differentials(
                    local_geometry,
                    values->m_precomputed.m_eta,
                    local_geometry.m_shading_point->is_entering(),
                    outgoing);
            }
            else sample.compute_specular_reflected_differentials(local_geometry, outgoing);
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            return 0.0f;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            return 0.0f;
        }

        float sample_ior(
            SamplingContext&            sampling_context,
            const void*                 data) const override
        {
            return static_cast<const InputValues*>(data)->m_ior;
        }

        void compute_absorption(
            const void*                 data,
            const float                 distance,
            Spectrum&                   absorption) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            const float d = values->m_volume_density * values->m_volume_scale * distance;

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                //
                // Reference:
                //
                //   Beer-Lambert law:
                //   https://en.wikipedia.org/wiki/Beer%E2%80%93Lambert_law
                //

                const float a = 1.0f - (values->m_transmittance[i] * values->m_transmittance_multiplier);
                const float optical_depth = a * d;
                absorption[i] = std::exp(-optical_depth);
            }
        }

      private:
        typedef SpecularBTDFInputValues InputValues;
    };
}


//
// SpecularBTDFFactory class implementation.
//

void SpecularBTDFFactory::release()
{
    delete this;
}

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
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
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
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "transmittance_multiplier")
            .insert("label", "Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_multiplier")
            .insert("label", "Fresnel Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_density")
            .insert("label", "Volume Density")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_scale")
            .insert("label", "Volume Scale")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
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

}   // namespace renderer
