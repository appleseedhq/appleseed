
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
#include "diffusebtdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Diffuse BTDF.
    //

    const char* Model = "diffuse_btdf";

    class DiffuseBTDFImpl
      : public BSDF
    {
      public:
        DiffuseBTDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Transmissive, ScatteringMode::Diffuse, params)
        {
            m_inputs.declare("transmittance", InputFormat::SpectralReflectance);
            m_inputs.declare("transmittance_multiplier", InputFormat::Float, "1.0");
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
            values->m_precomputed.m_backfacing = !shading_point.is_entering();
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
            if (!ScatteringMode::has_diffuse(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the incoming direction.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            Vector3f wi = sample_hemisphere_cosine(s);
            const float cos_in = wi.y;

            // Compute the probability density of the sampled direction.
            const float probability = cos_in * RcpPi<float>();
            assert(probability > 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(ScatteringMode::Diffuse, probability);

                // Flip the incoming direction to the other side of the surface.
                wi.y = -wi.y;

                sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));

                // Compute the BRDF value.
                sample.m_value.m_diffuse = values->m_transmittance;
                sample.m_value.m_diffuse *= values->m_transmittance_multiplier;
                sample.m_aov_components.m_albedo = sample.m_value.m_diffuse;
                sample.m_value.m_diffuse *= RcpPi<float>();
                sample.m_value.m_beauty = sample.m_value.m_diffuse;

                sample.m_min_roughness = 1.0f;
                sample.compute_diffuse_differentials(outgoing);
            }
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
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0f;

            const Vector3f& n = local_geometry.m_shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);

            if (cos_in * cos_on < 0.0f)
            {
                const InputValues* values = static_cast<const InputValues*>(data);

                // Compute the BRDF value.
                value.m_diffuse = values->m_transmittance;
                value.m_diffuse *= values->m_transmittance_multiplier * RcpPi<float>();
                value.m_beauty = value.m_diffuse;

                // Return the probability density of the sampled direction.
                return std::abs(cos_in) * RcpPi<float>();
            }
            else
            {
                // No transmission in the same hemisphere as outgoing.
                return 0.0f;
            }
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            if (!ScatteringMode::has_diffuse(modes))
                return 0.0f;

            const Vector3f& n = local_geometry.m_shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);

            if (cos_in * cos_on < 0.0f)
                return std::abs(cos_in) * RcpPi<float>();
            else
            {
                // No transmission in the same hemisphere as outgoing.
                return 0.0f;
            }
        }

      private:
        typedef DiffuseBTDFInputValues InputValues;
    };

    typedef BSDFWrapper<DiffuseBTDFImpl> DiffuseBTDF;
}


//
// DiffuseBTDFFactory class implementation.
//

void DiffuseBTDFFactory::release()
{
    delete this;
}

const char* DiffuseBTDFFactory::get_model() const
{
    return Model;
}

Dictionary DiffuseBTDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Diffuse BTDF");
}

DictionaryArray DiffuseBTDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

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

    return metadata;
}

auto_release_ptr<BSDF> DiffuseBTDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new DiffuseBTDF(name, params));
}

}   // namespace renderer
