
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "sheenbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacetbrdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Sheen BRDF.
    //
    // Reference:
    //
    //   Physically-Based Shading at Disney
    //   https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
    //

    const char* Model = "sheen_brdf";
    const char* MicrofacetModel = "microfacet_normal_mapping_sheen_brdf";

    class SheenBRDFImpl
      : public BSDF
    {
      public:
        SheenBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormat::Float, "1.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
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
            if (!ScatteringMode::has_glossy(modes))
                return;

            sample.set_to_scattering(ScatteringMode::Glossy, RcpTwoPi<float>());
            sample.m_min_roughness = 1.0f;

            // Compute the incoming direction.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_uniform(s);
            const Vector3f incoming = local_geometry.m_shading_basis.transform_to_parent(wi);
            sample.m_incoming = Dual3f(incoming);

            const Vector3f h = normalize(incoming + outgoing.get_value());
            const float cos_ih = dot(incoming, h);
            const float fh = pow_int<5>(saturate(1.0f - cos_ih));

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            sample.m_value.m_glossy = values->m_reflectance;
            sample.m_value.m_glossy *= fh * values->m_reflectance_multiplier;
            sample.m_value.m_beauty = sample.m_value.m_glossy;

            sample.compute_diffuse_differentials(outgoing);
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
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const Vector3f h = normalize(incoming + outgoing);
            const float cos_ih = dot(incoming, h);
            const float fh = pow_int<5>(saturate(1.0f - cos_ih));

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            value.m_glossy = values->m_reflectance;
            value.m_glossy *= fh * values->m_reflectance_multiplier;
            value.m_beauty = value.m_glossy;

            // Return the probability density of the sampled direction.
            return RcpTwoPi<float>();
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // Return the probability density of the sampled direction.
            return RcpTwoPi<float>();
        }

      private:
        typedef SheenBRDFInputValues InputValues;
    };

    class MicrofacetSheenBRDFImpl
      : public SheenBRDFImpl
    {
      public:
        using SheenBRDFImpl::SheenBRDFImpl;

        const char* get_model() const override
        {
            return MicrofacetModel;
        }
    };

    typedef BSDFWrapper<SheenBRDFImpl> SheenBRDF;
    typedef MicrofacetBRDFWrapper<MicrofacetSheenBRDFImpl> MicrofacetSheenBRDF;
}


//
// SheenBRDFFactory class implementation.
//

void SheenBRDFFactory::release()
{
    delete this;
}

const char* SheenBRDFFactory::get_model() const
{
    return Model;
}

Dictionary SheenBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Sheen BRDF");
}

DictionaryArray SheenBRDFFactory::get_input_metadata() const
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

    return metadata;
}

auto_release_ptr<BSDF> SheenBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SheenBRDF(name, params));
}


//
// MicrofacetSheenBRDFFactory class implementation.
//

const char* MicrofacetSheenBRDFFactory::get_model() const
{
    return MicrofacetModel;
}

Dictionary MicrofacetSheenBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", MicrofacetModel)
            .insert("label", "Microfacet Sheen BRDF");
}

auto_release_ptr<BSDF> MicrofacetSheenBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetSheenBRDF(name, params));
}

}   // namespace renderer
