
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
#include "lambertianbrdf.h"

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
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>

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
    // Lambertian BRDF.
    //

    const char* Model = "lambertian_brdf";
    const char* MicrofacetModel = "microfacet_normal_mapping_lambertian_brdf";

    class LambertianBRDFImpl
      : public BSDF
    {
      public:
        LambertianBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse, params)
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
            if (!ScatteringMode::has_diffuse(modes))
                return;

            // Compute the incoming direction.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_cosine(s);
            sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));

            // Compute the probability density of the sampled direction.
            const float probability = wi.y * RcpPi<float>();
            assert(probability > 0.0f);

            if (probability > 1.0e-6f)
            {
                // Set the scattering mode.
                sample.set_to_scattering(ScatteringMode::Diffuse, probability);

                // Compute the BRDF value.
                const LambertianBRDFInputValues* values = static_cast<const LambertianBRDFInputValues*>(data);
                sample.m_value.m_diffuse = values->m_reflectance;
                sample.m_value.m_diffuse *= values->m_reflectance_multiplier;
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

            // Compute the BRDF value.
            const LambertianBRDFInputValues* values = static_cast<const LambertianBRDFInputValues*>(data);
            value.m_diffuse = values->m_reflectance;
            value.m_diffuse *= values->m_reflectance_multiplier * RcpPi<float>();
            value.m_beauty = value.m_diffuse;

            // Return the probability density of the sampled direction.
            const Vector3f& n = local_geometry.m_shading_basis.get_normal();
            const float cos_in = std::abs(dot(incoming, n));
            return cos_in * RcpPi<float>();
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

            // Return the probability density of the sampled direction.
            const Vector3f& n = local_geometry.m_shading_basis.get_normal();
            const float cos_in = std::abs(dot(incoming, n));
            return cos_in * RcpPi<float>();
        }
    };

    class MicrofacetLambertianBRDFImpl
      : public LambertianBRDFImpl
    {
      public:
        using LambertianBRDFImpl::LambertianBRDFImpl;

        const char* get_model() const override
        {
            return MicrofacetModel;
        }
    };

    typedef BSDFWrapper<LambertianBRDFImpl> LambertianBRDF;
    typedef MicrofacetBRDFWrapper<MicrofacetLambertianBRDFImpl> MicrofacetLambertianBRDF;
}


//
// LambertianBRDFFactory class implementation.
//

void LambertianBRDFFactory::release()
{
    delete this;
}

const char* LambertianBRDFFactory::get_model() const
{
    return Model;
}

Dictionary LambertianBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Lambertian BRDF");
}

DictionaryArray LambertianBRDFFactory::get_input_metadata() const
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

auto_release_ptr<BSDF> LambertianBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new LambertianBRDF(name, params));
}


//
// MicrofacetLambertianBRDFFactory class implementation.
//

const char* MicrofacetLambertianBRDFFactory::get_model() const
{
    return MicrofacetModel;
}

Dictionary MicrofacetLambertianBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", MicrofacetModel)
            .insert("label", "Microfacet Lambertian BRDF");
}

auto_release_ptr<BSDF> MicrofacetLambertianBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetLambertianBRDF(name, params));
}

}   // namespace renderer
