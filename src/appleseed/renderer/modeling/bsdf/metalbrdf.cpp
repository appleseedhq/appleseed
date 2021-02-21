
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "metalbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacetbrdfwrapper.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/bsdf/specularhelper.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
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
    // Metal BRDF.
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    //   [2] Revisiting Physically Based Shading at Imageworks
    //       http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
    //
    //   [3] Practical multiple scattering compensation for microfacet models
    //       https://blog.selfshadow.com/publications/turquin/ms_comp_final.pdf
    //

    const char* Model = "metal_brdf";
    const char* MicrofacetModel = "microfacet_normal_mapping_metal_brdf";

    class MetalBRDFImpl
      : public BSDF
    {
      public:
        MetalBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Glossy | ScatteringMode::Specular, params)
        {
            m_inputs.declare("normal_reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("edge_tint", InputFormat::SpectralReflectance);
            m_inputs.declare("edge_tint_weight", InputFormat::Float, "1.0");
            m_inputs.declare("reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("roughness", InputFormat::Float, "0.15");
            m_inputs.declare("anisotropy", InputFormat::Float, "0.0");
            m_inputs.declare("energy_compensation", InputFormat::Float, "0.0");
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

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);

            if (values->m_edge_tint_weight != 0.0f)
            {
                fresnel_lazanyi_schlick_a(
                    values->m_precomputed.m_a,
                    values->m_normal_reflectance,
                    values->m_edge_tint,
                    values->m_edge_tint_weight);
            }
            else
                values->m_precomputed.m_a.set(0.0f);
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
            const InputValues* values = static_cast<const InputValues*>(data);

            const FresnelConductorSchlickLazanyi f(
                values->m_normal_reflectance,
                values->m_precomputed.m_a,
                values->m_reflectance_multiplier);

            // If roughness is zero use reflection.
            if (values->m_roughness == 0.0f)
            {
                if (ScatteringMode::has_specular(modes))
                {
                    SpecularBRDFHelper::sample(f, local_geometry, outgoing, sample);
                    sample.m_value.m_beauty = sample.m_value.m_glossy;
                }

                return;
            }

            if (ScatteringMode::has_glossy(modes))
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropy,
                    alpha_x,
                    alpha_y);

                MicrofacetBRDFHelper<GGXMDF>::sample(
                    sampling_context,
                    values->m_roughness,
                    alpha_x,
                    alpha_y,
                    f,
                    local_geometry,
                    outgoing,
                    sample);

                if (sample.get_mode() != ScatteringMode::None)
                {
                    apply_energy_compensation_factor(
                        values,
                        outgoing.get_value(),
                        local_geometry.m_shading_basis.get_normal(),
                        sample.m_value.m_glossy);

                    sample.m_min_roughness = values->m_roughness;
                }

                sample.m_value.m_beauty = sample.m_value.m_glossy;
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
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const FresnelConductorSchlickLazanyi f(
                values->m_normal_reflectance,
                values->m_precomputed.m_a,
                values->m_reflectance_multiplier);

            const float pdf =
                MicrofacetBRDFHelper<GGXMDF>::evaluate(
                    alpha_x,
                    alpha_y,
                    f,
                    local_geometry,
                    outgoing,
                    incoming,
                    value.m_glossy);

            apply_energy_compensation_factor(
                values,
                outgoing,
                local_geometry.m_shading_basis.get_normal(),
                value.m_glossy);

            value.m_beauty = value.m_glossy;

            assert(pdf >= 0.0f);
            return pdf;
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

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const float pdf =
                MicrofacetBRDFHelper<GGXMDF>::pdf(
                    alpha_x,
                    alpha_y,
                    local_geometry,
                    outgoing,
                    incoming);

            assert(pdf >= 0.0f);
            return pdf;
        }

      private:
        typedef MetalBRDFInputValues InputValues;

        static void apply_energy_compensation_factor(
            const InputValues*          values,
            const Vector3f&             outgoing,
            const Vector3f&             n,
            Spectrum&                   value)
        {
            if (values->m_energy_compensation != 0.0f)
            {
                const float Ess = get_directional_albedo(
                    std::abs(dot(outgoing, n)),
                    values->m_roughness);

                if (Ess == 0.0f)
                    return;

                Spectrum fms = values->m_normal_reflectance;
                fms *= values->m_energy_compensation * (1.0f - Ess) / Ess;
                fms += Spectrum(1.0f);
                value *= fms;
            }
        }
    };

    class MicrofacetMetalBRDFImpl
      : public MetalBRDFImpl
    {
      public:
        using MetalBRDFImpl::MetalBRDFImpl;

        const char* get_model() const override
        {
            return MicrofacetModel;
        }
    };

    typedef BSDFWrapper<MetalBRDFImpl> MetalBRDF;
    typedef MicrofacetBRDFWrapper<MicrofacetMetalBRDFImpl> MicrofacetMetalBRDF;
}


//
// MetalBRDFFactory class implementation.
//

void MetalBRDFFactory::release()
{
    delete this;
}

const char* MetalBRDFFactory::get_model() const
{
    return Model;
}

Dictionary MetalBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Metal BRDF");
}

DictionaryArray MetalBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "normal_reflectance")
            .insert("label", "Normal Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.92"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edge_tint")
            .insert("label", "Edge Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.98"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edge_tint_weight")
            .insert("label", "Edge Tint Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "1.0"));

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
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.15"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "energy_compensation")
            .insert("label", "Energy Compensation")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<BSDF> MetalBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MetalBRDF(name, params));
}


//
// MicrofacetMetalBRDFFactory class implementation.
//

const char* MicrofacetMetalBRDFFactory::get_model() const
{
    return MicrofacetModel;
}

Dictionary MicrofacetMetalBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", MicrofacetModel)
            .insert("label", "Microfacet Metal BRDF");
}

auto_release_ptr<BSDF> MicrofacetMetalBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetMetalBRDF(name, params));
}

}   // namespace renderer
