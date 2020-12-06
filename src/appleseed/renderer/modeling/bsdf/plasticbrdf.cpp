
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "plasticbrdf.h"

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
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>

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
    // Plastic BRDF.
    //
    // References:
    //
    //   [1] A Physically-Based Reflectance Model Combining Reflection and Diffraction
    //       https://hal.inria.fr/hal-01386157
    //
    //   [2] Mitsuba renderer
    //       https://github.com/mitsuba-renderer/mitsuba
    //

    const char* Model = "plastic_brdf";
    const char* MicrofacetModel = "microfacet_normal_mapping_plastic_brdf";

    class PlasticBRDFImpl
      : public BSDF
    {
      public:
        PlasticBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse | ScatteringMode::Glossy | ScatteringMode::Specular, params)
        {
            m_inputs.declare("diffuse_reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("diffuse_reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("specular_reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("specular_reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("roughness", InputFormat::Float, "0.15");
            m_inputs.declare("ior", InputFormat::Float, "1.5");
            m_inputs.declare("internal_scattering", InputFormat::Float, "1.0");
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

            // Apply multipliers to input values.
            values->m_specular_reflectance *= values->m_specular_reflectance_multiplier;
            values->m_diffuse_reflectance *= values->m_diffuse_reflectance_multiplier;

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);

            new (&values->m_precomputed) InputValues::Precomputed();
            const float outside_ior = shading_point.get_ray().get_current_ior();
            values->m_precomputed.m_eta = outside_ior / values->m_ior;

            values->m_precomputed.m_specular_weight = std::max(max_value(values->m_specular_reflectance), 0.0f);
            values->m_precomputed.m_diffuse_weight = std::max(max_value(values->m_diffuse_reflectance), 0.0f);
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
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);

            // Compute the microfacet normal by sampling the MDF.
            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing.get_value());
            sampling_context.split_in_place(3, 1);
            const Vector3f s = sampling_context.next2<Vector3f>();
            const Vector3f m =
                alpha == 0.0f
                    ? Vector3f(0.0f, 1.0f, 0.0f)
                    : GGXMDF::sample(wo, Vector2f(s[0], s[1]), alpha);

            const float F = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, F);

            Vector3f wi;

            // Choose between specular/glossy and diffuse.
            if (ScatteringMode::has_glossy(modes) &&
                (!ScatteringMode::has_diffuse(modes) || s[2] < specular_probability))
            {
                wi = improve_normalization(reflect(wo, m));
                if (wi.y <= 0.0f)
                    return;

                if (alpha == 0.0f)
                {
                    if (!ScatteringMode::has_specular(modes))
                        return;

                    sample.set_to_scattering(ScatteringMode::Specular, DiracDelta);
                    sample.m_value.m_glossy = values->m_specular_reflectance;
                    sample.m_value.m_glossy *= F;
                    sample.m_value.m_beauty = sample.m_value.m_glossy;
                    sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));
                    sample.m_min_roughness = values->m_roughness;
                    sample.compute_glossy_reflected_differentials(local_geometry, values->m_roughness, outgoing);
                }
                else
                {
                    const float probability = specular_pdf(alpha, wo, m) * specular_probability;
                    assert(probability >= 0.0f);

                    if (probability > 1.0e-6f)
                    {
                        sample.set_to_scattering(ScatteringMode::Glossy, probability);
                        sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));
                        sample.m_min_roughness = values->m_roughness;

                        evaluate_specular(
                            values->m_specular_reflectance,
                            alpha,
                            wi,
                            wo,
                            m,
                            F,
                            sample.m_value.m_glossy);
                        sample.m_value.m_beauty = sample.m_value.m_glossy;

                        sample.compute_glossy_reflected_differentials(local_geometry, values->m_roughness, outgoing);
                    }
                }
            }
            else
            {
                wi = sample_hemisphere_cosine(Vector2f(s[0], s[1]));

                const float probability = wi.y * RcpPi<float>() * (1.0f - specular_probability);
                assert(probability >= 0.0f);

                if (probability > 1.0e-6f)
                {
                    sample.set_to_scattering(ScatteringMode::Diffuse, probability);
                    sample.m_incoming = Dual3f(local_geometry.m_shading_basis.transform_to_parent(wi));
                    sample.m_min_roughness = values->m_roughness;

                    const float Fi = fresnel_reflectance(wi, m, values->m_precomputed.m_eta);
                    evaluate_diffuse(
                        values->m_diffuse_reflectance,
                        values->m_precomputed.m_eta,
                        values->m_internal_scattering,
                        F,
                        Fi,
                        sample.m_value.m_diffuse);
                    sample.m_value.m_beauty = sample.m_value.m_diffuse;
                    sample.m_aov_components.m_albedo = values->m_diffuse_reflectance;

                    sample.compute_diffuse_differentials(outgoing);
                }
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
            const InputValues* values = static_cast<const InputValues*>(data);
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);

            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
            const Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);
            const Vector3f m =
                alpha == 0.0f
                    ? Vector3f(0.0f, 1.0f, 0.0f)
                    : normalize(wi + wo);

            const float Fo = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float Fi = fresnel_reflectance(wi, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, Fo);

            float pdf_glossy = 0.0f, pdf_diffuse = 0.0f;

            if (ScatteringMode::has_glossy(modes))
            {
                evaluate_specular(
                    values->m_specular_reflectance,
                    alpha,
                    wi,
                    wo,
                    m,
                    Fo,
                    value.m_glossy);

                pdf_glossy = specular_pdf(alpha, wo, m);
            }

            if (ScatteringMode::has_diffuse(modes))
            {
                evaluate_diffuse(
                    values->m_diffuse_reflectance,
                    values->m_precomputed.m_eta,
                    values->m_internal_scattering,
                    Fo,
                    Fi,
                    value.m_diffuse);

                pdf_diffuse = std::abs(wi.y) * RcpPi<float>();
            }

            value.m_beauty = value.m_diffuse;
            value.m_beauty += value.m_glossy;

            const float pdf =
                ScatteringMode::has_diffuse_and_glossy(modes) ? lerp(pdf_diffuse, pdf_glossy, specular_probability) :
                ScatteringMode::has_diffuse(modes) ? pdf_diffuse :
                ScatteringMode::has_glossy(modes) ? pdf_glossy : 0.0f;
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
            const InputValues* values = static_cast<const InputValues*>(data);
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);

            const Vector3f wo = local_geometry.m_shading_basis.transform_to_local(outgoing);
            const Vector3f wi = local_geometry.m_shading_basis.transform_to_local(incoming);
            const Vector3f m =
                alpha == 0.0f
                    ? Vector3f(0.0f, 1.0f, 0.0f)
                    : normalize(wi + wo);

            const float F = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, F);

            const float pdf_glossy =
                ScatteringMode::has_glossy(modes)
                    ? specular_pdf(alpha, wo, m)
                    : 0.0f;

            const float pdf_diffuse =
                ScatteringMode::has_diffuse(modes)
                    ? std::abs(wi.y) * RcpPi<float>()
                    : 0.0f;

            const float pdf =
                ScatteringMode::has_diffuse_and_glossy(modes) ? lerp(pdf_diffuse, pdf_glossy, specular_probability) :
                ScatteringMode::has_diffuse(modes) ? pdf_diffuse :
                ScatteringMode::has_glossy(modes) ? pdf_glossy : 0.0f;
            assert(pdf >= 0.0f);

            return pdf;
        }

      private:
        typedef PlasticBRDFInputValues InputValues;

        static float choose_specular_probability(
            const InputValues&          values,
            const float                 F)
        {
            const float specular_weight = F * values.m_precomputed.m_specular_weight;
            const float diffuse_weight = (1.0f - F) * values.m_precomputed.m_diffuse_weight;
            const float total_weight = specular_weight + diffuse_weight;
            return total_weight == 0.0f ? 1.0f : specular_weight / total_weight;
        }

        static float fresnel_reflectance(
            const Vector3f&             w,
            const Vector3f&             m,
            const float                 eta)
        {
            const float cos_wm(dot(w, m));
            if (cos_wm < 0.0f)
                return 0.0f;

            float F;
            fresnel_reflectance_dielectric(
                F,
                eta,
                std::min(cos_wm, 1.0f));

            return F;
        }

        static void evaluate_specular(
            const Spectrum&             specular_reflectance,
            const float                 alpha,
            const Vector3f&             wi,
            const Vector3f&             wo,
            const Vector3f&             m,
            const float                 F,
            Spectrum&                   value)
        {
            if (alpha == 0.0f)
                return;

            const float denom = std::abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            value = specular_reflectance;
            const float D = GGXMDF::D(m, alpha);
            const float G = GGXMDF::G(wi, wo, m, alpha);
            value *= F * D * G / denom;
        }

        static float specular_pdf(
            const float                 alpha,
            const Vector3f&             wo,
            const Vector3f&             m)
        {
            if (alpha == 0.0f)
                return 0.0f;

            const float cos_wom = dot(wo, m);
            if (cos_wom == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * std::abs(cos_wom));
            return jacobian * GGXMDF::pdf(wo, m, alpha, alpha);
        }

        static void evaluate_diffuse(
            const Spectrum&             diffuse_reflectance,
            const float                 eta,
            const float                 internal_scattering,
            const float                 Fo,
            const float                 Fi,
            Spectrum&                   value)
        {
            const float eta2 = square(eta);
            const float fdr = fresnel_internal_diffuse_reflectance(1.0f / eta);
            const float T = (1.0f - Fo) * (1.0f - Fi);

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float pd = diffuse_reflectance[i];
                const float non_linear_term = 1.0f - lerp(1.0f, pd, internal_scattering) * fdr;
                value[i] = (T * pd * eta2 * RcpPi<float>()) / non_linear_term;
            }
        }
    };

    class MicrofacetPlasticBRDFImpl
      : public PlasticBRDFImpl
    {
      public:
        using PlasticBRDFImpl::PlasticBRDFImpl;

        const char* get_model() const override
        {
            return MicrofacetModel;
        }
    };

    typedef BSDFWrapper<PlasticBRDFImpl> PlasticBRDF;
    typedef MicrofacetBRDFWrapper<MicrofacetPlasticBRDFImpl> MicrofacetPlasticBRDF;
}


//
// PlasticBRDFFactory class implementation.
//

void PlasticBRDFFactory::release()
{
    delete this;
}

const char* PlasticBRDFFactory::get_model() const
{
    return Model;
}

Dictionary PlasticBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Plastic BRDF");
}

DictionaryArray PlasticBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "diffuse_reflectance")
            .insert("label", "Diffuse Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diffuse_reflectance_multiplier")
            .insert("label", "Diffuse Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance")
            .insert("label", "Specular Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance_multiplier")
            .insert("label", "Specular Reflectance Multiplier")
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
            .insert("name", "internal_scattering")
            .insert("label", "Internal Scattering")
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
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSDF> PlasticBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new PlasticBRDF(name, params));
}


//
// MicrofacetPlasticBRDFFactory class implementation.
//

const char* MicrofacetPlasticBRDFFactory::get_model() const
{
    return MicrofacetModel;
}

Dictionary MicrofacetPlasticBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", MicrofacetModel)
            .insert("label", "Microfacet Plastic BRDF");
}

auto_release_ptr<BSDF> MicrofacetPlasticBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetPlasticBRDF(name, params));
}

}   // namespace renderer
