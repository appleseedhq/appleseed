
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "disneybrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/color/colorspace.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingPoint; }

using namespace foundation;

namespace renderer
{

namespace
{
    float schlick_fresnel(const float u)
    {
        const float m = saturate(1.0f - u);
        const float m2 = square(m);
        const float m4 = square(m2);
        return m4 * m;
    }

    void mix_spectra(
        const Spectrum&     a,
        const Spectrum&     b,
        const float         t,
        Spectrum&           result)
    {
        const float one_minus_t = 1.0f - t;

        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            result[i] = one_minus_t * a[i] + t * b[i];
    }

    void mix_one_with_spectra(
        const Spectrum&     b,
        const float         t,
        Spectrum&           result)
    {
        const float one_minus_t = 1.0f - t;

        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            result[i] = one_minus_t + t * b[i];
    }

    void mix_spectra_with_one(
        const Spectrum&     a,
        const float         t,
        Spectrum&           result)
    {
        const float one_minus_t = 1.0f - t;

        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            result[i] = one_minus_t * a[i] + t;
    }

    class DisneySpecularFresnelFun
    {
      public:
        explicit DisneySpecularFresnelFun(const DisneyBRDFInputValues& values)
          : m_values(values)
        {
        }

        void operator()(
            const Vector3f& o,
            const Vector3f& h,
            const Vector3f& n,
            Spectrum&       value) const
        {
            mix_one_with_spectra(
                m_values.m_precomputed.m_tint_color,
                m_values.m_specular_tint,
                value);
            value *= m_values.m_specular * 0.08f;
            mix_spectra(value, m_values.m_base_color, m_values.m_metallic, value);
            const float cos_oh = std::abs(dot(o, h));
            mix_spectra_with_one(value, schlick_fresnel(cos_oh), value);
        }

      private:
        const DisneyBRDFInputValues& m_values;
    };

    class DisneyClearcoatFresnelFun
    {
      public:
        explicit DisneyClearcoatFresnelFun(const DisneyBRDFInputValues& values)
          : m_values(values)
        {
        }

        void operator()(
            const Vector3f& o,
            const Vector3f& h,
            const Vector3f& n,
            Spectrum&       value) const
        {
            const float cos_oh = std::abs(dot(o, h));
            value.set(mix(0.04f, 1.0f, schlick_fresnel(cos_oh)) * 0.25f * m_values.m_clearcoat);
        }

      private:
        const DisneyBRDFInputValues& m_values;
    };


    //
    // Diffuse component of the Disney BRDF.
    //

    class DisneyDiffuseComponent
    {
      public:
        void sample(
            SamplingContext&                sampling_context,
            const DisneyBRDFInputValues*    values,
            const BSDF::LocalGeometry&      local_geometry,
            const Dual3f&                   outgoing,
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_cosine(s);
            const Vector3f incoming = local_geometry.m_shading_basis.transform_to_parent(wi);
            sample.m_incoming = Dual3f(incoming);

            // Compute the component value and the probability density of the sampled direction.
            const float probability =
                evaluate(
                    values,
                    local_geometry,
                    outgoing.get_value(),
                    incoming,
                    sample.m_value.m_diffuse);
            assert(probability > 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(ScatteringMode::Diffuse, probability);
                sample.m_aov_components.m_albedo = values->m_base_color;
                sample.compute_diffuse_differentials(outgoing);
            }
        }

        float evaluate(
            const DisneyBRDFInputValues*    values,
            const BSDF::LocalGeometry&      local_geometry,
            const Vector3f&                 outgoing,
            const Vector3f&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            const Vector3f n(local_geometry.m_shading_basis.get_normal());
            const Vector3f h(normalize(incoming + outgoing));

            // Using the absolute values of cos_on and cos_in creates discontinuities.
            const float cos_on = dot(n, outgoing);
            const float cos_in = dot(n, incoming);
            const float cos_ih = dot(incoming, h);

            const float fl = schlick_fresnel(cos_in);
            const float fv = schlick_fresnel(cos_on);
            float fd = 0.0f;

            if (values->m_subsurface != 1.0f)
            {
                const float fd90 = 0.5f + 2.0f * square(cos_ih) * values->m_roughness;
                fd = mix(1.0f, fd90, fl) * mix(1.0f, fd90, fv);
            }

            if (values->m_subsurface > 0.0f)
            {
                // Based on Hanrahan-Krueger BRDF approximation of isotropic BSRDF.
                // The 1.25 scale is used to (roughly) preserve albedo.
                // Fss90 is used to "flatten" retroreflection based on roughness.
                const float fss90 = square(cos_ih) * values->m_roughness;
                const float fss = mix(1.0f, fss90, fl) * mix(1.0f, fss90, fv);
                const float ss = 1.25f * (fss * (1.0f / (std::abs(cos_on) + std::abs(cos_in)) - 0.5f) + 0.5f);
                fd = mix(fd, ss, values->m_subsurface);
            }

            value = values->m_base_color;
            value *= fd * RcpPi<float>() * (1.0f - values->m_metallic);

            // Return the probability density of the sampled direction.
            return std::abs(cos_in) * RcpPi<float>();
        }

        float evaluate_pdf(
            const BSDF::LocalGeometry&      local_geometry,
            const Vector3f&                 incoming) const
        {
            const Vector3f& n = local_geometry.m_shading_basis.get_normal();
            const float cos_in = std::abs(dot(incoming, n));
            return cos_in * RcpPi<float>();
        }
    };


    //
    // Sheen component of the Disney BRDF.
    //

    class DisneySheenComponent
    {
      public:
        void sample(
            SamplingContext&                sampling_context,
            const DisneyBRDFInputValues*    values,
            const BSDF::LocalGeometry&      local_geometry,
            const Dual3f&                   outgoing,
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_uniform(s);
            const Vector3f incoming = local_geometry.m_shading_basis.transform_to_parent(wi);
            sample.m_incoming = Dual3f(incoming);

            // Compute the component value and the probability density of the sampled direction.
            const float probability =
                evaluate(
                    values,
                    local_geometry,
                    outgoing.get_value(),
                    incoming,
                    sample.m_value.m_glossy);
            assert(probability > 0.0f);

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(ScatteringMode::Glossy, probability);
                sample.compute_diffuse_differentials(outgoing);
            }
        }

        float evaluate(
            const DisneyBRDFInputValues*    values,
            const BSDF::LocalGeometry&      local_geometry,
            const Vector3f&                 outgoing,
            const Vector3f&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            // Compute the component value.
            const Vector3f h(normalize(incoming + outgoing));
            const float cos_ih = dot(incoming, h);
            const float fh = schlick_fresnel(cos_ih);
            mix_one_with_spectra(
                values->m_precomputed.m_tint_color,
                values->m_sheen_tint,
                value);
            value *= fh * values->m_sheen * (1.0f - values->m_metallic);

            // Return the probability density of the sampled direction.
            return RcpTwoPi<float>();
        }

        float evaluate_pdf(
            const BSDF::LocalGeometry&      local_geometry,
            const Vector3f&                 incoming) const
        {
            return RcpTwoPi<float>();
        }
    };


    //
    // Disney BRDF.
    //
    // Reference:
    //
    //   Physically-Based Shading at Disney
    //   https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
    //

    namespace
    {
        const char* Model = "disney_brdf";
    }

    class DisneyBRDFImpl
      : public BSDF
    {
      public:
        // Components of this BRDF.
        enum Components
        {
            DiffuseComponent,
            SheenComponent,
            SpecularComponent,
            ClearcoatComponent,
            NumComponents
        };

        DisneyBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse | ScatteringMode::Glossy, params)
        {
            m_inputs.declare("base_color", InputFormat::SpectralReflectance);
            m_inputs.declare("subsurface", InputFormat::Float, "0.0");
            m_inputs.declare("metallic", InputFormat::Float, "0.0");
            m_inputs.declare("specular", InputFormat::Float, "0.0");
            m_inputs.declare("specular_tint", InputFormat::Float, "0.0");
            m_inputs.declare("anisotropic", InputFormat::Float, "0.0");
            m_inputs.declare("roughness", InputFormat::Float, "0.1");
            m_inputs.declare("sheen", InputFormat::Float, "0.0");
            m_inputs.declare("sheen_tint", InputFormat::Float, "0.0");
            m_inputs.declare("clearcoat", InputFormat::Float, "0.0");
            m_inputs.declare("clearcoat_gloss", InputFormat::Float, "1.0");
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

            values->m_roughness = std::max(values->m_roughness, shading_point.get_ray().m_min_roughness);

            new (&values->m_precomputed) InputValues::Precomputed();

            const Color3f tint_xyz =
                values->m_base_color.reflectance_to_ciexyz(g_std_lighting_conditions);

            values->m_precomputed.m_tint_color.set(
                tint_xyz[1] > 0.0f
                    ? ciexyz_to_linear_rgb(tint_xyz / tint_xyz[1])
                    : Color3f(1.0f),
                g_std_lighting_conditions,
                Spectrum::Reflectance);

            values->m_precomputed.m_base_color_luminance = tint_xyz[1];
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

            // Compute component weights.
            float weights[NumComponents];
            if (!compute_component_weights(values, modes, weights))
                return;

            // Compute CDF to sample components.
            float cdf[NumComponents];
            cdf[DiffuseComponent] = weights[DiffuseComponent];
            cdf[SheenComponent] = cdf[DiffuseComponent] + weights[SheenComponent];
            cdf[SpecularComponent] = cdf[SheenComponent] + weights[SpecularComponent];
            cdf[ClearcoatComponent] = cdf[SpecularComponent] + weights[ClearcoatComponent];

            // Choose which of the components to sample.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();

            // Sample the chosen component.
            float probability;
            if (s < cdf[DiffuseComponent])
            {
                DisneyDiffuseComponent().sample(
                    sampling_context,
                    values,
                    local_geometry,
                    outgoing,
                    sample);
                probability = weights[DiffuseComponent] * sample.get_probability();
                weights[DiffuseComponent] = 0.0f;
            }
            else if (s < cdf[SheenComponent])
            {
                DisneySheenComponent().sample(
                    sampling_context,
                    values,
                    local_geometry,
                    outgoing,
                    sample);
                probability = weights[SheenComponent] * sample.get_probability();
                weights[SheenComponent] = 0.0f;
            }
            else if (s < cdf[SpecularComponent])
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);
                MicrofacetBRDFHelper<GGXMDF>::sample(
                    sampling_context,
                    values->m_roughness,
                    alpha_x,
                    alpha_y,
                    DisneySpecularFresnelFun(*values),
                    local_geometry,
                    outgoing,
                    sample);
                probability = weights[SpecularComponent] * sample.get_probability();
                weights[SpecularComponent] = 0.0f;
            }
            else
            {
                const float alpha = clearcoat_roughness(values);
                MicrofacetBRDFHelper<GTR1MDF>::sample(
                    sampling_context,
                    alpha,
                    alpha,
                    alpha,
                    DisneyClearcoatFresnelFun(*values),
                    local_geometry,
                    outgoing,
                    sample);
                probability = weights[ClearcoatComponent] * sample.get_probability();
                weights[ClearcoatComponent] = 0.0f;
            }

            if (sample.get_mode() == ScatteringMode::None)
                return;

            const Vector3f& incoming = sample.m_incoming.get_value();

            if (weights[DiffuseComponent] > 0.0f)
            {
                Spectrum diffuse;
                probability +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate(
                        values,
                        local_geometry,
                        outgoing.get_value(),
                        incoming,
                        diffuse);
                sample.m_value.m_diffuse += diffuse;
            }

            if (weights[SheenComponent] > 0.0f)
            {
                Spectrum sheen;
                probability +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate(
                        values,
                        local_geometry,
                        outgoing.get_value(),
                        incoming,
                        sheen);
                sample.m_value.m_glossy += sheen;
            }

            if (weights[SpecularComponent] > 0.0f)
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);
                Spectrum spec;
                probability +=
                    weights[SpecularComponent] *
                    MicrofacetBRDFHelper<GGXMDF>::evaluate(
                        alpha_x,
                        alpha_y,
                        DisneySpecularFresnelFun(*values),
                        local_geometry,
                        outgoing.get_value(),
                        incoming,
                        spec);
                sample.m_value.m_glossy += spec;
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                Spectrum clear;
                probability +=
                    weights[ClearcoatComponent] *
                    MicrofacetBRDFHelper<GTR1MDF>::evaluate(
                        alpha,
                        alpha,
                        DisneyClearcoatFresnelFun(*values),
                        local_geometry,
                        outgoing.get_value(),
                        incoming,
                        clear);
                sample.m_value.m_glossy += clear;
            }

            if (probability > 1.0e-6f)
            {
                sample.set_to_scattering(sample.get_mode(), probability);
                sample.m_value.m_beauty = sample.m_value.m_diffuse;
                sample.m_value.m_beauty += sample.m_value.m_glossy;
                sample.m_min_roughness = values->m_roughness;
            }
            else
                sample.set_to_absorption();
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

            // Compute component weights.
            float weights[NumComponents];
            compute_component_weights(values, modes, weights);

            float pdf = 0.0f;

            if (weights[DiffuseComponent] > 0.0f)
            {
                pdf +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate(
                        values,
                        local_geometry,
                        outgoing,
                        incoming,
                        value.m_diffuse);
            }

            if (weights[SheenComponent] > 0.0f)
            {
                pdf +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate(
                        values,
                        local_geometry,
                        outgoing,
                        incoming,
                        value.m_glossy);
            }

            if (weights[SpecularComponent] > 0.0f)
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);

                Spectrum spec;
                const float spec_pdf =
                    MicrofacetBRDFHelper<GGXMDF>::evaluate(
                        alpha_x,
                        alpha_y,
                        DisneySpecularFresnelFun(*values),
                        local_geometry,
                        outgoing,
                        incoming,
                        spec);

                if (spec_pdf > 0.0f)
                {
                    pdf += weights[SpecularComponent] * spec_pdf;
                    value.m_glossy += spec;
                }
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                Spectrum clearcoat;
                const float clearcoat_pdf =
                    MicrofacetBRDFHelper<GTR1MDF>::evaluate(
                        alpha,
                        alpha,
                        DisneyClearcoatFresnelFun(*values),
                        local_geometry,
                        outgoing,
                        incoming,
                        clearcoat);

                if (clearcoat_pdf > 0.0f)
                {
                    pdf += weights[ClearcoatComponent] * clearcoat_pdf;
                    value.m_glossy += clearcoat;
                }
            }

            value.m_beauty = value.m_diffuse;
            value.m_beauty += value.m_glossy;

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

            // Compute component weights.
            float weights[NumComponents];
            compute_component_weights(values, modes, weights);

            float pdf = 0.0f;

            if (weights[DiffuseComponent] > 0.0f)
            {
                pdf +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate_pdf(local_geometry, incoming);
            }

            if (weights[SheenComponent] > 0.0f)
            {
                pdf +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate_pdf(local_geometry, incoming);
            }

            if (weights[SpecularComponent] > 0.0f)
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);
                pdf +=
                    weights[SpecularComponent] *
                    MicrofacetBRDFHelper<GGXMDF>::pdf(
                        alpha_x,
                        alpha_y,
                        local_geometry,
                        outgoing,
                        incoming);
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                pdf +=
                    weights[ClearcoatComponent] *
                    MicrofacetBRDFHelper<GTR1MDF>::pdf(
                        alpha,
                        alpha,
                        local_geometry,
                        outgoing,
                        incoming);
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

      private:
        typedef DisneyBRDFInputValues InputValues;

        static bool compute_component_weights(
            const InputValues*          values,
            const int                   modes,
            float                       weights[NumComponents])
        {
            weights[DiffuseComponent] =
                ScatteringMode::has_diffuse(modes)
                    ? lerp(values->m_precomputed.m_base_color_luminance, 0.0f, values->m_metallic)
                    : 0.0f;

            if (ScatteringMode::has_glossy(modes))
            {
                weights[SheenComponent] = lerp(values->m_sheen, 0.0f, values->m_metallic);
                weights[SpecularComponent] = lerp(values->m_specular, 1.0f, values->m_metallic);
                weights[ClearcoatComponent] = values->m_clearcoat * 0.25f;
            }
            else
            {
                weights[SheenComponent] = 0.0f;
                weights[SpecularComponent] = 0.0f;
                weights[ClearcoatComponent] = 0.0f;
            }

            const float total_weight =
                weights[DiffuseComponent] +
                weights[SheenComponent] +
                weights[SpecularComponent] +
                weights[ClearcoatComponent];

            if (total_weight == 0.0f)
                return false;

            const float rcp_total_weight = 1.0f / total_weight;
            weights[DiffuseComponent] *= rcp_total_weight;
            weights[SheenComponent] *= rcp_total_weight;
            weights[SpecularComponent] *= rcp_total_weight;
            weights[ClearcoatComponent] *= rcp_total_weight;

            return true;
        }

        static float clearcoat_roughness(const InputValues* values)
        {
            return mix(0.1f, 0.001f, values->m_clearcoat_gloss);
        }
    };

    typedef BSDFWrapper<DisneyBRDFImpl> DisneyBRDF;
}


//
// DisneyBRDFFactory class implementation.
//

void DisneyBRDFFactory::release()
{
    delete this;
}

const char* DisneyBRDFFactory::get_model() const
{
    return Model;
}

Dictionary DisneyBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Disney BRDF")
            .insert("default_model", "true");
}

DictionaryArray DisneyBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.9"));

    metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

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
            .insert("default", "0.1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen_tint")
            .insert("label", "Sheen Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat Gloss")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSDF> DisneyBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new DisneyBRDF(name, params));
}

}   // namespace renderer
