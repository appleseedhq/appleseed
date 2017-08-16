
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/matrix.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

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

        if (a.size() == b.size())
        {
            result.resize(a.size());

            for (size_t i = 0, e = a.size(); i < e; ++i)
                result[i] = one_minus_t * a[i] + t * b[i];
        }
        else
        {
            result.resize(Spectrum::Samples);

            Spectrum up_a, up_b;
            Spectrum::upgrade(a, up_a);
            Spectrum::upgrade(b, up_b);

            for (size_t i = 0; i < Spectrum::Samples; ++i)
                result[i] = one_minus_t * up_a[i] + t * up_b[i];
        }
    }

    struct DisneySpecularFresnelFun
    {
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
            mix_spectra(
                Color3f(1.0f),
                m_values.m_precomputed.m_tint_color,
                m_values.m_specular_tint,
                value);
            value *= m_values.m_specular * 0.08f;
            mix_spectra(value, m_values.m_base_color, m_values.m_metallic, value);
            mix_spectra(value, Color3f(1.0f), schlick_fresnel(dot(o, h)), value);
        }

        const DisneyBRDFInputValues& m_values;
    };

    struct DisneyClearcoatFresnelFun
    {
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
            value.set(mix(0.04f, 1.0f, schlick_fresnel(dot(o, h))) * 0.25f * m_values.m_clearcoat);
        }

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
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            const Vector3f incoming = sample.m_shading_basis.transform_to_parent(wi);

            sample.m_probability =
                evaluate(
                    values,
                    sample.m_shading_basis,
                    sample.m_outgoing.get_value(),
                    incoming,
                    sample.m_value.m_diffuse);
            assert(sample.m_probability > 0.0f);

            sample.m_mode = ScatteringMode::Diffuse;
            sample.m_incoming = Dual3f(incoming);
            sample.compute_reflected_differentials();
        }

        float evaluate(
            const DisneyBRDFInputValues*    values,
            const Basis3f&                  shading_basis,
            const Vector3f&                 outgoing,
            const Vector3f&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            const Vector3f n(shading_basis.get_normal());
            const Vector3f h(normalize(incoming + outgoing));

            const float cos_on = dot(n, outgoing);
            const float cos_in = dot(n, incoming);
            const float cos_ih = dot(incoming, h);

            float fd = 0.0f;
            const float fl = schlick_fresnel(cos_in);
            const float fv = schlick_fresnel(cos_on);

            if (values->m_subsurface != 1.0f)
            {
                const float fd90 = 0.5f + 2.0f * square(cos_ih) * values->m_roughness;
                fd = mix(1.0f, fd90, fl) * mix(1.0f, fd90, fv);
            }

            if (values->m_subsurface > 0.0f)
            {
                // Based on Hanrahan-Krueger BRDF approximation of isotropic BSRDF.
                // The 1.25f scale is used to (roughly) preserve albedo.
                // Fss90 is used to "flatten" retroreflection based on roughness.
                const float fss90 = square(cos_ih) * values->m_roughness;
                const float fss = mix(1.0f, fss90, fl) * mix(1.0f, fss90, fv);
                const float ss = 1.25f * (fss * (1.0f / (cos_on + cos_in) - 0.5f) + 0.5f);
                fd = mix(fd, ss, values->m_subsurface);
            }

            value = values->m_base_color;
            value *= fd * RcpPi<float>() * (1.0f - values->m_metallic);

            // Return the probability density of the sampled direction.
            return evaluate_pdf(shading_basis, incoming);
        }

        float evaluate_pdf(
            const Basis3f&                  shading_basis,
            const Vector3f&                 incoming) const
        {
            return dot(incoming, shading_basis.get_normal()) * RcpPi<float>();
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
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_uniform(s);

            // Transform the incoming direction to parent space.
            const Vector3f incoming = sample.m_shading_basis.transform_to_parent(wi);

            sample.m_probability =
                evaluate(
                    values,
                    sample.m_shading_basis,
                    sample.m_outgoing.get_value(),
                    incoming,
                    sample.m_value.m_diffuse);
            assert(sample.m_probability > 0.0f);

            sample.m_mode = ScatteringMode::Diffuse;
            sample.m_incoming = Dual3f(incoming);
            sample.compute_reflected_differentials();
        }

        float evaluate(
            const DisneyBRDFInputValues*    values,
            const Basis3f&                  shading_basis,
            const Vector3f&                 outgoing,
            const Vector3f&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            const Vector3f h(normalize(incoming + outgoing));
            const float cos_ih = dot(incoming, h);

            mix_spectra(
                Color3f(1.0f),
                values->m_precomputed.m_tint_color,
                values->m_sheen_tint,
                value);
            const float fh = schlick_fresnel(cos_ih);
            value *= fh * values->m_sheen * (1.0f - values->m_metallic);

            // Return the probability density of the sampled direction.
            return evaluate_pdf(shading_basis, incoming);
        }

        float evaluate_pdf(
            const Basis3f&                  shading_basis,
            const Vector3f&                 incoming) const
        {
            return RcpTwoPi<float>();
        }
    };


    //
    // Disney BRDF.
    //
    // References:
    //
    //   [1] Physically-Based Shading at Disney
    //       https://disney-animation.s3.amazonaws.com/library/s2012_pbs_disney_brdf_notes_v2.pdf
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
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse | ScatteringMode::Glossy, params)
          , m_lighting_conditions(nullptr)
        {
            m_inputs.declare("base_color", InputFormatSpectralReflectance);
            m_inputs.declare("subsurface", InputFormatFloat, "0.0");
            m_inputs.declare("metallic", InputFormatFloat, "0.0");
            m_inputs.declare("specular", InputFormatFloat, "0.0");
            m_inputs.declare("specular_tint", InputFormatFloat, "0.0");
            m_inputs.declare("anisotropic", InputFormatFloat, "0.0");
            m_inputs.declare("roughness", InputFormatFloat, "0.1");
            m_inputs.declare("sheen", InputFormatFloat, "0.0");
            m_inputs.declare("sheen_tint", InputFormatFloat, "0.0");
            m_inputs.declare("clearcoat", InputFormatFloat, "0.0");
            m_inputs.declare("clearcoat_gloss", InputFormatFloat, "1.0");
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch = 0) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Get the lighting conditions and XYZ to RGB matrix from the frame.
            const Frame* frame = project.get_frame();
            assert(frame);

            m_lighting_conditions = &frame->get_lighting_conditions();
            m_xyz_to_rgb = Matrix3f(frame->get_xyz_to_rgb_matrix());

            return true;
        }

        void on_frame_end(
            const Project&          project,
            const BaseGroup*        parent)
        {
            m_lighting_conditions = nullptr;
            BSDF::on_frame_end(project, parent);
        }

        virtual size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();

            const Color3f tint_xyz =
                values->m_base_color.is_rgb()
                    ? linear_rgb_to_ciexyz(values->m_base_color.rgb())
                    : spectrum_to_ciexyz<float>(*m_lighting_conditions, values->m_base_color);

            values->m_precomputed.m_tint_color =
                tint_xyz[1] > 0.0f
                    ? ciexyz_to_linear_rgb(tint_xyz / tint_xyz[1], m_xyz_to_rgb)
                    : Color3f(1.0f);

            values->m_precomputed.m_base_color_luminance = tint_xyz[1];
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const int               modes,
            BSDFSample&             sample) const override
        {
            // No reflection below the shading surface.
            const Vector3f& outgoing = sample.m_outgoing.get_value();
            const Vector3f& n = sample.m_shading_basis.get_normal();
            const float cos_on = dot(outgoing, n);
            if (cos_on < 0.0f)
                return;

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
            if (s < cdf[DiffuseComponent])
            {
                DisneyDiffuseComponent().sample(sampling_context, values, sample);
                sample.m_probability *= weights[DiffuseComponent];
                weights[DiffuseComponent] = 0.0f;
            }
            else if (s < cdf[SheenComponent])
            {
                DisneySheenComponent().sample(sampling_context, values, sample);
                sample.m_probability *= weights[SheenComponent];
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
                const GGXMDF ggx_mdf;
                MicrofacetBRDFHelper::sample(
                    sampling_context,
                    ggx_mdf,
                    alpha_x,
                    alpha_y,
                    0.0f,
                    DisneySpecularFresnelFun(*values),
                    cos_on,
                    sample);
                sample.m_probability *= weights[SpecularComponent];
                weights[SpecularComponent] = 0.0f;
            }
            else
            {
                const float alpha = clearcoat_roughness(values);
                const GTR1MDF gtr1_mdf;
                MicrofacetBRDFHelper::sample(
                    sampling_context,
                    gtr1_mdf,
                    alpha,
                    alpha,
                    0.0f,
                    DisneyClearcoatFresnelFun(*values),
                    cos_on,
                    sample);
                sample.m_probability *= weights[ClearcoatComponent];
                weights[ClearcoatComponent] = 0.0f;
            }

            // No reflection below the shading surface.
            const Vector3f& incoming = sample.m_incoming.get_value();
            const float cos_in = dot(incoming, n);
            if (cos_in <= 0.0f)
                return;

            if (weights[DiffuseComponent] > 0.0f)
            {
                Spectrum diffuse;
                sample.m_probability +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate(
                        values,
                        sample.m_shading_basis,
                        outgoing,
                        incoming,
                        diffuse);
                sample.m_value.m_diffuse += diffuse;
            }

            if (weights[SheenComponent] > 0.0f)
            {
                Spectrum sheen;
                sample.m_probability +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate(
                        values,
                        sample.m_shading_basis,
                        outgoing,
                        incoming,
                        sheen);
                sample.m_value.m_diffuse += sheen;
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
                const GGXMDF ggx_mdf;
                sample.m_probability +=
                    weights[SpecularComponent] *
                    MicrofacetBRDFHelper::evaluate(
                        ggx_mdf,
                        alpha_x,
                        alpha_y,
                        0.0f,
                        sample.m_shading_basis,
                        outgoing,
                        incoming,
                        DisneySpecularFresnelFun(*values),
                        cos_in,
                        cos_on,
                        spec);
                sample.m_value.m_glossy += spec;
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                Spectrum clear;
                const GTR1MDF gtr1_mdf;
                sample.m_probability +=
                    weights[ClearcoatComponent] *
                    MicrofacetBRDFHelper::evaluate(
                        gtr1_mdf,
                        alpha,
                        alpha,
                        0.0f,
                        sample.m_shading_basis,
                        outgoing,
                        incoming,
                        DisneyClearcoatFresnelFun(*values),
                        cos_in,
                        cos_on,
                        clear);
                sample.m_value.m_glossy += clear;
            }

            sample.m_value.m_beauty = sample.m_value.m_diffuse;
            sample.m_value.m_beauty += sample.m_value.m_glossy;
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
            ShadingComponents&      value) const override
        {
            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in <= 0.0f || cos_on <= 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute component weights.
            float weights[NumComponents];
            compute_component_weights(values, modes, weights);

            value.set(0.0f);
            float pdf = 0.0f;

            if (weights[DiffuseComponent] > 0.0f)
            {
                pdf +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate(
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        value.m_diffuse);
            }

            if (weights[SheenComponent] > 0.0f)
            {
                Spectrum sheen;
                pdf +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate(
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        sheen);
                value.m_diffuse += sheen;
            }

            if (weights[SpecularComponent] > 0.0f)
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);
                const GGXMDF ggx_mdf;
                pdf +=
                    weights[SpecularComponent] *
                    MicrofacetBRDFHelper::evaluate(
                        ggx_mdf,
                        alpha_x,
                        alpha_y,
                        0.0f,
                        shading_basis,
                        outgoing,
                        incoming,
                        DisneySpecularFresnelFun(*values),
                        cos_in,
                        cos_on,
                        value.m_glossy);
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                Spectrum clear;
                const GTR1MDF gtr1_mdf;
                pdf +=
                    weights[ClearcoatComponent] *
                    MicrofacetBRDFHelper::evaluate(
                        gtr1_mdf,
                        alpha,
                        alpha,
                        0.0f,
                        shading_basis,
                        outgoing,
                        incoming,
                        DisneyClearcoatFresnelFun(*values),
                        cos_in,
                        cos_on,
                        clear);
                value.m_glossy += clear;
            }

            value.m_beauty = value.m_diffuse;
            value.m_beauty += value.m_glossy;
            return pdf;
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const override
        {
            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute component weights.
            float weights[NumComponents];
            compute_component_weights(values, modes, weights);

            float pdf = 0.0f;

            if (weights[DiffuseComponent] > 0.0f)
            {
                pdf +=
                    weights[DiffuseComponent] *
                    DisneyDiffuseComponent().evaluate_pdf(shading_basis, incoming);
            }

            if (weights[SheenComponent] > 0.0f)
            {
                pdf +=
                    weights[SheenComponent] *
                    DisneySheenComponent().evaluate_pdf(shading_basis, incoming);
            }

            if (weights[SpecularComponent] > 0.0f)
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropic,
                    alpha_x,
                    alpha_y);
                const GGXMDF ggx_mdf;
                pdf +=
                    weights[SpecularComponent] *
                    MicrofacetBRDFHelper::pdf(
                        ggx_mdf,
                        alpha_x,
                        alpha_y,
                        0.0f,
                        shading_basis,
                        outgoing,
                        incoming);
            }

            if (weights[ClearcoatComponent] > 0.0f)
            {
                const float alpha = clearcoat_roughness(values);
                const GTR1MDF gtr1_mdf;
                pdf +=
                    weights[ClearcoatComponent] *
                    MicrofacetBRDFHelper::pdf(
                        gtr1_mdf,
                        alpha,
                        alpha,
                        0.0f,
                        shading_basis,
                        outgoing,
                        incoming);
            }

            return pdf;
        }

      private:
        typedef DisneyBRDFInputValues InputValues;

        const LightingConditions*   m_lighting_conditions;
        Matrix3f                    m_xyz_to_rgb;

        static bool compute_component_weights(
            const InputValues*      values,
            const int               modes,
            float                   weights[NumComponents])
        {
            if (ScatteringMode::has_diffuse(modes))
            {
                weights[DiffuseComponent] = lerp(values->m_precomputed.m_base_color_luminance, 0.0f, values->m_metallic);
                weights[SheenComponent] = lerp(values->m_sheen, 0.0f, values->m_metallic);
            }
            else
            {
                weights[DiffuseComponent] = 0.0f;
                weights[SheenComponent] = 0.0f;
            }

            if (ScatteringMode::has_glossy(modes))
            {
                weights[SpecularComponent] = lerp(values->m_specular, 1.0f, values->m_metallic);
                weights[ClearcoatComponent] = values->m_clearcoat * 0.25f;
            }
            else
            {
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
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

auto_release_ptr<BSDF> DisneyBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new DisneyBRDF(name, params));
}

}   // namespace renderer
