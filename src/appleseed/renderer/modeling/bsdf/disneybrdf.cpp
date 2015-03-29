
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacetbrdfhelper.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>

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
    double schlick_fresnel(const double u)
    {
        const double m = saturate(1.0 - u);
        const double m2 = square(m);
        const double m4 = square(m2);
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
        explicit DisneySpecularFresnelFun(
            const DisneyBRDFInputValues& values)
          : m_values(values)
        {
        }

        void operator()(
            const Vector3d& o,
            const Vector3d& h,
            const Vector3d& n,
            Spectrum&       value) const
        {
            mix_spectra(Color3f(1.0f), m_values.m_tint_color, static_cast<float>(m_values.m_specular_tint), value);
            value *= static_cast<float>(m_values.m_specular * 0.08);
            mix_spectra(value, m_values.m_base_color, static_cast<float>(m_values.m_metallic), value);
            mix_spectra(value, Color3f(1.0f), static_cast<float>(schlick_fresnel(dot(o, h))), value);
        }

        const DisneyBRDFInputValues& m_values;
    };

    struct DisneyClearcoatFresnelFun
    {
        explicit DisneyClearcoatFresnelFun(
            const DisneyBRDFInputValues& values)
          : m_values(values)
        {
        }

        void operator()(
            const Vector3d& o,
            const Vector3d& h,
            const Vector3d& n,
            Spectrum&       value) const
        {
            value.set(
                static_cast<float>(
                    mix(0.04, 1.0, schlick_fresnel(dot(o, h))) * 0.25 * m_values.m_clearcoat));
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
            const DisneyBRDFInputValues*    values,
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction in local space.
            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            const Vector3d incoming = sample.get_shading_basis().transform_to_parent(wi);
            sample.set_probability(
                evaluate(
                    values,
                    sample.get_shading_basis(),
                    sample.get_outgoing_vector(),
                    incoming,
                    sample.value()));

            assert(sample.get_probability() > 0.0);
            sample.set_mode(BSDFSample::Diffuse);
            sample.set_incoming(incoming);
        }

        double evaluate(
            const DisneyBRDFInputValues*    values,
            const Basis3d&                  shading_basis,
            const Vector3d&                 outgoing,
            const Vector3d&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            const Vector3d n(shading_basis.get_normal());
            const Vector3d h(normalize(incoming + outgoing));

            const double cos_on = dot(n, outgoing);
            const double cos_in = dot(n, incoming);
            const double cos_ih = dot(incoming, h);

            double fd = 0.0;
            const double fl = schlick_fresnel(cos_in);
            const double fv = schlick_fresnel(cos_on);

            if (values->m_subsurface != 1.0)
            {
                const double fd90 = 0.5 + 2.0 * square(cos_ih) * values->m_roughness;
                fd = mix(1.0, fd90, fl) * mix(1.0, fd90, fv);
            }

            if (values->m_subsurface > 0.0)
            {
                // Based on Hanrahan-Krueger brdf approximation of isotropic bssrdf
                // 1.25 scale is used to (roughly) preserve albedo
                // Fss90 used to "flatten" retroreflection based on roughness

                const double fss90 = square(cos_ih) * values->m_roughness;
                const double fss = mix(1.0, fss90, fl) * mix(1.0, fss90, fv);
                const double ss = 1.25 * (fss * (1.0 / (cos_on + cos_in) - 0.5) + 0.5);
                fd = mix(fd, ss, values->m_subsurface);
            }

            value = values->m_base_color;
            value *= static_cast<float>(fd * RcpPi * (1.0 - values->m_metallic));

            // Return the probability density of the sampled direction.
            return evaluate_pdf(shading_basis, incoming);
        }

        double evaluate_pdf(
            const Basis3d&                  shading_basis,
            const Vector3d&                 incoming) const
        {
            return dot(incoming, shading_basis.get_normal()) * RcpPi;
        }
    };


    //
    // Sheen component of the Disney BRDF.
    //

    class DisneySheenComponent
    {
      public:
        void sample(
            const DisneyBRDFInputValues*    values,
            BSDFSample&                     sample) const
        {
            // Compute the incoming direction in local space.
            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();
            const Vector3d wi = sample_hemisphere_uniform(s);

            // Transform the incoming direction to parent space.
            const Vector3d incoming = sample.get_shading_basis().transform_to_parent(wi);
            sample.set_probability(
                evaluate(
                    values,
                    sample.get_shading_basis(),
                    sample.get_outgoing_vector(),
                    incoming,
                    sample.value()));

            assert(sample.get_probability() > 0.0);
            sample.set_mode(BSDFSample::Diffuse);
            sample.set_incoming(incoming);
        }

        double evaluate(
            const DisneyBRDFInputValues*    values,
            const Basis3d&                  shading_basis,
            const Vector3d&                 outgoing,
            const Vector3d&                 incoming,
            Spectrum&                       value) const
        {
            // This code is mostly ported from the GLSL implementation
            // in Disney's BRDF explorer.

            const Vector3d h(normalize(incoming + outgoing));
            const double cos_ih = dot(incoming, h);

            mix_spectra(
                Color3f(1.0f),
                values->m_tint_color,
                static_cast<float>(values->m_sheen_tint),
                value);
            const double fh = schlick_fresnel(cos_ih);
            value *= static_cast<float>(fh * values->m_sheen * (1.0 - values->m_metallic));

            // Return the probability density of the sampled direction.
            return evaluate_pdf(shading_basis, incoming);
        }

        double evaluate_pdf(
            const Basis3d&                  shading_basis,
            const Vector3d&                 incoming) const
        {
            return RcpTwoPi;
        }
    };


    //
    // Disney BRDF implementation.
    //

    namespace
    {
        const char* Model = "disney_brdf";
    }

    class DisneyBRDFImpl
      : public BSDF
    {
      public:
        // BRDF components.
        enum
        {
            DiffuseComponent,
            SheenComponent,
            SpecularComponent,
            CleatcoatComponent,
            NumComponents
        };

        DisneyBRDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Reflective, BSDFSample::Diffuse | BSDFSample::Glossy, params)
        {
            m_inputs.declare("base_color", InputFormatSpectralReflectance);
            m_inputs.declare("subsurface", InputFormatScalar, "0.0");
            m_inputs.declare("metallic", InputFormatScalar, "0.0");
            m_inputs.declare("specular", InputFormatScalar, "0.5");
            m_inputs.declare("specular_tint", InputFormatScalar, "0.0");
            m_inputs.declare("anisotropic", InputFormatScalar, "0.0");
            m_inputs.declare("roughness", InputFormatScalar, "0.5");
            m_inputs.declare("sheen", InputFormatScalar, "0.0");
            m_inputs.declare("sheen_tint", InputFormatScalar, "0.5");
            m_inputs.declare("clearcoat", InputFormatScalar, "0.0");
            m_inputs.declare("clearcoat_gloss", InputFormatScalar, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset) const APPLESEED_OVERRIDE
        {
            BSDF::evaluate_inputs(
                shading_context,
                input_evaluator,
                shading_point,
                offset);

            char* ptr = reinterpret_cast<char*>(input_evaluator.data());
            DisneyBRDFInputValues* values = reinterpret_cast<DisneyBRDFInputValues*>(ptr + offset);
            values->precompute_tint_color();
        }

        virtual void sample(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const DisneyBRDFInputValues* values =
                reinterpret_cast<const DisneyBRDFInputValues*>(data);

            double cdf[NumComponents];
            compute_component_cdf(values, cdf);

            // Choose which of the components to sample.
            sample.get_sampling_context().split_in_place(1, 1);
            const double s = sample.get_sampling_context().next_double2();

            if (s < cdf[DiffuseComponent])
                DisneyDiffuseComponent().sample(values, sample);
            else if (s < cdf[SheenComponent])
                DisneySheenComponent().sample(values, sample);
            else if (s < cdf[SpecularComponent])
            {
                double alpha_x, alpha_y;
                specular_roughness(values, alpha_x, alpha_y);
                const GGXMDF<double> ggx_mdf;
                MicrofacetBRDFHelper<double>::sample(
                    ggx_mdf,
                    alpha_x,
                    alpha_y,
                    alpha_x,
                    alpha_y,
                    DisneySpecularFresnelFun(*values),
                    sample);
            }
            else //if (s < cdf[CleatcoatComponent])
            {
                const double alpha = clearcoat_roughness(values);
                const BerryMDF<double> berry_mdf;
                MicrofacetBRDFHelper<double>::sample(
                    berry_mdf,
                    alpha,
                    alpha,
                    0.25,
                    0.25,
                    DisneyClearcoatFresnelFun(*values),
                    sample);
            }
        }

        virtual double evaluate(
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
            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const DisneyBRDFInputValues* values =
                reinterpret_cast<const DisneyBRDFInputValues*>(data);

            double weights[NumComponents];
            compute_component_weights(values, weights);

            value.set(0.0f);
            double pdf = 0.0;

            if (modes & BSDFSample::Diffuse)
            {
                if (weights[DiffuseComponent] != 0.0)
                {
                    pdf += DisneyDiffuseComponent().evaluate(
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        value) * weights[DiffuseComponent];
                }

                if (weights[SheenComponent] != 0.0)
                {
                    Spectrum sheen;
                    pdf += DisneySheenComponent().evaluate(
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        sheen) * weights[SheenComponent];
                    value += sheen;
                }
            }

            if (modes & BSDFSample::Glossy)
            {
                if (weights[SpecularComponent] != 0.0)
                {
                    Spectrum spec;
                    double alpha_x, alpha_y;
                    specular_roughness(values, alpha_x, alpha_y);
                    const GGXMDF<double> ggx_mdf;
                    pdf += MicrofacetBRDFHelper<double>::evaluate(
                        ggx_mdf,
                        alpha_x,
                        alpha_y,
                        alpha_x,
                        alpha_y,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        DisneySpecularFresnelFun(*values),
                        spec) * weights[SpecularComponent];
                    value += spec;
                }

                if (weights[CleatcoatComponent] != 0.0)
                {
                    Spectrum clear;
                    const double alpha = clearcoat_roughness(values);
                    const BerryMDF<double> berry_mdf;
                    pdf += MicrofacetBRDFHelper<double>::evaluate(
                        berry_mdf,
                        alpha,
                        alpha,
                        0.25,
                        0.25,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        DisneyClearcoatFresnelFun(*values),
                        clear) * weights[CleatcoatComponent];
                    value += clear;
                }
            }

            return pdf;
        }

        virtual double evaluate_pdf(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            const DisneyBRDFInputValues* values =
                reinterpret_cast<const DisneyBRDFInputValues*>(data);

            double weights[NumComponents];
            compute_component_weights(values, weights);

            double pdf = 0.0;

            if (modes & BSDFSample::Diffuse)
            {
                if (weights[DiffuseComponent] != 0.0)
                {
                    pdf += DisneyDiffuseComponent().evaluate_pdf(
                        shading_basis,
                        incoming) * weights[DiffuseComponent];
                }

                if (weights[SheenComponent] != 0.0)
                {
                    pdf += DisneySheenComponent().evaluate_pdf(
                        shading_basis,
                        incoming) * weights[SheenComponent];
                }
            }

            if (modes & BSDFSample::Glossy)
            {
                if (weights[SpecularComponent] != 0.0)
                {
                    double alpha_x, alpha_y;
                    specular_roughness(values, alpha_x, alpha_y);
                    const GGXMDF<double> ggx_mdf;
                    pdf += MicrofacetBRDFHelper<double>::pdf(
                        ggx_mdf,
                        alpha_x,
                        alpha_y,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes) * weights[SpecularComponent];
                }

                if (weights[CleatcoatComponent] != 0.0)
                {
                    const double alpha = clearcoat_roughness(values);
                    const BerryMDF<double> berry_mdf;
                    pdf += MicrofacetBRDFHelper<double>::pdf(
                        berry_mdf,
                        alpha,
                        alpha,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes) * weights[CleatcoatComponent];
                }
            }

            return pdf;
        }

      private:
        typedef DisneyBRDFInputValues InputValues;

        void compute_component_weights(
            const DisneyBRDFInputValues*    values,
            double                          weights[NumComponents]) const
        {
            weights[DiffuseComponent] =
                lerp(values->m_base_color_luminance, 0.0, values->m_metallic);
            weights[SheenComponent] =
                lerp(values->m_sheen, 0.0, values->m_metallic);
            weights[SpecularComponent] =
                lerp(values->m_specular, 1.0, values->m_metallic);
            weights[CleatcoatComponent] =
                values->m_clearcoat * 0.25;

            const double total_weight =
                weights[DiffuseComponent] +
                weights[SheenComponent] +
                weights[SpecularComponent] +
                weights[CleatcoatComponent];

            if (total_weight == 0.0)
                return;

            const double total_weight_rcp = 1.0 / total_weight;
            weights[DiffuseComponent]   *= total_weight_rcp;
            weights[SheenComponent]     *= total_weight_rcp;
            weights[SpecularComponent]  *= total_weight_rcp;
            weights[CleatcoatComponent] *= total_weight_rcp;
        }

        void compute_component_cdf(
            const DisneyBRDFInputValues*    values,
            double                          cdf[NumComponents]) const
        {
            compute_component_weights(values, cdf);
            cdf[SheenComponent]     += cdf[DiffuseComponent];
            cdf[SpecularComponent]  += cdf[SheenComponent];
            cdf[CleatcoatComponent] += cdf[SpecularComponent];
        }

        void specular_roughness(
            const DisneyBRDFInputValues*    values,
            double&                         alpha_x,
            double&                         alpha_y) const
        {
            const double aspect = sqrt(1.0 - values->m_anisotropic * 0.9);
            alpha_x = max(0.001, square(values->m_roughness) / aspect);
            alpha_y = max(0.001, square(values->m_roughness) * aspect);
        }

        double clearcoat_roughness(const DisneyBRDFInputValues* values) const
        {
            return mix(0.1, 0.001, values->m_clearcoat_gloss);
        }
    };

    typedef BSDFWrapper<DisneyBRDFImpl> DisneyBRDF;
}

void DisneyBRDFInputValues::precompute_tint_color()
{
    const Color3f tint_xyz =
        m_base_color.is_rgb()
            ? linear_rgb_to_ciexyz(m_base_color.rgb())
            : spectrum_to_ciexyz<float>(g_std_lighting_conditions, m_base_color);

    m_tint_color =
        tint_xyz[1] > 0.0f
            ? ciexyz_to_linear_rgb(tint_xyz / tint_xyz[1])
            : Color3f(1.0f);

    m_base_color_luminance = static_cast<double>(tint_xyz[1]);
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
            .insert("default", "0.5"));

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
            .insert("default", "0.5"));

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
            .insert("default", "0.5"));

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
            .insert("default", "0.5"));

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

}   // namespace renderer
