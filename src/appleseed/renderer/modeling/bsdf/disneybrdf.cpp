
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet2.h"
#include "foundation/math/sampling.h"
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
        return square(m2) * m;
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


    //
    // DisneyDiffuseComponent: diffuse part of Disney BRDF.
    //

    class DisneyDiffuseComponent
    {
      public:
        BSDF::Mode sample(
            SamplingContext&                sampling_context,
            const DisneyBRDFInputValues*    values,
            const Basis3d&                  shading_basis,
            const Vector3d&                 outgoing,
            Vector3d&                       incoming,
            Spectrum&                       value,
            double&                         probability) const
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);
            probability = evaluate(
                values,
                shading_basis,
                outgoing,
                incoming,
                value);

            assert(probability > 0.0);
            return BSDF::Diffuse;
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
    // DisneySheenComponent: sheen part of Disney BRDF.
    //

    class DisneySheenComponent
    {
      public:
        BSDF::Mode sample(
            SamplingContext&                sampling_context,
            const DisneyBRDFInputValues*    values,
            const Basis3d&                  shading_basis,
            const Vector3d&                 outgoing,
            Vector3d&                       incoming,
            Spectrum&                       value,
            double&                         probability) const
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_uniform(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);
            probability = evaluate(
                values,
                shading_basis,
                outgoing,
                incoming,
                value);

            assert(probability > 0.0);
            return BSDF::Diffuse;
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
    // Berry microfacet distribution function.
    // It's used in the clearcoat layer.
    //

    template <typename T>
    class BerryMDF2
      : public MDF<T>
    {
      public:
        typedef boost::mpl::bool_<false> IsAnisotropicType;

        BerryMDF2() {}

      private:
        virtual Vector<T, 3> do_sample(
            const Vector<T, 2>&  s,
            const T              alpha_x,
            const T              alpha_y) const APPLESEED_OVERRIDE
        {
            const T alpha_x_2 = square(alpha_x);
            const T a = T(1.0) - pow(alpha_x_2, T(1.0) - s[0]);
            const T cos_theta = sqrt(a / (T(1.0) - alpha_x_2));
            const T sin_theta  = sqrt(T(1.0) - square(cos_theta));
            T cos_phi, sin_phi;
            this->sample_phi(s[1], cos_phi, sin_phi);
            return Vector<T, 3>::unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);
        }

        virtual T do_eval_D(
            const Vector<T, 3>&  h,
            const T              alpha_x,
            const T              alpha_y) const APPLESEED_OVERRIDE
        {
            const T alpha_x_2 = square(alpha_x);
            const T cos_theta_2 = square(this->cos_theta(h));
            const T a = (alpha_x_2 - T(1.0)) / (T(Pi) * log(alpha_x_2));
            const T b = (T(1.0) / (T(1.0) + (alpha_x_2 - T(1.0)) * cos_theta_2));
            return a * b;
        }

        virtual T do_eval_G(
            const Vector<T, 3>&  incoming,
            const Vector<T, 3>&  outgoing,
            const Vector<T, 3>&  h,
            const T              alpha_x,
            const T              alpha_y) const APPLESEED_OVERRIDE
        {
            return
                GGXSmithMaskingShadowing<T>::G(
                    incoming,
                    outgoing,
                    h,
                    alpha_x,
                    alpha_y);
        }

        virtual T do_eval_pdf(
            const Vector<T, 3>&  h,
            const T              alpha_x,
            const T              alpha_y) const APPLESEED_OVERRIDE
        {
            if (this->cos_theta(h) == T(0.0))
                return T(0.0);

            const T alpha_x_2 = square(alpha_x);
            const T a = (alpha_x_2 - T(1.0)) / (T(Pi) * log(alpha_x_2));
            const T b = (T(1.0) / (T(1.0) + (alpha_x_2 - T(1.0)) * this->cos_theta(h)));
            return a * b;
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
          : BSDF(name, Reflective, Diffuse | Glossy, params)
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

        virtual Mode sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            Vector3d&               incoming,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            const DisneyBRDFInputValues* values =
                reinterpret_cast<const DisneyBRDFInputValues*>(data);

            double cdf[NumComponents];
            compute_component_cdf(values, cdf);

            // Choose which of the components to sample.
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next_double2();

            if (s < cdf[DiffuseComponent])
            {
                return
                    DisneyDiffuseComponent().sample(
                        sampling_context,
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        value,
                        probability);
            }

            if (s < cdf[SheenComponent])
            {
                return
                    DisneySheenComponent().sample(
                        sampling_context,
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        value,
                        probability);
            }

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_on < 0.0)
                return Absorption;

            const MDF<double>* mdf = 0;
            double alpha_x, alpha_y, alpha_gx, alpha_gy;

            if (s < cdf[SpecularComponent])
            {
                mdf = &m_specular_mdf;
                specular_roughness(values, alpha_x, alpha_y);
                alpha_gx = alpha_x;
                alpha_gy = alpha_y;
            }
            else
            {
                mdf = &m_clearcoat_mdf;
                alpha_x = alpha_y = clearcoat_roughness(values);
                alpha_gx = alpha_gy = 0.25;
            }

            // Compute the incoming direction by sampling the MDF.
            sampling_context.split_in_place(2, 1);
            const Vector2d s2 = sampling_context.next_vector2<2>();
            const Vector3d m = mdf->sample(s2, alpha_x, alpha_y);
            const Vector3d h = shading_basis.transform_to_parent(m);
            incoming = reflect(outgoing, h);

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, n);
            if (cos_in < 0.0)
                return Absorption;

            const double D =
                mdf->D(
                    m,
                    alpha_x,
                    alpha_y);

            const double G =
                mdf->G(
                    shading_basis.transform_to_local(incoming),
                    shading_basis.transform_to_local(outgoing),
                    m,
                    alpha_gx,
                    alpha_gy);

            const double cos_oh = dot(outgoing, h);

            if (s < cdf[SpecularComponent])
                specular_f(values, cos_oh, value);
            else
                value.set(static_cast<float>(clearcoat_f(values->m_clearcoat, cos_oh)));

            value *= static_cast<float>((D * G) / (4.0 * cos_on * cos_in));
            probability = mdf->pdf(m, alpha_x, alpha_y) / (4.0 * cos_oh);
            return Glossy;
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

            if (modes & Diffuse)
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

            if (!(modes & Glossy))
                return pdf;

            const Vector3d h = normalize(incoming + outgoing);
            const Vector3d m = shading_basis.transform_to_local(h);
            const Vector3d wi = shading_basis.transform_to_local(incoming);
            const Vector3d wo = shading_basis.transform_to_local(outgoing);
            const double cos_oh = dot(outgoing, h);

            if (weights[SpecularComponent] != 0.0)
            {
                double alpha_x, alpha_y;
                specular_roughness(values, alpha_x, alpha_y);

                const double D =
                    m_specular_mdf.D(
                        m,
                        alpha_x,
                        alpha_y);

                const double G =
                    m_specular_mdf.G(
                        wo,
                        wi,
                        m,
                        alpha_x,
                        alpha_y);

                Spectrum specular_value;
                specular_f(values, cos_oh, specular_value);
                specular_value *= static_cast<float>(D * G / (4.0 * cos_on * cos_in));
                value += specular_value;

                pdf += m_specular_mdf.pdf(m, alpha_x, alpha_y)  / (4.0 * cos_oh) * weights[SpecularComponent];
            }

            if (weights[CleatcoatComponent] != 0.0)
            {
                const double alpha = clearcoat_roughness(values);

                const double D =
                    m_clearcoat_mdf.D(
                        m,
                        alpha,
                        alpha);

                const double G =
                    m_clearcoat_mdf.G(
                        wo,
                        wi,
                        m,
                        0.25,
                        0.25);

                const double F = clearcoat_f(values->m_clearcoat, cos_oh);

                Spectrum clearcoat_value;
                clearcoat_value.set(static_cast<float>(D * G * F / (4.0 * cos_on * cos_in)));
                value += clearcoat_value;

                pdf += m_clearcoat_mdf.pdf(m, alpha, alpha)  / (4.0 * cos_oh) * weights[CleatcoatComponent];
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

            if (modes & Diffuse)
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

            if (!(modes & Glossy))
                return pdf;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_in < 0.0 || cos_on < 0.0)
                return pdf;

            const Vector3d h = normalize(incoming + outgoing);
            const Vector3d hl = shading_basis.transform_to_local(h);
            const double cos_oh = dot(outgoing, h);

            if (weights[SpecularComponent] != 0.0)
            {
                double alpha_x, alpha_y;
                specular_roughness(values, alpha_x, alpha_y);
                pdf += m_specular_mdf.pdf(hl, alpha_x, alpha_y) / (4.0 * cos_oh) * weights[SpecularComponent];
            }

            if (weights[CleatcoatComponent] != 0.0)
            {
                const double alpha = clearcoat_roughness(values);
                pdf += m_specular_mdf.pdf(hl, alpha, alpha) / (4.0 * cos_oh) * weights[CleatcoatComponent];
            }

            return pdf;
        }

      private:
        typedef DisneyBRDFInputValues InputValues;

        const GGXMDF2<double>   m_specular_mdf;
        const BerryMDF2<double> m_clearcoat_mdf;

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

            const double total_weight_rcp = 1.0 / total_weight;
            weights[DiffuseComponent] *= total_weight_rcp;
            weights[SheenComponent] *= total_weight_rcp;
            weights[SpecularComponent] *= total_weight_rcp;
            weights[CleatcoatComponent] *= total_weight_rcp;
        }

        void compute_component_cdf(
            const DisneyBRDFInputValues*    values,
            double                          cdf[NumComponents]) const
        {
            compute_component_weights(values, cdf);
            cdf[SheenComponent] += cdf[DiffuseComponent];
            cdf[SpecularComponent] += cdf[SheenComponent];
            cdf[CleatcoatComponent] = 1.0;
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

        void specular_f(
            const DisneyBRDFInputValues*    values,
            const double                    cos_oh,
            Spectrum&                       f) const
        {
            mix_spectra(Color3f(1.0f), values->m_tint_color, static_cast<float>(values->m_specular_tint), f);
            f *= static_cast<float>(values->m_specular * 0.08);
            mix_spectra(f, values->m_base_color, static_cast<float>(values->m_metallic), f);
            mix_spectra(f, Color3f(1.0f), static_cast<float>(schlick_fresnel(cos_oh)), f);
        }

        double clearcoat_roughness(const DisneyBRDFInputValues* values) const
        {
            return mix(0.1, 0.001, values->m_clearcoat_gloss);
        }

        double clearcoat_f(const double clearcoat, const double cos_oh) const
        {
            return mix(0.04, 1.0, schlick_fresnel(cos_oh)) * 0.25 * clearcoat;
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

const char* DisneyBRDFFactory::get_human_readable_model() const
{
    return "Disney BRDF";
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
