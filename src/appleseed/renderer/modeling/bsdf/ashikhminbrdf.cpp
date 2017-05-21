
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "ashikhminbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fp.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
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
    //
    // Ashikhmin-Shirley BRDF.
    //
    // References:
    //
    //   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.4558&rep=rep1&type=pdf
    //   http://jesper.kalliope.org/blog/library/dbrdfs.pdf
    //

    const char* Model = "ashikhmin_brdf";

    class AshikhminBRDFImpl
      : public BSDF
    {
      public:
        AshikhminBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, ScatteringMode::Diffuse | ScatteringMode::Glossy, params)
        {
            m_inputs.declare("diffuse_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("diffuse_reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("glossy_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("glossy_reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("shininess_u", InputFormatFloat);
            m_inputs.declare("shininess_v", InputFormatFloat);
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const int           modes,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_diffuse_or_glossy(modes))
                return;

            // No reflection below the shading surface.
            const float cos_on = dot(sample.m_outgoing.get_value(), sample.m_shading_basis.get_normal());
            if (cos_on < 0.0f)
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute reflectance-related values.
            RVal rval;
            if (!compute_rval(rval, values))
                return;

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            // Generate a uniform sample in [0,1)^3.
            sampling_context.split_in_place(3, 1);
            const Vector3f s = sampling_context.next2<Vector3f>();

            ScatteringMode::Mode mode;
            Vector3f h, incoming;
            float exp;

            // Select a component and sample it to compute the incoming direction.
            if (ScatteringMode::has_diffuse(modes) &&
                (!ScatteringMode::has_glossy(modes) || s[2] < rval.m_pd))
            {
                mode = ScatteringMode::Diffuse;

                // Compute the incoming direction in local space.
                const Vector3f wi = sample_hemisphere_cosine(Vector2f(s[0], s[1]));

                // Transform the incoming direction to parent space.
                incoming = sample.m_shading_basis.transform_to_parent(wi);

                // Compute the halfway vector in world space.
                h = normalize(incoming + sample.m_outgoing.get_value());

                // Compute the glossy exponent, needed to evaluate the PDF.
                const float cos_hn = dot(h, sample.m_shading_basis.get_normal());
                const float cos_hu = dot(h, sample.m_shading_basis.get_tangent_u());
                const float cos_hv = dot(h, sample.m_shading_basis.get_tangent_v());
                const float exp_den = 1.0f - cos_hn * cos_hn;
                const float exp_u = values->m_nu * cos_hu * cos_hu;
                const float exp_v = values->m_nv * cos_hv * cos_hv;
                exp = exp_den == 0.0f ? FP<float>::pos_inf() : (exp_u + exp_v) / exp_den;
            }
            else
            {
                mode = ScatteringMode::Glossy;

                float cos_phi, sin_phi;

                if (sval.m_isotropic)
                {
                    const float phi = s[0] * TwoPi<float>();

                    cos_phi = cos(phi);
                    sin_phi = sin(phi);

                    exp = values->m_nu;
                }
                else
                {
                    const float phi = sample_anisotropic_glossy(sval.m_k, s[0]);

                    cos_phi = cos(phi);
                    sin_phi = sin(phi);

                    const float exp_u = values->m_nu * cos_phi * cos_phi;
                    const float exp_v = values->m_nv * sin_phi * sin_phi;

                    exp = exp_u + exp_v;
                }

                const float cos_theta = pow(1.0f - s[1], 1.0f / (exp + 1.0f));
                const float sin_theta = sqrt(1.0f - cos_theta * cos_theta);

                // Compute the halfway vector in world space.
                h = sample.m_shading_basis.transform_to_parent(
                    Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi));

                // Compute the incoming direction in world space.
                incoming =
                    force_above_surface(
                        reflect(sample.m_outgoing.get_value(), h), sample.m_geometric_normal);
            }

            // No reflection below the shading surface.
            const float cos_in = dot(incoming, sample.m_shading_basis.get_normal());
            if (cos_in < 0.0f)
                return;

            // Compute dot products.
            const float cos_oh = min(abs(dot(sample.m_outgoing.get_value(), h)), 1.0f);
            const float cos_hn = dot(h, sample.m_shading_basis.get_normal());

            float pdf_diffuse = 0.0f, pdf_glossy = 0.0f;
            sample.m_value.set(0.0f);

            if (ScatteringMode::has_diffuse(modes))
            {
                // Evaluate the diffuse component of the BRDF (equation 5).
                const float a = 1.0f - pow5(1.0f - 0.5f * cos_in);
                const float b = 1.0f - pow5(1.0f - 0.5f * cos_on);
                madd(sample.m_value, rval.m_kd, a * b);

                // Evaluate the PDF of the diffuse component.
                pdf_diffuse = cos_in * RcpPi<float>();
                assert(pdf_diffuse > 0.0f);
            }

            if (ScatteringMode::has_glossy(modes))
            {
                // Evaluate the glossy component of the BRDF (equation 4).
                const float num = sval.m_kg * pow(cos_hn, exp);
                const float den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
                Spectrum glossy;
                fresnel_reflectance_dielectric_schlick(glossy, rval.m_scaled_rg, cos_oh, values->m_fr_multiplier);
                madd(sample.m_value, glossy, num / den);

                // Evaluate the PDF of the glossy component (equation 8).
                pdf_glossy = num / cos_oh;      // omit division by 4 since num = pdf(h) / 4
                assert(pdf_glossy >= 0.0f);
            }

            sample.m_mode = mode;
            sample.m_probability =
                ScatteringMode::has_diffuse_and_glossy(modes) ? rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy :
                ScatteringMode::has_diffuse(modes) ? pdf_diffuse : pdf_glossy;
            sample.m_incoming = Dual3f(incoming);
            sample.compute_reflected_differentials();
        }

        virtual float evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3f&     geometric_normal,
            const Basis3f&      shading_basis,
            const Vector3f&     outgoing,
            const Vector3f&     incoming,
            const int           modes,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3f& shading_normal = shading_basis.get_normal();
            const float cos_in = dot(incoming, shading_normal);
            const float cos_on = dot(outgoing, shading_normal);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute reflectance-related values.
            RVal rval;
            if (!compute_rval(rval, values))
                return 0.0f;

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            // Compute the halfway vector in world space.
            const Vector3f h = normalize(incoming + outgoing);

            // Compute dot products.
            const float cos_oh = dot(outgoing, h);
            const float cos_hn = dot(h, shading_normal);
            const float cos_hu = dot(h, shading_basis.get_tangent_u());
            const float cos_hv = dot(h, shading_basis.get_tangent_v());

            float pdf_diffuse = 0.0f, pdf_glossy = 0.0f;
            value.set(0.0f);

            if (ScatteringMode::has_diffuse(modes))
            {
                // Evaluate the diffuse component of the BRDF (equation 5).
                const float a = 1.0f - pow5(1.0f - 0.5f * cos_in);
                const float b = 1.0f - pow5(1.0f - 0.5f * cos_on);
                madd(value, rval.m_kd, a * b);

                // Evaluate the PDF of the diffuse component.
                pdf_diffuse = cos_in * RcpPi<float>();
                assert(pdf_diffuse >= 0.0f);
            }

            if (ScatteringMode::has_glossy(modes))
            {
                // Evaluate the glossy component of the BRDF (equation 4).
                const float exp_num_u = values->m_nu * cos_hu * cos_hu;
                const float exp_num_v = values->m_nv * cos_hv * cos_hv;
                const float exp_den = 1.0f - cos_hn * cos_hn;
                const float exp = (exp_num_u + exp_num_v) / exp_den;
                const float num = exp_den == 0.0f ? 0.0f : sval.m_kg * pow(cos_hn, exp);
                const float den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
                Spectrum glossy;
                fresnel_reflectance_dielectric_schlick(glossy, rval.m_scaled_rg, cos_oh, values->m_fr_multiplier);
                madd(value, glossy, num / den);

                // Evaluate the PDF of the glossy component (equation 8).
                pdf_glossy = num / cos_oh;      // omit division by 4 since num = pdf(h) / 4
                assert(pdf_glossy >= 0.0f);
            }

            return
                ScatteringMode::has_diffuse_and_glossy(modes) ? rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy :
                ScatteringMode::has_diffuse(modes) ? pdf_diffuse :
                ScatteringMode::has_glossy(modes) ? pdf_glossy : 0.0f;
        }

        virtual float evaluate_pdf(
            const void*         data,
            const Vector3f&     geometric_normal,
            const Basis3f&      shading_basis,
            const Vector3f&     outgoing,
            const Vector3f&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3f& shading_normal = shading_basis.get_normal();
            const float cos_in = dot(incoming, shading_normal);
            const float cos_on = dot(outgoing, shading_normal);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute (or retrieve precomputed) reflectance-related values.
            RVal rval;
            if (!compute_rval(rval, values))
                return 0.0f;

            // Compute shininess-related values.
            SVal sval;
            compute_sval(sval, values->m_nu, values->m_nv);

            // Compute the halfway vector in world space.
            const Vector3f h = normalize(incoming + outgoing);

            // Compute dot products.
            const float cos_oh = dot(outgoing, h);
            const float cos_hn = dot(h, shading_normal);
            const float cos_hu = dot(h, shading_basis.get_tangent_u());
            const float cos_hv = dot(h, shading_basis.get_tangent_v());

            float pdf_diffuse = 0.0f, pdf_glossy = 0.0f;

            if (ScatteringMode::has_diffuse(modes))
            {
                // Evaluate the PDF of the diffuse component.
                pdf_diffuse = cos_in * RcpPi<float>();
                assert(pdf_diffuse >= 0.0f);
            }

            if (ScatteringMode::has_glossy(modes))
            {
                // Evaluate the PDF for the halfway vector (equation 6).
                const float exp_num_u = values->m_nu * cos_hu * cos_hu;
                const float exp_num_v = values->m_nv * cos_hv * cos_hv;
                const float exp_den = 1.0f - cos_hn * cos_hn;
                const float exp = (exp_num_u + exp_num_v) / exp_den;
                const float num = exp_den == 0.0f ? 0.0f : sval.m_kg * pow(cos_hn, exp);

                // Evaluate the PDF of the glossy component (equation 8).
                pdf_glossy = num / cos_oh;      // omit division by 4 since num = pdf(h) / 4
                assert(pdf_glossy >= 0.0f);
            }

            return
                ScatteringMode::has_diffuse_and_glossy(modes) ? rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy :
                ScatteringMode::has_diffuse(modes) ? pdf_diffuse :
                ScatteringMode::has_glossy(modes) ? pdf_glossy : 0.0f;
        }

      private:
        typedef AshikhminBRDFInputValues InputValues;

        // Precomputed reflectance-related values.
        struct RVal
        {
            Spectrum    m_kd;               // constant factor of diffuse component
            Spectrum    m_scaled_rg;        // glossy reflectance scaled by multiplier
            float       m_pd;               // probability of diffuse component
            float       m_pg;               // probability of glossy component
        };

        // Precomputed shininess-related values.
        struct SVal
        {
            float       m_kg;               // constant factor of glossy component
            float       m_k;                // constant factor needed during hemisphere (anisotropic case only)
            bool        m_isotropic;        // true if the U and V shininess values are the same
        };

        template <typename T>
        static T pow5(const T x)
        {
            const T x2 = x * x;
            return x2 * x2 * x;
        }

        static bool compute_rval(RVal& rval, const InputValues* values)
        {
            // Scale and clamp the diffuse reflectance.
            Spectrum scaled_rd = values->m_rd;
            scaled_rd *= values->m_rd_multiplier;
            saturate_in_place(scaled_rd);

            // Scale and clamp the glossy reflectance.
            rval.m_scaled_rg = values->m_rg;
            rval.m_scaled_rg *= values->m_rg_multiplier;
            saturate_in_place(rval.m_scaled_rg);

            // Compute average diffuse and glossy reflectances.
            const float rd_avg = average_value(scaled_rd);
            const float rg_avg = average_value(rval.m_scaled_rg);
            const float sum = rd_avg + rg_avg;
            if (sum == 0.0f)
                return false;

            // Compute probabilities of glossy and diffuse components.
            rval.m_pd = rd_avg / sum;
            rval.m_pg = 1.0f - rval.m_pd;
            assert(feq(rval.m_pd + rval.m_pg, 1.0f));

            // Precompute constant factor of diffuse component (equation 5).
            rval.m_kd.set(1.0f);
            rval.m_kd -= rval.m_scaled_rg;
            rval.m_kd *= scaled_rd;
            rval.m_kd *= 28.0f / (23.0f * Pi<float>());
            assert(min_value(rval.m_kd) >= 0.0f);

            return true;
        }

        static void compute_sval(SVal& sval, const float nu, const float nv)
        {
            // Check for isotropicity.
            sval.m_isotropic = feq(nu, nv, 1.0e-6f);

            // Precompute constant factor of glossy component (equations 4 and 6).
            sval.m_kg = sqrt((nu + 1.0f) * (nv + 1.0f)) / (8.0f * Pi<float>());

            if (!sval.m_isotropic)
            {
                // Precompute constant factor needed during hemisphere sampling.
                sval.m_k = sqrt((nu + 1.0f) / (nv + 1.0f));
            }
            else
                sval.m_k = 0.0f;
        }

        static float sample_anisotropic_glossy(const float k, const float s)
        {
            if (s < 0.25f)
            {
                // First quadrant.
                const float b = tan(HalfPi<float>() * (4.0f * s));
                return atan(k * b);
            }
            else if (s < 0.5f)
            {
                // Second quadrant.
                const float b = tan(HalfPi<float>() * (4.0f * s - 1.0f));
                return atan(k * b) + HalfPi<float>();
            }
            else if (s < 0.75f)
            {
                // Third quadrant.
                const float b = tan(HalfPi<float>() * (4.0f * s - 2.0f));
                return atan(k * b) + Pi<float>();
            }
            else
            {
                // Fourth quadrant.
                const float b = tan(HalfPi<float>() * (4.0f * s - 3.0f));
                return atan(k * b) + Pi<float>() + HalfPi<float>();
            }
        }
    };

    typedef BSDFWrapper<AshikhminBRDFImpl> AshikhminBRDF;
}


//
// AshikhminBRDFFactory class implementation.
//

const char* AshikhminBRDFFactory::get_model() const
{
    return Model;
}

Dictionary AshikhminBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Ashikhmin-Shirley BRDF");
}

DictionaryArray AshikhminBRDFFactory::get_input_metadata() const
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
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diffuse_reflectance_multiplier")
            .insert("label", "Diffuse Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "glossy_reflectance")
            .insert("label", "Glossy Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "glossy_reflectance_multiplier")
            .insert("label", "Glossy Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_multiplier")
            .insert("label", "Fresnel Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shininess_u")
            .insert("label", "Shininess U")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "100.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shininess_v")
            .insert("label", "Shininess V")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "100.0"));

    return metadata;
}

auto_release_ptr<BSDF> AshikhminBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new AshikhminBRDF(name, params));
}

auto_release_ptr<BSDF> AshikhminBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new AshikhminBRDF(name, params));
}

}   // namespace renderer
