
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/uniforminputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }

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
          : BSDF(name, params, Reflective)
          , m_uniform_reflectance(false)
          , m_uniform_shininess(false)
        {
            m_inputs.declare("diffuse_reflectance", InputFormatSpectrum);
            m_inputs.declare("glossy_reflectance", InputFormatSpectrum);
            m_inputs.declare("shininess_u", InputFormatScalar);
            m_inputs.declare("shininess_v", InputFormatScalar);
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual void on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) override
        {
            BSDF::on_frame_begin(project, assembly);

            UniformInputEvaluator uniform_input_evaluator;
            const InputValues* uniform_values =
                static_cast<const InputValues*>(uniform_input_evaluator.evaluate(m_inputs));

            m_uniform_reflectance =
                m_inputs.source("diffuse_reflectance")->is_uniform() &&
                m_inputs.source("glossy_reflectance")->is_uniform();

            if (m_uniform_reflectance)
            {
                m_compute_rval_return_value =
                    compute_rval(uniform_values->m_rd, uniform_values->m_rg, m_uniform_rval);
            }

            m_uniform_shininess =
                m_inputs.source("shininess_u")->is_uniform() &&
                m_inputs.source("shininess_v")->is_uniform();
            
            if (m_uniform_shininess)
            {
                compute_sval(uniform_values->m_nu, uniform_values->m_nv, m_uniform_sval);
            }
        }

        FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute (or retrieve precomputed) reflectance-related values.
            RVal rval;
            if (!get_rval(rval, values))
            {
                mode = None;
                return;
            }

            // Compute (or retrieve precomputed) shininess-related values.
            SVal sval;
            get_sval(sval, values);

            // Generate a uniform sample in [0,1)^3.
            sampling_context.split_in_place(3, 1);
            const Vector3d s = sampling_context.next_vector2<3>();

            Vector3d h;
            double exp;

            // Select a component and sample it to compute the incoming direction.
            if (s[2] < rval.m_pd)
            {
                mode = Diffuse;

                // Compute the incoming direction in local space.
                const Vector3d wi = sample_hemisphere_cosine(Vector2d(s[0], s[1]));

                // Transform the incoming direction to parent space.
                incoming = shading_basis.transform_to_parent(wi);

                // No reflection in or below the geometric surface.
                const double cos_ig = dot(incoming, geometric_normal);
                if (cos_ig <= 0.0)
                {
                    mode = None;
                    return;
                }

                // Compute the halfway vector in world space.
                h = normalize(incoming + outgoing);

                // Compute the glossy exponent, needed to evaluate the PDF.
                const double cos_hn = dot(h, shading_basis.get_normal());
                const double cos_hu = dot(h, shading_basis.get_tangent_u());
                const double cos_hv = dot(h, shading_basis.get_tangent_v());
                const double exp_den = 1.0 - cos_hn * cos_hn;
                const double exp_u = values->m_nu * cos_hu * cos_hu;
                const double exp_v = values->m_nv * cos_hv * cos_hv;
                exp = (exp_u + exp_v) / exp_den;
            }
            else
            {
                mode = Glossy;

                double cos_phi, sin_phi;

                if (sval.m_isotropic)
                {
                    const double phi = s[0] * TwoPi;

                    cos_phi = cos(phi);
                    sin_phi = sin(phi);

                    exp = values->m_nu;
                }
                else
                {
                    double phi;

                    if (s[0] < 0.25)
                    {
                        // First quadrant.
                        const double b = tan(HalfPi * (4.0 * s[0]));
                        phi = atan(sval.m_k * b);
                    }
                    else if (s[0] < 0.5)
                    {
                        // Second quadrant.
                        const double b = tan(HalfPi * (4.0 * s[0] - 1.0));
                        phi = atan(sval.m_k * b) + HalfPi;
                    }
                    else if (s[0] < 0.75)
                    {
                        // Third quadrant.
                        const double b = tan(HalfPi * (4.0 * s[0] - 2.0));
                        phi = atan(sval.m_k * b) + Pi;
                    }
                    else
                    {
                        // Fourth quadrant.
                        const double b = tan(HalfPi * (4.0 * s[0] - 3.0));
                        phi = atan(sval.m_k * b) + Pi + HalfPi;
                    }

                    cos_phi = cos(phi);
                    sin_phi = sin(phi);

                    const double exp_u = values->m_nu * cos_phi * cos_phi;
                    const double exp_v = values->m_nv * sin_phi * sin_phi;

                    exp = exp_u + exp_v;
                }

                const double cos_theta = pow(1.0 - s[1], 1.0 / (exp + 1.0));
                const double sin_theta = sqrt(1.0 - cos_theta * cos_theta);

                // Compute the halfway vector in world space.
                h = shading_basis.transform_to_parent(
                        Vector3d::unit_vector(
                            cos_theta,
                            sin_theta,
                            cos_phi,
                            sin_phi));

                // Compute the incoming direction in world space.
                incoming = reflect(outgoing, h);

                // Force the incoming direction to be above the geometric surface.
                incoming = force_above_surface(incoming, geometric_normal);
            }

            const Vector3d& shading_normal = shading_basis.get_normal();

            // No reflection in or below the shading surface.
            const double cos_in = dot(incoming, shading_normal);
            if (cos_in <= 0.0)
            {
                mode = None;
                return;
            }

            // Compute dot products.
            const double cos_on = abs(dot(outgoing, shading_normal));
            const double cos_oh = abs(dot(outgoing, h));
            const double cos_hn = abs(dot(h, shading_normal));

            // Evaluate the glossy component of the BRDF (equation 4).
            const double num = pow(cos_hn, exp);
            const double den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
            value = schlick_fresnel_reflection(values->m_rg, cos_oh);
            value *= static_cast<float>(sval.m_kg * num / den);

            // Evaluate the diffuse component of the BRDF (equation 5).
            const double a = 1.0 - pow5(1.0 - 0.5 * cos_in);
            const double b = 1.0 - pow5(1.0 - 0.5 * cos_on);
            value += rval.m_kd * static_cast<float>(a * b);

            // Evaluate the PDF of the diffuse component.
            const double pdf_diffuse = cos_in * RcpPi;
            assert(pdf_diffuse > 0.0);

            // Evaluate the PDF of the glossy component (equation 8).
            const double pdf_h = sval.m_kg * num;
            const double pdf_glossy = pdf_h / cos_oh;
            assert(pdf_glossy >= 0.0);

            // Evaluate the final PDF.
            probability = rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy;
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute (or retrieve precomputed) reflectance-related values.
            RVal rval;
            if (!get_rval(rval, values))
                return 0.0;

            // Compute (or retrieve precomputed) shininess-related values.
            SVal sval;
            get_sval(sval, values);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);

            // Compute dot products.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = abs(dot(incoming, shading_normal));
            const double cos_on = abs(dot(outgoing, shading_normal));
            const double cos_oh = abs(dot(outgoing, h));
            const double cos_hn = abs(dot(h, shading_normal));
            const double cos_hu = dot(h, shading_basis.get_tangent_u());
            const double cos_hv = dot(h, shading_basis.get_tangent_v());

            // Evaluate the glossy component of the BRDF (equation 4).
            const double exp_num_u = values->m_nu * cos_hu * cos_hu;
            const double exp_num_v = values->m_nv * cos_hv * cos_hv;
            const double exp_den = 1.0 - cos_hn * cos_hn;
            const double exp = (exp_num_u + exp_num_v) / exp_den;
            const double num = sval.m_kg * pow(cos_hn, exp);
            const double den = cos_oh * (cos_in + cos_on - cos_in * cos_on);
            Spectrum glossy = schlick_fresnel_reflection(values->m_rg, cos_oh);
            glossy *= static_cast<float>(num / den);

            // Evaluate the diffuse component of the BRDF (equation 5).
            const double a = 1.0 - pow5(1.0 - 0.5 * cos_in);
            const double b = 1.0 - pow5(1.0 - 0.5 * cos_on);
            Spectrum diffuse = rval.m_kd;
            diffuse *= static_cast<float>(a * b);

            // Return the sum of the glossy and diffuse components.
            value = glossy;
            value += diffuse;

            // Evaluate the PDF of the glossy component (equation 8).
            const double pdf_glossy = num / cos_oh;
            assert(pdf_glossy >= 0.0);

            // Evaluate the PDF of the diffuse component.
            const double pdf_diffuse = cos_in * RcpPi;
            assert(pdf_diffuse >= 0.0);

            // Evaluate the final PDF. Note that the probability might be zero,
            // e.g. if m_pd is zero (the BSDF has no diffuse component) and
            // pdf_glossy is also zero (because of numerical imprecision: the
            // value of pdf_glossy depends on the value of pdf_h, which might
            // end up being zero if cos_hn is small and exp is very high).
            return rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute (or retrieve precomputed) reflectance-related values.
            RVal rval;
            if (!get_rval(rval, values))
                return 0.0;

            // Compute (or retrieve precomputed) shininess-related values.
            SVal sval;
            get_sval(sval, values);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);

            // Compute dot products.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = abs(dot(incoming, shading_normal));
            const double cos_on = abs(dot(outgoing, shading_normal));
            const double cos_oh = abs(dot(outgoing, h));
            const double cos_hn = abs(dot(h, shading_normal));
            const double cos_hu = dot(h, shading_basis.get_tangent_u());
            const double cos_hv = dot(h, shading_basis.get_tangent_v());

            // Evaluate the PDF for the halfway vector (equation 6).
            const double exp_num_u = values->m_nu * cos_hu * cos_hu;
            const double exp_num_v = values->m_nv * cos_hv * cos_hv;
            const double exp_den = 1.0 - cos_hn * cos_hn;
            const double exp = (exp_num_u + exp_num_v) / exp_den;
            const double pdf_h = sval.m_kg * pow(cos_hn, exp);

            // Evaluate the PDF of the glossy component (equation 8).
            const double pdf_glossy = pdf_h / cos_oh;
            assert(pdf_glossy >= 0.0);

            // Evaluate the PDF of the diffuse component.
            const double pdf_diffuse = cos_in * RcpPi;
            assert(pdf_diffuse >= 0.0);

            // Evaluate the final PDF. Note that the probability might be zero,
            // e.g. if m_pd is zero (the BSDF has no diffuse component) and
            // pdf_glossy is also zero (because of numerical imprecision: the
            // value of pdf_glossy depends on the value of pdf_h, which might
            // end up being zero if cos_hn is small and exp is very high).
            return rval.m_pd * pdf_diffuse + rval.m_pg * pdf_glossy;
        }

      private:
        struct InputValues
        {
            Spectrum    m_rd;           // diffuse reflectance of the substrate
            Alpha       m_rd_alpha;     // alpha channel of diffuse reflectance
            Spectrum    m_rg;           // glossy reflectance at normal incidence
            Alpha       m_rg_alpha;     // alpha channel of glossy reflectance
            double      m_nu;           // Phong-like exponent in first tangent direction
            double      m_nv;           // Phong-like exponent in second tangent direction
        };

        // Precomputed reflectance-related values.
        struct RVal
        {
            Spectrum    m_kd;
            double      m_pd;
            double      m_pg;
        };
        
        // Precomputed shininess-related values.
        struct SVal
        {
            double      m_kg;
            double      m_k;
            bool        m_isotropic;
        };

        bool            m_uniform_reflectance;
        bool            m_uniform_shininess;
        RVal            m_uniform_rval;
        SVal            m_uniform_sval;
        bool            m_compute_rval_return_value;

        static double pow5(const double x)
        {
            const double x2 = x * x;
            return x2 * x2 * x;
        }

        static bool compute_rval(const Spectrum& rd, const Spectrum& rg, RVal& rval)
        {
            // Compute average diffuse and glossy reflectances.
            const double rd_avg = average_value(rd);
            const double rg_avg = average_value(rg);
            const double sum = rd_avg + rg_avg;
            if (sum == 0.0)
                return false;

            // Compute probabilities of glossy and diffuse components.
            rval.m_pd = rd_avg / sum;
            rval.m_pg = 1.0 - rval.m_pd;
            assert(feq(rval.m_pd + rval.m_pg, 1.0));

            // Precompute constant factor of diffuse component (equation 5).
            rval.m_kd.set(1.0f);
            rval.m_kd -= rg;
            rval.m_kd *= rd;
            rval.m_kd *= static_cast<float>(28.0 / (23.0 * Pi));

            return true;
        }

        static void compute_sval(const double nu, const double nv, SVal& sval)
        {
            // Check for isotropicity.
            sval.m_isotropic = feq(nu, nv, 1.0e-6);

            // Precompute constant factor of glossy component (equations 4 and 6).
            sval.m_kg = sqrt((nu + 1.0) * (nv + 1.0)) / (8.0 * Pi);

            if (!sval.m_isotropic)
            {
                // Precompute constant factor needed during hemisphere sampling.
                sval.m_k = sqrt((nu + 1.0) / (nv + 1.0));
            }
        }

        bool get_rval(RVal& rval, const InputValues* values) const
        {
            if (m_uniform_reflectance)
            {
                rval = m_uniform_rval;
                return m_compute_rval_return_value;
            }
            else return compute_rval(values->m_rd, values->m_rg, rval);
        }

        void get_sval(SVal& sval, const InputValues* values) const
        {
            if (m_uniform_shininess)
                sval = m_uniform_sval;
            else compute_sval(values->m_nu, values->m_nv, sval);
        }
    };

    typedef BRDFWrapper<AshikhminBRDFImpl> AshikhminBRDF;
}


//
// AshikhminBRDFFactory class implementation.
//

const char* AshikhminBRDFFactory::get_model() const
{
    return Model;
}

const char* AshikhminBRDFFactory::get_human_readable_model() const
{
    return "Ashikhmin-Shirley BRDF";
}

DictionaryArray AshikhminBRDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "diffuse_reflectance")
            .insert("label", "Diffuse Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "glossy_reflectance")
            .insert("label", "Glossy Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "shininess_u")
            .insert("label", "Shininess U")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1000.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "shininess_v")
            .insert("label", "Shininess V")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1000.0"));

    return definitions;
}

auto_release_ptr<BSDF> AshikhminBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new AshikhminBRDF(name, params));
}

}   // namespace renderer
