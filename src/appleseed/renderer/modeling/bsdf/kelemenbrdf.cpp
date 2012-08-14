
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
#include "kelemenbrdf.h"

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
#include "foundation/math/microfacet.h"
#include "foundation/math/qmc.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/maplefile.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Kelemen BRDF.
    //
    // Reference:
    //
    //   http://sirkan.iit.bme.hu/~szirmay/scook.pdf
    //

    const char* Model = "kelemen_brdf";

    const size_t AlbedoTableSize = 32;
    const size_t AlbedoSampleCount = 1024;

    class KelemenBRDFImpl
      : public BSDF
    {
      public:
        KelemenBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, params)
        {
            m_inputs.declare("matte_reflectance", InputFormatSpectrum);
            m_inputs.declare("matte_reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("specular_reflectance", InputFormatSpectrum);
            m_inputs.declare("specular_reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("roughness", InputFormatScalar);
        }

        ~KelemenBRDFImpl()
        {
            assert(m_mdf.get() == 0);
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
            const Project&      project,
            const Assembly&     assembly) override
        {
            if (!BSDF::on_frame_begin(project, assembly))
                return false;

            // todo: implement proper error handling.
            assert(m_inputs.source("specular_reflectance")->is_uniform());
            assert(m_inputs.source("specular_reflectance_multiplier")->is_uniform());
            assert(m_inputs.source("roughness")->is_uniform());

            UniformInputEvaluator input_evaluator;
            const InputValues* values =
                static_cast<const InputValues*>(input_evaluator.evaluate(m_inputs));

            // Construct the Microfacet Distribution Function.
            m_mdf.reset(new WardMDF<double>(values->m_roughness));

            // Precompute the specular albedo curve.
            Spectrum rs(values->m_rs);
            rs *= static_cast<float>(values->m_rs_multiplier);
            compute_specular_albedo(*m_mdf.get(), rs, m_a_spec);

            // Precompute the average specular albedo.
            Spectrum a_spec_avg;
            compute_average_specular_albedo(m_a_spec, a_spec_avg);

            // Precompute the normalization constant for the matte component.
            Spectrum s_denom(1.0f);
            s_denom -= a_spec_avg;
            s_denom *= static_cast<float>(Pi);
            m_s.set(1.0f);
            m_s /= s_denom;

            // plot_specular_albedo_curves();

            return true;
        }

        virtual void on_frame_end(
            const Project&      project,
            const Assembly&     assembly)
        {
            m_mdf.reset();

            BSDF::on_frame_end(project, assembly);
        }

        FORCE_INLINE virtual Mode sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            // Define aliases to match the notations in the paper.
            const Vector3d& V = outgoing;
            const Vector3d& N = shading_basis.get_normal();

            // Compute the outgoing angle.
            const double dot_VN = max(dot(V, N), 0.0);

            // Compute the specular albedo for the outgoing angle.
            Spectrum specular_albedo_V;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);

            // Compute the matte albedo.
            Spectrum matte_albedo(1.0f);
            matte_albedo -= specular_albedo_V;
            matte_albedo *= values->m_rm;
            matte_albedo *= static_cast<float>(values->m_rm_multiplier);

            // Compute the probability of a specular bounce.
            const double specular_prob = average_value(specular_albedo_V);

            // Compute the probability of a matte bounce.
            const double matte_prob = average_value(matte_albedo);

            // Generate a uniform sample in [0,1)^3.
            sampling_context.split_in_place(3, 1);
            const Vector3d s = sampling_context.next_vector2<3>();

            Mode mode;
            Vector3d H;
            double dot_LN, dot_HN, dot_HV;

            // Select a component and sample it to compute the incoming direction.
            if (s[2] < matte_prob)
            {
                mode = Diffuse;

                // Compute the incoming direction in local space.
                const Vector3d wi = sample_hemisphere_cosine(Vector2d(s[0], s[1]));

                // Transform the incoming direction to parent space.
                incoming = shading_basis.transform_to_parent(wi);

                // Compute the halfway vector.
                H = normalize(incoming + V);

                dot_LN = wi.y;
                dot_HN = max(dot(H, N), 0.0);
                dot_HV = dot(H, V);
            }
            else if (s[2] < matte_prob + specular_prob)
            {
                mode = Glossy;

                // Sample the microfacet distribution to get an halfway vector H.
                const Vector3d local_H = m_mdf->sample(Vector2d(s[0], s[1]));

                // Transform the halfway vector to parent space.
                H = shading_basis.transform_to_parent(local_H);

                dot_HV = dot(H, V);

                // The incoming direction is the reflection of V around H.
                incoming = (dot_HV + dot_HV) * H - V;

                dot_LN = dot(incoming, N);
                dot_HN = local_H.y;

                // No reflection in or below the shading surface.
                if (dot_LN <= 0.0)
                    return Absorption;
            }
            else
            {
                return Absorption;
            }

            // No reflection in or below the geometric surface.
            const double cos_ig = dot(incoming, geometric_normal);
            if (cos_ig <= 0.0)
                return Absorption;

            // Compute the specular albedo for the incoming angle.
            Spectrum specular_albedo_L;
            evaluate_a_spec(m_a_spec, dot_LN, specular_albedo_L);

            // Specular component (equation 3).
            Spectrum rs(values->m_rs);
            rs *= static_cast<float>(values->m_rs_multiplier);
            Spectrum fr_spec;
            evaluate_fr_spec(*m_mdf.get(), rs, dot_HV, dot_HN, fr_spec);

            // Matte component (last equation of section 2.2).
            value.set(1.0f);
            value -= specular_albedo_L;
            value *= matte_albedo;
            value *= m_s;

            // The final value of the BRDF is the sum of the specular and matte components.
            value += fr_spec;

            // Compute the PDF of the incoming direction for the specular component.
            const double pdf_H = m_mdf->evaluate_pdf(dot_HN);
            const double pdf_specular = pdf_H / (4.0 * dot_HV);
            assert(pdf_specular >= 0.0);

            // Compute the PDF of the incoming direction for the matte component.
            const double pdf_matte = dot_LN * RcpPi;
            assert(pdf_matte >= 0.0);

            // Evaluate the final PDF.
            probability = specular_prob * pdf_specular + matte_prob * pdf_matte;
            assert(probability >= 0.0);

            // Return the scattering mode.
            return mode;
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            value.set(0.0f);
            double probability = 0.0;

            // Define aliases to match the notations in the paper.
            const Vector3d& V = outgoing;
            const Vector3d& L = incoming;
            const Vector3d& N = shading_basis.get_normal();

            const double dot_VN = abs(dot(V, N));
            const double dot_LN = abs(dot(L, N));

            // Compute the halfway vector.
            const Vector3d H = normalize(L + V);
            const double dot_HN = dot(H, N);
            const double dot_HL = dot(H, L);

            // Compute the specular albedos for the outgoing and incoming angles.
            Spectrum specular_albedo_V, specular_albedo_L;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);
            evaluate_a_spec(m_a_spec, dot_LN, specular_albedo_L);

            if (modes & Diffuse)
            {
                // Compute the matte albedo.
                Spectrum matte_albedo(1.0f);
                matte_albedo -= specular_albedo_V;
                matte_albedo *= values->m_rm;
                matte_albedo *= static_cast<float>(values->m_rm_multiplier);

                // Compute the matte component (last equation of section 2.2).
                Spectrum matte_comp(1.0f);
                matte_comp -= specular_albedo_L;
                matte_comp *= matte_albedo;
                matte_comp *= m_s;
                value += matte_comp;

                // Compute the probability of a matte bounce.
                const double matte_prob = average_value(matte_albedo);

                // Compute the PDF of the incoming direction for the matte component.
                const double pdf_matte = dot_LN * RcpPi;
                assert(pdf_matte >= 0.0);
                probability += matte_prob * pdf_matte;
            }

            if (modes & Glossy)
            {
                // Compute the specular component (equation 3).
                Spectrum rs(values->m_rs);
                rs *= static_cast<float>(values->m_rs_multiplier);
                Spectrum fr_spec;
                evaluate_fr_spec(*m_mdf.get(), rs, dot_HL, dot_HN, fr_spec);
                value += fr_spec;

                // Compute the probability of a specular bounce.
                const double specular_prob = average_value(specular_albedo_V);

                // Compute the PDF of the incoming direction for the specular component.
                const double pdf_H = m_mdf->evaluate_pdf(dot_HN);
                const double pdf_specular = pdf_H / (4.0 * dot_HL);
                assert(pdf_specular >= 0.0);
                probability += specular_prob * pdf_specular;
            }

            return probability;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            double probability = 0.0;

            // Define aliases to match the notations in the paper.
            const Vector3d& V = outgoing;
            const Vector3d& L = incoming;
            const Vector3d& N = shading_basis.get_normal();

            const double dot_VN = abs(dot(V, N));
            const double dot_LN = abs(dot(L, N));

            // Compute the halfway vector.
            const Vector3d H = normalize(L + V);
            const double dot_HN = dot(H, N);
            const double dot_HL = dot(H, L);

            // Compute the specular albedo for the outgoing angle.
            Spectrum specular_albedo_V;
            evaluate_a_spec(m_a_spec, dot_VN, specular_albedo_V);

            if (modes & Diffuse)
            {
                // Compute the matte albedo.
                Spectrum matte_albedo(1.0f);
                matte_albedo -= specular_albedo_V;
                matte_albedo *= values->m_rm;
                matte_albedo *= static_cast<float>(values->m_rm_multiplier);

                // Compute the probability of a matte bounce.
                const double matte_prob = average_value(matte_albedo);

                // Compute the PDF of the incoming direction for the matte component.
                const double pdf_matte = dot_LN * RcpPi;
                assert(pdf_matte >= 0.0);
                probability += matte_prob * pdf_matte;
            }

            if (modes & Glossy)
            {
                // Compute the probability of a specular bounce.
                const double specular_prob = average_value(specular_albedo_V);

                // Compute the PDF of the incoming direction for the specular component.
                const double pdf_H = m_mdf->evaluate_pdf(dot_HN);
                const double pdf_specular = pdf_H / (4.0 * dot_HL);
                assert(pdf_specular >= 0.0);
                probability += specular_prob * pdf_specular;
            }

            return probability;
        }

      private:
        struct InputValues
        {
            Spectrum            m_rm;                           // matte reflectance of the substrate
            Alpha               m_rm_alpha;                     // unused
            double              m_rm_multiplier;                // matte reflectance multiplier
            Spectrum            m_rs;                           // specular reflectance at normal incidence
            Alpha               m_rs_alpha;                     // unused
            double              m_rs_multiplier;                // specular reflectance multiplier
            double              m_roughness;                    // technically, root-mean-square of the microfacets slopes
        };

        auto_ptr<WardMDF<double> >  m_mdf;                      // Microfacet Distribution Function
        Spectrum                    m_a_spec[AlbedoTableSize];  // albedo of the specular component as V varies
        Spectrum                    m_s;                        // normalization constant for the matte component

        // Evaluate the specular component of the BRDF (equation 3).
        template <typename MDF>
        static void evaluate_fr_spec(
            const MDF&          mdf,
            const Spectrum&     rs,
            const double        dot_HL,     // cos_beta in the paper
            const double        dot_HN,
            Spectrum&           fr_spec)
        {
            assert(dot_HL >= 0.0);
            assert(dot_HN >= 0.0);

            fr_spec = schlick_fresnel_reflection(rs, dot_HL);
            fr_spec *= static_cast<float>(mdf.evaluate(dot_HN) / (4.0 * dot_HL * dot_HL));
        }

        // Compute the specular albedo function.
        template <typename MDF>
        static void compute_specular_albedo(
            const MDF&          mdf,
            const Spectrum&     rs,
            Spectrum            albedo[])
        {
            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                // Compute an outgoing direction V in the XY plane.
                const double cos_theta = static_cast<double>(i) / (AlbedoTableSize - 1);
                const double sin_theta = sqrt(1.0 - cos_theta * cos_theta);
                const Vector3d V(sin_theta, cos_theta, 0.0);

                // Compute the albedo for this outgoing direction.
                compute_specular_albedo(mdf, rs, V, albedo[i]);
            }
        }

        // Compute the albedo of the specular component for a given outgoing direction.
        // See Physically Based Rendering, first edition, pp. 689-690.
        template <typename MDF>
        static void compute_specular_albedo(
            const MDF&          mdf,
            const Spectrum&     rs,
            const Vector3d&     V,
            Spectrum&           albedo)
        {
            // V must lie above or in the surface.
            assert(V.y >= 0.0);

            albedo.set(0.0f);

            for (size_t i = 0; i < AlbedoSampleCount; ++i)
            {
                // Generate a uniform sample in [0,1)^2.
                static const size_t Bases[] = { 2 };
                const Vector2d s = hammersley_sequence<double, 2>(Bases, i, AlbedoSampleCount);

                // Sample the microfacet distribution to get an halfway vector H.
                const Vector3d H = mdf.sample(s);
                const double dot_HV = dot(H, V);
                if (dot_HV <= 0.0)
                    continue;

                // L is the reflection of V around H.
                const Vector3d L = (dot_HV + dot_HV) * H - V;

                // Reject L if it lies in or below the surface.
                if (L.y <= 0.0)
                    continue;

                // Compute the PDF of L.
                const double dot_HN = H.y;
                const double pdf_H = mdf.evaluate_pdf(dot_HN);
                const double pdf_L = pdf_H / (4.0 * dot_HV);
                assert(pdf_L > 0.0);

                // Sanity checks.
                assert(is_normalized(V));
                assert(is_normalized(H));
                assert(is_normalized(L));

                // Evaluate the specular component for this (L, V) pair.
                Spectrum fr_spec;
                fr_spec = schlick_fresnel_reflection(rs, dot_HV);
                fr_spec *= static_cast<float>((L.y * mdf.evaluate(dot_HN)) / (4.0 * pdf_L * dot_HV * dot_HV));
                albedo += fr_spec;
            }

            albedo /= static_cast<float>(AlbedoSampleCount);
        }

        // Compute the average specular albedo.
        static void compute_average_specular_albedo(
            const Spectrum      a_spec[],
            Spectrum&           a_spec_avg)
        {
            a_spec_avg.set(0.0f);

            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                const double cos_theta = static_cast<double>(i) / (AlbedoTableSize - 1);
                const double sin_theta = sqrt(1.0 - cos_theta * cos_theta);

                Spectrum sample = a_spec[i];
                sample *= static_cast<float>(cos_theta * sin_theta);

                a_spec_avg += sample;
            }

            a_spec_avg *= static_cast<float>(HalfPi / AlbedoTableSize);     // integration over theta
            a_spec_avg *= static_cast<float>(TwoPi);                        // integration over phi
            a_spec_avg *= static_cast<float>(RcpPi);                        // average
        }

        // Evaluate the specular albedo function for an arbitrary angle.
        static void evaluate_a_spec(
            const Spectrum      a_spec[],
            const double        cos_theta,
            Spectrum&           result)
        {
            assert(cos_theta >= 0.0 && cos_theta <= 1.0);

            const double t = (AlbedoTableSize - 1) * cos_theta;
            const size_t i = truncate<size_t>(t);
            const double x = t - i;

            assert(i <= AlbedoTableSize - 1);
            assert(x >= 0.0 && x < 1.0);

            if (i < AlbedoTableSize - 1)
            {
                // Piecewise linear reconstruction.
                const Spectrum& prev_a = a_spec[i];
                const Spectrum& next_a = a_spec[i + 1];
                result = next_a;
                result -= prev_a;
                result *= static_cast<float>(x);
                result += prev_a;
            }
            else
            {
                result = a_spec[AlbedoTableSize - 1];
            }
        }

        static void plot_specular_albedo_curves()
        {
            MapleFile file("albedo.txt");
            plot_specular_albedo_curve(file, 0.8, Spectrum(1.0f));
            plot_specular_albedo_curve(file, 0.4, Spectrum(1.0f));
            plot_specular_albedo_curve(file, 0.03, Spectrum(1.0f));
        }

        static void plot_specular_albedo_curve(
            MapleFile&          file,
            const double        m,
            const Spectrum&     rs)
        {
            const string suffix = "_" + replace(to_string(m), ".", "_");

            generate_specular_albedo_plot_data(file, "ward_" + suffix, WardMDF<double>(m), rs);
            generate_specular_albedo_plot_data(file, "beckmann_" + suffix, BeckmannMDF<double>(m), rs);

            file.plot(
                make_vector(
                    MaplePlotDef("ward_" + suffix)
                        .set_legend("Specular Albedo with Ward MDF for m=" + to_string(m))
                        .set_style("point")
                        .set_color("black"),
                    MaplePlotDef("ward_" + suffix + "_reconstruction")
                        .set_legend("Specular Albedo Reconstruction with Ward MDF for m=" + to_string(m))
                        .set_style("line")
                        .set_color("red"),
                    MaplePlotDef("beckmann_" + suffix)
                        .set_legend("Specular Albedo with Beckmann MDF for m=" + to_string(m))
                        .set_style("point")
                        .set_color("black"),
                    MaplePlotDef("beckmann_" + suffix + "_reconstruction")
                        .set_legend("Specular Albedo Reconstruction with Beckmann MDF for m=" + to_string(m))
                        .set_style("line")
                        .set_color("blue")));
        }

        template <typename MDF>
        static void generate_specular_albedo_plot_data(
            MapleFile&          file,
            const string&       name,
            const MDF&          mdf,
            const Spectrum&     rs)
        {
            Spectrum a_spec[AlbedoTableSize];
            compute_specular_albedo(mdf, rs, a_spec);

            double angle[AlbedoTableSize];
            double albedo[AlbedoTableSize];

            for (size_t i = 0; i < AlbedoTableSize; ++i)
            {
                const double cos_angle = static_cast<double>(i) / (AlbedoTableSize - 1);

                angle[i] = acos(cos_angle);
                albedo[i] = average_value(a_spec[i]);
            }

            const size_t PointCount = 256;
            double reconstruction_angle[PointCount];
            double reconstruction_albedo[PointCount];

            for (size_t i = 0; i < PointCount; ++i)
            {
                const double cos_angle = static_cast<double>(i) / (PointCount - 1);

                Spectrum rec_albedo;
                evaluate_a_spec(a_spec, cos_angle, rec_albedo);

                reconstruction_angle[i] = acos(cos_angle);
                reconstruction_albedo[i] = average_value(rec_albedo);
            }

            file.define(name, AlbedoTableSize, angle, albedo);
            file.define(name + "_reconstruction", PointCount, reconstruction_angle, reconstruction_albedo);
        }
    };

    typedef BRDFWrapper<KelemenBRDFImpl> KelemenBRDF;
}


//
// KelemenBRDFFactory class implementation.
//

const char* KelemenBRDFFactory::get_model() const
{
    return Model;
}

const char* KelemenBRDFFactory::get_human_readable_model() const
{
    return "Kelemen BRDF";
}

DictionaryArray KelemenBRDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "matte_reflectance")
            .insert("label", "Matte Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "matte_reflectance_multiplier")
            .insert("label", "Matte Reflectance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "specular_reflectance")
            .insert("label", "Specular Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "specular_reflectance_multiplier")
            .insert("label", "Specular Reflectance Multiplier")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "0.5"));

    return definitions;
}

auto_release_ptr<BSDF> KelemenBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new KelemenBRDF(name, params));
}

}   // namespace renderer
