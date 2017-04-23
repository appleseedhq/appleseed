
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

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
using namespace std;

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

    class PlasticBRDFImpl
      : public BSDF
    {
      public:
        PlasticBRDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Reflective, ScatteringMode::All, params)
        {
            m_inputs.declare("specular_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("specular_reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("roughness", InputFormatFloat, "0.15");
            m_inputs.declare("highlight_falloff", InputFormatFloat, "0.4");
            m_inputs.declare("ior", InputFormatFloat, "1.5");
            m_inputs.declare("diffuse_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("diffuse_reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("internal_scattering", InputFormatFloat, "1.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);
            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "ggx",
                    make_vector("beckmann", "ggx", "gtr1", "std"),
                    context);

            if (mdf == "ggx")
                m_mdf.reset(new GGXMDF());
            else if (mdf == "beckmann")
                m_mdf.reset(new BeckmannMDF());
            else if (mdf == "gtr1")
                m_mdf.reset(new GTR1MDF());
            else if (mdf == "std")
                m_mdf.reset(new StdMDF());
            else return false;

            return true;
        }

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);

            // Apply multipliers to input values.
            values->m_specular_reflectance *= values->m_specular_reflectance_multiplier;
            values->m_diffuse_reflectance *= values->m_diffuse_reflectance_multiplier;

            new (&values->m_precomputed) InputValues::Precomputed();
            const float outside_ior = shading_point.get_ray().get_current_ior();
            values->m_precomputed.m_eta = outside_ior / values->m_ior;

            values->m_precomputed.m_specular_weight = max(max_value(values->m_specular_reflectance), 0.0f);
            values->m_precomputed.m_diffuse_weight = max(max_value(values->m_diffuse_reflectance), 0.0f);
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const Basis3f& shading_basis(sample.m_shading_basis);
            const Vector3f& n = shading_basis.get_normal();
            const Vector3f& outgoing = sample.m_outgoing.get_value();
            const float cos_on = min(dot(outgoing, n), 1.0f);
            if (cos_on < 0.0f)
                return;

            const InputValues* values = static_cast<const InputValues*>(data);
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            // Compute the microfacet normal by sampling the MDF.
            const Vector3f wo = shading_basis.transform_to_local(outgoing);
            sampling_context.split_in_place(4, 1);
            const Vector4f s = sampling_context.next2<Vector4f>();

            const Vector3f m = alpha == 0.0f ?
                Vector3f(0.0f, 1.0f, 0.0f) :
                m_mdf->sample(wo, Vector3f(s[0], s[1], s[2]), alpha, alpha, gamma);

            const float F = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, F);

            Vector3f wi;

            // Choose between specular and diffuse.
            if (s[3] < specular_probability)
            {
                wi = improve_normalization(reflect(wo, m));
                if (wi.y <= 0.0f)
                    return;

                if (alpha == 0.0f)
                {
                    sample.m_value = values->m_specular_reflectance;
                    sample.m_value *= F;
                    sample.m_probability = DiracDelta;
                    sample.m_mode = ScatteringMode::Specular;
                }
                else
                {
                    evaluate_specular(
                        values->m_specular_reflectance,
                        *m_mdf,
                        alpha,
                        gamma,
                        wi,
                        wo,
                        m,
                        F,
                        sample.m_value);

                    sample.m_probability = specular_pdf(*m_mdf, alpha, gamma, wo, m) * specular_probability;
                    sample.m_mode = ScatteringMode::Glossy;
                }
            }
            else
            {
                wi = sample_hemisphere_cosine(Vector2f(s[0], s[1]));
                const float Fi = fresnel_reflectance(wi, m, values->m_precomputed.m_eta);
                evaluate_diffuse(
                    values->m_diffuse_reflectance,
                    values->m_precomputed.m_eta,
                    values->m_internal_scattering,
                    F,
                    Fi,
                    sample.m_value);

                sample.m_mode = ScatteringMode::Diffuse;
                sample.m_probability = wi.y * RcpPi<float>() * (1.0f - specular_probability);
            }

            sample.m_incoming = Dual3f(shading_basis.transform_to_parent(wi));
            sample.compute_reflected_differentials();
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
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wo = shading_basis.transform_to_local(outgoing);
            const Vector3f wi = shading_basis.transform_to_local(incoming);

            const Vector3f m = alpha == 0.0f ?
                Vector3f(0.0f, 1.0f, 0.0f) :
                normalize(wi + wo);

            const float Fo = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float Fi = fresnel_reflectance(wi, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, Fo);

            float probability = 0.0f;
            value.resize(values->m_specular_reflectance.size());
            value.set(0.0f);

            if (ScatteringMode::has_glossy(modes))
            {
                evaluate_specular(
                    values->m_specular_reflectance,
                    *m_mdf,
                    alpha,
                    gamma,
                    wi,
                    wo,
                    m,
                    Fo,
                    value);

                probability = specular_probability * specular_pdf(*m_mdf, alpha, gamma, wo, m);
            }

            if (ScatteringMode::has_diffuse(modes))
            {
                Spectrum substrate_value;
                evaluate_diffuse(
                    values->m_diffuse_reflectance,
                    values->m_precomputed.m_eta,
                    values->m_internal_scattering,
                    Fo,
                    Fi,
                    substrate_value);

                probability += wi.y * RcpPi<float>() * (1.0f - specular_probability);
                value += substrate_value;
            }

            return probability;
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = dot(outgoing, n);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const float alpha = microfacet_alpha_from_roughness(values->m_roughness);
            const float gamma = highlight_falloff_to_gama(values->m_highlight_falloff);

            const Vector3f wo = shading_basis.transform_to_local(outgoing);
            const Vector3f wi = shading_basis.transform_to_local(incoming);

            const Vector3f m = alpha == 0.0f ?
                Vector3f(0.0f, 1.0f, 0.0f) :
                normalize(wi + wo);

            const float F = fresnel_reflectance(wo, m, values->m_precomputed.m_eta);
            const float specular_probability = choose_specular_probability(*values, F);

            float probability = 0.0f;

            if (ScatteringMode::has_glossy(modes))
                probability = specular_probability * specular_pdf(*m_mdf, alpha, gamma, wo, m);

            if (ScatteringMode::has_diffuse(modes))
                probability += wi.y * RcpPi<float>() * (1.0f - specular_probability);

            return probability;
        }

      private:
        typedef PlasticBRDFInputValues InputValues;

        static float choose_specular_probability(
            const InputValues&      values,
            const float             F)
        {
            const float specular_weight = F * values.m_precomputed.m_specular_weight;
            const float diffuse_weight = (1.0f - F) * values.m_precomputed.m_diffuse_weight;

            // Normalize weights.
            const float total_weight = specular_weight + diffuse_weight;

            if (total_weight == 0.0f)
                return 1.0f;

            return specular_weight / total_weight;
        }

        static float fresnel_reflectance(
            const Vector3f&         w,
            const Vector3f&         m,
            const float             eta)
        {
            const float cos_wm(dot(w, m));

            if (cos_wm < 0.0f)
                return 0.0f;

            float F;
            fresnel_reflectance_dielectric(
                F,
                eta,
                min(dot(w, m), 1.0f));
            return F;
        }

        static void evaluate_specular(
            const Spectrum&         specular_reflectance,
            const MDF&              mdf,
            const float             alpha,
            const float             gamma,
            const Vector3f&         wi,
            const Vector3f&         wo,
            const Vector3f&         m,
            const float             F,
            Spectrum&               value)
        {
            if (alpha == 0.0f)
                return;

            const float denom = abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            value = specular_reflectance;
            const float D = mdf.D(m, alpha, alpha, gamma);
            const float G = mdf.G(wi, wo, m, alpha, alpha, gamma);
            value *= F * D * G / denom;
        }

        static float specular_pdf(
            const MDF&              mdf,
            const float             alpha,
            const float             gamma,
            const Vector3f&         wo,
            const Vector3f&         m)
        {
            if (alpha == 0.0f)
                return 0.0f;

            const float cos_wom = dot(wo, m);
            if (cos_wom == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * abs(cos_wom));
            return jacobian * mdf.pdf(wo, m, alpha, alpha, gamma);
        }

        static void evaluate_diffuse(
            const Spectrum&         diffuse_reflectance,
            const float             eta,
            const float             internal_scattering,
            const float             Fo,
            const float             Fi,
            Spectrum&               value)
        {
            const float eta2 = square(eta);
            const float fdr = fresnel_internal_diffuse_reflectance(1.0f / eta);
            const float T = (1.0f - Fo) * (1.0f - Fi);

            value.resize(diffuse_reflectance.size());
            for (size_t i = 0, e = diffuse_reflectance.size(); i < e; ++i)
            {
                float pd = diffuse_reflectance[i];
                const float non_linear_term = 1.0f - lerp(1.0f, pd, internal_scattering) * fdr;
                value[i] = (T * pd * eta2 * RcpPi<float>()) / non_linear_term;
            }
        }

        auto_ptr<MDF>   m_mdf;
    };

    typedef BSDFWrapper<PlasticBRDFImpl> PlasticBRDF;
}


//
// PlasticBRDFFactory class implementation.
//

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
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Beckmann", "beckmann")
                    .insert("GGX", "ggx")
                    .insert("GTR1", "gtr1")
                    .insert("STD", "std"))
            .insert("use", "required")
            .insert("default", "ggx"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance")
            .insert("label", "Specular Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_reflectance_multiplier")
            .insert("label", "Specular Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("default", "0.15"));

    metadata.push_back(
        Dictionary()
            .insert("name", "highlight_falloff")
            .insert("label", "Highlight Falloff")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "optional")
            .insert("default", "0.4"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "1.0")
            .insert("max_value", "2.5")
            .insert("use", "required")
            .insert("default", "1.5"));

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
            .insert("name", "internal_scattering")
            .insert("label", "Internal Scattering")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
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

auto_release_ptr<BSDF> PlasticBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new PlasticBRDF(name, params));
}

}   // namespace renderer
