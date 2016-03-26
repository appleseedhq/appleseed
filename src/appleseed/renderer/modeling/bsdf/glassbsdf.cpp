
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "glassbsdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <cmath>
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
    // Glass BSDF.
    //
    //    A future version of this BSDF will support multiple-scattering.
    //    For that reason, the only available microfacet distribution functions
    //    are those that support it (Beckmann and GGX).
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces.
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    //   [2] Extending the Disney BRDF to a BSDF with Integrated Subsurface Scattering.
    //       http://blog.selfshadow.com/publications/s2015-shading-course/burley/s2015_pbs_disney_bsdf_slides.pdf
    //

    const char* Model = "glass_bsdf";

    //
    // The GlassBSDF is used in two different contexts,
    // as an appleseed BSDF and as an OSL closure.
    //
    //  - When used as an appleseed BSDF, the normal is flipped
    //    when shading a backfacing point.
    //
    //  - When used as an OSL closure, the normal is not flipped
    //    when shading a backfacing point.
    //
    // To handle the two cases in an uniform way, the BSDF accepts a
    // backfacing policy class as a template parameter.
    //

    struct AppleseedBackfacingPolicy
    {
        AppleseedBackfacingPolicy(
            const Basis3d&  shading_basis,
            const bool      backfacing)
          : m_basis(shading_basis)
          , m_backfacing(backfacing)
        {
            if (m_backfacing)
            {
                m_basis =
                    Basis3d(
                        -shading_basis.get_normal(),
                         shading_basis.get_tangent_u(),
                        -shading_basis.get_tangent_v());
            }
            else
                m_basis = shading_basis;
        }

        const Vector3d& get_normal() const
        {
            return m_basis.get_normal();
        }

        const Vector3d transform_to_local(const Vector3d& v) const
        {
            return m_basis.transform_to_local(v);
        }

        const Vector3d transform_to_parent(const Vector3d& v) const
        {
            return m_basis.transform_to_parent(v);
        }

        Basis3d     m_basis;
        const bool  m_backfacing;
    };

    struct OSLBackfacingPolicy
    {
        OSLBackfacingPolicy(
            const Basis3d&  shading_basis,
            const bool      backfacing)
          : m_basis(shading_basis)
        {
        }

        const Vector3d& get_normal() const
        {
            return m_basis.get_normal();
        }

        const Vector3d transform_to_local(const Vector3d& v) const
        {
            return m_basis.transform_to_local(v);
        }

        const Vector3d transform_to_parent(const Vector3d& v) const
        {
            return m_basis.transform_to_parent(v);
        }

        const Basis3d&  m_basis;
    };

    template <typename BackfacingPolicy>
    class GlassBSDFImpl
      : public BSDF
    {
      public:
        GlassBSDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("surface_transmittance", InputFormatSpectralReflectance);
            m_inputs.declare("surface_transmittance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("reflection_tint", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("refraction_tint", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("roughness", InputFormatScalar, "0.15");
            m_inputs.declare("anisotropic", InputFormatScalar, "0.0");
            m_inputs.declare("ior", InputFormatScalar, "1.5");
            m_inputs.declare("volume_transmittance", InputFormatSpectralReflectance, "1.0");
            m_inputs.declare("volume_transmittance_distance", InputFormatScalar, "0.0");
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
            const Project&      project,
            const Assembly&     assembly,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);
            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "ggx",
                    make_vector("beckmann", "ggx"),
                    context);

            if (mdf == "ggx")
                m_mdf.reset(new GGXMDF<double>());
            else if (mdf == "beckmann")
                m_mdf.reset(new BeckmannMDF<double>());
            else return false;

            return true;
        }

        virtual size_t compute_input_data_size(
            const Assembly&     assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(InputValues), 16);
        }

        APPLESEED_FORCE_INLINE virtual void prepare_inputs(
            const ShadingPoint& shading_point,
            void*               data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);

            if (shading_point.is_entering())
            {
                values->m_from_ior =
                    shading_point.get_ray().get_current_ior();
                values->m_to_ior = values->m_ior;
                values->m_backfacing = false;
            }
            else
            {
                values->m_from_ior = values->m_ior;
                values->m_to_ior =
                    shading_point.get_ray().get_previous_ior();
                values->m_backfacing = true;
            }

            values->m_reflection_color  = values->m_surface_transmittance;
            values->m_reflection_color *= values->m_reflection_tint;
            values->m_reflection_color *= static_cast<float>(values->m_surface_transmittance_multiplier);

            // [2] Surface absorption, page 5.
            values->m_refraction_color  = values->m_surface_transmittance;
            values->m_refraction_color *= values->m_refraction_tint;
            values->m_refraction_color *= static_cast<float>(values->m_surface_transmittance_multiplier);
            values->m_refraction_color  = sqrt(values->m_refraction_color);
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const BackfacingPolicy backfacing_policy(
                sample.get_shading_basis(),
                values->m_backfacing);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            const Vector3d& outgoing = sample.m_outgoing.get_value();
            const Vector3d& n = backfacing_policy.get_normal();

            // Compute the microfacet normal by sampling the MDF.
            sampling_context.split_in_place(4, 1);
            const Vector4d s = sampling_context.next_vector2<4>();
            Vector3d wo = backfacing_policy.transform_to_local(outgoing);
            Vector3d m =
                m_mdf->sample(
                    wo.y < 0.0 ? -wo : wo,
                    Vector3d(s[0], s[1], s[2]),
                    alpha_x,
                    alpha_y);

            const double cos_oh = dot(wo, m);
            const double eta = values->m_from_ior / values->m_to_ior;
            double F = fresnel_reflection(eta, cos_oh);

            bool is_refraction;
            Vector3d incoming, wi;
            double term, jacobian;

            // Choose between reflection and refraction.
            if (s[3] < F)
            {
                // Reflection.
                is_refraction = false;

                const Vector3d h = backfacing_policy.transform_to_parent(m);
                reflection_vector(outgoing, h, incoming);

                const double cos_in = dot(incoming, n);
                const double cos_on = dot(outgoing, n);

                // If incoming and outgoing are on different sides
                // of the surface, this is not a reflection.
                if (cos_in * cos_on <= 0.0)
                    return;

                term = reflection_term(cos_in, cos_on);
                jacobian = reflection_jacobian(cos_oh);
                sample.m_value = values->m_reflection_color;

                wi = backfacing_policy.transform_to_local(incoming);
                if (wo.y < 0.0)
                {
                    wo = -wo;
                    wi = -wi;
                }
            }
            else
            {
                // Refraction.
                is_refraction = true;

                const Vector3d h = backfacing_policy.transform_to_parent(m);
                if (!refraction_vector(outgoing, h, eta, incoming))
                    return;         // ignore total internal reflection

                const double cos_in = dot(incoming, n);
                const double cos_on = dot(outgoing, n);

                // If incoming and outgoing are on the same side
                // of the surface, this is not a refraction.
                if (cos_in * cos_on > 0.0)
                    return;

                F = 1.0 - F;

                term = refraction_term(
                    outgoing,
                    incoming,
                    cos_on,
                    cos_in,
                    h,
                    values->m_from_ior,
                    values->m_to_ior);

                if (adjoint)
                    term *= square(eta);

                jacobian = refraction_jacobian(
                    outgoing,
                    incoming,
                    h,
                    values->m_from_ior,
                    values->m_to_ior);

                sample.m_value = values->m_refraction_color;

                wi = backfacing_policy.transform_to_local(incoming);
                if (wo.y < 0.0)
                {
                    wo = -wo;
                    wi = -wi;
                }
            }

            sample.m_probability = m_mdf->pdf(wo, m, alpha_x, alpha_y) * jacobian * F;
            if (sample.m_probability == 0.0)
                return;

            sample.m_mode = ScatteringMode::Glossy;
            sample.m_incoming = Dual3d(incoming);

            const double G =
                m_mdf->G(
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y);

            const double D = m_mdf->D(m, alpha_x, alpha_y);
            sample.m_value *= static_cast<float>(F * D * G * term);

            if (is_refraction)
                sample.compute_transmitted_differentials(eta);
            else
                sample.compute_reflected_differentials();
        }

        APPLESEED_FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            const BackfacingPolicy backfacing_policy(
                shading_basis,
                values->m_backfacing);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            const Vector3d& n = backfacing_policy.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);

            const double eta = values->m_from_ior / values->m_to_ior;

            Vector3d m, wi, wo;
            double F, term, jacobian;

            if (cos_in * cos_on > 0.0)
            {
                // Reflection.
                const Vector3d h = half_reflection_vector(
                    outgoing,
                    incoming,
                    n);

                const double cos_oh = dot(outgoing, h);
                F = fresnel_reflection(eta, cos_oh);

                term = reflection_term(cos_in, cos_on);
                jacobian = reflection_jacobian(cos_oh);

                value = values->m_reflection_color;

                m = backfacing_policy.transform_to_local(h);
                wi = backfacing_policy.transform_to_local(incoming);
                wo = backfacing_policy.transform_to_local(outgoing);
                if (wo.y < 0.0)
                {
                    wo = -wo;
                    wi = -wi;
                }
            }
            else
            {
                // Refraction.
                const Vector3d h = half_refraction_vector(
                    outgoing,
                    incoming,
                    values->m_from_ior,
                    values->m_to_ior,
                    n);

                const double cos_oh = dot(outgoing, h);
                F = fresnel_transmission(eta, cos_oh);

                term =
                    refraction_term(
                        outgoing,
                        incoming,
                        cos_on,
                        cos_in,
                        h,
                        values->m_from_ior,
                        values->m_to_ior);

                if (adjoint)
                    term *= square(eta);

                jacobian =
                    refraction_jacobian(
                        outgoing,
                        incoming,
                        h,
                        values->m_from_ior,
                        values->m_to_ior);

                value = values->m_refraction_color;

                m = backfacing_policy.transform_to_local(h);
                wo = backfacing_policy.transform_to_local(outgoing);
                wi = backfacing_policy.transform_to_local(incoming);
                if (wo.y < 0.0)
                {
                    wo = -wo;
                    wi = -wi;
                }
            }

            const double G =
                m_mdf->G(
                    wi,
                    wo,
                    m,
                    alpha_x,
                    alpha_y);

            const double D = m_mdf->D(m, alpha_x, alpha_y);

            value *= static_cast<float>(F * D * G * term);
            return m_mdf->pdf(wo, m, alpha_x, alpha_y) * jacobian * F;
        }

        APPLESEED_FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            const BackfacingPolicy backfacing_policy(
                shading_basis,
                values->m_backfacing);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            const Vector3d& n = backfacing_policy.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);

            const double eta = values->m_from_ior / values->m_to_ior;

            Vector3d wo, m;
            double F, jacobian;

            if (cos_in * cos_on > 0.0)
            {
                // Reflection.
                const Vector3d h = half_reflection_vector(
                    outgoing,
                    incoming,
                    n);

                const double cos_oh = dot(outgoing, h);
                F = fresnel_reflection(
                    eta,
                    cos_oh);

                jacobian = reflection_jacobian(cos_oh);

                m = backfacing_policy.transform_to_local(h);
                wo = backfacing_policy.transform_to_local(outgoing);
                if (wo.y < 0.0)
                    wo = -wo;
            }
            else
            {
                // Refraction.
                const Vector3d h = half_refraction_vector(
                    outgoing,
                    incoming,
                    values->m_from_ior,
                    values->m_to_ior,
                    n);

                const double cos_oh = dot(outgoing, h);
                F = fresnel_transmission(eta, cos_oh);

                jacobian =
                    refraction_jacobian(
                        outgoing,
                        incoming,
                        h,
                        values->m_from_ior,
                        values->m_to_ior);

                m = backfacing_policy.transform_to_local(h);
                wo = backfacing_policy.transform_to_local(outgoing);
                if (wo.y < 0.0)
                    wo = -wo;
            }

            return
                m_mdf->pdf(
                    wo,
                    m,
                    alpha_x,
                    alpha_y) * jacobian * F;
        }

        virtual double sample_ior(
            SamplingContext&    sampling_context,
            const void*         data) const APPLESEED_OVERRIDE
        {
            return static_cast<const InputValues*>(data)->m_ior;
        }

        virtual void compute_absorption(
            const void*         data,
            const double        distance,
            Spectrum&           absorption) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (values->m_volume_transmittance_distance != 0.0)
            {
                // [2] Volumetric absorption reparameterization, page 5.
                absorption.resize(values->m_volume_transmittance.size());
                const float d = static_cast<float>(distance / values->m_volume_transmittance_distance);
                for (size_t i = 0, e = absorption.size(); i < e; ++i)
                {
                    const float a = log(max(values->m_volume_transmittance[i], 0.01f));
                    absorption[i] = exp(a * d);
                }
            }
            else
            {
                absorption.set(1.0f);
            }
        }

      private:
        typedef GlassBSDFInputValues InputValues;

        auto_ptr<MDF<double> > m_mdf;

        static double fresnel_reflection(
            const double        eta,
            const double        cos_oh)
        {
            double F;

            if (cos_oh >= 0.0)
            {
                fresnel_reflectance_dielectric(
                    F,
                    eta,
                    cos_oh);
            }
            else
            {
                fresnel_reflectance_dielectric(
                    F,
                    1.0 / eta,
                    -cos_oh);
            }

            return F;
        }

        static double fresnel_transmission(
            const double        eta,
            const double        cos_oh)
        {
            return 1.0 - fresnel_reflection(eta, cos_oh);
        }

        static void reflection_vector(
            const Vector3d&     outgoing,
            const Vector3d&     h,
            Vector3d&           incoming)
        {
            incoming = reflect(outgoing, h);
        }

        static Vector3d half_reflection_vector(
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const Vector3d&     n)
        {
            // [1] eq. 13
            const Vector3d h = normalize(incoming + outgoing);
            return dot(h, n) >= 0.0 ? h : -h;
        }

        static double reflection_term(
            const double        cos_in,
            const double        cos_on)
        {
            // [1] eq. 20.
            const double denom = 4.0 * cos_on * cos_in;
            if (denom == 0.0)
                return 0.0;

            return 1.0 / (4.0 * cos_on * cos_in);
        }

        static double reflection_jacobian(
            const double        cos_oh)
        {
            // [1] eq. 14.
            if (cos_oh == 0.0)
                return 0.0;

            return 1.0 / abs(4.0 * abs(cos_oh));
        }

        static bool refraction_vector(
            const Vector3d&     outgoing,
            const Vector3d&     h,
            const double        eta,
            Vector3d&           incoming)
        {
            const double cos_theta_i = dot(outgoing, h);
            const double sin_theta_i2 = 1.0 - square(cos_theta_i);
            const double sin_theta_t2 = sin_theta_i2 * square(eta);
            const double cos_theta_t2 = 1.0 - sin_theta_t2;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection.
                return false;
            }

            const double cos_theta_t = sqrt(cos_theta_t2);
            incoming =
                cos_theta_i > 0.0
                    ? (eta * cos_theta_i - cos_theta_t) * h - eta * outgoing
                    : (eta * cos_theta_i + cos_theta_t) * h - eta * outgoing;

            incoming = improve_normalization(incoming);
            return true;
        }

        static Vector3d half_refraction_vector(
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const double        ior_o,
            const double        ior_i,
            const Vector3d&     n)
        {
            // [1] eq. 13
            const Vector3d h = normalize(ior_i * incoming + ior_o * outgoing);
            return dot(h, n) >= 0.0 ? h : -h;
        }

        static double refraction_term(
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const double        cos_on,
            const double        cos_in,
            const Vector3d&     h,
            const double        ior_o,
            const double        ior_i)
        {
            // [1] eq. 21
            const double cos_ih = dot(h, incoming);
            const double cos_oh = dot(h, outgoing);

            const double sqrt_denom = ior_i * cos_ih + ior_o * cos_oh;
            if (sqrt_denom == 0.0)
                return 0.0;

            return
                abs((cos_ih * cos_oh) / (cos_in * cos_on)) *
                square(ior_o / sqrt_denom);
        }

        static double refraction_jacobian(
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const Vector3d&     h,
            const double        ior_o,
            const double        ior_i)
        {
            // [1] eq. 17
            const double cos_ih = dot(h, incoming);
            const double cos_oh = dot(h, outgoing);

            const double sqrt_denom = ior_i * cos_ih + ior_o * cos_oh;
            if (sqrt_denom == 0.0)
                return 0.0;

            return abs(cos_oh) * square(ior_o / sqrt_denom);
        }
    };

    typedef
        BSDFWrapper<
            GlassBSDFImpl<AppleseedBackfacingPolicy> > AppleseedGlassBSDF;

    typedef
        BSDFWrapper<
            GlassBSDFImpl<OSLBackfacingPolicy> > OSLGlassBSDF;
}


//
// GlassBSDFFactory class implementation.
//

const char* GlassBSDFFactory::get_model() const
{
    return Model;
}

Dictionary GlassBSDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Glass BSDF");
}

DictionaryArray GlassBSDFFactory::get_input_metadata() const
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
                    .insert("GGX", "ggx"))
            .insert("use", "required")
            .insert("default", "ggx"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_transmittance")
            .insert("label", "Surface Transmittance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.85"));

    metadata.push_back(
        Dictionary()
            .insert("name", "surface_transmittance_multiplier")
            .insert("label", "Surface Transmittance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflection_tint")
            .insert("label", "Reflection Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.00"));

    metadata.push_back(
        Dictionary()
            .insert("name", "refraction_tint")
            .insert("label", "Refraction Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.00"));

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
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min_value", "-1.0")
            .insert("max_value", "1.0")
            .insert("default", "0.0"));

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
            .insert("name", "volume_transmittance")
            .insert("label", "Volume Transmittace")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.00"));

    metadata.push_back(
        Dictionary()
            .insert("name", "volume_transmittance_distance")
            .insert("label", "Volume Transmittance Distance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min_value", "0.0")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<BSDF> GlassBSDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new AppleseedGlassBSDF(name, params));
}

auto_release_ptr<BSDF> GlassBSDFFactory::create_osl(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new OSLGlassBSDF(name, params));
}

}   // namespace renderer
