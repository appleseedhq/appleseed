
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "microfacetbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling.h"
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
    // Microfacet BRDF.
    //

    const char* Model = "microfacet_brdf";

    class MicrofacetBRDFImpl
      : public BSDF
    {
      public:
        MicrofacetBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, BSDFSample::Glossy, params)
        {
            m_inputs.declare("glossiness", InputFormatScalar);
            m_inputs.declare("glossiness_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatScalar, "1.0");
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
                    "blinn",
                    make_vector("blinn", "beckmann", "ward", "ggx"),
                    context);

            if (mdf == "beckmann")
                m_mdf = Beckmann;
            else if (mdf == "ward")
                m_mdf = Ward;
            else if (mdf == "ggx")
                m_mdf = GGX;
            else m_mdf = Blinn;

            return true;
        }

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const
        {
            // No reflection below the shading surface.
            const Vector3d& n = sample.get_shading_normal();
            const double cos_on = min(dot(sample.get_outgoing(), n), 1.0);
            if (cos_on < 0.0)
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the incoming direction by sampling the MDF.
            sample.get_sampling_context().split_in_place(3, 1);
            const Vector3d s = sample.get_sampling_context().next_vector2<3>();
            const Vector3d wo = sample.get_shading_basis().transform_to_local(sample.get_outgoing());
            Vector3d m;
            double mdf_value, mdf_pdf;
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;
            sample_mdf(s, wo, glossiness, m, mdf_value, mdf_pdf);
            if (mdf_pdf == 0.0)
                return;

            const Vector3d h = sample.get_shading_basis().transform_to_parent(m);
            sample.set_incoming(reflect(sample.get_outgoing(), h));
            const double cos_oh = dot(sample.get_outgoing(), h);

            // No reflection below the shading surface.
            const double cos_in = dot(sample.get_incoming(), n);
            if (cos_in < 0.0)
                return;

            // Compute the BRDF value.
            const double g =
                evaluate_attenuation(
                    sample.get_shading_basis().transform_to_local(sample.get_incoming()),
                    wo,
                    m,
                    glossiness);

            fresnel_dielectric_schlick(sample.value(), values->m_reflectance, cos_on, values->m_fr_multiplier);
            sample.value() *= static_cast<float>(mdf_value * g / (4.0 * cos_on * cos_in) * values->m_reflectance_multiplier);

            // Compute the PDF value.
            sample.set_probability(mdf_pdf / (4.0 * cos_oh));

            // Set the scattering mode.
            sample.set_mode(BSDFSample::Glossy);
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
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);
            const double cos_oh = dot(outgoing, h);

            // Evaluate the MDF.
            double mdf_value, mdf_pdf;
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;
            const Vector3d wo = shading_basis.transform_to_local(outgoing);
            const Vector3d m = shading_basis.transform_to_local(h);
            evaluate_mdf(wo, m, glossiness, mdf_value, mdf_pdf);

            // Compute the BRDF value.
            const double g =
                evaluate_attenuation(
                    shading_basis.transform_to_local(incoming),
                    wo,
                    m,
                    glossiness);

            fresnel_dielectric_schlick(value, values->m_reflectance, cos_on, values->m_fr_multiplier);
            value *= static_cast<float>(mdf_value * g / (4.0 * cos_on * cos_in) * values->m_reflectance_multiplier);

            // Compute and return the PDF value.
            return mdf_pdf / (4.0 * cos_oh);
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            if (!(modes & BSDFSample::Glossy))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            // Compute the halfway vector in world space.
            const Vector3d h = normalize(incoming + outgoing);
            const double cos_hn = dot(h, n);
            const double cos_oh = dot(outgoing, h);

            // Compute and return the PDF value.
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;

            return evaluate_mdf_pdf(
                shading_basis.transform_to_local(outgoing),
                shading_basis.transform_to_local(h),
                glossiness) / (4.0 * cos_oh);
        }

      private:
        typedef MicrofacetBRDFInputValues InputValues;

        enum MDF
        {
            Blinn,
            Beckmann,
            Ward,
            GGX
        };

        MDF m_mdf;

        double glossiness_to_blinn_exponent(const double g) const
        {
            return 100.0 * pow_int(g, 3) + 9900.0 * pow(g, 30.0);
        }

        double glossiness_to_roughness(const double g) const
        {
            return max(1.0 - g, 1.0e-6);
        }

        void sample_mdf(
            const Vector3d&     s,
            const Vector3d&     v,
            const double        glossiness,
            Vector3d&           direction,
            double&             value,
            double&             pdf) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> mdf;
                    direction = mdf.sample(v, s, e, e);
                    value = mdf.D(direction, e, e);
                    pdf = mdf.pdf(v, direction, e, e);
                }
                break;

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> mdf;
                    direction = mdf.sample(v, s, a, a);
                    value = mdf.D(direction, a, a);
                    pdf = mdf.pdf(v, direction, a, a);
                }
                break;

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> mdf;
                    direction = mdf.sample(v, s, a, a);
                    value = mdf.D(direction, a, a);
                    pdf = mdf.pdf(v, direction, a, a);
                }
                break;

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> mdf;
                    direction = mdf.sample(v, s, a, a);
                    value = mdf.D(direction, a, a);
                    pdf = mdf.pdf(v, direction, a, a);
                }
                break;

              assert_otherwise;
            }
        }

        void evaluate_mdf(
            const Vector3d&     v,
            const Vector3d&     h,
            const double        glossiness,
            double&             value,
            double&             pdf) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> mdf;
                    value = mdf.D(h, e, e);
                    pdf = mdf.pdf(v, h, e, e);
                }
                break;

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> mdf;
                    value = mdf.D(h, a, a);
                    pdf = mdf.pdf(v, h, a, a);
                }
                break;

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> mdf;
                    value = mdf.D(h, a, a);
                    pdf = mdf.pdf(v, h, a, a);
                }
                break;

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> mdf;
                    value = mdf.D(h, a, a);
                    pdf = mdf.pdf(v, h, a, a);
                }
                break;

              assert_otherwise;
            }
        }

        double evaluate_mdf_pdf(
            const Vector3d&     v,
            const Vector3d&     h,
            const double        glossiness) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> mdf;
                    return mdf.pdf(v, h, e, e);
                }

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> mdf;
                    return mdf.pdf(v, h, a, a);
                }

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> mdf;
                    return mdf.pdf(v, h, a, a);
                }

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> mdf;
                    return mdf.pdf(v, h, a, a);
                }

              assert_otherwise;
            }

            // To keep some compilers happy.
            return 0.0;
        }

        double evaluate_attenuation(
            const Vector3d& wi,
            const Vector3d& wo,
            const Vector3d& h,
            const double    glossiness) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> mdf;
                    return mdf.G(wi, wo, h, e, e);
                }

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> mdf;
                    return mdf.G(wi, wo, h, a, a);
                }

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> mdf;
                    return mdf.G(wi, wo, h, a, a);
                }

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> mdf;
                    return mdf.G(wi, wo, h, a, a);
                }

              assert_otherwise;
            }

            // To keep some compilers happy.
            return 0.0;
        }
    };

    typedef BSDFWrapper<MicrofacetBRDFImpl> MicrofacetBRDF;
}


//
// MicrofacetBRDFFactory class implementation.
//

const char* MicrofacetBRDFFactory::get_model() const
{
    return Model;
}

Dictionary MicrofacetBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Microfacet BRDF");
}

DictionaryArray MicrofacetBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Blinn", "blinn")
                    .insert("Beckmann", "beckmann")
                    .insert("Ward", "ward")
                    .insert("GGX", "ggx"))
            .insert("use", "required")
            .insert("default", "blinn"));

    metadata.push_back(
        Dictionary()
            .insert("name", "glossiness")
            .insert("label", "Glossiness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "glossiness_multiplier")
            .insert("label", "Glossiness Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
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

    return metadata;
}

auto_release_ptr<BSDF> MicrofacetBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetBRDF(name, params));
}

}   // namespace renderer
