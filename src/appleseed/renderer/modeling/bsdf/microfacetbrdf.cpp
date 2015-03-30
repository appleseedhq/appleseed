
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
#include "renderer/modeling/bsdf/microfacetbrdfhelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
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

    struct FresnelDielectricSchlickFun
    {
        FresnelDielectricSchlickFun(
            const Spectrum& reflectance,
            const double    fr_multiplier)
          : m_reflectance(reflectance)
          , m_fr_multiplier(fr_multiplier)
        {
        }

        void operator()(
            const Vector3d& o,
            const Vector3d& h,
            const Vector3d& n,
            Spectrum&       value) const
        {
            fresnel_dielectric_schlick(
                value,
                m_reflectance,
                dot(o, n),
                m_fr_multiplier);
        }

        const Spectrum& m_reflectance;
        const double    m_fr_multiplier;
    };

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
            const InputValues* values = static_cast<const InputValues*>(data);
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;

            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> blinn_mdf;
                    MicrofacetBRDFHelper<double>::sample(
                        blinn_mdf,
                        e,
                        e,
                        e,
                        e,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        sample);
                }
                break;

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> beckmann_mdf;
                    MicrofacetBRDFHelper<double>::sample(
                        beckmann_mdf,
                        a,
                        a,
                        a,
                        a,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        sample);
                }
                break;

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> ward_mdf;
                    MicrofacetBRDFHelper<double>::sample(
                        ward_mdf,
                        a,
                        a,
                        a,
                        a,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        sample);
                }
                break;

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> ggx_mdf;
                    MicrofacetBRDFHelper<double>::sample(
                        ggx_mdf,
                        a,
                        a,
                        a,
                        a,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        sample);
                }
                break;

              assert_otherwise;
            }

            sample.value() *= static_cast<float>(values->m_reflectance_multiplier);
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
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;

            double pdf = 0.0;

            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> blinn_mdf;
                    pdf = MicrofacetBRDFHelper<double>::evaluate(
                        blinn_mdf,
                        e,
                        e,
                        e,
                        e,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        value);
                }
                break;

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> beckman_mdf;
                    pdf = MicrofacetBRDFHelper<double>::evaluate(
                        beckman_mdf,
                        a,
                        a,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        value);
                }
                break;

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> ward_mdf;
                    pdf = MicrofacetBRDFHelper<double>::evaluate(
                        ward_mdf,
                        a,
                        a,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        value);
                }
                break;

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> ggx_mdf;
                    pdf =  MicrofacetBRDFHelper<double>::evaluate(
                        ggx_mdf,
                        a,
                        a,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        FresnelDielectricSchlickFun(values->m_reflectance, values->m_fr_multiplier),
                        value);
                }
                break;

              assert_otherwise;
            }

            value *= static_cast<float>(values->m_reflectance_multiplier);
            return pdf;
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
            const double glossiness = values->m_glossiness * values->m_glossiness_multiplier;

            switch (m_mdf)
            {
              case Blinn:
                {
                    const double e = glossiness_to_blinn_exponent(glossiness);
                    const BlinnMDF<double> blinn_mdf;
                    return MicrofacetBRDFHelper<double>::pdf(
                        blinn_mdf,
                        e,
                        e,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes);
                }
                break;

              case Beckmann:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const BeckmannMDF<double> beckmann_mdf;
                    return MicrofacetBRDFHelper<double>::pdf(
                        beckmann_mdf,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes);
                }
                break;

              case Ward:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const WardMDF<double> ward_mdf;
                    return MicrofacetBRDFHelper<double>::pdf(
                        ward_mdf,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes);
                }
                break;

              case GGX:
                {
                    const double a = glossiness_to_roughness(glossiness);
                    const GGXMDF<double> ggx_mdf;
                    return MicrofacetBRDFHelper<double>::pdf(
                        ggx_mdf,
                        a,
                        a,
                        shading_basis,
                        outgoing,
                        incoming,
                        modes);
                }
                break;

              default:
                 assert(false);
                 return 0.0;
            }
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            double      m_glossiness;
            double      m_glossiness_multiplier;
            Spectrum    m_reflectance;
            double      m_reflectance_multiplier;
            double      m_fr_multiplier;            // Fresnel multiplier
        };

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
