
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/uniforminputevaluator.h"
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
#include <cmath>
#include <string>

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
          : BSDF(name, Reflective, params)
        {
            m_inputs.declare("mdf_parameter", InputFormatScalar);
            m_inputs.declare("reflectance", InputFormatSpectrum);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatScalar, "1.0");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly))
                return false;

            const EntityDefMessageContext context("bsdf", get_name());
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

            // Compute the incoming direction by sampling the MDF.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            Vector3d h;
            double mdf_value, mdf_pdf;
            sample_mdf(values->m_mdf_parameter, s, h, mdf_value, mdf_pdf);
            h = shading_basis.transform_to_parent(h);
            incoming = reflect(outgoing, h);

            // No reflection in or below the geometric surface.
            const double cos_ig = dot(incoming, geometric_normal);
            if (cos_ig <= 0.0)
                return Absorption;

            const Vector3d& n = shading_basis.get_normal();
            const double cos_on = abs(dot(outgoing, n));
            const double cos_in = abs(dot(incoming, n));
            const double cos_hn = abs(dot(h, n));
            const double cos_oh = abs(dot(outgoing, h));

            // Compute the BRDF value.
            const double g = evaluate_attenuation(cos_on, cos_in, cos_hn, cos_oh);
            value = fresnel_dielectric_schlick(values->m_reflectance, cos_on, values->m_fr_multiplier);
            value *= static_cast<float>(mdf_value * g / (4.0 * cos_on * cos_in) * values->m_reflectance_multiplier);

            // Compute the PDF value.
            probability = mdf_pdf / (4.0 * cos_oh);

            return Diffuse;
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
            if (!(modes & Diffuse))
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d h = normalize(incoming + outgoing);
            const Vector3d& n = shading_basis.get_normal();
            const double cos_on = abs(dot(outgoing, n));
            const double cos_in = abs(dot(incoming, n));
            const double cos_hn = abs(dot(h, n));
            const double cos_oh = abs(dot(outgoing, h));

            // Evaluate the MDF.
            double mdf_value, mdf_pdf;
            evaluate_mdf(values->m_mdf_parameter, cos_hn, mdf_value, mdf_pdf);

            // Compute the BRDF value.
            const double g = evaluate_attenuation(cos_on, cos_in, cos_hn, cos_oh);
            value = fresnel_dielectric_schlick(values->m_reflectance, cos_on, values->m_fr_multiplier);
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
            if (!(modes & Diffuse))
                return 0.0;

            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d h = normalize(incoming + outgoing);
            const Vector3d& n = shading_basis.get_normal();
            const double cos_hn = abs(dot(h, n));
            const double cos_oh = abs(dot(outgoing, h));

            // Compute and return the PDF value.
            return evaluate_mdf_pdf(values->m_mdf_parameter, cos_hn) / (4.0 * cos_oh);
        }

      private:
        DECLARE_INPUT_VALUES(InputValues)
        {
            double      m_mdf_parameter;
            Spectrum    m_reflectance;
            Alpha       m_reflectance_alpha;        // unused
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

        MDF             m_mdf;

        static double evaluate_attenuation(
            const double        cos_on,
            const double        cos_in,
            const double        cos_hn,
            const double        cos_oh)
        {
            const double rcp_cos_oh = 1.0 / cos_oh;
            const double a = 2.0 * cos_hn * cos_on * rcp_cos_oh;
            const double b = 2.0 * cos_hn * cos_in * rcp_cos_oh;
            return min(a, b, 1.0);
        }

        void sample_mdf(
            const double        mdf_parameter,
            const Vector2d&     s,
            Vector3d&           direction,
            double&             value,
            double&             pdf) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    BlinnMDF<double> mdf(mdf_parameter);
                    direction = mdf.sample(s);
                    value = mdf.evaluate(direction.y);
                    pdf = mdf.evaluate_pdf(direction.y);
                }
                break;

              case Beckmann:
                {
                    BeckmannMDF<double> mdf(mdf_parameter);
                    direction = mdf.sample(s);
                    value = mdf.evaluate(direction.y);
                    pdf = mdf.evaluate_pdf(direction.y);
                }
                break;

              case Ward:
                {
                    WardMDF<double> mdf(mdf_parameter);
                    direction = mdf.sample(s);
                    value = mdf.evaluate(direction.y);
                    pdf = mdf.evaluate_pdf(direction.y);
                }
                break;

              case GGX:
                {
                    GGXMDF<double> mdf(mdf_parameter);
                    direction = mdf.sample(s);
                    value = mdf.evaluate(direction.y);
                    pdf = mdf.evaluate_pdf(direction.y);
                }
                break;

              assert_otherwise;
            }
        }

        void evaluate_mdf(
            const double        mdf_parameter,
            const double        cos_hn,
            double&             value,
            double&             pdf) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    BlinnMDF<double> mdf(mdf_parameter);
                    value = mdf.evaluate(cos_hn);
                    pdf = mdf.evaluate_pdf(cos_hn);
                }
                break;

              case Beckmann:
                {
                    BeckmannMDF<double> mdf(mdf_parameter);
                    value = mdf.evaluate(cos_hn);
                    pdf = mdf.evaluate_pdf(cos_hn);
                }
                break;

              case Ward:
                {
                    WardMDF<double> mdf(mdf_parameter);
                    value = mdf.evaluate(cos_hn);
                    pdf = mdf.evaluate_pdf(cos_hn);
                }
                break;

              case GGX:
                {
                    GGXMDF<double> mdf(mdf_parameter);
                    value = mdf.evaluate(cos_hn);
                    pdf = mdf.evaluate_pdf(cos_hn);
                }
                break;

              assert_otherwise;
            }
        }

        double evaluate_mdf_pdf(
            const double        mdf_parameter,
            const double        cos_hn) const
        {
            switch (m_mdf)
            {
              case Blinn:
                {
                    BlinnMDF<double> mdf(mdf_parameter);
                    return mdf.evaluate_pdf(cos_hn);
                }

              case Beckmann:
                {
                    BeckmannMDF<double> mdf(mdf_parameter);
                    return mdf.evaluate_pdf(cos_hn);
                }

              case Ward:
                {
                    WardMDF<double> mdf(mdf_parameter);
                    return mdf.evaluate_pdf(cos_hn);
                }

              case GGX:
                {
                    GGXMDF<double> mdf(mdf_parameter);
                    return mdf.evaluate_pdf(cos_hn);
                }

              default:
                return 0.0;
            }
        }
    };

    typedef BRDFWrapper<MicrofacetBRDFImpl> MicrofacetBRDF;
}


//
// MicrofacetBRDFFactory class implementation.
//

const char* MicrofacetBRDFFactory::get_model() const
{
    return Model;
}

const char* MicrofacetBRDFFactory::get_human_readable_model() const
{
    return "Microfacet BRDF";
}

DictionaryArray MicrofacetBRDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("widget", "dropdown_list")
            .insert("dropdown_items",
                Dictionary()
                    .insert("Blinn", "blinn")
                    .insert("Beckmann", "beckmann")
                    .insert("Ward", "ward")
                    .insert("GGX", "ggx"))
            .insert("use", "required")
            .insert("default", "blinn"));

    definitions.push_back(
        Dictionary()
            .insert("name", "mdf_parameter")
            .insert("label", "Distribution Function Parameter")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Reflectance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "fresnel_multiplier")
            .insert("label", "Fresnel Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return definitions;
}

auto_release_ptr<BSDF> MicrofacetBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MicrofacetBRDF(name, params));
}

}   // namespace renderer
