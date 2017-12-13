
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "metalbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/bsdf/specularhelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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
    // Metal BRDF.
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    //   [2] Revisiting Physically Based Shading at Imageworks
    //       http://blog.selfshadow.com/publications/s2017-shading-course/imageworks/s2017_pbs_imageworks_slides.pdf
    //

    const char* Model = "metal_brdf";

    class MetalBRDFImpl
      : public BSDF
    {
      public:
        MetalBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Glossy | ScatteringMode::Specular, params)
        {
            m_inputs.declare("normal_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("edge_tint", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("roughness", InputFormatFloat, "0.15");
            m_inputs.declare("highlight_falloff", InputFormatFloat, "0.4");
            m_inputs.declare("anisotropy", InputFormatFloat, "0.0");
            m_inputs.declare("energy_compensation", InputFormatFloat, "0.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(InputValues);
        }

        void prepare_inputs(
            Arena&                      arena,
            const ShadingPoint&         shading_point,
            void*                       data) const override
        {
            InputValues* values = static_cast<InputValues*>(data);
            new (&values->m_precomputed) InputValues::Precomputed();

            artist_friendly_fresnel_conductor_reparameterization(
                values->m_normal_reflectance,
                values->m_edge_tint,
                values->m_precomputed.m_n,
                values->m_precomputed.m_k);
            values->m_precomputed.m_outside_ior = shading_point.get_ray().get_current_ior();

            average_artist_friendly_fresnel_reflectance_conductor(
                values->m_normal_reflectance,
                values->m_edge_tint,
                values->m_precomputed.m_fresnel_average);
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);
            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "ggx",
                    make_vector("beckmann", "ggx", "std"),
                    context);

            if (mdf == "ggx")
                m_mdf_type = GGX;
            else if (mdf == "beckmann")
                m_mdf_type = Beckmann;
            else if (mdf == "std")
                m_mdf_type = Std;
            else return false;

            return true;
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const FresnelConductorFun f(
                values->m_precomputed.m_n,
                values->m_precomputed.m_k,
                values->m_precomputed.m_outside_ior,
                values->m_reflectance_multiplier);

            // If roughness is zero use reflection.
            if (values->m_roughness == 0.0f)
            {
                if (ScatteringMode::has_specular(modes))
                    SpecularBRDFHelper::sample(f, sample);

                sample.m_value.m_beauty = sample.m_value.m_glossy;
                return;
            }

            if (ScatteringMode::has_glossy(modes))
            {
                float alpha_x, alpha_y;
                microfacet_alpha_from_roughness(
                    values->m_roughness,
                    values->m_anisotropy,
                    alpha_x,
                    alpha_y);

                const Vector3f& n = sample.m_shading_basis.get_normal();
                const Vector3f& outgoing = sample.m_outgoing.get_value();
                const float cos_on = abs(dot(outgoing, n));

                switch (m_mdf_type)
                {
                  case GGX:
                    {
                        GGXMDF mdf;
                        MicrofacetBRDFHelper::sample(
                            sampling_context,
                            mdf,
                            alpha_x,
                            alpha_y,
                            1.0f,
                            f,
                            cos_on,
                            sample);

                        const float cos_in = dot(sample.m_shading_basis.get_normal(), sample.m_incoming.get_value());
                        add_energy_compensation_term(mdf, values, cos_in, cos_on, sample.m_value.m_glossy);
                    }
                    break;

                  case Beckmann:
                    {
                        BeckmannMDF mdf;
                        MicrofacetBRDFHelper::sample(
                            sampling_context,
                            mdf,
                            alpha_x,
                            alpha_y,
                            1.0f,
                            f,
                            cos_on,
                            sample);

                        const float cos_in = dot(sample.m_shading_basis.get_normal(), sample.m_incoming.get_value());
                        add_energy_compensation_term(mdf, values, cos_in, cos_on, sample.m_value.m_glossy);
                    }
                    break;

                  case Std:
                    {
                        StdMDF mdf;
                        MicrofacetBRDFHelper::sample(
                            sampling_context,
                            mdf,
                            alpha_x,
                            alpha_y,
                            highlight_falloff_to_gama(values->m_highlight_falloff),
                            f,
                            cos_on,
                            sample);
                    }
                    break;

                  assert_otherwise;
                }

                sample.m_value.m_beauty = sample.m_value.m_glossy;
            }
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = abs(dot(incoming, n));
            const float cos_on = abs(dot(outgoing, n));

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            const FresnelConductorFun f(
                values->m_precomputed.m_n,
                values->m_precomputed.m_k,
                values->m_precomputed.m_outside_ior,
                values->m_reflectance_multiplier);

            float pdf;

            switch (m_mdf_type)
            {
              case GGX:
                {
                    GGXMDF mdf;
                    pdf = MicrofacetBRDFHelper::evaluate(
                        mdf,
                        alpha_x,
                        alpha_y,
                        1.0f,
                        shading_basis,
                        outgoing,
                        incoming,
                        f,
                        cos_in,
                        cos_on,
                        value.m_glossy);
                    add_energy_compensation_term(mdf, values, cos_in, cos_on, value.m_glossy);
                }
                break;

              case Beckmann:
                {
                    BeckmannMDF mdf;
                    pdf = MicrofacetBRDFHelper::evaluate(
                        mdf,
                        alpha_x,
                        alpha_y,
                        1.0f,
                        shading_basis,
                        outgoing,
                        incoming,
                        f,
                        cos_in,
                        cos_on,
                        value.m_glossy);
                    add_energy_compensation_term(mdf, values, cos_in, cos_on, value.m_glossy);
                }
                break;

              case Std:
                {
                    StdMDF mdf;
                    pdf = MicrofacetBRDFHelper::evaluate(
                        mdf,
                        alpha_x,
                        alpha_y,
                        highlight_falloff_to_gama(values->m_highlight_falloff),
                        shading_basis,
                        outgoing,
                        incoming,
                        f,
                        cos_in,
                        cos_on,
                        value.m_glossy);
                }
                break;

              default:
                assert(false);
                pdf = 0.0f;
                break;
            }

            value.m_beauty = value.m_glossy;
            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            float alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropy,
                alpha_x,
                alpha_y);

            switch (m_mdf_type)
            {
              case GGX:
                {
                    GGXMDF mdf;
                    return MicrofacetBRDFHelper::pdf(
                        mdf,
                        alpha_x,
                        alpha_y,
                        1.0f,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              case Beckmann:
                {
                    BeckmannMDF mdf;
                    return MicrofacetBRDFHelper::pdf(
                        mdf,
                        alpha_x,
                        alpha_y,
                        1.0f,
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              case Std:
                {
                    StdMDF mdf;
                    return MicrofacetBRDFHelper::pdf(
                        mdf,
                        alpha_x,
                        alpha_y,
                        highlight_falloff_to_gama(values->m_highlight_falloff),
                        shading_basis,
                        outgoing,
                        incoming);
                }
                break;

              default:
                assert(false);
                return 0.0f;
            }
        }

      private:
        typedef MetalBRDFInputValues InputValues;

        enum MDFType
        {
            GGX = 0,
            Beckmann,
            Std
        };

        MDFType m_mdf_type;

        template <typename MDF>
        static void add_energy_compensation_term(
            const MDF&                  mdf,
            const InputValues*          values,
            const float                 cos_in,
            const float                 cos_on,
            Spectrum&                   value)
        {
            if (values->m_energy_compensation != 0.0f)
            {
                float fms;
                float eavg;
                microfacet_energy_compensation_term(
                    mdf,
                    values->m_roughness,
                    cos_in,
                    cos_on,
                    fms,
                    eavg);

                Spectrum fterm = values->m_precomputed.m_fresnel_average;
                fterm *= 1.0f - eavg;

                const Spectrum one(1.0f);
                fterm /= one - values->m_precomputed.m_fresnel_average * eavg;

                madd(
                    value,
                    fterm,
                    values->m_energy_compensation * values->m_reflectance_multiplier * fms);
            }
        }
    };

    typedef BSDFWrapper<MetalBRDFImpl> MetalBRDF;
}


//
// MetalBRDFFactory class implementation.
//

void MetalBRDFFactory::release()
{
    delete this;
}

const char* MetalBRDFFactory::get_model() const
{
    return Model;
}

Dictionary MetalBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Metal BRDF");
}

DictionaryArray MetalBRDFFactory::get_input_metadata() const
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
                    .insert("STD", "std"))
            .insert("use", "required")
            .insert("default", "ggx"));

    metadata.push_back(
        Dictionary()
            .insert("name", "normal_reflectance")
            .insert("label", "Normal Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.92"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edge_tint")
            .insert("label", "Edge Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.98"));

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
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.15"));

    metadata.push_back(
        Dictionary()
            .insert("name", "highlight_falloff")
            .insert("label", "Highlight Falloff")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.4"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "energy_compensation")
            .insert("label", "Energy Compensation")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<BSDF> MetalBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MetalBRDF(name, params));
}

}   // namespace renderer
