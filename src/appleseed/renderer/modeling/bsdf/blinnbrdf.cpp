
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "blinnbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/dual.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Blinn BRDF.
    //

    class FresnelFun
    {
      public:
        explicit FresnelFun(const float eta)
          : m_eta(eta)
        {
        }

        void operator()(
            const Vector3f& o,
            const Vector3f& h,
            const Vector3f& n,
            Spectrum&       value) const
        {
            const float cos_oh = std::abs(dot(o, h));

            float f;
            fresnel_reflectance_dielectric(f, m_eta, cos_oh);

            value.set(f);
        }

      private:
        const float m_eta;
    };

    const char* Model = "blinn_brdf";

    class BlinnBRDFImpl
      : public BSDF
    {
      public:
        BlinnBRDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("exponent", InputFormatFloat, "0.5");
            m_inputs.declare("ior", InputFormatFloat, "1.5");
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
            values->m_precomputed.m_outside_ior = shading_point.get_ray().get_current_ior();
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return;

            const InputValues* values = static_cast<const InputValues*>(data);

            const FresnelFun f(values->m_precomputed.m_outside_ior / values->m_ior);
            MicrofacetBRDFHelper<BlinnMDF>::sample(
                sampling_context,
                1.0f,
                values->m_exponent,
                values->m_exponent,
                f,
                local_geometry,
                outgoing,
                sample);
            sample.m_value.m_beauty = sample.m_value.m_glossy;
            sample.m_min_roughness = 1.0f;
            sample.compute_glossy_reflected_differentials(local_geometry, 1.0f, outgoing);
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);
            const FresnelFun f(values->m_precomputed.m_outside_ior / values->m_ior);

            const float pdf =
                MicrofacetBRDFHelper<BlinnMDF>::evaluate(
                    values->m_exponent,
                    values->m_exponent,
                    f,
                    local_geometry,
                    outgoing,
                    incoming,
                    value.m_glossy);
            assert(pdf >= 0.0f);

            value.m_beauty = value.m_glossy;

            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            const InputValues* values = static_cast<const InputValues*>(data);

            const float pdf =
                MicrofacetBRDFHelper<BlinnMDF>::pdf(
                    values->m_exponent,
                    values->m_exponent,
                    local_geometry,
                    outgoing,
                    incoming);
            assert(pdf >= 0.0f);

            return pdf;
        }

      private:
        typedef BlinnBRDFInputValues InputValues;
    };

    typedef BSDFWrapper<BlinnBRDFImpl> BlinnBRDF;
}


//
// BlinnBRDFFactory class implementation.
//

void BlinnBRDFFactory::release()
{
    delete this;
}

const char* BlinnBRDFFactory::get_model() const
{
    return Model;
}

Dictionary BlinnBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Blinn BRDF");
}

DictionaryArray BlinnBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "exponent")
            .insert("label", "Exponent")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.5"));

    return metadata;
}

auto_release_ptr<BSDF> BlinnBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new BlinnBRDF(name, params));
}

}   // namespace renderer
