
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "velvetbrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/input/inputarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
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
    // Velvet BRDF.
    //
    // References:
    //
    //  [1] http://blog.selfshadow.com/publications/s2013-shading-course/rad/s2013_pbs_rad_notes.pdf
    //

    const char* Model = "velvet_brdf";

    class VelvetBRDFImpl
      : public BSDF
    {
      public:
        VelvetBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("roughness", InputFormatFloat);
            m_inputs.declare("roughness_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("fresnel_normal_reflectance", InputFormatFloat, "1.0");
            m_inputs.declare("fresnel_multiplier", InputFormatFloat, "1.0");
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
            if (!ScatteringMode::has_glossy(modes))
                return;

            const Vector3f& n = sample.m_shading_basis.get_normal();
            const float cos_on = min(dot(sample.m_outgoing.get_value(), n), 1.0f);
            if (cos_on < 0.0f)
                return;

            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2f s = sampling_context.next2<Vector2f>();
            const Vector3f wi = sample_hemisphere_uniform(s);

            // Transform the incoming direction to parent space.
            const Vector3f incoming = sample.m_shading_basis.transform_to_parent(wi);

            // No reflection below the shading surface.
            const float cos_in = dot(incoming, n);
            if (cos_in < 0.0f)
                return;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            eval_velvet(
                values,
                n,
                incoming,
                sample.m_outgoing.get_value(),
                cos_in,
                cos_on,
                sample.m_value);

            // Compute the probability density of the sampled direction.
            sample.m_probability = RcpTwoPi<float>();

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Glossy;

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
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            const float cos_on = min(dot(outgoing, n), 1.0f);
            if (cos_in < 0.0f || cos_on < 0.0f)
                return 0.0f;

            // Compute the BRDF value.
            const InputValues* values = static_cast<const InputValues*>(data);
            eval_velvet(
                values,
                n,
                incoming,
                outgoing,
                cos_in,
                cos_on,
                value);

            // Return the probability density of the sampled direction.
            return RcpTwoPi<float>();
        }

        virtual float evaluate_pdf(
            const void*         data,
            const Vector3f&     geometric_normal,
            const Basis3f&      shading_basis,
            const Vector3f&     outgoing,
            const Vector3f&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0f;

            // No reflection below the shading surface.
            const Vector3f& n = shading_basis.get_normal();
            const float cos_in = dot(incoming, n);
            if (cos_in < 0.0f)
                return 0.0f;

            return RcpTwoPi<float>();
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float       m_roughness;
            float       m_roughness_multiplier;
            Spectrum    m_reflectance;
            float       m_reflectance_multiplier;
            float       m_fresnel_normal_reflectance;
            float       m_fresnel_multiplier;
        };

        void eval_velvet(
            const InputValues*  values,
            const Vector3f&     n,
            const Vector3f&     incoming,
            const Vector3f&     outgoing,
            const float        cos_in,
            const float        cos_on,
            Spectrum&           value) const
        {
            value = values->m_reflectance;
            const Vector3f h = normalize(incoming + outgoing);
            const float roughness = values->m_roughness * values->m_roughness_multiplier;
            const float D = velvet_distribution(dot(n, h), roughness);
            const float denom = velvet_denom(cos_in, cos_on);
            float F = fresnel_factor(values, n, outgoing);
            value *= values->m_reflectance_multiplier * D * F / denom;
        }

        const float velvet_distribution(const float cos_nh, const float roughness) const
        {
            // [1] equation 22.
            const float cos_nh2 = square(cos_nh);
            const float sin2 = 1.0f - cos_nh2;
            const float cot2 = cos_nh2 / sin2;
            const float m2 = max(square(roughness), 0.000001f);
            const float A = 4.0f;
            const float cnorm = 1.0f / (Pi<float>() * (1 + A * m2));
            return cnorm * (1.0f + (A * exp(-cot2 / m2) / square(sin2)));
        }

        const float velvet_denom(const float cos_in, const float cos_on) const
        {
            // [1] equation 23.
            return 4.0f * (cos_in + cos_on - cos_in * cos_on);
        }

        float fresnel_factor(
            const InputValues*  values,
            const Vector3f&     n,
            const Vector3f&     outgoing) const
        {
            float F = 1.0f;
            if (values->m_fresnel_normal_reflectance != 1.0f)
            {
                fresnel_reflectance_dielectric_schlick(
                    F,
                    values->m_fresnel_normal_reflectance,
                    dot(outgoing, n));
            }

            return F * values->m_fresnel_multiplier;
        }
    };

    typedef BSDFWrapper<VelvetBRDFImpl> VelvetBRDF;
}


//
// VelvetBRDFFactory class implementation.
//

const char* VelvetBRDFFactory::get_model() const
{
    return Model;
}

Dictionary VelvetBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Velvet BRDF");
}

DictionaryArray VelvetBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness_multiplier")
            .insert("label", "Roughness Multiplier")
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
            .insert("name", "fresnel_normal_reflectance")
            .insert("label", "Fresnel Normal Reflectance")
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

auto_release_ptr<BSDF> VelvetBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new VelvetBRDF(name, params));
}

auto_release_ptr<BSDF> VelvetBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new VelvetBRDF(name, params));
}

}   // namespace renderer
