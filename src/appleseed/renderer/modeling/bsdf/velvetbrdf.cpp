
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
            m_inputs.declare("roughness", InputFormatScalar);
            m_inputs.declare("roughness_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("fresnel_normal_reflectance", InputFormatScalar, "1.0");
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

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            const Vector3d& n = sample.get_shading_normal();
            const double cos_on = min(dot(sample.m_outgoing.get_value(), n), 1.0);
            if (cos_on < 0.0)
                return;

            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_uniform(s);

            // Transform the incoming direction to parent space.
            const Vector3d incoming = sample.get_shading_basis().transform_to_parent(wi);

            // No reflection below the shading surface.
            const double cos_in = dot(incoming, n);
            if (cos_in < 0.0)
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
            sample.m_probability = RcpTwoPi;

            // Set the scattering mode.
            sample.m_mode = ScatteringMode::Glossy;

            sample.m_incoming = Dual3d(incoming);
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

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = min(dot(outgoing, n), 1.0);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

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
            return RcpTwoPi;
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

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            if (cos_in < 0.0)
                return 0.0;

            return RcpTwoPi;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            ScalarInput m_roughness;
            ScalarInput m_roughness_multiplier;
            Spectrum    m_reflectance;
            ScalarInput m_reflectance_multiplier;
            ScalarInput m_fresnel_normal_reflectance;
            ScalarInput m_fresnel_multiplier;
        };

        void eval_velvet(
            const InputValues*  values,
            const Vector3d&     n,
            const Vector3d&     incoming,
            const Vector3d&     outgoing,
            const double        cos_in,
            const double        cos_on,
            Spectrum&           value) const
        {
            value = values->m_reflectance;
            const Vector3d h = normalize(incoming + outgoing);
            const double roughness = values->m_roughness * values->m_roughness_multiplier;
            const double D = velvet_distribution(dot(n, h), roughness);
            const double denom = velvet_denom(cos_in, cos_on);
            double F = fresnel_factor(values, n, outgoing);
            value *= static_cast<float>(values->m_reflectance_multiplier * D * F / denom);
        }

        const double velvet_distribution(const double cos_nh, const double roughness) const
        {
            // [1] equation 22.
            const double cos_nh2 = square(cos_nh);
            const double sin2 = 1.0 - cos_nh2;
            const double cot2 = cos_nh2 / sin2;
            const double m2 = max(square(roughness), 0.000001);
            const double A = 4.0;
            const double cnorm = 1.0 / (Pi * (1 + A * m2));
            return cnorm * (1.0 + (A * exp(-cot2 / m2) / square(sin2)));
        }

        const double velvet_denom(const double cos_in, const double cos_on) const
        {
            // [1] equation 23.
            return 4.0 * (cos_in + cos_on - cos_in * cos_on);
        }

        double fresnel_factor(
            const InputValues*  values,
            const Vector3d&     n,
            const Vector3d&     outgoing) const
        {
            double F = 1.0;
            if (values->m_fresnel_normal_reflectance != 1.0)
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
