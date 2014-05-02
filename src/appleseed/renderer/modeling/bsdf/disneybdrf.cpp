
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "disneybdrf.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    template<class T>
    T mix(const T a, const T b, float t)
    {
        return (a * (1.0f - t)) + (b * t);
    }

    float luminance(
        const LightingConditions& lighting_conditions,
        const Spectrum& s)
    {
        Color3f xyz = spectrum_to_ciexyz<float>(lighting_conditions, s);
        return xyz[1];
    }

    void mix_spectrums(
        const Spectrum&     a,
        const Spectrum&     b,
        float               t,
        Spectrum&           result)
    {
        result = a;
        result *= (1.0f - t);
        Spectrum tmp = b;
        tmp *= t;
        result += tmp;
    }

    double schlick_fresnel(double u)
    {
        double m = std::max(std::min(1.0 - u, 1.0), 0.0);
        double m2 = m * m;
        return m2 * m2 * m; // pow(m,5)
    }

    double smithG_GGX(double Ndotv, double alphaG)
    {
        double a = alphaG * alphaG;
        double b = Ndotv * Ndotv;
        return 1.0 / (Ndotv + std::sqrt(a + b - a * b));
    }
    
    enum Component
    {
        DiffuseComponent,
        SpecularComponent,
        ClearcoatComponent
    };
    
    double get_component_weights(
        const DisneyBRDFInputValues&    values,
        double                          weights[3])
    {
        weights[DiffuseComponent] = 1.0 - values.m_metallic;
        weights[SpecularComponent] = 1.0;
        weights[ClearcoatComponent] = 0.25 * values.m_clearcoat;
        double total_weight =
            weights[DiffuseComponent] +
            weights[SpecularComponent] +
            weights[ClearcoatComponent];

        return total_weight;        
    }
    
    Component choose_component(
        SamplingContext&                sampling_context,
        const DisneyBRDFInputValues&    values)
    {
        double weights[3];
        double total_weight = get_component_weights(values, weights);
        
        // Choose one of the components.
        sampling_context.split_in_place(1, 1);
        const double s = sampling_context.next_double2() * total_weight;
        
        if (s < weights[DiffuseComponent])
            return DiffuseComponent;
        else if ( s < weights[DiffuseComponent] + weights[SpecularComponent])
            return SpecularComponent;
        else
            return ClearcoatComponent;
    }
    
    void diffuse_brdf_value(
        const DisneyBRDFInputValues*    values,
        const Basis3d&                  shading_basis,
        const Vector3d&                 outgoing,
        const Vector3d&                 incoming,
        Spectrum&                       value)
    {
        /*
        Vector3d h(normalize(incoming + outgoing));
        Vector3d n(shading_basis.get_normal());
        
        const double ndotl = dot(n, outgoing);
        const double ndotv = dot(n, incoming);
        const double ldoth = dot(outgoing, h);
        
        value = values->m_base_color;
        
        float fl = schlick_fresnel(ndotl);
        float fv = schlick_fresnel(ndotv);
        float fd90 = 0.0f + 2.0f * ldoth * ldoth * values->m_roughness;
        float fd = mix(1.0f, fd90, fl) * mix(1.0f, fd90, fv);
        
        if (values->m_subsurface > 0.0)
        {
            float fss90 = ldoth * ldoth * values->m_roughness;
            float fss = mix(1.0f, fss90, fl) * mix(1.0f, fss90, fv);
            float ss = 1.25f * (fss * (1.0f / (ndotl + ndotv) - 0.5f) + 0.5f);
            fd = mix(fd, ss, values->m_subsurface);
        }
        
        value *= static_cast<float>(fd * RcpPi);
        
        if (values->m_sheen > 0.0)
        {
            const float clum = luminance(values->m_base_color);
            Spectrum ctint = values->m_base_color;
            
            // In a spectral color space, this is a bit fishy... 
            // Review.
            if (clum <= 0.0f)
                ctint /= clum;
            
            float fh = schlick_fresnel(ldoth);
            Spectrum csheen;            
            mix_spectrums(m_white_spectrum, ctint, static_cast<float>(values->m_sheen_tint), csheen);
            csheen *= static_cast<float>(fh * values->m_sheen);
            value += csheen;
        }

        value *= (1.0 - values->m_metallic);
        */
    }

    void specular_brdf_value(
        const DisneyBRDFInputValues*    values,
        const Basis3d&                  shading_basis,
        const Vector3d&                 outgoing,
        const Vector3d&                 incoming,
        Spectrum&                       value)
    {
        value.set(0.0f);
    }

    void clearcoat_brdf_value(
        const DisneyBRDFInputValues*    values,
        const Basis3d&                  shading_basis,
        const Vector3d&                 outgoing,
        const Vector3d&                 incoming,
        Spectrum&                       value)
    {
        value.set(0.0f);
    }
}

//
// Disney BRDF implementation.
//

namespace
{

const char* Model = "disney_brdf";

}

DisneyBRDFImpl::DisneyBRDFImpl(
    const char*         name,
    const ParamArray&   params)
  : BSDF(name, Reflective, Diffuse | Glossy, params)
{
    m_inputs.declare("base_color", InputFormatSpectralReflectance);
    m_inputs.declare("subsurface", InputFormatScalar, "0.0");
    m_inputs.declare("metallic", InputFormatScalar, "0.0");
    m_inputs.declare("specular", InputFormatScalar, "0.0");
    m_inputs.declare("specular_tint", InputFormatScalar, "0.0");
    m_inputs.declare("anisotropic", InputFormatScalar, "0.0");
    m_inputs.declare("roughness", InputFormatScalar, "0.5");
    m_inputs.declare("sheen", InputFormatScalar, "0.0");
    m_inputs.declare("sheen_tint", InputFormatScalar, "0.0");
    m_inputs.declare("clearcoat", InputFormatScalar, "0.0");
    m_inputs.declare("clearcoat_gloss", InputFormatScalar, "0.0");
    
    m_lighting_conditions = LightingConditions(
                IlluminantCIED65, 
                XYZCMFCIE196410Deg);
    
    linear_rgb_reflectance_to_spectrum(
                Color3f(1.0f, 1.0f, 1.0f),
                m_white_spectrum);
}
    
void DisneyBRDFImpl::release()
{
    delete this;
}

const char* DisneyBRDFImpl::get_model() const
{
    return Model;
}

BSDF::Mode DisneyBRDFImpl::sample(
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
    value.set(0.0f);
    
    const InputValues* values = static_cast<const InputValues*>(data);
    switch (choose_component(sampling_context, *values))
    {
        case DiffuseComponent:
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);

            diffuse_brdf_value(
                        values,
                        shading_basis,
                        outgoing,
                        incoming,
                        value);

            // Compute the probability density of the sampled direction.
            probability = wi.y * RcpPi;
            assert(probability > 0.0);

            // Return the scattering mode.
            return Diffuse;
        }
        break;

        case SpecularComponent:
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);

            // Compute the probability density of the sampled direction.
            probability = wi.y * RcpPi;
            assert(probability > 0.0);

            return Glossy;
        }
        break;

        case ClearcoatComponent:
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);
    
            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);
    
            // Compute the probability density of the sampled direction.
            probability = wi.y * RcpPi;
            assert(probability > 0.0);
    
            return Glossy;
        }
        break;

        assert_otherwise;
    }
}

double DisneyBRDFImpl::evaluate(
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
    // No reflection below the shading surface.
    const Vector3d& n = shading_basis.get_normal();
    const double cos_in = dot(incoming, n);
    const double cos_on = dot(outgoing, n);
    if (cos_in < 0.0 || cos_on < 0.0)
        return 0.0;
    
    const InputValues* values = static_cast<const InputValues*>(data);
    
    // evaluate diffuse + specular + clearcoat, weighted.
    
    /*
    // Compute the BRDF value.
    diffuse_brdf_value(
                values,
                shading_basis,
                outgoing,
                incoming,
                value);
    
    // Return the probability density of the sampled direction.
    return cos_in * RcpPi;
    */
}

double DisneyBRDFImpl::evaluate_pdf(
    const void*         data,
    const Vector3d&     geometric_normal,
    const Basis3d&      shading_basis,
    const Vector3d&     outgoing,
    const Vector3d&     incoming,
    const int           modes) const
{
    // No reflection below the shading surface.
    const Vector3d& n = shading_basis.get_normal();
    const double cos_in = dot(incoming, n);
    const double cos_on = dot(outgoing, n);
    if (cos_in < 0.0 || cos_on < 0.0)
        return 0.0;
    
    // evaluate pdfs
    
    //return cos_in * RcpPi;
}

LightingConditions DisneyBRDFImpl::m_lighting_conditions;
Spectrum DisneyBRDFImpl::m_white_spectrum;

//
// DisneyBRDFFactory class implementation.
//

const char* DisneyBRDFFactory::get_model() const
{
    return Model;
}

const char* DisneyBRDFFactory::get_human_readable_model() const
{
    return "Disney BRDF";
}

DictionaryArray DisneyBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));
    
    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen_tint")
            .insert("label", "Sheen Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat Gloss")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));
    
    return metadata;
}

auto_release_ptr<BSDF> DisneyBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new DisneyBRDF(name, params));
}

}   // namespace renderer
