
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "disneydiffusebdrf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"
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
    //
    // DisneyDiffuse BRDF.
    //

    const char* Model = "disneydiffuse_brdf";

    class DisneyDiffuseBRDFImpl
      : public BSDF
    {
      public:
        DisneyDiffuseBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, Diffuse, params)
        {
            m_inputs.declare("base_color", InputFormatSpectralReflectance);
            m_inputs.declare("subsurface", InputFormatScalar, "0.0");
            m_inputs.declare("roughness", InputFormatScalar, "0.5");
            m_inputs.declare("sheen", InputFormatScalar, "0.0");
            m_inputs.declare("sheen_tint", InputFormatScalar, "0.0");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
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
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);

            // Compute the BRDF value.
            brdf_value(
                data,
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

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            // Compute the BRDF value.
            brdf_value(
                data,
                shading_basis,
                outgoing,
                incoming,
                value);

            // Return the probability density of the sampled direction.
            return cos_in * RcpPi;
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

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;
            
            return cos_in * RcpPi;
        }

      private:
        typedef DisneyDiffuseBRDFInputValues InputValues;
        
        template<class T>
        T mix(const T a, const T b, float t) const
        {
            return (a * (1.0f - t)) + (b * t);
        }
        
        float luminance(const Spectrum& s) const
        {
            float l = 0.0f;
            
            for (int i = 0; i < Spectrum::Samples; ++i)
                l += s[i];
            
            return l / static_cast<float>(Spectrum::Samples);
        }
        
        void mix_spectrums(
            const Spectrum&     a,
            const Spectrum&     b,
            float               t,
            Spectrum&           result) const
        {
            result = a;
            result *= (1.0f - t);
            Spectrum tmp = b;
            tmp *= t;
            result += tmp;
        }
        
        float schlick_fresnel(float u) const
        {
            float m = std::max(std::min(1.0f - u, 1.0f), 0.0f);
            float m2 = m * m;
            return m2 * m2 * m; // pow(m,5)
        }
        
        void brdf_value(
            const void*         data,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            Vector3d h(normalize(incoming + outgoing));
            Vector3d n(shading_basis.get_normal());
            
            const double ndotl = dot(n, outgoing);
            const double ndotv = dot(n, incoming);
            const double ldoth = dot(outgoing, h);
            
            const InputValues* values = static_cast<const InputValues*>(data);
            
            const float clum = luminance(values->m_base_color);
            Spectrum ctint = values->m_base_color;
            
            if (clum <= 0.0f)            
                ctint /= clum;

            float fl = schlick_fresnel(ndotl);
            float fv = schlick_fresnel(ndotv);
            float fd90 = 0.0f + 2.0f * ldoth * ldoth * values->m_roughness;
            float fd = mix(1.0f, fd90, fl) * mix(1.0f, fd90, fv);
            
            float fss90 = ldoth * ldoth * values->m_roughness;
            float fss = mix(1.0f, fss90, fl) * mix(1.0f, fss90, fv);
            float ss = 1.25f * (fss * (1.0f / (ndotl + ndotv) - 0.5f) + 0.5f);

            float fh = schlick_fresnel(ldoth);
            const Spectrum one(1.0f);
            Spectrum csheen;            
            mix_spectrums(one, ctint, static_cast<float>(values->m_sheen_tint), csheen);
            csheen *= static_cast<float>(fh * values->m_sheen);

            value = values->m_base_color;
            value *= static_cast<float>(mix(fd, ss, values->m_subsurface) * RcpPi);
            value += csheen;
        }
    };

    typedef BSDFWrapper<DisneyDiffuseBRDFImpl> DisneyDiffuseBRDF;
}


//
// DisneyDiffuseBRDFFactory class implementation.
//

const char* DisneyDiffuseBRDFFactory::get_model() const
{
    return Model;
}

const char* DisneyDiffuseBRDFFactory::get_human_readable_model() const
{
    return "DisneyDiffuse BRDF";
}

DictionaryArray DisneyDiffuseBRDFFactory::get_input_metadata() const
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

    return metadata;
}

auto_release_ptr<BSDF> DisneyDiffuseBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new DisneyDiffuseBRDF(name, params));
}

}   // namespace renderer
