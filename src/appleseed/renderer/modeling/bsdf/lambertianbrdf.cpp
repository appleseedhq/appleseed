
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "lambertianbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"

// Forward declarations.
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Lambertian BRDF.
    //

    const char* Model = "lambertian_brdf";

    class LambertianBRDFImpl
      : public BSDF
    {
      public:
        LambertianBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
          , m_uniform_reflectance(false)
        {
            set_name(name);

            m_inputs.declare("reflectance", InputFormatSpectrum);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual void on_frame_begin(
            const Project&      project,
            const void*         data)
        {
            if (m_inputs.source("reflectance")->is_uniform())
            {
                m_uniform_reflectance = true;

                const InputValues* values = static_cast<const InputValues*>(data);
                m_brdf_value = values->m_reflectance;
                m_brdf_value *= static_cast<float>(RcpPi);
            }
        }

        virtual void sample(
            const void*         data,
            const bool          adjoint,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     s,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            // Compute the incoming direction in local space.
            const Vector3d wi = sample_hemisphere_cosine(Vector2d(s[0], s[1]));
            assert(wi.y >= 0.0);

            // No reflection in or below the shading surface.
            if (wi.y <= 0.0)
            {
                mode = None;
                return;
            }

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);

            // No reflection in or below the geometric surface.
            const double cos_ig = dot(incoming, geometric_normal);
            if (cos_ig <= 0.0)
            {
                mode = None;
                return;
            }

            // Compute the ratio BRDF/PDF.
            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_reflectance;
            value *= static_cast<float>(1.0 / wi.y);

            // Compute the probability density of the sampled direction.
            probability = wi.y * RcpPi;
            assert(probability > 0.0);

            // Set the scattering mode.
            mode = Diffuse;
        }

        virtual void evaluate(
            const void*         data,
            const bool          adjoint,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            // No reflection in or below the shading surface.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = dot(incoming, shading_normal);
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_in <= 0.0 || cos_on <= 0.0)
            {
                value.set(0.0f);
                return;
            }

            if (m_uniform_reflectance)
                value = m_brdf_value;
            else
            {
                const InputValues* values = static_cast<const InputValues*>(data);
                value = values->m_reflectance;
                value *= static_cast<float>(1.0 / Pi);
            }
        }

        virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            // No reflection in or below the shading surface.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = dot(incoming, shading_normal);
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_in <= 0.0 || cos_on <= 0.0)
                return 0.0;

            return cos_in * RcpPi;
        }

      private:
        struct InputValues
        {
            Spectrum    m_reflectance;          // diffuse reflectance (albedo, technically)
            Alpha       m_reflectance_alpha;    // alpha channel of diffuse reflectance
        };

        bool            m_uniform_reflectance;
        Spectrum        m_brdf_value;           // precomputed value of the BRDF (albedo/Pi)
    };

    typedef BRDFWrapper<LambertianBRDFImpl> LambertianBRDF;
}


//
// LambertianBRDFFactory class implementation.
//

const char* LambertianBRDFFactory::get_model() const
{
    return Model;
}

const char* LambertianBRDFFactory::get_human_readable_model() const
{
    return "Lambertian Reflection";
}

DictionaryArray LambertianBRDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

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

    return definitions;
}

auto_release_ptr<BSDF> LambertianBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<BSDF>(
            new LambertianBRDF(name, params));
}

}   // namespace renderer
