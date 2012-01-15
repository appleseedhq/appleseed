
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "foundation/utility/containers/specializedarrays.h"

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
          : BSDF(name, params)
          , m_uniform_reflectance(false)
        {
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
            const Assembly&     assembly,
            const void*         uniform_data)
        {
            BSDF::on_frame_begin(project, assembly, uniform_data);

            if (m_inputs.source("reflectance")->is_uniform())
            {
                m_uniform_reflectance = true;

                const InputValues* values = static_cast<const InputValues*>(uniform_data);
                m_brdf_value = values->m_reflectance;
                m_brdf_value *= static_cast<float>(RcpPi);
            }
        }

        FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            // Compute the incoming direction in local space.
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next_vector2<2>();
            const Vector3d wi = sample_hemisphere_cosine(s);

            // Transform the incoming direction to parent space.
            incoming = shading_basis.transform_to_parent(wi);

            // No reflection in or below the geometric surface.
            const double cos_ig = dot(incoming, geometric_normal);
            if (cos_ig <= 0.0)
            {
                mode = None;
                return;
            }

            // Compute the BRDF value.
            if (m_uniform_reflectance)
                value = m_brdf_value;
            else
            {
                const InputValues* values = static_cast<const InputValues*>(data);
                value = values->m_reflectance;
                value *= static_cast<float>(RcpPi);
            }

            // Compute the probability density of the sampled direction.
            probability = wi.y * RcpPi;
            assert(probability > 0.0);

            // Set the scattering mode.
            mode = Diffuse;
        }

        FORCE_INLINE virtual bool evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value,
            double*             probability) const
        {
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);

            // No reflection in or below the shading surface.
            if (cos_in <= 0.0 || cos_on <= 0.0)
                return false;

            // Compute the BRDF value.
            if (m_uniform_reflectance)
                value = m_brdf_value;
            else
            {
                const InputValues* values = static_cast<const InputValues*>(data);
                value = values->m_reflectance;
                value *= static_cast<float>(RcpPi);
            }

            // Compute the probability density of the sampled direction.
            if (probability)
                *probability = cos_in * RcpPi;

            return true;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);

            // No reflection in or below the shading surface.
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
    return "Lambertian BRDF";
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
    return auto_release_ptr<BSDF>(new LambertianBRDF(name, params));
}

}   // namespace renderer
