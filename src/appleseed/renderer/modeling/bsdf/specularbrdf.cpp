
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "specularbrdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Specular BRDF.
    //

    const char* Model = "specular_brdf";

    class SpecularBRDFImpl
      : public BSDF
    {
      public:
        SpecularBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, params, Reflective)
        {
            m_inputs.declare("reflectance", InputFormatSpectrum);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
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
            const Vector3d& shading_normal = shading_basis.get_normal();

            // Compute the incoming direction.
            incoming = reflect(outgoing, shading_normal);

            // Force the incoming direction to be above the geometric surface.
            incoming = force_above_surface(incoming, geometric_normal);

            // Compute the BRDF value.
            const double cos_in = abs(dot(incoming, shading_normal));
            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_reflectance;
            value *= static_cast<float>(values->m_reflectance_multiplier / cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            probability = DiracDelta;

            // Set the scattering mode.
            mode = Specular;
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value) const
        {
            return 0.0;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            return 0.0;
        }

      private:
        struct InputValues
        {
            Spectrum    m_reflectance;              // specular reflectance
            Alpha       m_reflectance_alpha;        // unused
            double      m_reflectance_multiplier;   // specular reflectance multiplier
        };
    };

    typedef BRDFWrapper<SpecularBRDFImpl> SpecularBRDF;
}


//
// SpecularBRDFFactory class implementation.
//

const char* SpecularBRDFFactory::get_model() const
{
    return Model;
}

const char* SpecularBRDFFactory::get_human_readable_model() const
{
    return "Specular BRDF";
}

DictionaryArray SpecularBRDFFactory::get_widget_definitions() const
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

    definitions.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return definitions;
}

auto_release_ptr<BSDF> SpecularBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new SpecularBRDF(name, params));
}

}   // namespace renderer
