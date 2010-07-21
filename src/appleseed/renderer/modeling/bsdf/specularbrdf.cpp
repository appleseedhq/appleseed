
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Specular BRDF.
    //

    class SpecularBRDFImpl
      : public BSDF
    {
      public:
        // Constructor.
        SpecularBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
          , m_name(name)
        {
            m_inputs.declare("reflectance", InputFormatSpectrum);
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this BSDF.
        virtual const char* get_model() const
        {
            return SpecularBRDFFactory::get_model();
        }

        // Return the name of this BSDF.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // Given an outgoing direction, sample the BSDF and compute the incoming
        // direction, the probability density with which it was chosen, the value
        // of the BSDF divided by the probability density and the scattering mode.
        virtual void sample(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     s,                      // sample in [0,1)^3
            const Vector3d&     outgoing,               // world space outgoing direction, unit-length
            Vector3d&           incoming,               // world space incoming direction, unit-length
            Spectrum&           value,                  // BSDF value divided by PDF value
            double&             probability,            // PDF value
            Mode&               mode) const             // scattering mode
        {
            const Vector3d& shading_normal = shading_basis.get_normal();

            // Compute the incoming direction.
            incoming = reflect(outgoing, shading_normal);

            // Force the incoming direction to be above the geometric surface.
            incoming = force_above_surface(incoming, geometric_normal);

            // No reflection in or below the shading surface.
            const double cos_in = dot(incoming, shading_normal);
            if (cos_in <= 0.0)
            {
                mode = None;
                return;
            }

            // Compute the ratio BRDF/PDF.
            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_reflectance;
            value *= static_cast<float>(1.0 / cos_in);

            // The probability density of the sampled direction is the Dirac delta.
            probability = DiracDelta;

            // Set the scattering mode.
            mode = Specular;
        }

        // Evaluate the BSDF for a given pair of directions.
        virtual void evaluate(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing,               // world space outgoing direction, unit-length
            const Vector3d&     incoming,               // world space incoming direction, unit-length
            Spectrum&           value) const            // BSDF value for this pair of directions
        {
            value.set(0.0f);
        }

        // Evaluate the PDF for a given pair of directions.
        virtual double evaluate_pdf(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing,               // world space outgoing direction, unit-length
            const Vector3d&     incoming) const         // world space incoming direction, unit-length
        {
            return 0.0;
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_reflectance;                  // specular reflectance
            Alpha       m_reflectance_alpha;            // alpha channel of specular reflectance
        };

        const string    m_name;
    };

    typedef BRDFWrapper<SpecularBRDFImpl> SpecularBRDF;

}   // anonymous namespace


//
// SpecularBRDFFactory class implementation.
//

// Return a string identifying this BSDF model.
const char* SpecularBRDFFactory::get_model()
{
    return "specular_brdf";
}

// Create a new specular BRDF.
auto_release_ptr<BSDF> SpecularBRDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<BSDF>(
            new SpecularBRDF(name, params));
}

}   // namespace renderer
