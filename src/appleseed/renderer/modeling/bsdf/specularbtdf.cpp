
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
#include "specularbtdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/btdfwrapper.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/fresnel.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Specular BTDF.
    //

    class SpecularBTDFImpl
      : public BSDF
    {
      public:
        // Constructor.
        SpecularBTDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
          , m_name(name)
        {
            m_inputs.declare("reflectance", InputFormatSpectrum);
            m_inputs.declare("ior", InputFormatScalar);
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this BSDF.
        virtual const char* get_model() const
        {
            return SpecularBTDFFactory::get_model();
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
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3d& shading_normal = shading_basis.get_normal();

            const bool entering = dot(shading_normal, geometric_normal) > 0.0;

            const double ior_outside = 1.0;             // todo: remove

            // Figure out the incoming and transmitted IOR.
            double ior_i, ior_t;
            if (entering)
            {
                ior_i = ior_outside;
                ior_t = values->m_ior;
            }
            else
            {
                ior_i = values->m_ior;
                ior_t = ior_outside;
            }

            const double eta = ior_i / ior_t;

            const double cos_theta_i = dot(outgoing, shading_normal);
            const double sin_theta_i2 = 1.0 - cos_theta_i * cos_theta_i;
            const double cos_theta_t2 = 1.0 - eta * eta * sin_theta_i2;

            if (cos_theta_t2 < 0.0)
            {
                // Total internal reflection.
                assert(!entering);
                incoming = reflect(outgoing, shading_normal);
                value = values->m_reflectance;
            }
            else
            {
                const double cos_theta_t = sqrt(cos_theta_t2);

                // Compute the Fresnel reflection factor.
                const Spectrum fresnel_reflection =
                    fresnel_reflection_no_polarization(
                        Spectrum(static_cast<Spectrum::ValueType>(ior_i)),
                        Spectrum(static_cast<Spectrum::ValueType>(ior_t)),
                        abs(cos_theta_i),
                        cos_theta_t);

                const float reflection_prob = min(max_value(fresnel_reflection), 1.0f);

                if (s[0] < static_cast<double>(reflection_prob))
                {
                    // Compute the reflected direction.
                    incoming = reflect(outgoing, shading_normal);

                    // Force the incoming direction to be above the geometric surface.
                    incoming = force_above_surface(incoming, geometric_normal);

                    // Compute the reflected radiance.
                    value = values->m_reflectance;
                    value *= fresnel_reflection;
                    value /= reflection_prob;
                }
                else
                {
                    // Compute the refracted direction.
                    incoming =
                        cos_theta_i > 0.0
                            ? (eta * cos_theta_i - cos_theta_t) * shading_normal - eta * outgoing
                            : (eta * cos_theta_i + cos_theta_t) * shading_normal - eta * outgoing;

                    // Compute the refracted radiance.
                    value = values->m_reflectance;
                    value *= Spectrum(1.0f) - fresnel_reflection;
                    value /= 1.0f - reflection_prob;
                }
            }

            // Divide the value of the BTDF by the PDF.
            const double cos_in = abs(dot(incoming, shading_normal));
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
            Spectrum    m_reflectance;                  // specular transmittance
            Alpha       m_reflectance_alpha;            // alpha channel of specular transmittance
            double      m_ior;                          // index of refraction
        };

        const string    m_name;
    };

    typedef public BTDFWrapper<SpecularBTDFImpl> SpecularBTDF;

}   // anonymous namespace


//
// SpecularBTDFFactory class implementation.
//

// Return a string identifying this BSDF model.
const char* SpecularBTDFFactory::get_model()
{
    return "specular_btdf";
}

// Create a new specular BTDF.
auto_release_ptr<BSDF> SpecularBTDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<BSDF>(
            new SpecularBTDF(name, params));
}

}   // namespace renderer
