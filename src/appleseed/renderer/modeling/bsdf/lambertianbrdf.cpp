
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
#include "lambertianbrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/brdfwrapper.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Lambertian BRDF.
    //

    class LambertianBRDFImpl
      : public BSDF
    {
      public:
        // Constructor.
        LambertianBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
          , m_name(name)
          , m_uniform_reflectance(false)
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
            return LambertianBRDFFactory::get_model();
        }

        // Return the name of this BSDF.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // This method is called once before rendering each frame.
        virtual void on_frame_begin(
            const Scene&        scene,
            const void*         data)                   // input values
        {
            if (m_inputs.source("reflectance")->is_uniform())
            {
                m_uniform_reflectance = true;

                // Precompute the value of the BRDF.
                const InputValues* values = static_cast<const InputValues*>(data);
                m_brdf_value = values->m_reflectance;
                m_brdf_value *= static_cast<float>(1.0 / Pi);
            }
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
            probability = wi.y * (1.0 / Pi);
            assert(probability > 0.0);

            // Set the scattering mode.
            mode = Diffuse;
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

        // Evaluate the PDF for a given pair of directions.
        virtual double evaluate_pdf(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing,               // world space outgoing direction, unit-length
            const Vector3d&     incoming) const         // world space incoming direction, unit-length
        {
            // No reflection in or below the shading surface.
            const Vector3d& shading_normal = shading_basis.get_normal();
            const double cos_in = dot(incoming, shading_normal);
            const double cos_on = dot(outgoing, shading_normal);
            if (cos_in <= 0.0 || cos_on <= 0.0)
                return 0.0;

            return cos_in * (1.0 / Pi);
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_reflectance;                  // diffuse reflectance
            Alpha       m_reflectance_alpha;            // alpha channel of diffuse reflectance
        };

        const string    m_name;
        bool            m_uniform_reflectance;
        Spectrum        m_brdf_value;                   // precomputed value of the BRDF
    };

    typedef public BRDFWrapper<LambertianBRDFImpl> LambertianBRDF;

}   // anonymous namespace


//
// LambertianBRDFFactory class implementation.
//

// Return a string identifying this BSDF model.
const char* LambertianBRDFFactory::get_model()
{
    return "lambertian_brdf";
}

// Create a new Lambertian BRDF.
auto_release_ptr<BSDF> LambertianBRDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<BSDF>(
            new LambertianBRDF(name, params));
}

}   // namespace renderer
