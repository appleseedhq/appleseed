
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
#include "phongbrdf.h"

// appleseed.renderer headers.
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
    // Phong BRDF.
    //

    class PhongBRDF
      : public BSDF
    {
      public:
        // Constructor.
        PhongBRDF(
            const char*         name,
            const ParamArray&   params)
          : BSDF(params)
          , m_name(name)
        {
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this BSDF.
        virtual const char* get_model() const
        {
            return PhongBRDFFactory::get_model();
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
            // todo: implement.
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
            // todo: implement.
        }

        // Evaluate the PDF for a given pair of directions.
        virtual double evaluate_pdf(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing,               // world space outgoing direction, unit-length
            const Vector3d&     incoming) const         // world space incoming direction, unit-length
        {
            // todo: implement.
            return 0.0;
        }

      private:
        // Input values.
        struct InputValues
        {
        };

        const string    m_name;
    };

}   // anonymous namespace


//
// PhongBRDFFactory class implementation.
//

// Return a string identifying this BSDF model.
const char* PhongBRDFFactory::get_model()
{
    return "phong_brdf";
}

// Create a new Phong BRDF.
auto_release_ptr<BSDF> PhongBRDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<BSDF>(
            new PhongBRDF(name, params));
}

}   // namespace renderer
