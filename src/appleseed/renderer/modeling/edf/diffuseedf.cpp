
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
#include "diffuseedf.h"

// appleseed.renderer headers.
#include "renderer/modeling/edf/edf.h"
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
    // Diffuse EDF.
    //

    class DiffuseEDF
      : public EDF
    {
      public:
        // Constructor.
        DiffuseEDF(
            const char*         name,
            const ParamArray&   params)
          : EDF(params)
          , m_name(name)
        {
            m_inputs.declare("exitance", InputFormatSpectrum);
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this EDF.
        virtual const char* get_model() const
        {
            return DiffuseEDFFactory::get_model();
        }

        // Return the name of this EDF.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // Sample the EDF and compute the emission direction, the probability
        // density with which it was chosen and the value of the EDF for this
        // direction.
        virtual void sample(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector2d&     s,                      // sample in [0,1)^2
            Vector3d&           outgoing,               // world space emission direction, unit-length
            Spectrum&           value,                  // EDF value for this direction
            double&             probability) const      // PDF value
        {
            assert(is_normalized(geometric_normal));

            // Compute emission direction.
            outgoing = sample_hemisphere_cosine(s);
            outgoing = shading_basis.transform_to_parent(outgoing);

            // Compute value.
            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_exitance;

            // Compute probability.
            probability = 1.0 / Pi;
        }

        // Evaluate the EDF for a given emission direction.
        virtual void evaluate(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing,               // world space emission direction, unit-length
            Spectrum&           value) const            // EDF value for this direction
        {
            assert(is_normalized(geometric_normal));
            assert(is_normalized(outgoing));

            const InputValues* values = static_cast<const InputValues*>(data);
            value = values->m_exitance;
        }

        // Evaluate the PDF for a given emission direction.
        virtual double evaluate_pdf(
            const void*         data,                   // input values
            const Vector3d&     geometric_normal,       // world space geometric normal, unit-length
            const Basis3d&      shading_basis,          // world space orthonormal basis around shading normal
            const Vector3d&     outgoing) const         // world space emission direction, unit-length
        {
            assert(is_normalized(geometric_normal));
            assert(is_normalized(outgoing));

            return 1.0 / Pi;
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_exitance;                     // radiant exitance, in W.m^-2
            Alpha       m_exitance_alpha;               // alpha channel of radiant exitance
        };

        const string    m_name;
    };

}   // anonymous namespace


//
// DiffuseEDFFactory class implementation.
//

// Return a string identifying this EDF model.
const char* DiffuseEDFFactory::get_model()
{
    return "diffuse_edf";
}

// Create a new diffuse EDF.
auto_release_ptr<EDF> DiffuseEDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<EDF>(
            new DiffuseEDF(name, params));
}

}   // namespace renderer
