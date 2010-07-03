
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
#include "gradientenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/sampling.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{

    //
    // Gradient environment EDF.
    //

    class GradientEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        // Constructor.
        GradientEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(params)
          , m_name(name)
        {
            m_inputs.declare("horizon_exitance", InputFormatSpectrum);
            m_inputs.declare("zenith_exitance", InputFormatSpectrum);
        }

        // Delete this instance.
        virtual void release()
        {
            delete this;
        }

        // Return a string identifying the model of this EDF.
        virtual const char* get_model() const
        {
            return GradientEnvironmentEDFFactory::get_model();
        }

        // Return the name of this EDF.
        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        // This method is called once before rendering each frame.
        virtual void on_frame_begin(const Scene& scene)
        {
            assert(m_inputs.source("horizon_exitance"));
            assert(m_inputs.source("zenith_exitance"));

            check_uniform("horizon_exitance");
            check_uniform("zenith_exitance");

            m_inputs.evaluate_uniforms(&m_values);
        }

        // Sample the EDF and compute the emission direction, the probability
        // density with which it was chosen and the value of the EDF for this
        // direction.
        virtual void sample(
            InputEvaluator&         input_evaluator,
            const Vector2d&         s,                      // sample in [0,1)^2
            Vector3d&               outgoing,               // world space emission direction, unit-length
            Spectrum&               value,                  // EDF value for this direction
            double&                 probability) const      // PDF value
        {
            // Compute emission direction.
            outgoing = sample_sphere_uniform(s);

            // Compute value.
            compute_gradient(outgoing.y, value);

            // Compute probability.
            probability = 1.0 / (4.0 * Pi);
        }

        // Evaluate the EDF for a given emission direction.
        virtual void evaluate(
            InputEvaluator&         input_evaluator,
            const Vector3d&         outgoing,               // world space emission direction, unit-length
            Spectrum&               value) const            // EDF value for this direction
        {
            assert(is_normalized(outgoing));

            // Compute value.
            compute_gradient(outgoing.y, value);
        }

        // Evaluate the PDF for a given emission direction.
        virtual double evaluate_pdf(
            InputEvaluator&         input_evaluator,
            const Vector3d&         outgoing) const         // world space emission direction, unit-length
        {
            assert(is_normalized(outgoing));

            // Compute probability.
            return 1.0 / (4.0 * Pi);
        }

      private:
        // Input values.
        struct InputValues
        {
            Spectrum    m_horizon_exitance;
            Alpha       m_horizon_exitance_alpha;   // unused
            Spectrum    m_zenith_exitance;
            Alpha       m_zenith_exitance_alpha;    // unused
        };

        const string    m_name;
        InputValues     m_values;

        void check_uniform(const char* input_name) const
        {
            if (!m_inputs.source(input_name)->is_uniform())
            {
                RENDERER_LOG_ERROR(
                    "the \"%s\" input of a \"%s\" must be bound to a scalar or a color",
                    input_name,
                    GradientEnvironmentEDFFactory::get_model());
            }
        }

        void compute_gradient(const double y, Spectrum& output) const
        {
            // Compute the blending factor between the horizon and zenith colors.
            const double angle = acos(abs(y));
            const double blend = angle * (1.0 / HalfPi);

            // Blend the horizon and zenith exitances.
            const float k = static_cast<float>(blend);
            Spectrum horizon_exitance = m_values.m_horizon_exitance;
            horizon_exitance *= k;
            output = m_values.m_zenith_exitance;
            output *= 1.0f - k;
            output += horizon_exitance;
        }
    };

}   // anonymous namespace


//
// GradientEnvironmentEDFFactory class implementation.
//

// Return a string identifying this EDF model.
const char* GradientEnvironmentEDFFactory::get_model()
{
    return "gradient_environment_edf";
}

// Create a new gradient EDF.
auto_release_ptr<EnvironmentEDF> GradientEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new GradientEnvironmentEDF(name, params));
}

}   // namespace renderer
