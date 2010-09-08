
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
        GradientEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(params)
          , m_name(name)
        {
            m_inputs.declare("horizon_exitance", InputFormatSpectrum);
            m_inputs.declare("zenith_exitance", InputFormatSpectrum);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return GradientEnvironmentEDFFactory::get_model();
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual void on_frame_begin(const Scene& scene)
        {
            assert(m_inputs.source("horizon_exitance"));
            assert(m_inputs.source("zenith_exitance"));

            check_uniform("horizon_exitance");
            check_uniform("zenith_exitance");

            m_inputs.evaluate_uniforms(&m_values);
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const
        {
            outgoing = sample_sphere_uniform(s);
            compute_gradient(outgoing.y, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const
        {
            assert(is_normalized(outgoing));
            compute_gradient(outgoing.y, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const
        {
            assert(is_normalized(outgoing));
            compute_gradient(outgoing.y, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual double evaluate_pdf(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing) const
        {
            assert(is_normalized(outgoing));
            return 1.0 / (4.0 * Pi);
        }

      private:
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
}


//
// GradientEnvironmentEDFFactory class implementation.
//

const char* GradientEnvironmentEDFFactory::get_model()
{
    return "gradient_environment_edf";
}

auto_release_ptr<EnvironmentEDF> GradientEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new GradientEnvironmentEDF(name, params));
}

}   // namespace renderer
