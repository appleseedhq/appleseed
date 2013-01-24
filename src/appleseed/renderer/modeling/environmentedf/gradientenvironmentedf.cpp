
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
#include "gradientenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Gradient environment EDF.
    //

    const char* Model = "gradient_environment_edf";

    class GradientEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        GradientEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("horizon_exitance", InputFormatSpectrum);
            m_inputs.declare("zenith_exitance", InputFormatSpectrum);
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual bool on_frame_begin(const Project& project) override
        {
            if (!EnvironmentEDF::on_frame_begin(project))
                return false;

            if (!check_uniform("horizon_exitance") || !check_uniform("zenith_exitance"))
                return false;

            if (is_uniform_zero("horizon_exitance") && is_uniform_zero("zenith_exitance"))
                warn_zero_radiance();

            m_inputs.evaluate_uniforms(&m_values);

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            outgoing = sample_sphere_uniform(s);
            compute_gradient(outgoing.y, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const override
        {
            assert(is_normalized(outgoing));
            compute_gradient(outgoing.y, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            assert(is_normalized(outgoing));
            compute_gradient(outgoing.y, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual double evaluate_pdf(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing) const override
        {
            assert(is_normalized(outgoing));
            return 1.0 / (4.0 * Pi);
        }

      private:
        struct InputValues
        {
            Spectrum    m_horizon_radiance;             // radiance emitted at horizon, in W.m^-2.sr^-1
            Alpha       m_horizon_radiance_alpha;       // unused
            Spectrum    m_zenith_radiance;              // radiance emitted at zenith, in W.m^-2.sr^-1
            Alpha       m_zenith_radiance_alpha;        // unused
        };

        InputValues     m_values;

        void compute_gradient(const double y, Spectrum& output) const
        {
            // Compute the blending factor between the horizon and zenith colors.
            const double angle = acos(abs(y));
            const double blend = angle * (1.0 / HalfPi);

            // Blend the horizon and zenith radiances.
            const float k = static_cast<float>(blend);
            Spectrum horizon_radiance = m_values.m_horizon_radiance;
            horizon_radiance *= k;
            output = m_values.m_zenith_radiance;
            output *= 1.0f - k;
            output += horizon_radiance;
        }
    };
}


//
// GradientEnvironmentEDFFactory class implementation.
//

const char* GradientEnvironmentEDFFactory::get_model() const
{
    return Model;
}

const char* GradientEnvironmentEDFFactory::get_human_readable_model() const
{
    return "Gradient Environment EDF";
}

DictionaryArray GradientEnvironmentEDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "horizon_exitance")
            .insert("label", "Horizon Radiance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "zenith_exitance")
            .insert("label", "Zenith Radiance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", ""));

    return definitions;
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
