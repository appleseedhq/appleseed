
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
#include "preethamenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace renderer  { class InputEvaluator; }
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // An environment EDF implementing the Preetham day sky model.
    //
    // Reference:
    //
    //   http://www.cs.utah.edu/~shirley/papers/sunsky/sunsky.pdf
    //   http://tommyhinks.files.wordpress.com/2012/02/2007_a_critical_review_of_the_preetham_skylight_model.pdf
    //   http://liveweb.archive.org/http://www.eisscholle.de/articles/daysky.pdf
    //

    const char* Model = "preetham_environment_edf";

    class PreethamEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        PreethamEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(name, params)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
            m_inputs.declare("sun_theta", InputFormatScalar);
            m_inputs.declare("sun_phi", InputFormatScalar);
            m_inputs.declare("turbidity", InputFormatScalar);
            m_inputs.declare("horizon_shift", InputFormatScalar, "0.0");
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

            m_inputs.evaluate_uniforms(&m_values);

            m_sun_theta = deg_to_rad(m_values.m_sun_theta);
            m_sun_phi = deg_to_rad(m_values.m_sun_phi);
            m_sun_dir = Vector3d::unit_vector(m_sun_theta, m_sun_phi);

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
            probability = 1.0 / (4.0 * Pi);
            compute_value(outgoing, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const override
        {
            assert(is_normalized(outgoing));
            compute_value(outgoing, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            assert(is_normalized(outgoing));
            probability = 1.0 / (4.0 * Pi);
            compute_value(outgoing, value);
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
            double  m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
            double  m_sun_phi;                      // degrees
            double  m_turbidity;                    // atmosphere turbidity
            double  m_horizon_shift;
        };

        const LightingConditions    m_lighting_conditions;

        InputValues                 m_values;

        double                      m_sun_theta;    // sun zenith angle in radians, 0=zenith
        double                      m_sun_phi;      // radians
        Vector3d                    m_sun_dir;

        // Compute the zenith luminance, in Kcd/m^2.
        static double compute_zenith_luminance(
            const double        turbidity,
            const double        sun_theta)
        {
            const double xi = ((4.0 / 9.0) - turbidity / 120.0) * (Pi - 2.0 * sun_theta);
            return (4.0453 * turbidity - 4.9710) * tan(xi) - 0.2155 * turbidity + 2.4192;
        }

        // Compute the zenith x and y chromaticities.
        static void compute_zenith_chromacity(
            const double        turbidity,
            const double        sun_theta,
            double              xy[2])
        {
            // Compute turbidity^2, sun_theta^2 and sun_theta^3.
            // todo: precalculate.
            const double turbidity2 = turbidity * turbidity;
            const double sun_theta_2 = sun_theta * sun_theta;
            const double sun_theta_3 = sun_theta * sun_theta_2;

            // Compute x chromaticity.
            const double a =  0.00166 * turbidity2 - 0.02903 * turbidity + 0.11693;
            const double b = -0.00375 * turbidity2 + 0.06377 * turbidity - 0.21196;
            const double c =  0.00209 * turbidity2 - 0.03202 * turbidity + 0.06052;
            const double d =                         0.00394 * turbidity + 0.25886;
            xy[0] = a * sun_theta_3 + b * sun_theta_2 + c * sun_theta + d;

            // Compute y chromaticity.
            const double e =  0.00275 * turbidity2 - 0.04214 * turbidity + 0.15346;
            const double f = -0.00610 * turbidity2 + 0.08970 * turbidity - 0.26756;
            const double g =  0.00317 * turbidity2 - 0.04153 * turbidity + 0.06670;
            const double h =                         0.00516 * turbidity + 0.26688;
            xy[1] = e * sun_theta_3 + f * sun_theta_2 + g * sun_theta + h;
        }

        // Compute the coefficients of the luminance distribution function for a given turbidity value.
        static void compute_luminance_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] =  0.1787 * turbidity - 1.4630;
            coeffs[1] = -0.3554 * turbidity + 0.4275;
            coeffs[2] = -0.0227 * turbidity + 5.3251;
            coeffs[3] =  0.1206 * turbidity - 2.5771;
            coeffs[4] = -0.0670 * turbidity + 0.3703;
        }

        // Compute the coefficients of the x chromaticity distribution function for a given turbidity value.
        static void compute_xchroma_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] = -0.0193 * turbidity - 0.2592;
            coeffs[1] = -0.0665 * turbidity + 0.0008;
            coeffs[2] = -0.0004 * turbidity + 0.2125;
            coeffs[3] = -0.0641 * turbidity - 0.8989;
            coeffs[4] = -0.0033 * turbidity + 0.0452;
        }

        // Compute the coefficients of the y chromaticity distribution function for a given turbidity value.
        static void compute_ychroma_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] = -0.0167 * turbidity - 0.2608;
            coeffs[1] = -0.0950 * turbidity + 0.0092;
            coeffs[2] = -0.0079 * turbidity + 0.2102;
            coeffs[3] = -0.0441 * turbidity - 1.6537;
            coeffs[4] = -0.0109 * turbidity + 0.0529;
        }

        // Perez formula describing the sky luminance distribution.
        static double perez(
            const double        theta,              // viewer zenith angle in radians, 0=zenith
            const double        gamma,              // sun-viewer relative azimuth angle in radians
            const double        coeffs[5])
        {
            const double u = 1.0 + coeffs[0] * exp(coeffs[1] / cos(theta));
            const double v = 1.0 + coeffs[2] * exp(coeffs[3] * gamma) + coeffs[4] * square(cos(gamma));
            return u * v;
        }

        // Compute one the three quantity defining the sky aspect: the sky luminance Y and the sky chromaticities x and y.
        static double compute_quantity(
            const double        theta,              // viewer zenith angle in radians, 0=zenith
            const double        gamma,              // sun-viewer relative azimuth angle in radians
            const double        sun_theta,
            const double        zenith_val,
            const double        coeffs[5])
        {
            return zenith_val * perez(theta, gamma, coeffs) / perez(0.0, sun_theta, coeffs);
        }

        // Compute the RGB sky color.
        static Color3f compute_sky_color(
            double              theta,              // viewer zenith angle in radians, 0=zenith
            const double        gamma,              // sun-viewer relative azimuth angle in radians
            const double        sun_theta,
            const double        turbidity)
        {
            // Compute luminance Y and chromaticities x and y at zenith.
            const double lum_zenith = compute_zenith_luminance(turbidity, sun_theta);
            double xy_zenith[2];
            compute_zenith_chromacity(turbidity, sun_theta, xy_zenith);

            // Compute coefficients of the Y, x and y distribution functions.
            double lum_coeffs[5], x_coeffs[5], y_coeffs[5];
            compute_luminance_coefficients(turbidity, lum_coeffs);
            compute_xchroma_coefficients(turbidity, x_coeffs);
            compute_ychroma_coefficients(turbidity, y_coeffs);

            // Compute luminance Y and chromaticities x and y.
            double lum = compute_quantity(theta, gamma, sun_theta, lum_zenith, lum_coeffs);
            const double x = compute_quantity(theta, gamma, sun_theta, xy_zenith[0], x_coeffs);
            const double y = compute_quantity(theta, gamma, sun_theta, xy_zenith[1], y_coeffs);

            // Scale the luminance value to a valid range.
            lum = 1.0 - exp((-1.0 / 25.0) * lum);

            // Convert Yxy to CIE XYZ tristimulus values.
            Color3f cie_xyz;
            cie_xyz[0] = static_cast<float>(x / y * lum);
            cie_xyz[1] = static_cast<float>(lum);
            cie_xyz[2] = static_cast<float>((1.0 - x - y) / y * lum);

            return cie_xyz;
        }

        void compute_value(
            const Vector3d&     outgoing,
            Spectrum&           value) const
        {
            // Shift the horizon.
            Vector3d shifted_outgoing = outgoing;
            shifted_outgoing.y -= m_values.m_horizon_shift;
            shifted_outgoing = normalize(shifted_outgoing);

            if (shifted_outgoing.y > 0.0)
            {
                const double theta = acos(shifted_outgoing.y);
                const double gamma = acos(dot(shifted_outgoing, m_sun_dir));

                const Color3f cie_xyz =
                    compute_sky_color(theta, gamma, m_sun_theta, m_values.m_turbidity);

                ciexyz_to_spectrum(m_lighting_conditions, cie_xyz, value);
            }
            else
            {
                // The average overall albedo of Earth is about 30% (http://en.wikipedia.org/wiki/Albedo).
                value.set(0.30f);
            }
        }
    };
}


//
// PreethamEnvironmentEDFFactory class implementation.
//

const char* PreethamEnvironmentEDFFactory::get_model() const
{
    return Model;
}

const char* PreethamEnvironmentEDFFactory::get_human_readable_model() const
{
    return "Preetham Environment EDF";
}

DictionaryArray PreethamEnvironmentEDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "sun_theta")
            .insert("label", "Sun Theta Angle")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "45.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "sun_phi")
            .insert("label", "Sun Phi Angle")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "0.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "turbidity")
            .insert("label", "Turbidity")
            .insert("widget", "text_box")
            .insert("use", "required")
            .insert("default", "3.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "horizon_shift")
            .insert("label", "Horizon Shift")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "0.0"));

    return definitions;
}

auto_release_ptr<EnvironmentEDF> PreethamEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new PreethamEnvironmentEDF(name, params));
}

}   // namespace renderer
