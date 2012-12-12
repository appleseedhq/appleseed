
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
#include "renderer/modeling/environmentedf/sphericalcoordinates.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
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
    // References:
    //
    //   http://www.cs.utah.edu/~shirley/papers/sunsky/sunsky.pdf
    //   http://www.cg.tuwien.ac.at/research/publications/2007/zotti-2007-wscg/zotti-2007-wscg-.pdf
    //   http://liveweb.archive.org/http://www.eisscholle.de/articles/daysky.pdf
    //   http://igad2.nhtv.nl/ompf2/viewtopic.php?f=3&t=33
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
            m_inputs.declare("turbidity_min", InputFormatScalar, "2.0");
            m_inputs.declare("turbidity_max", InputFormatScalar, "6.0");
            m_inputs.declare("luminance_multiplier", InputFormatScalar, "0.01");
            m_inputs.declare("saturation_multiplier", InputFormatScalar, "1.0");
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

            // Evaluate uniform values.
            m_inputs.evaluate_uniforms(&m_uniform_values);

            // Compute the sun direction.
            m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
            m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
            m_sun_dir = Vector3d::unit_vector(m_sun_theta, m_sun_phi);
            m_cos_sun_theta = cos(m_sun_theta);

            // Precompute some stuff if turbidity is uniform.
            m_uniform_turbidity = m_inputs.source("turbidity")->is_uniform();
            if (m_uniform_turbidity)
            {
                // Precompute the coefficients of the luminance and chromacity distribution functions.
                compute_Y_coefficients(m_uniform_values.m_turbidity, m_uniform_Y_coeffs);
                compute_x_coefficients(m_uniform_values.m_turbidity, m_uniform_x_coeffs);
                compute_y_coefficients(m_uniform_values.m_turbidity, m_uniform_y_coeffs);

                // Precompute the luminance and chromacity at zenith.
                m_uniform_Y_zenith = compute_zenith_luminance(m_uniform_values.m_turbidity, m_sun_theta);
                m_uniform_x_zenith = compute_zenith_x(m_uniform_values.m_turbidity, m_sun_theta);
                m_uniform_y_zenith = compute_zenith_y(m_uniform_values.m_turbidity, m_sun_theta);
            }

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            outgoing = sample_hemisphere_cosine(s);

            const Vector3d shifted_outgoing = shift(outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_color(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);

            probability = outgoing.y * RcpPi;
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const override
        {
            assert(is_normalized(outgoing));

            const Vector3d shifted_outgoing = shift(outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_color(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const override
        {
            assert(is_normalized(outgoing));

            const Vector3d shifted_outgoing = shift(outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_color(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);

            probability = outgoing.y > 0.0 ? outgoing.y * RcpPi : 0.0;
        }

        virtual double evaluate_pdf(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing) const override
        {
            assert(is_normalized(outgoing));

            return outgoing.y > 0.0 ? outgoing.y * RcpPi : 0.0;
        }

      private:
        struct InputValues
        {
            double  m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
            double  m_sun_phi;                      // degrees
            double  m_turbidity;                    // atmosphere turbidity
            double  m_turbidity_min;
            double  m_turbidity_max;
            double  m_luminance_multiplier;
            double  m_saturation_multiplier;
            double  m_horizon_shift;
        };

        const LightingConditions    m_lighting_conditions;

        InputValues                 m_uniform_values;

        double                      m_sun_theta;    // sun zenith angle in radians, 0=zenith
        double                      m_sun_phi;      // radians
        Vector3d                    m_sun_dir;
        double                      m_cos_sun_theta;

        bool                        m_uniform_turbidity;
        double                      m_uniform_Y_coeffs[5];
        double                      m_uniform_x_coeffs[5];
        double                      m_uniform_y_coeffs[5];
        double                      m_uniform_Y_zenith;
        double                      m_uniform_x_zenith;
        double                      m_uniform_y_zenith;

        // Compute the coefficients of the luminance distribution function.
        static void compute_Y_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] =  0.1787 * turbidity - 1.4630;
            coeffs[1] = -0.3554 * turbidity + 0.4275;
            coeffs[2] = -0.0227 * turbidity + 5.3251;
            coeffs[3] =  0.1206 * turbidity - 2.5771;
            coeffs[4] = -0.0670 * turbidity + 0.3703;
        }

        // Compute the coefficients of the x chromaticity distribution function.
        static void compute_x_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] = -0.0193 * turbidity - 0.2592;
            coeffs[1] = -0.0665 * turbidity + 0.0008;
            coeffs[2] = -0.0004 * turbidity + 0.2125;
            coeffs[3] = -0.0641 * turbidity - 0.8989;
            coeffs[4] = -0.0033 * turbidity + 0.0452;
        }

        // Compute the coefficients of the y chromaticity distribution function.
        static void compute_y_coefficients(
            const double        turbidity,
            double              coeffs[5])
        {
            coeffs[0] = -0.0167 * turbidity - 0.2608;
            coeffs[1] = -0.0950 * turbidity + 0.0092;
            coeffs[2] = -0.0079 * turbidity + 0.2102;
            coeffs[3] = -0.0441 * turbidity - 1.6537;
            coeffs[4] = -0.0109 * turbidity + 0.0529;
        }

        // Compute the luminance at zenith, in Kcd/m^2.
        static double compute_zenith_luminance(
            const double        turbidity,
            const double        sun_theta)
        {
            const double xi = ((4.0 / 9.0) - turbidity / 120.0) * (Pi - 2.0 * sun_theta);
            return (4.0453 * turbidity - 4.9710) * tan(xi) - 0.2155 * turbidity + 2.4192;
        }

        // Compute the x chromaticity at zenith.
        static double compute_zenith_x(
            const double        turbidity,
            const double        sun_theta)
        {
            const double a = ( 0.00166 * turbidity - 0.02903) * turbidity + 0.11693;
            const double b = (-0.00375 * turbidity + 0.06377) * turbidity - 0.21196;
            const double c = ( 0.00209 * turbidity - 0.03202) * turbidity + 0.06052;
            const double d = (                       0.00394) * turbidity + 0.25886;
            return ((a * sun_theta + b) * sun_theta + c) * sun_theta + d;
        }

        // Compute the y chromaticity at zenith.
        static double compute_zenith_y(
            const double        turbidity,
            const double        sun_theta)
        {
            const double e = ( 0.00275 * turbidity - 0.04214) * turbidity + 0.15346;
            const double f = (-0.00610 * turbidity + 0.08970) * turbidity - 0.26756;
            const double g = ( 0.00317 * turbidity - 0.04153) * turbidity + 0.06670;
            const double h = (                       0.00516) * turbidity + 0.26688;
            return ((e * sun_theta + f) * sun_theta + g) * sun_theta + h;
        }

        // Perez formula describing the sky luminance distribution.
        static double perez(
            const double        rcp_cos_theta,
            const double        gamma,
            const double        cos_gamma,
            const double        coeffs[5])
        {
            const double u = 1.0 + coeffs[0] * exp(coeffs[1] * rcp_cos_theta);
            const double v = 1.0 + coeffs[2] * exp(coeffs[3] * gamma)
                                 + coeffs[4] * cos_gamma * cos_gamma;
            return u * v;
        }

        // Compute one the three quantity defining the sky aspect: the sky luminance Y and the sky chromaticities x and y.
        static double compute_quantity(
            const double        rcp_cos_theta,
            const double        gamma,
            const double        cos_gamma,
            const double        sun_theta,
            const double        cos_sun_theta,
            const double        zenith_val,
            const double        coeffs[5])
        {
            return
                  zenith_val
                * perez(rcp_cos_theta, gamma, cos_gamma, coeffs)
                / perez(1.0, sun_theta, cos_sun_theta, coeffs);
        }

        // Compute the sky color in a given direction.
        void compute_sky_color(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const
        {
            const double rcp_cos_theta = 1.0 / outgoing.y;
            const double cos_gamma = dot(outgoing, m_sun_dir);
            const double gamma = acos(cos_gamma);

            Color3d xyY;

            if (m_uniform_turbidity)
            {
                // Compute the sky color in the xyY color space.
                xyY[0] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, m_uniform_x_zenith, m_uniform_x_coeffs);
                xyY[1] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, m_uniform_y_zenith, m_uniform_y_coeffs);
                xyY[2] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, m_uniform_Y_zenith, m_uniform_Y_coeffs);
            }
            else
            {
                // Evaluate turbidity.
                double theta, phi;
                double u, v;
                unit_vector_to_angles(outgoing, theta, phi);
                angles_to_unit_square(theta, phi, u, v);
                const double turbidity =
                    fit(
                        input_evaluator.evaluate<InputValues>(m_inputs, Vector2d(u, v))->m_turbidity,
                        0.0,
                        1.0,
                        m_uniform_values.m_turbidity_min,
                        m_uniform_values.m_turbidity_max);

                // Compute the coefficients of the luminance and chromacity distribution functions.
                double Y_coeffs[5], x_coeffs[5], y_coeffs[5];
                compute_Y_coefficients(turbidity, Y_coeffs);
                compute_x_coefficients(turbidity, x_coeffs);
                compute_y_coefficients(turbidity, y_coeffs);

                // Compute the luminance and chromaticity at zenith.
                const double Y_zenith = compute_zenith_luminance(turbidity, m_sun_theta);
                const double x_zenith = compute_zenith_x(turbidity, m_sun_theta);
                const double y_zenith = compute_zenith_y(turbidity, m_sun_theta);

                // Compute the sky color in the xyY color space.
                xyY[0] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, x_zenith, x_coeffs);
                xyY[1] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, y_zenith, y_coeffs);
                xyY[2] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, Y_zenith, Y_coeffs);
            }

            // Apply the luminance multiplier.
            xyY[2] *= m_uniform_values.m_luminance_multiplier;

            // Convert the sky color to the CIE XYZ color space.
            Color3f ciexyz = ciexyy_to_ciexyz(xyY);

            // Apply an optional saturation correction.
            if (m_uniform_values.m_saturation_multiplier != 1.0)
            {
                // Convert the sky color to linear RGB, then to HSL.
                Color3f linear_rgb = ciexyz_to_linear_rgb(ciexyz);
                Color3f hsl = linear_rgb_to_hsl(linear_rgb);

                // Apply the saturation multiplier.
                hsl[1] *= static_cast<float>(m_uniform_values.m_saturation_multiplier);

                // Convert the result back to linear RGB, then to CIE XYZ.
                linear_rgb = hsl_to_linear_rgb(hsl);
                ciexyz = linear_rgb_to_ciexyz(linear_rgb);
            }

            // Convert the sky color to a spectrum.
            ciexyz_to_spectrum(m_lighting_conditions, ciexyz, value);
        }

        Vector3d shift(Vector3d v) const
        {
            v.y -= m_uniform_values.m_horizon_shift;
            return normalize(v);
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
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "4.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "turbidity_min")
            .insert("label", "Turbidity Min")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "2.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "turbidity_max")
            .insert("label", "Turbidity Max")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "6.0"));

    definitions.push_back(
        Dictionary()
            .insert("name", "luminance_multiplier")
            .insert("label", "Luminance Multiplier")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "0.01"));

    definitions.push_back(
        Dictionary()
            .insert("name", "saturation_multiplier")
            .insert("label", "Saturation Multiplier")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "1.0"));

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
