
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
#include "hosekenvironmentedf.h"

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
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

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
    // Hosek & Wilkie model data.
    //

    #include "renderer/modeling/environmentedf/ArHosekSkyModelData.h"


    //
    // An environment EDF implementing the Hosek & Wilkie day sky model.
    //
    // Reference:
    //
    //   http://cgg.mff.cuni.cz/projects/SkylightModelling/
    //

    const char* Model = "hosek_environment_edf";

    class HosekEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        HosekEnvironmentEDF(
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
            m_inputs.declare("ground_albedo", InputFormatScalar, "0.3");
            m_inputs.declare("luminance_multiplier", InputFormatScalar, "1.0");
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

            m_inputs.evaluate_uniforms(&m_values);

            // Compute the sun direction.
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
            double  m_ground_albedo;
            double  m_luminance_multiplier;
            double  m_saturation_multiplier;
            double  m_horizon_shift;
        };

        const LightingConditions    m_lighting_conditions;

        InputValues                 m_values;

        double                      m_sun_theta;    // sun zenith angle in radians, 0=zenith
        double                      m_sun_phi;      // radians
        Vector3d                    m_sun_dir;

        static void compute_coefficients(
            const double            turbidity,
            const double            albedo,
            const double            sun_theta,
            double                  coeffs[3 * 9],
            double                  masterlum[3])
        {
            const double clamped_turbidity = clamp(turbidity, 1.0, 10.0) - 1.0;
            const size_t turbidity_low = truncate<size_t>(clamped_turbidity);
            const size_t turbidity_high = min(turbidity_low + 1, size_t(9));
            const double turbidity_interp = clamped_turbidity - turbidity_low;

            for (size_t w = 0; w < 3; ++w)
            {
                for (size_t p = 0; p < 9; ++p)
                {
                    coeffs[w * 9 + p] =
                        lerp(
                            lerp(
                                interpolate(datasetsXYZ[w] + ((0 * 10 + turbidity_low) * 9) * 6 + p, sun_theta),
                                interpolate(datasetsXYZ[w] + ((1 * 10 + turbidity_low) * 9) * 6 + p, sun_theta),
                                albedo),
                            lerp(
                                interpolate(datasetsXYZ[w] + ((0 * 10 + turbidity_high) * 9) * 6 + p, sun_theta),
                                interpolate(datasetsXYZ[w] + ((1 * 10 + turbidity_high) * 9) * 6 + p, sun_theta),
                                albedo),
                            turbidity_interp);
                }

                masterlum[w] =
                    lerp(
                        lerp(
                            interpolate_radiance(datasetsXYZRad[w] + (0 * 10 + turbidity_low) * 6, sun_theta),
                            interpolate_radiance(datasetsXYZRad[w] + (1 * 10 + turbidity_low) * 6, sun_theta),
                            albedo),
                        lerp(
                            interpolate_radiance(datasetsXYZRad[w] + (0 * 10 + turbidity_high) * 6, sun_theta),
                            interpolate_radiance(datasetsXYZRad[w] + (1 * 10 + turbidity_high) * 6, sun_theta),
                            albedo),
                        turbidity_interp);
            }
        }

        // Compute the coefficients of the radiance distribution function.
        static void compute_radiance_coefficients(
            const double*           dataset,
            const size_t            turbidity,
            const size_t            albedo,
            const double            sun_theta,
            double                  coeffs[9])
        {
            assert(turbidity < 10);
            assert(albedo < 2);

        }

        static double interpolate(
            const double*           params,
            const double            sun_theta)
        {
            const double eta = HalfPi - sun_theta;
            const double x1 = pow(eta * RcpHalfPi, (1.0 / 3.0));
            const double x2 = x1 * x1;
            const double x3 = x2 * x1;
            const double x4 = x2 * x2;
            const double x5 = x2 * x3;

            const double y1 = 1.0 - x1;
            const double y2 = y1 * y1;
            const double y3 = y2 * y1;
            const double y4 = y2 * y2;
            const double y5 = y2 * y3;

            return
                params[ 0] * y5 +
                params[ 9] * 5.0 * x1 * y4 +
                params[18] * 10.0 * x2 * y3 +
                params[27] * 10.0 * x3 * y2 +
                params[36] * 5.0 * x4 * y1 +
                params[45] * x5;
        }

        static double interpolate_radiance(
            const double*           params,
            const double            sun_theta)
        {
            const double eta = HalfPi - sun_theta;
            const double x1 = pow(eta * RcpHalfPi, (1.0 / 3.0));
            const double x2 = x1 * x1;
            const double x3 = x2 * x1;
            const double x4 = x2 * x2;
            const double x5 = x2 * x3;

            const double y1 = 1.0 - x1;
            const double y2 = y1 * y1;
            const double y3 = y2 * y1;
            const double y4 = y2 * y2;
            const double y5 = y2 * y3;

            return
                params[0] * y5 +
                params[1] * 5.0 * x1 * y4 +
                params[2] * 10.0 * x2 * y3 +
                params[3] * 10.0 * x3 * y2 +
                params[4] * 5.0 * x4 * y1 +
                params[5] * x5;
        }

        // Anisotropic term that places a localized glow around the solar point.
        static double xi(
            const double        g,
            const double        cos_alpha)
        {
            return (1.0 + cos_alpha * cos_alpha) / pow(1.0 + g * g - 2.0 * g * cos_alpha, (3.0 / 2.0));
        }

        // Extended Perez formula.
        static double perez(
            const double        cos_theta,
            const double        gamma,
            const double        cos_gamma,
            const double        coeffs[9])
        {
            // There is an error in the paper, coeffs[7] (H) and coeffs[8] (I) are reversed.
            const double u = 1.0 + coeffs[0] * exp(coeffs[1] / (cos_theta + 0.01));
            const double v =   coeffs[2]
                             + coeffs[3] * exp(coeffs[4] * gamma)
                             + coeffs[5] * cos_gamma * cos_gamma
                             + coeffs[6] * xi(coeffs[8], cos_gamma)
                             + coeffs[7] * sqrt(cos_theta);
            return u * v;
        }

        // Compute the sky color in a given direction.
        void compute_sky_color(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const
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
                    m_values.m_turbidity_min,
                    m_values.m_turbidity_max);

            // Compute the coefficients of the radiance distribution function and the master luminance value.
            double coeffs[3 * 9], masterlum[3];
            compute_coefficients(
                turbidity,
                m_values.m_ground_albedo,
                m_sun_theta,
                coeffs,
                masterlum);

            const double cos_gamma = dot(outgoing, m_sun_dir);
            const double gamma = acos(cos_gamma);

            // Compute the sky color in the CIE XYZ color space.
            Color3f ciexyz;
            ciexyz[0] = static_cast<float>(perez(outgoing.y, gamma, cos_gamma, coeffs + 0 * 9) * masterlum[0]);
            ciexyz[1] = static_cast<float>(perez(outgoing.y, gamma, cos_gamma, coeffs + 1 * 9) * masterlum[1]);
            ciexyz[2] = static_cast<float>(perez(outgoing.y, gamma, cos_gamma, coeffs + 2 * 9) * masterlum[2]);

            // Apply the luminance multiplier.
            ciexyz *= static_cast<float>(m_values.m_luminance_multiplier);

            // Then convert it to linear RGB, then to HSL.
            Color3f linear_rgb = ciexyz_to_linear_rgb(ciexyz);
            Color3f hsl = linear_rgb_to_hsl(linear_rgb);

            // Apply the saturation multiplier.
            hsl[1] *= static_cast<float>(m_values.m_saturation_multiplier);

            // Back to linear RGB, then to CIE XYZ.
            linear_rgb = hsl_to_linear_rgb(hsl);
            ciexyz = linear_rgb_to_ciexyz(linear_rgb);

            // And finally convert the sky color to a spectrum.
            ciexyz_to_spectrum(m_lighting_conditions, ciexyz, value);
        }

        Vector3d shift(Vector3d v) const
        {
            v.y -= m_values.m_horizon_shift;
            return normalize(v);
        }
    };
}


//
// HosekEnvironmentEDFFactory class implementation.
//

const char* HosekEnvironmentEDFFactory::get_model() const
{
    return Model;
}

const char* HosekEnvironmentEDFFactory::get_human_readable_model() const
{
    return "Hosek Environment EDF";
}

DictionaryArray HosekEnvironmentEDFFactory::get_widget_definitions() const
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
            .insert("name", "ground_albedo")
            .insert("label", "Ground Albedo")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "0.3"));

    definitions.push_back(
        Dictionary()
            .insert("name", "luminance_multiplier")
            .insert("label", "Luminance Multiplier")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "1.0"));

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

auto_release_ptr<EnvironmentEDF> HosekEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new HosekEnvironmentEDF(name, params));
}

}   // namespace renderer
