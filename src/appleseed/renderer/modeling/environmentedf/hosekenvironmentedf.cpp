
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class InputEvaluator; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Hosek & Wilkie model data.
    //

    #include "renderer/modeling/environmentedf/ArHosekSkyModelData_CIEXYZ.h"


    //
    // An environment EDF implementing the Hosek & Wilkie day sky model.
    //
    // Reference:
    //
    //   http://cgg.mff.cuni.cz/projects/SkylightModelling/
    //

    const char* Model = "hosek_environment_edf";

    // The smallest valid turbidity value.
    const double BaseTurbidity = 2.0;

    class HosekEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        HosekEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
            m_inputs.declare("sun_theta", InputFormatScalar);
            m_inputs.declare("sun_phi", InputFormatScalar);
            m_inputs.declare("turbidity", InputFormatScalar);
            m_inputs.declare("turbidity_multiplier", InputFormatScalar, "2.0");
            m_inputs.declare("ground_albedo", InputFormatScalar, "0.3");
            m_inputs.declare("luminance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("luminance_gamma", InputFormatScalar, "1.0");
            m_inputs.declare("saturation_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("horizon_shift", InputFormatScalar, "0.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!EnvironmentEDF::on_frame_begin(project, abort_switch))
                return false;

            // Evaluate uniform values.
            m_inputs.evaluate_uniforms(&m_uniform_values);

            // Compute the sun direction.
            m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
            m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
            m_sun_dir = Vector3d::make_unit_vector(m_sun_theta, m_sun_phi);

            // Precompute the coefficients of the radiance distribution function and
            // the master luminance value if turbidity is uniform.
            m_uniform_turbidity = m_inputs.source("turbidity")->is_uniform();
            if (m_uniform_turbidity)
            {
                // Apply turbidity multiplier and bias.
                m_uniform_values.m_turbidity *= m_uniform_values.m_turbidity_multiplier;
                m_uniform_values.m_turbidity += BaseTurbidity;

                compute_coefficients(
                    m_uniform_values.m_turbidity,
                    m_uniform_values.m_ground_albedo,
                    m_sun_theta,
                    m_uniform_coeffs,
                    m_uniform_master_Y);
            }

            return true;
        }

        virtual void sample(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const Vector2d&         s,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            const Vector3d local_outgoing = sample_hemisphere_cosine(s);
            probability = local_outgoing.y * RcpPi;

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0, scratch);
            outgoing = transform.vector_to_parent(local_outgoing);

            const Vector3d shifted_outgoing = shift(local_outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_radiance(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const Vector3d&         outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0, scratch);
            const Vector3d local_outgoing = transform.vector_to_local(outgoing);

            const Vector3d shifted_outgoing = shift(local_outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_radiance(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const Vector3d&         outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0, scratch);
            const Vector3d local_outgoing = transform.vector_to_local(outgoing);

            const Vector3d shifted_outgoing = shift(local_outgoing);
            if (shifted_outgoing.y > 0.0)
                compute_sky_radiance(input_evaluator, shifted_outgoing, value);
            else value.set(0.0f);

            probability = local_outgoing.y > 0.0 ? local_outgoing.y * RcpPi : 0.0;
        }

        virtual double evaluate_pdf(
            InputEvaluator&         input_evaluator,
            const Vector3d&         outgoing) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0, scratch);
            const Transformd::MatrixType& parent_to_local = transform.get_parent_to_local();
            const double local_outgoing_y =
                parent_to_local[ 4] * outgoing.x +
                parent_to_local[ 5] * outgoing.y +
                parent_to_local[ 6] * outgoing.z;

            return local_outgoing_y > 0.0 ? local_outgoing_y * RcpPi : 0.0;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            ScalarInput  m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
            ScalarInput  m_sun_phi;                      // degrees
            ScalarInput  m_turbidity;                    // atmosphere turbidity
            ScalarInput  m_turbidity_multiplier;
            ScalarInput  m_ground_albedo;
            ScalarInput  m_luminance_multiplier;
            ScalarInput  m_luminance_gamma;
            ScalarInput  m_saturation_multiplier;
            ScalarInput  m_horizon_shift;
        };

        const LightingConditions    m_lighting_conditions;

        InputValues                 m_uniform_values;

        double                      m_sun_theta;    // sun zenith angle in radians, 0=zenith
        double                      m_sun_phi;      // radians
        Vector3d                    m_sun_dir;

        bool                        m_uniform_turbidity;
        double                      m_uniform_coeffs[3 * 9];
        double                      m_uniform_master_Y[3];

        // Compute the coefficients of the radiance distribution function and the master luminance value.
        static void compute_coefficients(
            const double            turbidity,
            const double            albedo,
            const double            sun_theta,
            double                  coeffs[3 * 9],
            double                  master_Y[3])
        {
            const double clamped_turbidity = clamp(turbidity, 1.0, 10.0) - 1.0;
            const size_t turbidity_low = truncate<size_t>(clamped_turbidity);
            const size_t turbidity_high = min(turbidity_low + 1, size_t(9));
            const double turbidity_interp = clamped_turbidity - turbidity_low;

            // Compute solar elevation.
            const double eta = HalfPi - sun_theta;

            // Transform solar elevation to [0, 1] with more samples for low elevations.
            const double x1 = pow(eta * RcpHalfPi, (1.0 / 3.0));
            const double y1 = 1.0 - x1;

            // Compute the square and cube of x1 and (1 - x1).
            const double x2 = x1 * x1;
            const double x3 = x2 * x1;
            const double y2 = y1 * y1;
            const double y3 = y2 * y1;

            // Coefficients of the quintic Bezier interpolation polynomial.
            const double c0 = y2 * y3;
            const double c1 = 5.0 * x1 * y2 * y2;
            const double c2 = 10.0 * x2 * y3;
            const double c3 = 10.0 * x3 * y2;
            const double c4 = 5.0 * x2 * x2 * y1;
            const double c5 = x2 * x3;

            #define EVALPOLY1(dataset)  \
                (dataset)[ 0] * c0 +    \
                (dataset)[ 9] * c1 +    \
                (dataset)[18] * c2 +    \
                (dataset)[27] * c3 +    \
                (dataset)[36] * c4 +    \
                (dataset)[45] * c5

            #define EVALPOLY2(dataset)  \
                (dataset)[ 0] * c0 +    \
                (dataset)[ 1] * c1 +    \
                (dataset)[ 2] * c2 +    \
                (dataset)[ 3] * c3 +    \
                (dataset)[ 4] * c4 +    \
                (dataset)[ 5] * c5

            for (size_t w = 0; w < 3; ++w)
            {
                for (size_t p = 0; p < 9; ++p)
                {
                    const double clow0  = EVALPOLY1(datasetsXYZ[w] + (0 * 10 + turbidity_low ) * 9 * 6 + p);
                    const double clow1  = EVALPOLY1(datasetsXYZ[w] + (1 * 10 + turbidity_low ) * 9 * 6 + p);
                    const double chigh0 = EVALPOLY1(datasetsXYZ[w] + (0 * 10 + turbidity_high) * 9 * 6 + p);
                    const double chigh1 = EVALPOLY1(datasetsXYZ[w] + (1 * 10 + turbidity_high) * 9 * 6 + p);

                    coeffs[w * 9 + p] =
                        lerp(
                            lerp(clow0, clow1, albedo),
                            lerp(chigh0, chigh1, albedo),
                            turbidity_interp);
                }

                {
                    const double rlow0  = EVALPOLY2(datasetsXYZRad[w] + (0 * 10 + turbidity_low ) * 6);
                    const double rlow1  = EVALPOLY2(datasetsXYZRad[w] + (1 * 10 + turbidity_low ) * 6);
                    const double rhigh0 = EVALPOLY2(datasetsXYZRad[w] + (0 * 10 + turbidity_high) * 6);
                    const double rhigh1 = EVALPOLY2(datasetsXYZRad[w] + (1 * 10 + turbidity_high) * 6);

                    master_Y[w] =
                        lerp(
                            lerp(rlow0, rlow1, albedo),
                            lerp(rhigh0, rhigh1, albedo),
                            turbidity_interp);

                    master_Y[w] *= 1000.0;  // Kcd.m^-2 to cd.m^-2
                }
            }

            #undef EVALPOLY2
            #undef EVALPOLY1
        }

        // Anisotropic term that places a localized glow around the solar point.
        static double chi(
            const double        g,
            const double        cos_alpha)
        {
            const double k = 1.0 + g * g - 2.0 * g * cos_alpha;
            return (1.0 + cos_alpha * cos_alpha) / sqrt(k * k * k);
        }

        // Extended Perez formula.
        static double perez(
            const double        cos_theta,
            const double        sqrt_cos_theta,
            const double        gamma,
            const double        cos_gamma,
            const double        coeffs[9])
        {
            // There is an error in the paper, coeffs[7] (H) and coeffs[8] (I) are reversed.
            const double u = 1.0 + coeffs[0] * exp(coeffs[1] / (cos_theta + 0.01));
            const double v =   coeffs[2]
                             + coeffs[3] * exp(coeffs[4] * gamma)
                             + coeffs[5] * cos_gamma * cos_gamma
                             + coeffs[6] * chi(coeffs[8], cos_gamma)
                             + coeffs[7] * sqrt_cos_theta;
            return u * v;
        }

        // Compute the sky radiance along a given direction.
        void compute_sky_radiance(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const
        {
            if (m_uniform_values.m_luminance_multiplier == 0.0)
            {
                value.set(0.0f);
                return;
            }

            const double sqrt_cos_theta = sqrt(outgoing.y);
            const double cos_gamma = dot(outgoing, m_sun_dir);
            const double gamma = acos(cos_gamma);

            Color3f ciexyz;

            if (m_uniform_turbidity)
            {
                // Compute the sky color in the CIE XYZ color space.
                ciexyz[0] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 0 * 9) * m_uniform_master_Y[0]);
                ciexyz[1] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 1 * 9) * m_uniform_master_Y[1]);
                ciexyz[2] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 2 * 9) * m_uniform_master_Y[2]);
            }
            else
            {
                // Evaluate turbidity.
                double theta, phi;
                double u, v;
                unit_vector_to_angles(outgoing, theta, phi);
                angles_to_unit_square(theta, phi, u, v);
                double turbidity = input_evaluator.evaluate<InputValues>(m_inputs, Vector2d(u, v))->m_turbidity;

                // Apply turbidity multiplier and bias.
                turbidity *= m_uniform_values.m_turbidity_multiplier;
                turbidity += BaseTurbidity;

                // Compute the coefficients of the radiance distribution function and the master luminance value.
                double coeffs[3 * 9], master_Y[3];
                compute_coefficients(
                    turbidity,
                    m_uniform_values.m_ground_albedo,
                    m_sun_theta,
                    coeffs,
                    master_Y);

                // Compute the sky color in the CIE XYZ color space.
                ciexyz[0] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 0 * 9) * master_Y[0]);
                ciexyz[1] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 1 * 9) * master_Y[1]);
                ciexyz[2] = static_cast<float>(perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 2 * 9) * master_Y[2]);
            }

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

            // Split sky color into luminance and chromaticity.
            Color3f xyY = ciexyz_to_ciexyy(ciexyz);
            float luminance = xyY[2];
            RegularSpectrum31f spectrum;
            daylight_ciexy_to_spectrum(xyY[0], xyY[1], spectrum);
            value = spectrum;

            // Apply luminance gamma and multiplier.
            if (m_uniform_values.m_luminance_gamma != 1.0)
                luminance = fast_pow(luminance, static_cast<float>(m_uniform_values.m_luminance_gamma));
            luminance *= static_cast<float>(m_uniform_values.m_luminance_multiplier);

            // Compute the final sky radiance.
            value *=
                  luminance                                         // start with computed luminance
                / sum_value(value * Spectrum(XYZCMFCIE19312Deg[1])) // normalize to unit luminance
                * (1.0f / 683.0f)                                   // convert lumens to Watts
                * static_cast<float>(RcpPi);                        // convert irradiance to radiance
        }

        Vector3d shift(Vector3d v) const
        {
            v.y -= m_uniform_values.m_horizon_shift;
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

Dictionary HosekEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Hosek-Wilkie Environment EDF")
            .insert("default_model", "true")
            .insert("help", "Physical sky environment");
}

DictionaryArray HosekEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_sky_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "ground_albedo")
            .insert("label", "Ground Albedo")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "optional")
            .insert("default", "0.3")
            .insert("help", "Ground albedo (reflection coefficient of the ground)"));

    return metadata;
}

auto_release_ptr<EnvironmentEDF> HosekEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new HosekEnvironmentEDF(name, params));
}

auto_release_ptr<EnvironmentEDF> HosekEnvironmentEDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new HosekEnvironmentEDF(name, params));
}

}   // namespace renderer
