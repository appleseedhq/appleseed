
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/sphericalcoordinates.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;

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
    const float BaseTurbidity = 2.0f;

    class HosekEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        HosekEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("sun_theta", InputFormatFloat);
            m_inputs.declare("sun_phi", InputFormatFloat);
            m_inputs.declare("turbidity", InputFormatFloat);
            m_inputs.declare("turbidity_multiplier", InputFormatFloat, "2.0");
            m_inputs.declare("ground_albedo", InputFormatFloat, "0.3");
            m_inputs.declare("luminance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("luminance_gamma", InputFormatFloat, "1.0");
            m_inputs.declare("saturation_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("horizon_shift", InputFormatFloat, "0.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Evaluate uniform values.
            m_inputs.evaluate_uniforms(&m_uniform_values);

            // Compute the sun direction.
            m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
            m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
            m_sun_dir = Vector3f::make_unit_vector(m_sun_theta, m_sun_phi);

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

        void sample(
            const ShadingContext&   shading_context,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            const Vector3f local_outgoing = sample_hemisphere_cosine(s);

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            outgoing = transform.vector_to_parent(local_outgoing);
            const Vector3f shifted_outgoing = shift(local_outgoing);

            RegularSpectrum31f radiance;
            if (shifted_outgoing.y > 0.0f)
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);
            else radiance.set(0.0f);

            value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
            assert(probability >= 0.0f);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);
            const Vector3f shifted_outgoing = shift(local_outgoing);

            RegularSpectrum31f radiance;
            if (shifted_outgoing.y > 0.0f)
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);
            else radiance.set(0.0f);

            value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);
            const Vector3f shifted_outgoing = shift(local_outgoing);

            RegularSpectrum31f radiance;
            if (shifted_outgoing.y > 0.0f)
                compute_sky_radiance(shading_context, shifted_outgoing, radiance);
            else radiance.set(0.0f);

            value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
            assert(probability >= 0.0f);
        }

        float evaluate_pdf(
            const Vector3f&         outgoing) const override
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);
            const Vector3f shifted_outgoing = shift(local_outgoing);

            const float probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
            assert(probability >= 0.0f);

            return probability;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float   m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
            float   m_sun_phi;                      // degrees
            float   m_turbidity;                    // atmosphere turbidity
            float   m_turbidity_multiplier;
            float   m_ground_albedo;
            float   m_luminance_multiplier;
            float   m_luminance_gamma;
            float   m_saturation_multiplier;
            float   m_horizon_shift;
        };

        InputValues                 m_uniform_values;

        float                       m_sun_theta;    // sun zenith angle in radians, 0=zenith
        float                       m_sun_phi;      // radians
        Vector3f                    m_sun_dir;

        bool                        m_uniform_turbidity;
        float                       m_uniform_coeffs[3 * 9];
        float                       m_uniform_master_Y[3];

        // Compute the coefficients of the radiance distribution function and the master luminance value.
        static void compute_coefficients(
            const float             turbidity,
            const float             albedo,
            const float             sun_theta,
            float                   coeffs[3 * 9],
            float                   master_Y[3])
        {
            const float clamped_turbidity = clamp(turbidity, 1.0f, 10.0f) - 1.0f;
            const size_t turbidity_low = truncate<size_t>(clamped_turbidity);
            const size_t turbidity_high = std::min(turbidity_low + 1, size_t(9));
            const float turbidity_interp = clamped_turbidity - turbidity_low;

            // Compute solar elevation.
            const float eta = HalfPi<float>() - sun_theta;

            // Transform solar elevation to [0, 1] with more samples for low elevations.
            const float x1 = std::pow(eta * RcpHalfPi<float>(), (1.0f / 3.0f));
            const float y1 = 1.0f - x1;

            // Compute the square and cube of x1 and (1 - x1).
            const float x2 = x1 * x1;
            const float x3 = x2 * x1;
            const float y2 = y1 * y1;
            const float y3 = y2 * y1;

            // Coefficients of the quintic Bezier interpolation polynomial.
            const float c0 = y2 * y3;
            const float c1 = 5.0f * x1 * y2 * y2;
            const float c2 = 10.0f * x2 * y3;
            const float c3 = 10.0f * x3 * y2;
            const float c4 = 5.0f * x2 * x2 * y1;
            const float c5 = x2 * x3;

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
                    const float clow0  = EVALPOLY1(datasetsXYZ[w] + (0 * 10 + turbidity_low ) * 9 * 6 + p);
                    const float clow1  = EVALPOLY1(datasetsXYZ[w] + (1 * 10 + turbidity_low ) * 9 * 6 + p);
                    const float chigh0 = EVALPOLY1(datasetsXYZ[w] + (0 * 10 + turbidity_high) * 9 * 6 + p);
                    const float chigh1 = EVALPOLY1(datasetsXYZ[w] + (1 * 10 + turbidity_high) * 9 * 6 + p);

                    coeffs[w * 9 + p] =
                        lerp(
                            lerp(clow0, clow1, albedo),
                            lerp(chigh0, chigh1, albedo),
                            turbidity_interp);
                }

                {
                    const float rlow0  = EVALPOLY2(datasetsXYZRad[w] + (0 * 10 + turbidity_low ) * 6);
                    const float rlow1  = EVALPOLY2(datasetsXYZRad[w] + (1 * 10 + turbidity_low ) * 6);
                    const float rhigh0 = EVALPOLY2(datasetsXYZRad[w] + (0 * 10 + turbidity_high) * 6);
                    const float rhigh1 = EVALPOLY2(datasetsXYZRad[w] + (1 * 10 + turbidity_high) * 6);

                    master_Y[w] =
                        lerp(
                            lerp(rlow0, rlow1, albedo),
                            lerp(rhigh0, rhigh1, albedo),
                            turbidity_interp);

                    master_Y[w] *= 1000.0f;  // Kcd.m^-2 to cd.m^-2
                }
            }

            #undef EVALPOLY2
            #undef EVALPOLY1
        }

        // Anisotropic term that places a localized glow around the solar point.
        static float chi(
            const float             g,
            const float             cos_alpha)
        {
            const float k = 1.0f + g * g - 2.0f * g * cos_alpha;
            return (1.0f + cos_alpha * cos_alpha) / std::sqrt(k * k * k);
        }

        // Extended Perez formula.
        static float perez(
            const float             cos_theta,
            const float             sqrt_cos_theta,
            const float             gamma,
            const float             cos_gamma,
            const float             coeffs[9])
        {
            // There is an error in the paper, coeffs[7] (H) and coeffs[8] (I) are reversed.
            const float u = 1.0f + coeffs[0] * std::exp(coeffs[1] / (cos_theta + 0.01f));
            const float v =   coeffs[2]
                            + coeffs[3] * std::exp(coeffs[4] * gamma)
                            + coeffs[5] * cos_gamma * cos_gamma
                            + coeffs[6] * chi(coeffs[8], cos_gamma)
                            + coeffs[7] * sqrt_cos_theta;
            return u * v;
        }

        // Compute the sky radiance along a given direction.
        void compute_sky_radiance(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            RegularSpectrum31f&     radiance) const
        {
            if (m_uniform_values.m_luminance_multiplier == 0.0f)
            {
                radiance.set(0.0f);
                return;
            }

            const float sqrt_cos_theta = std::sqrt(outgoing.y);
            const float cos_gamma = dot(outgoing, m_sun_dir);
            const float gamma = std::acos(cos_gamma);

            Color3f ciexyz;

            if (m_uniform_turbidity)
            {
                // Compute the sky color in the CIE XYZ color space.
                ciexyz[0] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 0 * 9) * m_uniform_master_Y[0];
                ciexyz[1] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 1 * 9) * m_uniform_master_Y[1];
                ciexyz[2] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, m_uniform_coeffs + 2 * 9) * m_uniform_master_Y[2];
            }
            else
            {
                // Evaluate turbidity.
                float theta, phi;
                float u, v;
                unit_vector_to_angles(outgoing, theta, phi);
                angles_to_unit_square(theta, phi, u, v);
                InputValues values;
                m_inputs.evaluate(shading_context.get_texture_cache(), SourceInputs(Vector2f(u, v)), &values);
                float turbidity = values.m_turbidity;

                // Apply turbidity multiplier and bias.
                turbidity *= m_uniform_values.m_turbidity_multiplier;
                turbidity += BaseTurbidity;

                // Compute the coefficients of the radiance distribution function and the master luminance value.
                float coeffs[3 * 9], master_Y[3];
                compute_coefficients(
                    turbidity,
                    m_uniform_values.m_ground_albedo,
                    m_sun_theta,
                    coeffs,
                    master_Y);

                // Compute the sky color in the CIE XYZ color space.
                ciexyz[0] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 0 * 9) * master_Y[0];
                ciexyz[1] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 1 * 9) * master_Y[1];
                ciexyz[2] = perez(outgoing.y, sqrt_cos_theta, gamma, cos_gamma, coeffs + 2 * 9) * master_Y[2];
            }

            // Apply an optional saturation correction.
            if (m_uniform_values.m_saturation_multiplier != 1.0f)
            {
                // Convert the sky color to linear RGB, then to HSL.
                Color3f linear_rgb = ciexyz_to_linear_rgb(ciexyz);
                Color3f hsl = linear_rgb_to_hsl(linear_rgb);

                // Apply the saturation multiplier.
                hsl[1] *= m_uniform_values.m_saturation_multiplier;

                // Convert the result back to linear RGB, then to CIE XYZ.
                linear_rgb = hsl_to_linear_rgb(hsl);
                ciexyz = linear_rgb_to_ciexyz(linear_rgb);
            }

            // Split sky color into luminance and chromaticity.
            const Color3f xyY = ciexyz_to_ciexyy(ciexyz);
            float luminance = xyY[2];
            daylight_ciexy_to_spectrum(xyY[0], xyY[1], radiance);

            // Apply luminance gamma and multiplier.
            if (m_uniform_values.m_luminance_gamma != 1.0f)
                luminance = fast_pow(luminance, m_uniform_values.m_luminance_gamma);
            luminance *= m_uniform_values.m_luminance_multiplier;

            // Compute the final sky radiance.
            radiance *=
                  luminance                                         // start with computed luminance
                / sum_value(radiance * XYZCMFCIE19312Deg[1])        // normalize to unit luminance
                * (1.0f / 683.0f)                                   // convert lumens to Watts
                * RcpPi<float>();                                   // convert irradiance to radiance
        }

        Vector3f shift(Vector3f v) const
        {
            v.y -= m_uniform_values.m_horizon_shift;
            return normalize(v);
        }
    };
}


//
// HosekEnvironmentEDFFactory class implementation.
//

void HosekEnvironmentEDFFactory::release()
{
    delete this;
}

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
            .insert("help", "Physical sky environment")
            .insert("default_model", "true");
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
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "0.3")
            .insert("help", "Ground albedo (reflection coefficient of the ground)"));

    add_common_input_metadata(metadata);

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

}   // namespace renderer
