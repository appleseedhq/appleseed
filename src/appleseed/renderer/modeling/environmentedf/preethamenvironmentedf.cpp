
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
#include "preethamenvironmentedf.h"

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
#include <cassert>
#include <cmath>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;

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
    //   http://ompf2.com/viewtopic.php?f=3&t=33
    //

    const char* Model = "preetham_environment_edf";

    // The smallest valid turbidity value.
    const float BaseTurbidity = 2.0f;

    class PreethamEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        PreethamEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("sun_theta", InputFormatFloat);
            m_inputs.declare("sun_phi", InputFormatFloat);
            m_inputs.declare("turbidity", InputFormatFloat);
            m_inputs.declare("turbidity_multiplier", InputFormatFloat, "2.0");
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
            m_cos_sun_theta = std::cos(m_sun_theta);

            // Precompute some stuff if turbidity is uniform.
            m_uniform_turbidity = m_inputs.source("turbidity")->is_uniform();
            if (m_uniform_turbidity)
            {
                // Apply turbidity multiplier and bias.
                m_uniform_values.m_turbidity *= m_uniform_values.m_turbidity_multiplier;
                m_uniform_values.m_turbidity += BaseTurbidity;

                // Precompute the coefficients of the luminance and chromaticity distribution functions.
                compute_x_coefficients(m_uniform_values.m_turbidity, m_uniform_x_coeffs);
                compute_y_coefficients(m_uniform_values.m_turbidity, m_uniform_y_coeffs);
                compute_Y_coefficients(m_uniform_values.m_turbidity, m_uniform_Y_coeffs);

                // Precompute the luminance and chromaticity at zenith.
                m_uniform_x_zenith = compute_zenith_x(m_uniform_values.m_turbidity, m_sun_theta);
                m_uniform_y_zenith = compute_zenith_y(m_uniform_values.m_turbidity, m_sun_theta);
                m_uniform_Y_zenith = compute_zenith_Y(m_uniform_values.m_turbidity, m_sun_theta);
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
            float   m_luminance_multiplier;
            float   m_luminance_gamma;
            float   m_saturation_multiplier;
            float   m_horizon_shift;
        };

        InputValues                 m_uniform_values;

        float                       m_sun_theta;    // sun zenith angle in radians, 0=zenith
        float                       m_sun_phi;      // radians
        Vector3f                    m_sun_dir;
        float                       m_cos_sun_theta;

        bool                        m_uniform_turbidity;
        float                       m_uniform_x_coeffs[5];
        float                       m_uniform_y_coeffs[5];
        float                       m_uniform_Y_coeffs[5];
        float                       m_uniform_x_zenith;
        float                       m_uniform_y_zenith;
        float                       m_uniform_Y_zenith;

        // Compute the coefficients of the luminance distribution function.
        static void compute_Y_coefficients(
            const float             turbidity,
            float                   coeffs[5])
        {
            coeffs[0] =  0.1787f * turbidity - 1.4630f;
            coeffs[1] = -0.3554f * turbidity + 0.4275f;
            coeffs[2] = -0.0227f * turbidity + 5.3251f;
            coeffs[3] =  0.1206f * turbidity - 2.5771f;
            coeffs[4] = -0.0670f * turbidity + 0.3703f;
        }

        // Compute the coefficients of the x chromaticity distribution function.
        static void compute_x_coefficients(
            const float             turbidity,
            float                   coeffs[5])
        {
            coeffs[0] = -0.0193f * turbidity - 0.2592f;
            coeffs[1] = -0.0665f * turbidity + 0.0008f;
            coeffs[2] = -0.0004f * turbidity + 0.2125f;
            coeffs[3] = -0.0641f * turbidity - 0.8989f;
            coeffs[4] = -0.0033f * turbidity + 0.0452f;
        }

        // Compute the coefficients of the y chromaticity distribution function.
        static void compute_y_coefficients(
            const float             turbidity,
            float                   coeffs[5])
        {
            coeffs[0] = -0.0167f * turbidity - 0.2608f;
            coeffs[1] = -0.0950f * turbidity + 0.0092f;
            coeffs[2] = -0.0079f * turbidity + 0.2102f;
            coeffs[3] = -0.0441f * turbidity - 1.6537f;
            coeffs[4] = -0.0109f * turbidity + 0.0529f;
        }

        // Compute the luminance at zenith, in cd.m^-2.
        static float compute_zenith_Y(
            const float             turbidity,
            const float             sun_theta)
        {
            const float chi = ((4.0f / 9.0f) - turbidity / 120.0f) * (Pi<float>() - 2.0f * sun_theta);
            return 1000.0f * ((4.0453f * turbidity - 4.9710f) * std::tan(chi) - 0.2155f * turbidity + 2.4192f);
        }

        // Compute the x chromaticity at zenith.
        static float compute_zenith_x(
            const float             turbidity,
            const float             sun_theta)
        {
            const float a = ( 0.00166f * turbidity - 0.02903f) * turbidity + 0.11693f;
            const float b = (-0.00375f * turbidity + 0.06377f) * turbidity - 0.21196f;
            const float c = ( 0.00209f * turbidity - 0.03202f) * turbidity + 0.06052f;
            const float d = (                        0.00394f) * turbidity + 0.25886f;
            return ((a * sun_theta + b) * sun_theta + c) * sun_theta + d;
        }

        // Compute the y chromaticity at zenith.
        static float compute_zenith_y(
            const float             turbidity,
            const float             sun_theta)
        {
            const float e = ( 0.00275f * turbidity - 0.04214f) * turbidity + 0.15346f;
            const float f = (-0.00610f * turbidity + 0.08970f) * turbidity - 0.26756f;
            const float g = ( 0.00317f * turbidity - 0.04153f) * turbidity + 0.06670f;
            const float h = (                        0.00516f) * turbidity + 0.26688f;
            return ((e * sun_theta + f) * sun_theta + g) * sun_theta + h;
        }

        // Perez formula describing the sky luminance distribution.
        static float perez(
            const float             rcp_cos_theta,
            const float             gamma,
            const float             cos_gamma,
            const float             coeffs[5])
        {
            const float u = 1.0f + coeffs[0] * std::exp(coeffs[1] * rcp_cos_theta);
            const float v = 1.0f + coeffs[2] * std::exp(coeffs[3] * gamma)
                                 + coeffs[4] * cos_gamma * cos_gamma;
            return u * v;
        }

        // Compute one the three quantity defining the sky aspect: the sky luminance Y and the sky chromaticities x and y.
        static float compute_quantity(
            const float             rcp_cos_theta,
            const float             gamma,
            const float             cos_gamma,
            const float             sun_theta,
            const float             cos_sun_theta,
            const float             zenith_val,
            const float             coeffs[5])
        {
            return
                  zenith_val
                * perez(rcp_cos_theta, gamma, cos_gamma, coeffs)
                / perez(1.0f, sun_theta, cos_sun_theta, coeffs);     // todo: build into zenith_val
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

            const float rcp_cos_theta = 1.0f / outgoing.y;
            const float cos_gamma = clamp(dot(outgoing, m_sun_dir), -1.0f, 1.0f);
            const float gamma = std::acos(cos_gamma);

            Color3f xyY;

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

                // Compute the coefficients of the luminance and chromaticity distribution functions.
                float Y_coeffs[5], x_coeffs[5], y_coeffs[5];
                compute_Y_coefficients(turbidity, Y_coeffs);
                compute_x_coefficients(turbidity, x_coeffs);
                compute_y_coefficients(turbidity, y_coeffs);

                // Compute the luminance and chromaticity at zenith.
                const float x_zenith = compute_zenith_x(turbidity, m_sun_theta);
                const float y_zenith = compute_zenith_y(turbidity, m_sun_theta);
                const float Y_zenith = compute_zenith_Y(turbidity, m_sun_theta);

                // Compute the sky color in the xyY color space.
                xyY[0] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, x_zenith, x_coeffs);
                xyY[1] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, y_zenith, y_coeffs);
                xyY[2] = compute_quantity(rcp_cos_theta, gamma, cos_gamma, m_sun_theta, m_cos_sun_theta, Y_zenith, Y_coeffs);
            }

            // Apply an optional saturation correction.
            if (m_uniform_values.m_saturation_multiplier != 1.0f)
            {
                // Convert the sky color: CIE xyY -> CIE XYZ -> linear RGB -> HSL.
                Color3f ciexyz = ciexyy_to_ciexyz(xyY);
                Color3f linear_rgb = ciexyz_to_linear_rgb(ciexyz);
                Color3f hsl = linear_rgb_to_hsl(linear_rgb);

                // Apply the saturation multiplier.
                hsl[1] *= m_uniform_values.m_saturation_multiplier;

                // Convert the result back: HSL -> linear RGB -> CIE XYZ -> CIE xyY.
                linear_rgb = hsl_to_linear_rgb(hsl);
                ciexyz = linear_rgb_to_ciexyz(linear_rgb);
                xyY = ciexyz_to_ciexyy(ciexyz);
            }

            // Split sky color into luminance and chromaticity.
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
// PreethamEnvironmentEDFFactory class implementation.
//

void PreethamEnvironmentEDFFactory::release()
{
    delete this;
}

const char* PreethamEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary PreethamEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Preetham Environment EDF")
            .insert("help", "Physical sky environment");
}

DictionaryArray PreethamEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_sky_input_metadata(metadata);

    add_common_input_metadata(metadata);

    return metadata;
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
