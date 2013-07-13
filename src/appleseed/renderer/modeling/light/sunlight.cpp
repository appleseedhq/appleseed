
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "sunlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Physically-based sun light.
    //
    // References:
    //
    //   http://www.cs.utah.edu/~shirley/papers/sunsky/sunsky.pdf
    //   http://igad2.nhtv.nl/ompf2/viewtopic.php?f=3&t=33
    //

    const char* Model = "sun_light";

    class SunLight
      : public Light
    {
      public:
        SunLight(
            const char*             name,
            const ParamArray&       params)
          : Light(name, params)
        {
            m_inputs.declare("environment_edf", InputFormatEntity, "");
            m_inputs.declare("turbidity", InputFormatScalar);
            m_inputs.declare("radiance_multiplier", InputFormatScalar, "1.0");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const Assembly&         assembly) OVERRIDE
        {
            if (!Light::on_frame_begin(project, assembly))
                return false;

            m_inputs.evaluate_uniforms(&m_values);
            m_outgoing = normalize(get_transform().vector_to_parent(Vector3d(0.0, 0.0, -1.0)));

            // If the sun light is bound to an environment EDF, let it override the sun direction and turbidity.
            const EnvironmentEDF* env_edf = static_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
            if (env_edf)
                apply_env_edf_overrides(env_edf, m_outgoing, m_values.m_turbidity);

            m_basis.build(m_outgoing);

            compute_sun_radiance(
                m_outgoing,
                m_values.m_turbidity,
                m_values.m_radiance_multiplier,
                m_radiance);

            m_safe_scene_radius = project.get_scene()->compute_radius();

            return true;
        }

        virtual void sample(
            InputEvaluator&         input_evaluator,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const OVERRIDE
        {
            const Vector2d point_on_disk = sample_disk_uniform(s);
            position =
                m_basis.transform_to_parent(
                    m_safe_scene_radius * Vector3d(point_on_disk[0], -1.0, point_on_disk[1]));
            outgoing = m_outgoing;
            value = m_radiance;
            probability = RcpPi;
        }

        virtual void evaluate(
            InputEvaluator&         input_evaluator,
            const Vector3d&         target,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value) const OVERRIDE
        {
            position = target - m_safe_scene_radius * m_outgoing;
            outgoing = m_outgoing;
            value = m_radiance;
        }

      private:
        DECLARE_INPUT_VALUES(InputValues)
        {
            double      m_turbidity;                // atmosphere turbidity
            double      m_radiance_multiplier;      // emitted radiance multiplier
        };

        InputValues     m_values;
        Vector3d        m_outgoing;                 // world space
        Basis3d         m_basis;                    // world space
        Spectrum        m_radiance;
        double          m_safe_scene_radius;        // world space

        static void apply_env_edf_overrides(
            const EnvironmentEDF*   env_edf,
            Vector3d&               outgoing,
            double&                 turbidity)
        {
            // Use the sun direction from the EDF if it has one.
            const Source* sun_theta_src = env_edf->get_inputs().source("sun_theta");
            const Source* sun_phi_src = env_edf->get_inputs().source("sun_phi");
            if (sun_theta_src->is_uniform() && sun_phi_src->is_uniform())
            {
                double sun_theta, sun_phi;
                sun_theta_src->evaluate_uniform(sun_theta);
                sun_phi_src->evaluate_uniform(sun_phi);
                outgoing = -Vector3d::unit_vector(deg_to_rad(sun_theta), deg_to_rad(sun_phi));
            }

            // Use the sun turbidity from the EDF if it has one.
            const Source* turbidity_src = env_edf->get_inputs().source("turbidity");
            if (turbidity_src->is_uniform())
                turbidity_src->evaluate_uniform(turbidity);
        }

        static void compute_sun_radiance(
            const Vector3d&         outgoing,
            const double            turbidity,
            const double            radiance_multiplier,
            Spectrum&               radiance)
        {
            // Compute the relative optical mass.
            const float cos_theta = -static_cast<float>(outgoing.y);
            const float theta = acos(cos_theta);
            const float m = 1.0f / (cos_theta + 0.15f * pow(93.885f - rad_to_deg(theta), -1.253f));

            // Compute wavelengths in micrometers.
            const Spectrum wavelengths = g_light_wavelengths / 1000.0f;

            // Compute transmittance due to Rayleigh scattering.
            Spectrum tau_r;
            for (size_t i = 0; i < 31; ++i)
                tau_r[i] = exp(-0.008735f * m * pow(wavelengths[i], -4.08f));

            // Compute transmittance due to aerosols.
            const float Alpha = 1.3f;               // ratio of small to large particle sizes (0 to 4, typically 1.3)
            const float beta = 0.04608f * static_cast<float>(turbidity) - 0.04586f;
            Spectrum tau_a;
            for (size_t i = 0; i < 31; ++i)
                tau_a[i] = exp(-beta * m * pow(wavelengths[i], -Alpha));

            // Compute transmittance due to ozone absorption.
            const float L = 0.35f;                  // amount of ozone in cm
            static const float Ko[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.003f, 0.006f, 0.009f,
                0.014f, 0.021f, 0.030f, 0.040f,
                0.048f, 0.063f, 0.075f, 0.085f,
                0.103f, 0.120f, 0.120f, 0.115f,
                0.125f, 0.120f, 0.105f, 0.090f,
                0.079f, 0.067f, 0.057f, 0.048f,
                0.036f, 0.028f, 0.023f
            };
            Spectrum tau_o;
            for (size_t i = 0; i < 31; ++i)
                tau_o[i] = exp(-Ko[i] * L * m);

            // Compute transmittance due to mixed gases absorption.
            static const float Kg[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f
            };
            Spectrum tau_g;
            for (size_t i = 0; i < 31; ++i)
                tau_g[i] = exp(-1.41f * Kg[i] * m / pow(1.0f + 118.93f * Kg[i] * m, 0.45f));

            // Compute transmittance due to water vapor absorption.
            const float W = 2.0f;                   // precipitable water vapor in cm
            static const float Kwa[31] =
            {
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.000f, 0.000f, 0.000f,
                0.000f, 0.016f, 0.024f
            };
            Spectrum tau_wa;
            for (size_t i = 0; i < 31; ++i)
                tau_wa[i] = exp(-0.2385f * Kwa[i] * W * m / pow(1.0f + 20.07f * Kwa[i] * W * m, 0.45f));

            // Sun radiance in W.m^-2.sr^-1.um^-1.
            static const float SunRadianceValues[31] =
            {
                2112.75f, 2588.82f, 2582.91f, 2423.23f,
                2676.05f, 2965.83f, 3054.54f, 3005.75f,
                3066.37f, 2883.04f, 2871.21f, 2782.50f,
                2710.06f, 2723.36f, 2636.13f, 2550.38f,
                2506.02f, 2531.16f, 2535.59f, 2513.42f,
                2463.15f, 2417.32f, 2368.53f, 2321.21f,
                2282.77f, 2233.98f, 2197.02f, 2152.67f,
                2109.79f, 2072.83f, 2024.04f
            };

            // Compute the attenuated radiance of the sun.
            radiance = Spectrum(SunRadianceValues);
            radiance *= tau_r;
            radiance *= tau_a;
            radiance *= tau_o;
            radiance *= tau_g;
            radiance *= tau_wa;
            radiance *= static_cast<float>(radiance_multiplier);
        }
    };
}


//
// SunLightFactory class implementation.
//

const char* SunLightFactory::get_model() const
{
    return Model;
}

const char* SunLightFactory::get_human_readable_model() const
{
    return "Sun Light";
}

DictionaryArray SunLightFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "environment_edf")
            .insert("label", "Bind To")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("environment_edf", "Environment EDFs"))
            .insert("use", "optional"));

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
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("widget", "text_box")
            .insert("use", "optional")
            .insert("default", "1.0"));

    add_common_widget_definitions(definitions);

    return definitions;
}

auto_release_ptr<Light> SunLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new SunLight(name, params));
}

}   // namespace renderer
