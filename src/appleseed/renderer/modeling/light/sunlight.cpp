
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/lighttarget.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Physically-based Sun light.
    //
    // References:
    //
    //   http://www.cs.utah.edu/~shirley/papers/sunsky/sunsky.pdf
    //   http://igad2.nhtv.nl/ompf2/viewtopic.php?f=3&t=33
    //

    const char* Model = "sun_light";

    // The smallest valid turbidity value.
    const float BaseTurbidity = 2.0f;

    // Solid angle sustained by the Sun, as seen from Earth (in steradians).
    // Reference: http://en.wikipedia.org/wiki/Solid_angle#Sun_and_Moon
    const float SunSolidAngle = 6.87e-5f;

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
            m_inputs.declare("turbidity", InputFormatFloat);
            m_inputs.declare("radiance_multiplier", InputFormatFloat, "1.0");
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
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!Light::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Evaluate uniform inputs.
            m_inputs.evaluate_uniforms(&m_values);

            // If the Sun light is bound to an environment EDF, let it override the Sun's direction and turbidity.
            const EnvironmentEDF* env_edf = dynamic_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
            if (env_edf)
                apply_env_edf_overrides(env_edf);

            // Apply turbidity bias.
            m_values.m_turbidity += BaseTurbidity;

            const Scene::RenderData& scene_data = project.get_scene()->get_render_data();
            m_scene_center = Vector3d(scene_data.m_center);
            m_scene_radius = scene_data.m_radius;
            m_safe_scene_diameter = scene_data.m_safe_diameter;

            precompute_constants();

            return true;
        }

        virtual void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            sample_disk(
                light_transform,
                s,
                m_scene_center,
                m_scene_radius,
                position,
                outgoing,
                value,
                probability);
        }

        virtual void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector2d&         s,
            const LightTargetArray& targets,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            const size_t target_count = targets.size();

            if (target_count > 0)
            {
                const double x = s[0] * target_count;
                const size_t target_index = truncate<size_t>(x);
                const Vector2d target_s(x - target_index, s[1]);
                const LightTarget& target = targets[target_index];

                sample_disk(
                    light_transform,
                    target_s,
                    target.get_center(),
                    target.get_radius(),
                    position,
                    outgoing,
                    value,
                    probability);
            }
            else
            {
                sample_disk(
                    light_transform,
                    s,
                    m_scene_center,
                    m_scene_radius,
                    position,
                    outgoing,
                    value,
                    probability);
            }
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector3d&         target,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            outgoing = -normalize(light_transform.get_parent_z());
            position = target - m_safe_scene_diameter * outgoing;

            compute_sun_radiance(
                outgoing,
                m_values.m_turbidity,
                m_values.m_radiance_multiplier,
                value);

            value *= SunSolidAngle;
        }

        virtual float compute_distance_attenuation(
            const Vector3d&         target,
            const Vector3d&         position) const APPLESEED_OVERRIDE
        {
            return 1.0f;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float       m_turbidity;                // atmosphere turbidity
            float       m_radiance_multiplier;      // emitted radiance multiplier
        };

        Vector3d        m_scene_center;             // world space
        double          m_scene_radius;             // world space
        double          m_safe_scene_diameter;      // world space

        InputValues     m_values;

        Spectrum        m_k1;
        Spectrum        m_k2;

        void apply_env_edf_overrides(const EnvironmentEDF* env_edf)
        {
            // Use the Sun direction from the EDF if it has one.
            const Source* sun_theta_src = env_edf->get_inputs().source("sun_theta");
            const Source* sun_phi_src = env_edf->get_inputs().source("sun_phi");
            if (sun_theta_src && sun_theta_src->is_uniform() &&
                sun_phi_src && sun_phi_src->is_uniform())
            {
                float sun_theta, sun_phi;
                sun_theta_src->evaluate_uniform(sun_theta);
                sun_phi_src->evaluate_uniform(sun_phi);
                set_transform(
                    Transformd::from_local_to_parent(
                        Matrix4d::make_rotation(
                            Quaterniond::make_rotation(
                                Vector3d(0.0, 0.0, -1.0),
                                -Vector3d::make_unit_vector(deg_to_rad(sun_theta), deg_to_rad(sun_phi))))));
            }

            // Use the Sun turbidity from the EDF if it has one.
            const Source* turbidity_src = env_edf->get_inputs().source("turbidity");
            const Source* turbidity_multiplier_src = env_edf->get_inputs().source("turbidity_multiplier");
            if (turbidity_src && turbidity_src->is_uniform() &&
                turbidity_multiplier_src && turbidity_multiplier_src->is_uniform())
            {
                float turbidity_multiplier;
                turbidity_multiplier_src->evaluate_uniform(turbidity_multiplier);
                turbidity_src->evaluate_uniform(m_values.m_turbidity);
                m_values.m_turbidity *= turbidity_multiplier;
            }
        }

        void precompute_constants()
        {
            m_k1.resize(Spectrum::Samples);
            for (size_t i = 0; i < Spectrum::Samples; ++i)
                m_k1[i] = -0.008735f * pow(g_light_wavelengths_um[i], -4.08f);

            const float Alpha = 1.3f;               // ratio of small to large particle sizes (0 to 4, typically 1.3)

            m_k2.resize(Spectrum::Samples);
            for (size_t i = 0; i < Spectrum::Samples; ++i)
                m_k2[i] = pow(g_light_wavelengths_um[i], -Alpha);
        }

        void compute_sun_radiance(
            const Vector3d&         outgoing,
            const float             turbidity,
            const float             radiance_multiplier,
            Spectrum&               radiance) const
        {
            // Compute the relative optical mass.
            const float cos_theta = -static_cast<float>(outgoing.y);
            const float theta = acos(cos_theta);
            const float m = 1.0f / (cos_theta + 0.15f * pow(93.885f - rad_to_deg(theta), -1.253f));

            // Compute transmittance due to Rayleigh scattering.
            Spectrum tau_r;
            tau_r.resize(Spectrum::Samples);
            for (size_t i = 0; i < 31; ++i)
                tau_r[i] = exp(m * m_k1[i]);

            // Compute transmittance due to aerosols.
            const float beta = 0.04608f * turbidity - 0.04586f;
            Spectrum tau_a;
            tau_a.resize(Spectrum::Samples);
            for (size_t i = 0; i < 31; ++i)
                tau_a[i] = exp(-beta * m * m_k2[i]);

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
            tau_o.resize(Spectrum::Samples);
            for (size_t i = 0; i < 31; ++i)
                tau_o[i] = exp(-Ko[i] * L * m);

#ifdef COMPUTE_REDUNDANT
            // Compute transmittance due to mixed gases absorption.
            // Disabled since all coefficients are zero in the wavelength range of the simulation.
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
            tau_g.resize(Spectrum::Samples);
            for (size_t i = 0; i < 31; ++i)
                tau_g[i] = exp(-1.41f * Kg[i] * m / pow(1.0f + 118.93f * Kg[i] * m, 0.45f));
#endif

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
            tau_wa.resize(Spectrum::Samples);
            for (size_t i = 0; i < 31; ++i)
                tau_wa[i] = exp(-0.2385f * Kwa[i] * W * m / pow(1.0f + 20.07f * Kwa[i] * W * m, 0.45f));

            // Sun radiance in W.m^-2.sr^-1.um^-1.
            // The units in the paper are W.cm^-2.sr^-1.um^-1. We must multiply the values
            // by 10000 to obtain W.m^-2.sr^-1.um^-1. We must then divide them by 1000 to
            // obtain W.m^-2.sr^-1.nm^-1.
            static const float SunRadianceValues[31] =
            {
                21127.5f, 25888.2f, 25829.1f, 24232.3f,
                26760.5f, 29658.3f, 30545.4f, 30057.5f,
                30663.7f, 28830.4f, 28712.1f, 27825.0f,
                27100.6f, 27233.6f, 26361.3f, 25503.8f,
                25060.2f, 25311.6f, 25355.9f, 25134.2f,
                24631.5f, 24173.2f, 23685.3f, 23212.1f,
                22827.7f, 22339.8f, 21970.2f, 21526.7f,
                21097.9f, 20728.3f, 20240.4f
            };

            // Compute the attenuated radiance of the Sun.
            radiance = Spectrum(SunRadianceValues, Spectrum::Illuminance);
            radiance *= tau_r;
            radiance *= tau_a;
            radiance *= tau_o;
#ifdef COMPUTE_REDUNDANT
            radiance *= tau_g;      // always 1.0
#endif
            radiance *= tau_wa;
            radiance *= radiance_multiplier;
        }

        void sample_disk(
            const Transformd&       light_transform,
            const Vector2d&         s,
            const Vector3d&         disk_center,
            const double            disk_radius,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const
        {
            outgoing = -normalize(light_transform.get_parent_z());

            const Basis3d basis(outgoing);
            const Vector2d p = sample_disk_uniform(s);

            position =
                  disk_center
                - m_safe_scene_diameter * basis.get_normal()
                + disk_radius * p[0] * basis.get_tangent_u()
                + disk_radius * p[1] * basis.get_tangent_v();

            probability = 1.0f / (Pi<float>() * square(static_cast<float>(disk_radius)));

            compute_sun_radiance(
                outgoing,
                m_values.m_turbidity,
                m_values.m_radiance_multiplier,
                value);

            value *= SunSolidAngle;
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

Dictionary SunLightFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Sun Light")
            .insert("help", "Physically-based sun light");
}

DictionaryArray SunLightFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "environment_edf")
            .insert("label", "Bind To")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("environment_edf", "Environment EDFs"))
            .insert("use", "optional")
            .insert("help", "If an environment EDF is bound, use the sun angles and turbidity values from the environment"));

    metadata.push_back(
        Dictionary()
            .insert("name", "turbidity")
            .insert("label", "Turbidity")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "4.0")
            .insert("help", "Atmospheric haziness"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "10.0")
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> SunLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new SunLight(name, params));
}

auto_release_ptr<Light> SunLightFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Light>(new SunLight(name, params));
}

}   // namespace renderer
