
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
#include "sunlight.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/light/lighttarget.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class ShadingContext; }

using namespace foundation;

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
    //   http://ompf2.com/viewtopic.php?f=3&t=33
    //

    const char* Model = "sun_light";

    // Sun's radius, in millions of km.
    // Reference: https://en.wikipedia.org/wiki/Solar_radius
    const float SunRadius = 0.6957f;

    // The smallest valid turbidity value.
    const float BaseTurbidity = 2.0f;

    class SunLight
      : public Light
    {
      public:
        SunLight(
            const char*             name,
            const ParamArray&       params)
          : Light(name, params)
        {
            m_inputs.declare("environment_edf", InputFormat::Entity, "");
            m_inputs.declare("turbidity", InputFormat::Float);
            m_inputs.declare("radiance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("size_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("distance", InputFormat::Float, "149.6");
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
            if (!Light::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Evaluate uniform inputs.
            m_inputs.evaluate_uniforms(&m_values);

            // Warn if distance input is not uniform.
            Source* distance_src = get_inputs().source("distance");
            assert(distance_src != nullptr);
            if (!distance_src->is_uniform())
            {
                RENDERER_LOG_WARNING(
                    "distance between sun and scene \"%s\" is not uniform, using default value of 149.6 million km.",
                    get_path().c_str());
                m_values.m_distance = 149.6f;
            }

            // Warn if size multiplier input is not uniform.
            const Source* size_multiplier_src = get_inputs().source("size_multiplier");
            assert(size_multiplier_src != nullptr);
            if (!size_multiplier_src->is_uniform())
            {
                RENDERER_LOG_WARNING(
                    "size multiplier of the sun light \"%s\" is not uniform.",
                    get_path().c_str());
                m_values.m_size_multiplier = 1.0f;
            }

            // Compute the Sun's solid angle.
            // Reference: https://en.wikipedia.org/wiki/Solid_angle#Sun_and_Moon
            m_sun_solid_angle = TwoPi<float>() * (1.0f - std::cos(std::atan(SunRadius / m_values.m_distance)));

            // If the Sun light is bound to an environment EDF, let it override the Sun's direction and turbidity.
            const EnvironmentEDF* env_edf = dynamic_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
            if (env_edf != nullptr)
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

        void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            // todo: we need to choose a random direction as well in order to get
            // soft shadows when using photon mapping.
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

        void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector3d&         target_point,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            sample_sun_surface(
                light_transform,
                target_point,
                s,
                position,
                outgoing,
                value,
                probability);
        }

        void sample(
            const ShadingContext&   shading_context,
            const Transformd&       light_transform,
            const Vector2d&         s,
            const LightTargetArray& targets,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
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

        float compute_distance_attenuation(
            const Vector3d&         target,
            const Vector3d&         position) const override
        {
            return 1.0f;
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            float           m_turbidity;                // atmosphere turbidity
            float           m_radiance_multiplier;      // emitted radiance multiplier
            float           m_size_multiplier;          // Sun size multiplier
            float           m_distance;                 // distance between Sun and scene, in millions of km
        };

        Vector3d            m_scene_center;             // world space
        double              m_scene_radius;             // world space
        double              m_safe_scene_diameter;      // world space
        float               m_sun_solid_angle;          // Sun's solid angle, in steradians

        InputValues         m_values;

        RegularSpectrum31f  m_k1;
        RegularSpectrum31f  m_k2;

        void apply_env_edf_overrides(const EnvironmentEDF* env_edf)
        {
            // Use the Sun direction from the EDF if it has one.
            const Source* sun_theta_src = env_edf->get_inputs().source("sun_theta");
            const Source* sun_phi_src = env_edf->get_inputs().source("sun_phi");
            if (sun_theta_src != nullptr &&
                sun_theta_src->is_uniform() &&
                sun_phi_src != nullptr &&
                sun_phi_src->is_uniform())
            {
                float sun_theta, sun_phi;
                sun_theta_src->evaluate_uniform(sun_theta);
                sun_phi_src->evaluate_uniform(sun_phi);

                Transformd scratch;
                const Transformd& env_edf_transform = env_edf->transform_sequence().evaluate(0.0f, scratch);

                set_transform(
                    Transformd::from_local_to_parent(
                        Matrix4d::make_rotation(
                            Quaterniond::make_rotation(
                                Vector3d(0.0, 0.0, -1.0),   // default emission direction of this light
                                -Vector3d::make_unit_vector(deg_to_rad(sun_theta), deg_to_rad(sun_phi))))) *
                    env_edf_transform);
            }

            // Use the Sun turbidity from the EDF if it has one.
            const Source* turbidity_src = env_edf->get_inputs().source("turbidity");
            const Source* turbidity_multiplier_src = env_edf->get_inputs().source("turbidity_multiplier");
            if (turbidity_src != nullptr &&
                turbidity_src->is_uniform() &&
                turbidity_multiplier_src != nullptr &&
                turbidity_multiplier_src->is_uniform())
            {
                float turbidity_multiplier;
                turbidity_multiplier_src->evaluate_uniform(turbidity_multiplier);
                turbidity_src->evaluate_uniform(m_values.m_turbidity);
                m_values.m_turbidity *= turbidity_multiplier;
            }
        }

        void precompute_constants()
        {
            for (size_t i = 0; i < 31; ++i)
                m_k1[i] = -0.008735f * std::pow(g_light_wavelengths_um[i], -4.08f);

            const float Alpha = 1.3f;               // ratio of small to large particle sizes (0 to 4, typically 1.3)

            for (size_t i = 0; i < 31; ++i)
                m_k2[i] = std::pow(g_light_wavelengths_um[i], -Alpha);
        }

        void compute_sun_radiance(
            const Vector3d&         outgoing,
            const float             turbidity,
            const float             radiance_multiplier,
            RegularSpectrum31f&     radiance) const
        {
            // Compute the relative optical mass.
            const float cos_theta = -static_cast<float>(outgoing.y);
            const float theta = std::acos(cos_theta);
            const float theta_delta = 93.885f - rad_to_deg(theta);
            if (theta_delta < 0.0f)
            {
                radiance.set(0.0f);
                return;
            }
            const float m = 1.0f / (cos_theta + 0.15f * std::pow(theta_delta, -1.253f));

            // Compute transmittance due to Rayleigh scattering.
            RegularSpectrum31f tau_r;
            for (size_t i = 0; i < 31; ++i)
                tau_r[i] = std::exp(m * m_k1[i]);

            // Compute transmittance due to aerosols.
            const float beta = 0.04608f * turbidity - 0.04586f;
            RegularSpectrum31f tau_a;
            for (size_t i = 0; i < 31; ++i)
                tau_a[i] = std::exp(-beta * m * m_k2[i]);

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
            RegularSpectrum31f tau_o;
            for (size_t i = 0; i < 31; ++i)
                tau_o[i] = std::exp(-Ko[i] * L * m);

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
            RegularSpectrum31f tau_g;
            for (size_t i = 0; i < 31; ++i)
                tau_g[i] = std::exp(-1.41f * Kg[i] * m / std::pow(1.0f + 118.93f * Kg[i] * m, 0.45f));
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
            RegularSpectrum31f tau_wa;
            for (size_t i = 0; i < 31; ++i)
                tau_wa[i] = std::exp(-0.2385f * Kwa[i] * W * m / std::pow(1.0f + 20.07f * Kwa[i] * W * m, 0.45f));

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
            for (size_t i = 0; i < 31; ++i)
            {
                radiance[i] =
                    SunRadianceValues[i] *
                    tau_r[i] *
                    tau_a[i] *
                    tau_o[i] *
#ifdef COMPUTE_REDUNDANT
                    tau_g[i] *      // always 1.0
#endif
                    tau_wa[i] *
                    radiance_multiplier;
            }
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
            assert(probability > 0.0f);

            RegularSpectrum31f radiance;
            compute_sun_radiance(
                outgoing,
                m_values.m_turbidity,
                m_values.m_radiance_multiplier,
                radiance);

            value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            value *= m_sun_solid_angle;
        }

        void sample_sun_surface(
            const Transformd&       light_transform,
            const Vector3d&         target_point,
            const Vector2d&         s,
            Vector3d&               position,
            Vector3d&               outgoing,
            Spectrum&               value,
            float&                  probability) const
        {
            assert(m_safe_scene_diameter > 0.0);

            // sun_diameter = 1.3914
            // angular_diameter = 2 * arctan(sun_diameter / (2 * distance))
            // tan(angular_diameter / 2) * distance = sun_radius
            // tan(angular_diameter / 2) * scene_diameter = virtual_sun_radius
            // -> virtual_sun_radius = sun_radius * scene_diameter / distance
            double sun_radius = SunRadius * m_safe_scene_diameter / m_values.m_distance;
            sun_radius *= m_values.m_size_multiplier;

            outgoing = -normalize(light_transform.get_parent_z());

            const Basis3d basis(outgoing);
            const Vector2d p = sample_disk_uniform(s);

            position =
                  target_point
                - m_safe_scene_diameter * basis.get_normal()
                + sun_radius * p[0] * basis.get_tangent_u()
                + sun_radius * p[1] * basis.get_tangent_v();

            outgoing = normalize(target_point - position);

            RegularSpectrum31f radiance;
            compute_sun_radiance(
                outgoing,
                m_values.m_turbidity,
                m_values.m_radiance_multiplier,
                radiance);

            value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            value *= m_sun_solid_angle;

            //
            // The Sun is represented by a disk of finite radius. The Sun's illumination
            // at a given point of the scene is computed by integrating the Sun's
            // contribution over that disk. The probability density of a given sample on
            // that disk is 1 / Sun's disk surface area.
            //
            // Since compute_sun_radiance() assumes that the Sun is reduced to a point
            // infinitely far away, it's really returning an irradiance, and we need to
            // convert it back to a radiance by dividing its return value by the surface
            // area of the Sun's disk, which is equivalent to multiplying the probability
            // density by the Sun disk's surface area, which leaves us with probability = 1.
            //

            probability = 1.0f;
        }
    };
}


//
// SunLightFactory class implementation.
//

void SunLightFactory::release()
{
    delete this;
}

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
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "8.0")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "2.0")
            .insert("help", "Atmospheric haziness"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Light intensity multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "size_multiplier")
            .insert("label", "Size Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "100.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "The size multiplier allows to make the sun bigger or smaller, hence making it cast softer or harder shadows"));

    metadata.push_back(
        Dictionary()
            .insert("name", "distance")
            .insert("label", "Distance")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "500.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "149.6")
            .insert("help", "Distance between Sun and scene (millions of km)"));

    add_common_input_metadata(metadata);

    return metadata;
}

auto_release_ptr<Light> SunLightFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Light>(new SunLight(name, params));
}

}   // namespace renderer
