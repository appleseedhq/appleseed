
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
    constexpr  float SunRadius = 0.6957f;

    // The smallest valid turbidity value.
    constexpr  float BaseTurbidity = 2.0f;
}

SunLight::SunLight(
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

void SunLight::release()
{
    delete this;
}

const char* SunLight::get_model() const
{
    return Model;
}

bool SunLight::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Light::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    // Check if sun disk is visible.
    m_visible = m_params.get_optional<bool>("visible", true);

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
    m_sun_solid_angle = TwoPi<float>() * (1.0f - std::cos(std::atan(SunRadius * m_values.m_size_multiplier / m_values.m_distance)));

    const float default_solid_angle = TwoPi<float>() * (1.0f - std::cos(std::atan(SunRadius / 149.6f)));

    // Keep sun's irradiance constant for different sizes and distances.
    m_values.m_radiance_multiplier = m_values.m_radiance_multiplier * default_solid_angle / m_sun_solid_angle;

    // If the Sun light is bound to an environment EDF, let it override the Sun's direction and turbidity.
    EnvironmentEDF* env_edf = dynamic_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
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

void SunLight::sample(
    const ShadingContext&   shading_context,
    const Transformd&       light_transform,
    const Vector2d&         s,
    Vector3d&               position,
    Vector3d&               outgoing,
    Spectrum&               value,
    float&                  probability) const
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

void SunLight::sample(
    const ShadingContext&   shading_context,
    const Transformd&       light_transform,
    const Vector3d&         target_point,
    const Vector2d&         s,
    Vector3d&               position,
    Vector3d&               outgoing,
    Spectrum&               value,
    float&                  probability) const
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

void SunLight::sample(
    const ShadingContext&   shading_context,
    const Transformd&       light_transform,
    const Vector2d&         s,
    const LightTargetArray& targets,
    Vector3d&               position,
    Vector3d&               outgoing,
    Spectrum&               value,
    float&                  probability) const
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

float SunLight::compute_distance_attenuation(
    const Vector3d&         target,
    const Vector3d&         position) const
{
    return 1.0f;
}

void SunLight::evaluate(
                            const Vector3d& outgoing,
                            Spectrum& value)
{
    assert(is_normalized(outgoing));

    if (!m_visible)
    {
        value.set(0.0f);
        return;
    }

    const Vector3d local_outgoing = normalize(get_transform().point_to_local(outgoing));
    const double angle = std::acos(dot(local_outgoing, Vector3d(0.0, 0.0, 1.0)));

    const double max_angle = SunRadius * m_values.m_size_multiplier / m_values.m_distance;

    if (angle > std::atan(max_angle))
    {
        value.set(0.0f);
        return;
    }

    const double distance_to_center = std::tan(angle) * m_values.m_distance;

    RegularSpectrum31f radiance;
    compute_sun_radiance(
        -outgoing,
        m_values.m_turbidity,
        m_values.m_radiance_multiplier,
        radiance,
        square(static_cast<float>(distance_to_center)));

    value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
}

void SunLight::compute_sun_radiance(
    const Vector3d&         outgoing,
    const float             turbidity,
    const float             radiance_multiplier,
    RegularSpectrum31f&     radiance,
    const float             squared_distance_to_center) const
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
    const float L = 0.0035f;                  // amount of ozone in m
    static const float Ko[31] =
    {
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.300f, 0.600f, 0.900f,
        1.400f, 2.100f, 3.000f, 4.000f,
        4.800f, 6.300f, 7.500f, 8.500f,
        10.30f, 12.00f, 12.00f, 11.50f,
        12.50f, 12.00f, 10.50f, 9.000f,
        7.900f, 6.700f, 5.700f, 4.800f,
        3.600f, 2.800f, 2.300f
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
    const float W = 0.02f;                   // precipitable water vapor in m
    static const float Kwa[31] =
    {
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 0.000f, 0.000f, 0.000f,
        0.000f, 1.600f, 2.400f
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


    // Limb darkening.
    //
    // Reference:
    //
    //   Lintu, Andrei & Haber, Jörg & Magnor, Marcus.
    //   (2005). Realistic Solar Disc Rendering.
    //   http://wscg.zcu.cz/wscg2005/Papers_2005/Full/F17-full.pdf
    //

    constexpr float LimbDarkeningCoeficent = 0.6f; // Limb darkening coefficient for the sun for visible sunlight.
    float limb_darkening = 1.0f;
    if (squared_distance_to_center > 0.0f)
    {
        limb_darkening = (1.0f - LimbDarkeningCoeficent *
            (1.0f - std::sqrt(1.0f - squared_distance_to_center
            / square(SunRadius * m_values.m_size_multiplier))));
    }

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
            limb_darkening *
            m_sun_solid_angle *
            radiance_multiplier;
    }
}

void SunLight::apply_env_edf_overrides(EnvironmentEDF* env_edf)
{
    env_edf->get_inputs().find("sun_light").bind(this);

    // Use the Sun direction from the EDF if it has one.
    const Source* sun_theta_src = env_edf->get_inputs().source("sun_theta");
    const Source* sun_phi_src = env_edf->get_inputs().source("sun_phi");
    const Source* sun_shift_src = env_edf->get_inputs().source("horizon_shift");
    if (sun_theta_src != nullptr &&
        sun_theta_src->is_uniform() &&
        sun_phi_src != nullptr &&
        sun_phi_src->is_uniform() &&
        sun_shift_src != nullptr &&
        sun_shift_src->is_uniform())
    {
        float sun_theta, sun_phi, sun_shift;
        sun_theta_src->evaluate_uniform(sun_theta);
        sun_phi_src->evaluate_uniform(sun_phi);
        sun_shift_src->evaluate_uniform(sun_shift);

        Transformd scratch;
        const Transformd& env_edf_transform = env_edf->transform_sequence().evaluate(0.0f, scratch);

        set_transform(
            Transformd::from_local_to_parent(
                Matrix4d::make_translation(Vector3d(0.0, sun_shift, 0.0)) *
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

void SunLight::precompute_constants()
{
    for (size_t i = 0; i < 31; ++i)
        m_k1[i] = -0.008735f * std::pow(g_light_wavelengths_um[i], -4.08f);

    const float Alpha = 1.3f;               // ratio of small to large particle sizes (0 to 4, typically 1.3)

    for (size_t i = 0; i < 31; ++i)
        m_k2[i] = std::pow(g_light_wavelengths_um[i], -Alpha);
}

void SunLight::sample_disk(
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

    Basis3d basis(outgoing);
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

    // Shift sun.
    basis = Basis3d(-normalize(light_transform.get_parent_z() + light_transform.get_parent_origin()));

    // Repositioning.
    position =
        disk_center
        - m_safe_scene_diameter * basis.get_normal()
        + disk_radius * p[0] * basis.get_tangent_u()
        + disk_radius * p[1] * basis.get_tangent_v();

    value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
}

void SunLight::sample_sun_surface(
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

    Basis3d basis(outgoing);
    const Vector2d p = sample_disk_uniform(s);

    position =
        target_point
        - m_safe_scene_diameter * basis.get_normal()
        + sun_radius * p[0] * basis.get_tangent_u()
        + sun_radius * p[1] * basis.get_tangent_v();

    outgoing = normalize(target_point - position);
    Vector2d test = static_cast<double>(SunRadius * m_values.m_size_multiplier) * p;
    double squared_distance_to_center = test[0] * test[0] + test[1] * test[1];


    RegularSpectrum31f radiance;
    compute_sun_radiance(
        outgoing,
        m_values.m_turbidity,
        m_values.m_radiance_multiplier,
        radiance,
        static_cast<float>(squared_distance_to_center));

    // Shift sun.
    basis = Basis3d(-normalize(light_transform.get_parent_z() + light_transform.get_parent_origin()));

    // Repositioning.
    position =
        target_point
        - m_safe_scene_diameter * basis.get_normal()
        + sun_radius * p[0] * basis.get_tangent_u()
        + sun_radius * p[1] * basis.get_tangent_v();
    outgoing = normalize(target_point - position);

    value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);

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
            .insert("name", "visible")
            .insert("label", "Visible")
            .insert("type", "boolean")
            .insert("use", "optional")
            .insert("default", "true")
            .insert("help", "Make the sun visible to the camera"));

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
