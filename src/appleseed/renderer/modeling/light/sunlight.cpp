
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

using namespace foundation;

namespace renderer
{

namespace
{

    // Sun's radius, in millions of km.
    // Reference: https://en.wikipedia.org/wiki/Solar_radius
    constexpr float SunRadius = 0.6957f;

}

SunLight::SunLight(
    const char*                     name,
    const ParamArray&               params)
    : Light(name, params)
{
    m_inputs.declare("environment_edf", InputFormat::Entity, "");
    m_inputs.declare("turbidity", InputFormat::Float);
    m_inputs.declare("radiance_multiplier", InputFormat::Float, "1.0");
    m_inputs.declare("size_multiplier", InputFormat::Float, "1.0");
    m_inputs.declare("distance", InputFormat::Float, "149.6");
}

bool SunLight::on_frame_begin(
    const Project&                  project,
    const BaseGroup*                parent,
    OnFrameBeginRecorder&           recorder,
    IAbortSwitch*                   abort_switch)
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

    m_sun_size = SunRadius * m_values.m_size_multiplier;

    // Compute the Sun's solid angle.
    // Reference: https://en.wikipedia.org/wiki/Solid_angle#Sun_and_Moon
    m_sun_solid_angle = TwoPi<float>() * (1.0f - std::cos(std::atan(m_sun_size / m_values.m_distance)));

    // If the Sun light is bound to an environment EDF, let it override the Sun's direction and turbidity.
    EnvironmentEDF* env_edf = dynamic_cast<EnvironmentEDF*>(m_inputs.get_entity("environment_edf"));
    if (env_edf != nullptr)
        apply_env_edf_overrides(env_edf);

    const Scene::RenderData& scene_data = project.get_scene()->get_render_data();
    m_scene_center = Vector3d(scene_data.m_center);
    m_scene_radius = scene_data.m_radius;
    m_safe_scene_diameter = scene_data.m_safe_diameter;

    return true;
}

void SunLight::sample(
    const ShadingContext&           shading_context,
    const Transformd&               light_transform,
    const Vector2d&                 s,
    Vector3d&                       position,
    Vector3d&                       outgoing,
    Spectrum&                       value,
    float&                          probability) const
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
    const ShadingContext&           shading_context,
    const Transformd&               light_transform,
    const Vector3d&                 target_point,
    const Vector2d&                 s,
    Vector3d&                       position,
    Vector3d&                       outgoing,
    Spectrum&                       value,
    float&                          probability) const
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
    const ShadingContext&           shading_context,
    const Transformd&               light_transform,
    const Vector2d&                 s,
    const LightTargetArray&         targets,
    Vector3d&                       position,
    Vector3d&                       outgoing,
    Spectrum&                       value,
    float&                          probability) const
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

void SunLight::evaluate(
    const Vector3d&                 outgoing,
    Spectrum&                       value) const
{
    assert(is_normalized(outgoing));

    if (!m_visible)
    {
        value.set(0.0f);
        return;
    }

    const Vector3d local_outgoing = normalize(get_transform().point_to_local(outgoing));
    const double angle = std::acos(dot(local_outgoing, Vector3d(0.0, 0.0, 1.0)));

    const double max_angle = m_sun_size / m_values.m_distance;

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
    value /= m_sun_solid_angle;
}

float SunLight::compute_distance_attenuation(
    const foundation::Vector3d&     target,
    const foundation::Vector3d&     position) const
{
    return 1.0f;
}

float SunLight::compute_limb_darkening(const float squared_distance_to_center) const
{
    // Reference:
    //
    //   Lintu, Andrei & Haber, Jörg & Magnor, Marcus.
    //   (2005). Realistic Solar Disc Rendering.
    //   http://wscg.zcu.cz/wscg2005/Papers_2005/Full/F17-full.pdf
    //

    constexpr float LimbDarkeningCoeficent = 0.6f; // Limb darkening coefficient for the sun for visible HosekSunLight.
    float limb_darkening = 1.0f;
    if (squared_distance_to_center > 0.0f)
    {
        limb_darkening = (1.0f - LimbDarkeningCoeficent *
            (1.0f - std::sqrt(1.0f - squared_distance_to_center / square(m_sun_size))));
    }

    return limb_darkening;
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

void SunLight::sample_disk(
    const Transformd&               light_transform,
    const Vector2d&                 s,
    const Vector3d&                 disk_center,
    const double                    disk_radius,
    Vector3d&                       position,
    Vector3d&                       outgoing,
    Spectrum&                       value,
    float&                          probability) const
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
    const Transformd&               light_transform,
    const Vector3d&                 target_point,
    const Vector2d&                 s,
    Vector3d&                       position,
    Vector3d&                       outgoing,
    Spectrum&                       value,
    float&                          probability) const
{
    assert(m_safe_scene_diameter > 0.0);

    // sun_diameter = 1.3914
    // angular_diameter = 2 * arctan(sun_diameter / (2 * distance))
    // tan(angular_diameter / 2) * distance = sun_radius
    // tan(angular_diameter / 2) * scene_diameter = virtual_sun_radius
    // -> virtual_sun_radius = sun_radius * scene_diameter / distance
    double sun_radius = m_sun_size * m_safe_scene_diameter / m_values.m_distance;

    outgoing = -normalize(light_transform.get_parent_z());

    Basis3d basis(outgoing);
    const Vector2d p = sample_disk_uniform(s);

    position =
        target_point
        - m_safe_scene_diameter * basis.get_normal()
        + sun_radius * p[0] * basis.get_tangent_u()
        + sun_radius * p[1] * basis.get_tangent_v();

    outgoing = normalize(target_point - position);
    Vector2d xy = static_cast<double>(m_sun_size) * p;
    double squared_distance_to_center = xy[0] * xy[0] + xy[1] * xy[1];


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

}   // namespace renderer
