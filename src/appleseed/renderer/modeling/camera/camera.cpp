
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
#include "camera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cmath>
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Camera class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Camera::get_class_uid()
{
    return g_class_uid;
}

Camera::Camera(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

bool Camera::on_render_begin(
    const Project&          project,
    IAbortSwitch*           abort_switch)
{
    m_shutter_open_time = m_params.get_optional<float>("shutter_open_time", 0.0f);
    m_shutter_open_end_time = m_params.get_optional<float>("shutter_open_end_time", m_shutter_open_time);
    m_shutter_close_time = m_params.get_optional<float>("shutter_close_time", 1.0f);
    m_shutter_close_start_time = m_params.get_optional<float>("shutter_close_start_time", m_shutter_close_time);

    check_shutter_times_for_consistency();

    m_shutter_open_time_interval = m_shutter_close_time - m_shutter_open_time;

    m_motion_blur_enabled = m_shutter_open_time_interval > 0.0f;

    if (m_motion_blur_enabled)
    {
        m_normalized_open_end_time = inverse_lerp(m_shutter_open_time, m_shutter_close_time, m_shutter_open_end_time);
        m_normalized_open_end_time_half = m_normalized_open_end_time / 2.0f;
        m_normalized_close_start_time = inverse_lerp(m_shutter_open_time, m_shutter_close_time, m_shutter_close_start_time);
        m_shutter_pdf_max_height = 2.0f / (1.0f + m_normalized_close_start_time - m_normalized_open_end_time);
        m_open_linear_curve_slope = m_shutter_pdf_max_height / m_normalized_open_end_time;
        m_close_linear_curve_slope = -m_shutter_pdf_max_height / (1.0f - m_normalized_close_start_time);
        m_inverse_cdf_open_point = m_shutter_pdf_max_height * m_normalized_open_end_time_half;
        m_inverse_cdf_close_point = m_shutter_pdf_max_height * (m_normalized_close_start_time - m_normalized_open_end_time_half);
    }

    return true;
}

void Camera::on_render_end(const Project& project)
{
}

bool Camera::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!ConnectableEntity::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    m_transform_sequence.optimize();

    if (!m_transform_sequence.prepare())
        RENDERER_LOG_WARNING("camera \"%s\" has one or more invalid transforms.", get_path().c_str());

    return true;
}

bool Camera::project_point(
    const float             time,
    const Vector3d&         point,
    Vector2d&               ndc) const
{
    // Retrieve the camera transform.
    Transformd scratch;
    const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

    // Transform the point from world space to camera space.
    const Vector3d point_camera = transform.point_to_local(point);

    return project_camera_space_point(point_camera, ndc);
}

Vector2d Camera::extract_film_dimensions() const
{
    const Vector2d DefaultFilmDimensions(0.025, 0.025);     // in meters
    const double DefaultAspectRatio = DefaultFilmDimensions[0] / DefaultFilmDimensions[1];

    Vector2d film_dimensions;

    if (has_params("film_width", "film_height"))
    {
        film_dimensions[0] = get_greater_than_zero("film_width", DefaultFilmDimensions[0]);
        film_dimensions[1] = get_greater_than_zero("film_height", DefaultFilmDimensions[1]);
    }
    else if (has_params("film_width", "aspect_ratio"))
    {
        const double aspect_ratio = get_greater_than_zero("aspect_ratio", DefaultAspectRatio);
        film_dimensions[0] = get_greater_than_zero("film_width", DefaultFilmDimensions[0]);
        film_dimensions[1] = film_dimensions[0] / aspect_ratio;
    }
    else if (has_params("film_height", "aspect_ratio"))
    {
        const double aspect_ratio = get_greater_than_zero("aspect_ratio", DefaultAspectRatio);
        film_dimensions[1] = get_greater_than_zero("film_height", DefaultFilmDimensions[1]);
        film_dimensions[0] = film_dimensions[1] * aspect_ratio;
    }
    else
    {
        film_dimensions =
            m_params.get_required<Vector2d>("film_dimensions", DefaultFilmDimensions);

        if (film_dimensions[0] <= 0.0 || film_dimensions[1] <= 0.0)
        {
            RENDERER_LOG_ERROR(
                "while defining camera \"%s\": invalid value \"%f %f\" for parameter \"%s\"; "
                "using default value \"%f %f\".",
                get_path().c_str(),
                film_dimensions[0],
                film_dimensions[1],
                "film_dimensions",
                DefaultFilmDimensions[0],
                DefaultFilmDimensions[1]);

            film_dimensions = DefaultFilmDimensions;
        }
    }

    return film_dimensions;
}

namespace
{
    // Compute the focal length (in meters), given the film width (in meters)
    // and the horizontal field of view (in degrees).
    double hfov_to_focal_length(const double film_width, const double hfov)
    {
        return 0.5 * film_width / tan(0.5 * deg_to_rad(hfov));
    }
}

double Camera::extract_focal_length(const double film_width) const
{
    const double DefaultFocalLength = 0.035;    // in meters
    const double DefaultHFov = 54.0;            // in degrees

    if (has_param("focal_length"))
    {
        if (has_param("horizontal_fov"))
        {
            RENDERER_LOG_WARNING(
                "while defining camera \"%s\": the parameter \"horizontal_fov\" "
                "has precedence over \"focal_length\".",
                get_path().c_str());

            const double hfov = get_greater_than_zero("horizontal_fov", DefaultHFov);
            return hfov_to_focal_length(film_width, hfov);
        }
        else
        {
            return get_greater_than_zero("focal_length", DefaultFocalLength);
        }
    }
    else if (has_param("horizontal_fov"))
    {
        const double hfov = get_greater_than_zero("horizontal_fov", DefaultHFov);
        return hfov_to_focal_length(film_width, hfov);
    }
    else
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": no \"horizontal_fov\" or \"focal_length\" parameter found; "
            "using default focal length value \"%f\".",
            get_path().c_str(),
            DefaultFocalLength);

        return DefaultFocalLength;
    }
}

double Camera::extract_near_z() const
{
    const double DefaultNearZ = -0.001;         // in meters

    double near_z = m_params.get_optional<double>("near_z", DefaultNearZ);

    if (near_z > 0.0)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid near-Z value \"%f\", near-Z values must be negative or zero; "
            "using default value \"%f\".",
            get_path().c_str(),
            near_z,
            DefaultNearZ);

        near_z = DefaultNearZ;
    }

    return near_z;
}

namespace
{
    //
    // Shutter curves. Used in Camera::map_to_shutter_curve().
    //

    // Integrate and inverse opening/closing curve y = a * x + b to transform samples.
    float map_sample_to_linear_curve(const float a, const float b, const float x)
    {
        float constant;
        if (a > 0.0f)
        {
            // Shutter is opening.
            constant = 0.0f;
        }
        else
        {
            // Shutter is closing.
            constant = 1.0f - (a / 2.0f) - b;
        }
        return (sqrt(2.0f * a * (x - constant) + b * b) - b) / a;
    }
}

float Camera::map_to_shutter_curve(const float sample) const
{
    if (0.0f <= sample && sample < m_inverse_cdf_open_point)
    {
        // Shutter is opening.
        return map_sample_to_linear_curve(m_open_linear_curve_slope, 0.0f, sample);
    }
    else if (1.0f >= sample && sample > m_inverse_cdf_close_point)
    {
        // Shutter is closing.
        return map_sample_to_linear_curve(m_close_linear_curve_slope, -m_close_linear_curve_slope, sample);
    }
    else
    {
        // Shutter is fully opened.
        return sample / m_shutter_pdf_max_height + m_normalized_open_end_time_half;
    }
}

void Camera::initialize_ray(
    SamplingContext&        sampling_context,
    ShadingRay&             ray) const
{
    ray.m_tmin = 0.0;
    ray.m_tmax = numeric_limits<double>::max();
    ray.m_flags = VisibilityFlags::CameraRay;
    ray.m_depth = 0;

    float sample_time = 0.0f;

    if (m_motion_blur_enabled)
    {
        sampling_context.split_in_place(1, 1);
        sample_time = map_to_shutter_curve(sampling_context.next2<float>());
    }

    ray.m_time =
        ShadingRay::Time::create_with_normalized_time(
            sample_time,
            m_shutter_open_time,
            m_shutter_close_time);
}

bool Camera::has_param(const char* name) const
{
    return m_params.strings().exist(name);
}

bool Camera::has_params(const char* name1, const char* name2) const
{
    return has_param(name1) && has_param(name2);
}

double Camera::get_greater_than_zero(
    const char*     name,
    const double    default_value) const
{
    const double value = m_params.get_required<double>(name, default_value);

    if (value <= 0.0)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid value \"%f\" for parameter \"%s\"; "
            "using default value \"%f\".",
            get_path().c_str(),
            value,
            name,
            default_value);

        return default_value;
    }

    return value;
}

void Camera::check_shutter_times_for_consistency() const
{
    if (m_shutter_open_end_time < m_shutter_open_time)
        RENDERER_LOG_WARNING("shutter open time of camera \"%s\" is greater than shutter open end time", get_path().c_str());

    if (m_shutter_close_start_time < m_shutter_open_end_time)
        RENDERER_LOG_WARNING("shutter open end time of camera \"%s\" is greater than shutter close start time", get_path().c_str());

    if (m_shutter_close_start_time < m_shutter_open_time)
        RENDERER_LOG_WARNING("shutter open time of camera \"%s\" is greater than shutter close start time", get_path().c_str());

    if (m_shutter_close_time < m_shutter_open_time)
        RENDERER_LOG_WARNING("shutter open time of camera \"%s\" is greater than shutter close time", get_path().c_str());

    if (m_shutter_close_time < m_shutter_open_end_time)
        RENDERER_LOG_WARNING("shutter open end time of camera \"%s\" is greater than shutter close time", get_path().c_str());

    if (m_shutter_close_time < m_shutter_close_start_time)
        RENDERER_LOG_WARNING("shutter close start time of camera \"%s\" is greater than shutter close time", get_path().c_str());
}


//
// CameraFactory class implementation.
//

DictionaryArray CameraFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_open_time")
            .insert("label", "Shutter Open Time")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_open_end_time")
            .insert("label", "Shutter Open End Time")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_close_start_time")
            .insert("label", "Shutter Close Start Time")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_close_time")
            .insert("label", "Shutter Close Time")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

}   // namespace renderer
