
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
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
    const char*         name,
    const ParamArray&   params)
  : ConnectableEntity(g_class_uid, params)
{
    set_name(name);
}

bool Camera::on_frame_begin(
    const Project&      project,
    IAbortSwitch*       abort_switch)
{
    m_transform_sequence.optimize();

    if (!m_transform_sequence.prepare())
        RENDERER_LOG_WARNING("camera \"%s\" has one or more invalid transforms.", get_name());

    m_shutter_open_time = m_params.get_optional<double>("shutter_open_time", 0.0);
    m_shutter_close_time = m_params.get_optional<double>("shutter_close_time", 1.0);
    m_shutter_open_time_interval = m_shutter_close_time - m_shutter_open_time;

    return true;
}

void Camera::on_frame_end(const Project& project)
{
}

bool Camera::project_point(
    const double        time,
    const Vector3d&     point,
    Vector2d&           ndc) const
{
    // Retrieve the camera transform.
    Transformd tmp;
    const Transformd& transform = m_transform_sequence.evaluate(time, tmp);

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
                get_name(),
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
                get_name());

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
            get_name(),
            DefaultFocalLength);

        return DefaultFocalLength;
    }
}

double Camera::extract_f_stop() const
{
    const double DefaultFStop = 8.0;

    return get_greater_than_zero("f_stop", DefaultFStop);
}

void Camera::extract_focal_distance(
    bool&               autofocus_enabled,
    Vector2d&           autofocus_target,
    double&             focal_distance) const
{
    const Vector2d DefaultAFTarget(0.5);        // in NDC
    const double DefaultFocalDistance = 1.0;    // in meters

    if (has_param("focal_distance"))
    {
        if (has_param("autofocus_target"))
        {
            RENDERER_LOG_WARNING(
                "while defining camera \"%s\": autofocus is enabled; \"focal_distance\" parameter "
                "will be ignored.",
                get_name());

            autofocus_enabled = true;
            autofocus_target = m_params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
            focal_distance = 0.0;
        }
        else
        {
            autofocus_enabled = false;
            autofocus_target = DefaultAFTarget;
            focal_distance = m_params.get_required<double>("focal_distance", DefaultFocalDistance);
        }
    }
    else if (has_param("autofocus_target"))
    {
        autofocus_enabled = true;
        autofocus_target = m_params.get_required<Vector2d>("autofocus_target", DefaultAFTarget);
        focal_distance = 0.0;
    }
    else
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": no \"focal_distance\" or \"autofocus_target\" parameter found; "
            "using default focal distance value \"%f\".",
            get_name(),
            DefaultFocalDistance);

        autofocus_enabled = false;
        autofocus_target = DefaultAFTarget;
        focal_distance = DefaultFocalDistance;
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
            get_name(),
            near_z,
            DefaultNearZ);

        near_z = DefaultNearZ;
    }

    return near_z;
}

void Camera::initialize_ray(
    SamplingContext&    sampling_context,
    ShadingRay&         ray) const
{
    ray.m_tmin = 0.0;
    ray.m_tmax = numeric_limits<double>::max();
    ray.m_flags = VisibilityFlags::CameraRay;
    ray.m_depth = 0;

    if (m_shutter_open_time == m_shutter_close_time)
    {
        ray.m_time = ShadingRay::Time::create_with_normalized_time(
            0.0,
            m_shutter_open_time,
            m_shutter_close_time);
    }
    else
    {
        sampling_context.split_in_place(1, 1);
        ray.m_time = ShadingRay::Time::create_with_normalized_time(
            sampling_context.next_double2(),
            m_shutter_open_time,
            m_shutter_close_time);
    }
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
            get_name(),
            value,
            name,
            default_value);

        return default_value;
    }

    return value;
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
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_close_time")
            .insert("label", "Shutter Close Time")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

}   // namespace renderer
