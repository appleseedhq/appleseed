
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
#include "foundation/math/bezier.h"
#include "foundation/math/root.h"
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
  , m_use_bezier_shutter_curve(false)
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

    m_normalized_open_end_time = inverse_lerp(m_shutter_open_time,
                                              m_shutter_close_time,
                                              m_shutter_open_end_time);

    m_normalized_close_start_time = inverse_lerp(m_shutter_open_time,
                                                 m_shutter_close_time,
                                                 m_shutter_close_start_time);

    m_shutter_open_time_interval = m_shutter_close_time - m_shutter_open_time;
    m_motion_blur_enabled = m_shutter_open_time_interval > 0.0f;

    if (m_motion_blur_enabled && has_param("shutter_curve_control_points"))
    {
        // Initialize shutter curve parameters.
        m_use_bezier_shutter_curve = true;

        m_shutter_curve_bezier_control_points_normalized =
            m_params.get_required<ShutterCurveControlPoints>("shutter_curve_control_points");

        // Normalize time points.
        for (size_t i = 0; i < ShutterCurveControlPoints::Dimension; i += 2)
        {
            m_shutter_curve_bezier_control_points_normalized[i] =
                inverse_lerp(m_shutter_open_time,
                             m_shutter_close_time,
                             m_shutter_curve_bezier_control_points_normalized[i]);
        }

        // Use normalized time, so inverse sampling will end up with normalized time points.
        initialize_shutter_curve_bezier_cdfs(
            /* ot   */ 0.0f,
            /* oet  */ m_normalized_open_end_time,
            /* cst  */ m_normalized_close_start_time,
            /* ct   */ 1.0f,
            /* t00  */ m_shutter_curve_bezier_control_points_normalized[0],
            /* t01  */ m_shutter_curve_bezier_control_points_normalized[2],
            /* t10  */ m_shutter_curve_bezier_control_points_normalized[4],
            /* t11  */ m_shutter_curve_bezier_control_points_normalized[6],
            /* s00  */ m_shutter_curve_bezier_control_points_normalized[1],
            /* s01  */ m_shutter_curve_bezier_control_points_normalized[3],
            /* s10  */ m_shutter_curve_bezier_control_points_normalized[5],
            /* s11  */ m_shutter_curve_bezier_control_points_normalized[7]);

        m_shutter_curve_bezier_normalization_factor = get_shutter_curve_bezier_normalization_factor();

        m_shutter_curve_bezier_open_cdf =   m_shutter_curve_bezier_open_cdf
                                            * m_shutter_curve_bezier_normalization_factor;

        m_shutter_curve_bezier_close_cdf =  m_shutter_curve_bezier_close_cdf
                                            * m_shutter_curve_bezier_normalization_factor;

        m_inverse_cdf_open_point = evaluate_polynomial(m_shutter_curve_bezier_open_cdf, 1.0f);
        m_inverse_cdf_close_point = evaluate_polynomial(m_shutter_curve_bezier_close_cdf, 0.0f);
    }
    else if (m_motion_blur_enabled)
    {
        // Initialize linear shutter parameters.

        const float cst_minus_oet_plus_one = m_shutter_close_start_time - m_shutter_open_end_time + 1.0f;

        m_shutter_curve_linear_open_multiplier = cst_minus_oet_plus_one * m_shutter_open_end_time;
        m_shutter_curve_linear_fully_opened_multiplier = cst_minus_oet_plus_one / 2.0f;
        m_shutter_curve_linear_close_multiplier = cst_minus_oet_plus_one * (m_shutter_close_start_time - 1.0f);

        m_inverse_cdf_open_point = m_shutter_open_end_time / cst_minus_oet_plus_one;

        m_inverse_cdf_close_point = (2.0f * m_shutter_close_start_time - m_shutter_open_end_time) /
                                    cst_minus_oet_plus_one;
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
    // Used in map_to_shutter_curve_impl_bezier().
    //
    inline float inverse_sample_curve_patch(
        const Vector<float, 7>& curve,
        const float value,
        const float t0,
        const float t1,
        const float t2,
        const float t3)
    {
        const float eps = 1.0e-6f;
        const float a = 0.0f;
        const float b = 1.0f;

        float root = 0.0f;

        const auto f = [&curve, &value](float param)
            {
                return evaluate_polynomial(curve, param) - value;
            };

        const auto d = [&curve](float param)
            {
                return evaluate_polynomial_derivative(curve, param);
            };

        bool success = find_root_newton(
            f,
            d,
            a,
            b,
            eps,
            10,
            root);

        if (success)
        {
            return evaluate_bezier3(t0, t1, t2, t3, root);
        }

        //
        // Fallback to bisection.
        //

        success = find_root_bisection(
            f,
            a,
            b,
            eps,
            // This would be enough to get 10e-6 accuracy for the whole [a,b] interval.
            21,
            root);

        assert(success && "Cannot inverse sample shutter curve CDF");

        return evaluate_bezier3(t0, t1, t2, t3, root);
    }

} // namespace

// Linear curve consists of multiple linear patches.
float Camera::map_to_shutter_curve_impl_linear(const float sample) const
{
    if (sample < m_inverse_cdf_open_point)
    {
        // Shutter is opening.
        return sqrt(m_shutter_curve_linear_open_multiplier * sample);
    }
    else if (sample > m_inverse_cdf_close_point)
    {
        // Shutter is closing.
        return 1.0f - sqrt(m_shutter_curve_linear_close_multiplier * (sample - 1.0f));
    }
    else
    {
        // Shutter is fully opened.
        return m_shutter_curve_linear_fully_opened_multiplier * sample + m_shutter_open_end_time / 2.0f;
    }
}

//
// Parametric curve consists of two Bezier curves and constant patch between them.
// For more details look at
// https://github.com/appleseedhq/appleseed-wiki/blob/master/documents/Shutter%20Curve.pdf
//
float Camera::map_to_shutter_curve_impl_bezier(const float sample) const
{
    if (sample < m_inverse_cdf_open_point)
    {
        // Sample open curve.
        return inverse_sample_curve_patch(m_shutter_curve_bezier_open_cdf,
                                          sample,
                                          0.0f,
                                          m_shutter_curve_bezier_control_points_normalized[0],
                                          m_shutter_curve_bezier_control_points_normalized[2],
                                          m_normalized_open_end_time);
    }
    else if (sample <= m_inverse_cdf_close_point)
    {
        return  (sample - m_inverse_cdf_open_point)
                / m_shutter_curve_bezier_normalization_factor + m_normalized_open_end_time;
    }
    else
    {
        // Sample close curve.
        return inverse_sample_curve_patch(m_shutter_curve_bezier_close_cdf,
                                          sample,
                                          m_normalized_close_start_time,
                                          m_shutter_curve_bezier_control_points_normalized[4],
                                          m_shutter_curve_bezier_control_points_normalized[6],
                                          1.0f);
    }
}

void Camera::initialize_shutter_curve_bezier_cdfs(
    const float ot,
    const float oet,
    const float cst,
    const float ct,
    const float t00,
    const float t01,
    const float t10,
    const float t11,
    const float s00,
    const float s01,
    const float s10,
    const float s11)
{
    //
    //  Welcome to the polynomial hell!
    //
    m_shutter_curve_bezier_open_cdf[0] =    0.0f;

    m_shutter_curve_bezier_open_cdf[1] =    0.0f;

    m_shutter_curve_bezier_open_cdf[2] =    4.5f * (s00*t00 - ot*s00);

    m_shutter_curve_bezier_open_cdf[3] =    6.0f*s00 * (2.0f*ot - 3.0f*t00 + t01) -
                                            3.0f*s01 * (ot - t00);

    m_shutter_curve_bezier_open_cdf[4] =    0.75f * (
                                                    s00 * (3.0f*oet - 18.0f*ot + 36.0f*t00 - 21.0f*t01) +
                                                    s01 * (9.0f*ot - 15.0f*t00 + 6.0f*t01) +
                                                    t00 - ot
                                                    );

    m_shutter_curve_bezier_open_cdf[5] =    0.6f *  (
                                                    2.0f * (t01 + ot - 2.0f*t00) -
                                                    6.0f*s00*(oet - 2.0f*ot + 5.0f*t00 - 4.0f*t01) +
                                                    3.0f*s01*(oet - 3.0f*ot + 7.0f*t00 - 5.0f*t01)
                                                    );

    m_shutter_curve_bezier_open_cdf[6] =    0.5f * (3.0f*s00 - 3.0f*s01 + 1.0f) *
                                            (oet - ot + 3.0f*t00 - 3.0f*t01);

    m_shutter_curve_bezier_close_cdf[0] =   evaluate_polynomial(m_shutter_curve_bezier_open_cdf, 1.0f)
                                            + cst - oet;

    m_shutter_curve_bezier_close_cdf[1] =   3.0f * (t10-cst);

    m_shutter_curve_bezier_close_cdf[2] =   1.5f *  (
                                                    cst * (5.0f - 3.0f*s10) +
                                                    t10 * (3.0f*s10 - 7.0f) +
                                                    2.0f*t11
                                                    );

    m_shutter_curve_bezier_close_cdf[3] =   cst * (12.0f*s10 - 3.0f*s11 - 10.0f) +
                                            t10 * (3.0f*s11 -18.0f*s10 + 18) +
                                            t11 * (6.0f*s10 - 9.0f) +
                                            ct;

    m_shutter_curve_bezier_close_cdf[4] =   0.75f * (
                                                    cst * (9.0f*s11 -18.0f*s10 + 10.0f) +
                                                    ct * (3.0f*s10 - 3.0f) +
                                                    t10 * (36.0f*s10 - 15.0f*s11 - 22.0f) +
                                                    t11 * (6.0f*s11 -21.0f*s10 + 15.0f)
                                                    );

    m_shutter_curve_bezier_close_cdf[5] =   0.6f *  (
                                                    cst * (12*s10 - 9*s11 - 5.0f) +
                                                    ct * (3.0f*s11 - 6.0f*s10 + 3.0f) +
                                                    t10 * (21.0f*s11 - 30.0f*s10 + 13.0f) +
                                                    t11 * (24.0f*s10 - 15.0f*s11 - 11.0f)
                                                    );

    m_shutter_curve_bezier_close_cdf[6] =   0.5f *
                                            (3.0f*s10 - 3.0f*s11 - 1.0f) *
                                            (ct - cst + 3.0f*t10 - 3.0f*t11);
}

float Camera::get_shutter_curve_bezier_normalization_factor() const
{
    return 1.0f / evaluate_polynomial(m_shutter_curve_bezier_close_cdf, 1.0f);
}

float Camera::map_to_shutter_curve(const float sample) const
{
    assert(sample < 1.0f);

    return m_use_bezier_shutter_curve
        ? map_to_shutter_curve_impl_bezier(sample)
        : map_to_shutter_curve_impl_linear(sample);
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

    metadata.push_back(
        Dictionary()
            .insert("name", "shutter_curve_control_points")
            .insert("label", "Shutter Curve Control Points")
            .insert("type", "text")
            .insert("use", "optional"));

    return metadata;
}

}   // namespace renderer
