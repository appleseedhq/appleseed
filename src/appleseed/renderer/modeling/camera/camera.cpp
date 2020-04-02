
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
#include "camera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/bezier.h"
#include "foundation/math/polynomial.h"
#include "foundation/math/root.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cmath>
#include <limits>

using namespace foundation;

namespace renderer
{

//
// Camera class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();

    const float ShutterOpenBeginTimeDefaultValue = 0.0f;
    const float ShutterCloseEndTimeDefaultValue = 1.0f;
}

struct Camera::Impl
{
    float               m_normalized_open_end_time;
    float               m_normalized_close_begin_time;

    float               m_inverse_cdf_open_point;
    float               m_inverse_cdf_close_point;

    //
    // Linear shutter parameters.
    //

    float               m_shutter_curve_linear_open_multiplier;
    float               m_shutter_curve_linear_fully_opened_multiplier;
    float               m_shutter_curve_linear_close_multiplier;

    //
    // Bezier shutter curve parameters.
    //

    // Control points are represented in form [t0 s0 t1 s1 t2 s2 t3 s3]
    // where t stands for time and s stands for "shutter openness".
    typedef Vector<float, 8> ShutterCurveCP;

    bool                m_use_bezier_shutter_curve;
    ShutterCurveCP      m_shutter_curve_bezier_cp_normalized;

    // Normalization factor to make the area under the curve equal to 1.
    float               m_shutter_curve_bezier_normalization_factor;

    // Polynomials of open/close CDFs.
    Vector<float, 7>    m_shutter_curve_bezier_open_cdf;
    Vector<float, 7>    m_shutter_curve_bezier_close_cdf;
};

UniqueID Camera::get_class_uid()
{
    return g_class_uid;
}

Camera::Camera(
    const char*             name,
    const ParamArray&       params)
  : ConnectableEntity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);
}

Camera::~Camera()
{
    delete impl;
}

bool Camera::on_render_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnRenderBeginRecorder&  recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Entity::on_render_begin(project, parent, recorder, abort_switch))
        return false;

    m_shutter_open_begin_time =
        m_params.get_optional<float>(
            "shutter_open_begin_time",
            ShutterOpenBeginTimeDefaultValue);

    m_shutter_open_end_time =
        m_params.get_optional<float>(
            "shutter_open_end_time",
            m_shutter_open_begin_time);

    m_shutter_close_end_time =
        m_params.get_optional<float>(
            "shutter_close_end_time",
            ShutterCloseEndTimeDefaultValue);

    m_shutter_close_begin_time =
        m_params.get_optional<float>(
            "shutter_close_begin_time",
            m_shutter_close_end_time);

    check_shutter_times_for_consistency();

    m_shutter_time_interval = m_shutter_close_end_time - m_shutter_open_begin_time;
    m_motion_blur_enabled = m_shutter_time_interval > 0.0f;

    if (m_motion_blur_enabled)
    {
        impl->m_use_bezier_shutter_curve = has_param("shutter_curve_control_points");

        impl->m_normalized_open_end_time =
            inverse_lerp(
                m_shutter_open_begin_time,
                m_shutter_close_end_time,
                m_shutter_open_end_time);

        impl->m_normalized_close_begin_time =
            inverse_lerp(
                m_shutter_open_begin_time,
                m_shutter_close_end_time,
                m_shutter_close_begin_time);

        if (impl->m_use_bezier_shutter_curve)
            initialize_shutter_curve_bezier();
        else initialize_shutter_curve_linear();
    }

    return true;
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

double Camera::extract_near_z() const
{
    const double DefaultNearZ = -0.001;         // in meters

    double near_z = m_params.get_optional<double>("near_z", DefaultNearZ);

    if (near_z > 0.0)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid near z value \"%f\", near z values must be negative or zero; "
            "using default value \"%f\".",
            get_path().c_str(),
            near_z,
            DefaultNearZ);

        near_z = DefaultNearZ;
    }

    return near_z;
}

Vector2d Camera::extract_shift() const
{
    return Vector2d(
        m_params.get_optional<double>("shift_x", 0.0),
        m_params.get_optional<double>("shift_y", 0.0));
}

void Camera::check_shutter_times_for_consistency() const
{
    const auto properly_ordered =
        m_shutter_open_begin_time <= m_shutter_open_end_time &&
        m_shutter_open_end_time <= m_shutter_close_begin_time &&
        m_shutter_close_begin_time <= m_shutter_close_end_time;

    if (!properly_ordered)
    {
        RENDERER_LOG_WARNING(
            "while defining camera \"%s\": shutter times are not properly ordered; "
            "order should be: open begin time <= open end time <= close begin time <= close end time.",
            get_path().c_str());
    }
}

void Camera::initialize_shutter_curve_linear()
{
    float cbt_minus_oet_plus_one =
        impl->m_normalized_close_begin_time - impl->m_normalized_open_end_time + 1.0f;

    if (cbt_minus_oet_plus_one == 0.0f)
    {
        RENDERER_LOG_ERROR(
            "while defining camera \"%s\": invalid values \"%f\" \"%f\" for parameters \"%s\", \"%s\" "
            "using default values for parameters \"%s\", \"%s\", \"%s\", \"%s\" "
            "\"%f\", \"%f\", \"%f\", \"%f\".",
            get_path().c_str(),
            m_shutter_open_end_time,
            m_shutter_close_begin_time,
            "shutter_open_end_time",
            "shutter_close_begin_time",
            "shutter_open_begin_time",
            "shutter_open_end_time",
            "shutter_close_begin_time",
            "shutter_close_begin_time",
            ShutterOpenBeginTimeDefaultValue,
            ShutterOpenBeginTimeDefaultValue,
            ShutterCloseEndTimeDefaultValue,
            ShutterCloseEndTimeDefaultValue);

        m_shutter_open_begin_time = ShutterOpenBeginTimeDefaultValue;
        m_shutter_open_end_time = ShutterOpenBeginTimeDefaultValue;
        m_shutter_close_begin_time = ShutterCloseEndTimeDefaultValue;
        m_shutter_close_end_time = ShutterCloseEndTimeDefaultValue;

        impl->m_normalized_open_end_time =
            inverse_lerp(
                m_shutter_open_begin_time,
                m_shutter_close_end_time,
                m_shutter_open_end_time);

        impl->m_normalized_close_begin_time =
            inverse_lerp(
                m_shutter_open_begin_time,
                m_shutter_close_end_time,
                m_shutter_close_begin_time);

        m_shutter_time_interval = m_shutter_close_end_time - m_shutter_open_begin_time;

        cbt_minus_oet_plus_one =
            impl->m_normalized_close_begin_time - impl->m_normalized_open_end_time + 1.0f;
    }

    impl->m_shutter_curve_linear_open_multiplier =
        cbt_minus_oet_plus_one * impl->m_normalized_open_end_time;

    impl->m_shutter_curve_linear_fully_opened_multiplier = cbt_minus_oet_plus_one / 2.0f;

    impl->m_shutter_curve_linear_close_multiplier =
        cbt_minus_oet_plus_one * (impl->m_normalized_close_begin_time - 1.0f);

    impl->m_inverse_cdf_open_point =
        impl->m_normalized_open_end_time / cbt_minus_oet_plus_one;

    impl->m_inverse_cdf_close_point =
        (2.0f * impl->m_normalized_close_begin_time - impl->m_normalized_open_end_time) /
        cbt_minus_oet_plus_one;
}

void Camera::initialize_shutter_curve_bezier()
{
    impl->m_shutter_curve_bezier_cp_normalized =
    m_params.get_required<Impl::ShutterCurveCP>("shutter_curve_control_points");

    // Normalize time points.
    for (size_t i = 0; i < Impl::ShutterCurveCP::Dimension; i += 2)
    {
        impl->m_shutter_curve_bezier_cp_normalized[i] =
            inverse_lerp(
                m_shutter_open_begin_time,
                m_shutter_close_end_time,
                impl->m_shutter_curve_bezier_cp_normalized[i]);
    }

    // Use normalized time, so inverse sampling will end up with normalized time points.
    initialize_shutter_curve_bezier_cdfs(
        0.0f,                                               // ot
        impl->m_normalized_open_end_time,                   // oet
        impl->m_normalized_close_begin_time,                // cbt
        1.0f,                                               // ct
        impl->m_shutter_curve_bezier_cp_normalized[0],      // t00
        impl->m_shutter_curve_bezier_cp_normalized[2],      // t01
        impl->m_shutter_curve_bezier_cp_normalized[4],      // t10
        impl->m_shutter_curve_bezier_cp_normalized[6],      // t11
        impl->m_shutter_curve_bezier_cp_normalized[1],      // s00
        impl->m_shutter_curve_bezier_cp_normalized[3],      // s01
        impl->m_shutter_curve_bezier_cp_normalized[5],      // s10
        impl->m_shutter_curve_bezier_cp_normalized[7]);     // s11

    impl->m_shutter_curve_bezier_normalization_factor =
        1.0f / evaluate_polynomial(impl->m_shutter_curve_bezier_close_cdf, 1.0f);

    impl->m_shutter_curve_bezier_open_cdf =
        impl->m_shutter_curve_bezier_open_cdf *
        impl->m_shutter_curve_bezier_normalization_factor;

    impl->m_shutter_curve_bezier_close_cdf =
        impl->m_shutter_curve_bezier_close_cdf *
        impl->m_shutter_curve_bezier_normalization_factor;

    impl->m_inverse_cdf_open_point = evaluate_polynomial(impl->m_shutter_curve_bezier_open_cdf, 1.0f);
    impl->m_inverse_cdf_close_point = evaluate_polynomial(impl->m_shutter_curve_bezier_close_cdf, 0.0f);
}

void Camera::initialize_shutter_curve_bezier_cdfs(
    const float             ot,
    const float             oet,
    const float             cbt,
    const float             ct,
    const float             t00,
    const float             t01,
    const float             t10,
    const float             t11,
    const float             s00,
    const float             s01,
    const float             s10,
    const float             s11)
{
    impl->m_shutter_curve_bezier_open_cdf[0] =  0.0f;

    impl->m_shutter_curve_bezier_open_cdf[1] =  0.0f;

    impl->m_shutter_curve_bezier_open_cdf[2] =  4.5f * (s00*t00 - ot*s00);

    impl->m_shutter_curve_bezier_open_cdf[3] =  6.0f*s00 * (2.0f*ot - 3.0f*t00 + t01) -
                                                3.0f*s01 * (ot - t00);

    impl->m_shutter_curve_bezier_open_cdf[4] =  0.75f * (
                                                    s00 * (3.0f*oet - 18.0f*ot + 36.0f*t00 - 21.0f*t01) +
                                                    s01 * (9.0f*ot - 15.0f*t00 + 6.0f*t01) +
                                                    t00 - ot
                                                );

    impl->m_shutter_curve_bezier_open_cdf[5] =  0.6f *  (
                                                    2.0f * (t01 + ot - 2.0f*t00) -
                                                    6.0f*s00*(oet - 2.0f*ot + 5.0f*t00 - 4.0f*t01) +
                                                    3.0f*s01*(oet - 3.0f*ot + 7.0f*t00 - 5.0f*t01)
                                                );

    impl->m_shutter_curve_bezier_open_cdf[6] =  0.5f * (3.0f*s00 - 3.0f*s01 + 1.0f) *
                                                (oet - ot + 3.0f*t00 - 3.0f*t01);

    impl->m_shutter_curve_bezier_close_cdf[0] = evaluate_polynomial(impl->m_shutter_curve_bezier_open_cdf, 1.0f)
                                                + cbt - oet;

    impl->m_shutter_curve_bezier_close_cdf[1] = 3.0f * (t10-cbt);

    impl->m_shutter_curve_bezier_close_cdf[2] = 1.5f * (
                                                    cbt * (5.0f - 3.0f*s10) +
                                                    t10 * (3.0f*s10 - 7.0f) +
                                                    2.0f*t11
                                                );

    impl->m_shutter_curve_bezier_close_cdf[3] = cbt * (12.0f*s10 - 3.0f*s11 - 10.0f) +
                                                t10 * (3.0f*s11 -18.0f*s10 + 18) +
                                                t11 * (6.0f*s10 - 9.0f) +
                                                ct;

    impl->m_shutter_curve_bezier_close_cdf[4] = 0.75f * (
                                                    cbt * (9.0f*s11 -18.0f*s10 + 10.0f) +
                                                    ct * (3.0f*s10 - 3.0f) +
                                                    t10 * (36.0f*s10 - 15.0f*s11 - 22.0f) +
                                                    t11 * (6.0f*s11 -21.0f*s10 + 15.0f)
                                                );

    impl->m_shutter_curve_bezier_close_cdf[5] = 0.6f * (
                                                    cbt * (12*s10 - 9*s11 - 5.0f) +
                                                    ct * (3.0f*s11 - 6.0f*s10 + 3.0f) +
                                                    t10 * (21.0f*s11 - 30.0f*s10 + 13.0f) +
                                                    t11 * (24.0f*s10 - 15.0f*s11 - 11.0f)
                                                );

    impl->m_shutter_curve_bezier_close_cdf[6] = 0.5f *
                                                (3.0f*s10 - 3.0f*s11 - 1.0f) *
                                                (ct - cbt + 3.0f*t10 - 3.0f*t11);
}

void Camera::initialize_ray(
    SamplingContext&        sampling_context,
    ShadingRay&             ray) const
{
    ray.m_tmin = 0.0;
    ray.m_tmax = std::numeric_limits<double>::max();
    ray.m_flags = VisibilityFlags::CameraRay;
    ray.m_depth = 0;

    float sample_time;
    if (m_motion_blur_enabled)
    {
        sampling_context.split_in_place(1, 1);
        sample_time = map_to_shutter_curve(sampling_context.next2<float>());
    }
    else sample_time = 0.0f;

    ray.m_time =
        ShadingRay::Time::create_with_normalized_time(
            sample_time,
            m_shutter_open_begin_time,
            m_shutter_close_end_time);
}

float Camera::map_to_shutter_curve(const float sample) const
{
    assert(m_motion_blur_enabled);
    assert(sample < 1.0f);

    return impl->m_use_bezier_shutter_curve
        ? map_to_shutter_curve_impl_bezier(sample)
        : map_to_shutter_curve_impl_linear(sample);
}

namespace
{
    float inverse_sample_curve_patch(
        const Vector<float, 7>& curve,
        const float             value,
        const float             t0,
        const float             t1,
        const float             t2,
        const float             t3)
    {
        const auto f =
            [&curve, &value](float param)
            {
                return evaluate_polynomial(curve, param) - value;
            };

        const auto d =
            [&curve](float param)
            {
                return evaluate_polynomial_derivative(curve, param);
            };

        const float Eps = 1.0e-6f;

        float root = 0.0f;
        bool success =
            find_root_newton(
                f,
                d,
                0.0f,
                1.0f,
                Eps,
                10,
                root);

        if (success)
            return evaluate_bezier3(t0, t1, t2, t3, root);

        // Fall back to bisection.
        success =
            find_root_bisection(
                f,
                0.0f,
                1.0f,
                Eps,
                21,     // this would be enough to get 10e-6 accuracy for the whole [a,b] interval
                root);

        assert(success && "Cannot inverse sample shutter curve CDF");

        return evaluate_bezier3(t0, t1, t2, t3, root);
    }
}

float Camera::map_to_shutter_curve_impl_linear(const float sample) const
{
    // The linear curve consists of multiple linear segments.

    if (sample < impl->m_inverse_cdf_open_point)
    {
        // Shutter is opening.
        return std::sqrt(impl->m_shutter_curve_linear_open_multiplier * sample);
    }
    else if (sample > impl->m_inverse_cdf_close_point)
    {
        // Shutter is closing.
        return 1.0f - std::sqrt(impl->m_shutter_curve_linear_close_multiplier * (sample - 1.0f));
    }
    else
    {
        // Shutter is fully opened.
        return impl->m_shutter_curve_linear_fully_opened_multiplier * sample +
            impl->m_normalized_open_end_time / 2.0f;
    }
}

float Camera::map_to_shutter_curve_impl_bezier(const float sample) const
{
    // The parametric curve consists of two Bezier curves and constant segment between them.
    // See https://github.com/appleseedhq/appleseed-wiki/blob/master/documents/Shutter%20Curve.pdf for details.

    if (sample < impl->m_inverse_cdf_open_point)
    {
        // Shutter is opening.
        return
            inverse_sample_curve_patch(
                impl->m_shutter_curve_bezier_open_cdf,
                sample,
                0.0f,
                impl->m_shutter_curve_bezier_cp_normalized[0],
                impl->m_shutter_curve_bezier_cp_normalized[2],
                impl->m_normalized_open_end_time);
    }
    else if (sample > impl->m_inverse_cdf_close_point)
    {
        // Shutter is closing.
        return
            inverse_sample_curve_patch(
                impl->m_shutter_curve_bezier_close_cdf,
                sample,
                impl->m_normalized_close_begin_time,
                impl->m_shutter_curve_bezier_cp_normalized[4],
                impl->m_shutter_curve_bezier_cp_normalized[6],
                1.0f);
    }
    else
    {
        // Shutter is fully opened.
        return
            (sample - impl->m_inverse_cdf_open_point) /
            impl->m_shutter_curve_bezier_normalization_factor + impl->m_normalized_open_end_time;
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
            get_path().c_str(),
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
            .insert("name", "shutter_open_begin_time")
            .insert("label", "Shutter Open Begin Time")
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
            .insert("name", "shutter_close_begin_time")
            .insert("label", "Shutter Close Begin Time")
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
            .insert("name", "shutter_close_end_time")
            .insert("label", "Shutter Close End Time")
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

void CameraFactory::add_film_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "film_dimensions")
            .insert("label", "Film Dimensions")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "film_width")
            .insert("label", "Film Width")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "film_height")
            .insert("label", "Film Height")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aspect_ratio")
            .insert("label", "Aspect Ratio")
            .insert("type", "text")
                .insert("use", "required"));
}

void CameraFactory::add_lens_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "focal_length")
            .insert("label", "Focal Length")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "horizontal_fov")
            .insert("label", "Horizontal FOV")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "180.0")
                    .insert("type", "soft"))
                .insert("use", "required"));
}

void CameraFactory::add_clipping_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "near_z")
            .insert("label", "Near Z")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "-0.001"));
}

void CameraFactory::add_shift_metadata(DictionaryArray& metadata)
{
    metadata.push_back(
        Dictionary()
            .insert("name", "shift_x")
            .insert("label", "Shift X")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shift_y")
            .insert("label", "Shift Y")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "0.0"));
}

}   // namespace renderer
