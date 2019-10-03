
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "perspectivecamera.h"

// appleseed.renderer headers.
#include "renderer/kernel/rasterization/rasterizationcamera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/intersection/planesegment.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/api/apistring.h"

using namespace foundation;

namespace renderer
{

//
// PerspectiveCamera class implementation.
//

PerspectiveCamera::PerspectiveCamera(const char* name, const ParamArray& params)
  : Camera(name, params)
{
}

const foundation::Vector2d& PerspectiveCamera::get_film_dimensions() const
{
    return m_film_dimensions;
}

double PerspectiveCamera::get_focal_length() const
{
    return m_focal_length;
}

const foundation::Vector2d& PerspectiveCamera::get_shift() const
{
    return m_shift;
}

bool PerspectiveCamera::on_render_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnRenderBeginRecorder&  recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Camera::on_render_begin(project, parent, recorder, abort_switch))
        return false;

    // Extract the film dimensions from the camera parameters.
    m_film_dimensions = extract_film_dimensions();

    // Extract the focal length from the camera parameters.
    m_focal_length = extract_focal_length(m_film_dimensions[0]);

    // Extract the abscissa of the near plane from the camera parameters.
    m_near_z = extract_near_z();

    // Extract the shift from the camera parameters.
    m_shift = extract_shift();

    // Precompute reciprocals of film dimensions.
    m_rcp_film_width = 1.0 / m_film_dimensions[0];
    m_rcp_film_height = 1.0 / m_film_dimensions[1];

    // Precompute pixel area.
    const size_t pixel_count = project.get_frame()->image().properties().m_pixel_count;
    m_pixel_area = m_film_dimensions[0] * m_film_dimensions[1] / pixel_count;

    return true;
}

bool PerspectiveCamera::project_camera_space_point(
    const Vector3d&         point,
    Vector2d&               ndc) const
{
    // Cannot project the point if it is behind the near plane.
    if (point.z > m_near_z)
        return false;

    // Project the point onto the film plane.
    ndc = camera_to_ndc(point);

    // Projection was successful.
    return true;
}

bool PerspectiveCamera::project_segment(
    const float             time,
    const Vector3d&         a,
    const Vector3d&         b,
    Vector2d&               a_ndc,
    Vector2d&               b_ndc) const
{
    // Retrieve the camera transform.
    Transformd scratch;
    const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

    // Transform the segment to camera space.
    Vector3d local_a = transform.point_to_local(a);
    Vector3d local_b = transform.point_to_local(b);

    // Clip the segment against the near plane.
    if (!clip(Vector4d(0.0, 0.0, 1.0, -m_near_z), local_a, local_b))
        return false;

    // Project the segment onto the film plane.
    a_ndc = camera_to_ndc(local_a);
    b_ndc = camera_to_ndc(local_b);

    // Projection was successful.
    return true;
}

RasterizationCamera PerspectiveCamera::get_rasterization_camera() const
{
    RasterizationCamera rc;
    rc.m_aspect_ratio = m_film_dimensions[0] / m_film_dimensions[1];
    rc.m_hfov = focal_length_to_hfov(m_film_dimensions[0], m_focal_length);
    rc.m_shift_x = m_shift.x * m_rcp_film_width;
    rc.m_shift_y = m_shift.y * m_rcp_film_height;
    return rc;
}

double PerspectiveCamera::extract_focal_length(const double film_width) const
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
            return hfov_to_focal_length(film_width, deg_to_rad(hfov));
        }
        else
        {
            return get_greater_than_zero("focal_length", DefaultFocalLength);
        }
    }
    else if (has_param("horizontal_fov"))
    {
        const double hfov = get_greater_than_zero("horizontal_fov", DefaultHFov);
        return hfov_to_focal_length(film_width, deg_to_rad(hfov));
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

double PerspectiveCamera::hfov_to_focal_length(const double film_width, const double hfov)
{
    return 0.5 * film_width / std::tan(0.5 * hfov);
}

double PerspectiveCamera::focal_length_to_hfov(const double film_width, const double focal_length)
{
    return 2.0 * std::atan(film_width / (2.0 * focal_length));
}

Vector3d PerspectiveCamera::ndc_to_camera(const Vector2d& point) const
{
    return
        Vector3d(
            (0.5 - point.x) * m_film_dimensions[0] - m_shift.x,
            (point.y - 0.5) * m_film_dimensions[1] - m_shift.y,
            m_focal_length);
}

Vector2d PerspectiveCamera::camera_to_ndc(const Vector3d& point) const
{
    const double k = m_focal_length / point.z;
    return
        Vector2d(
            0.5 - ((point.x * k + m_shift.x) * m_rcp_film_width),
            0.5 + ((point.y * k + m_shift.y) * m_rcp_film_height));
}

}   // namespace renderer
