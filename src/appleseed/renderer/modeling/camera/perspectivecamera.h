
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

#pragma once

// appleseed.renderer headers.
#include "renderer/modeling/camera/camera.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Forward declarations.
namespace renderer      { class ParamArray; }
namespace renderer      { class RasterizationCamera; }

namespace renderer
{

//
// Perspective camera base class.
//

class PerspectiveCamera
  : public Camera
{
  public:
    PerspectiveCamera(
        const char*                     name,
        const ParamArray&               params);

    // Get the film dimensions in camera space, in meters.
    const foundation::Vector2d& get_film_dimensions() const;

    // Get the focal length in camera space, in meters.
    double get_focal_length() const;

    // Get the camera shift in camera space, in meters.
    const foundation::Vector2d& get_shift() const;

    // Focal length <-> horizontal field of view conversion functions.
    // Focal length and film width are expressed in meters; horizontal field of view is expressed in radians.
    static double hfov_to_focal_length(const double film_width, const double hfov);
    static double focal_length_to_hfov(const double film_width, const double focal_length);

    bool on_render_begin(
        const Project&                  project,
        const BaseGroup*                parent,
        OnRenderBeginRecorder&          recorder,
        foundation::IAbortSwitch*       abort_switch = nullptr) override;

    // Similar to project_point(), except that the input point is expressed in camera space.
    bool project_camera_space_point(
        const foundation::Vector3d&     point,
        foundation::Vector2d&           ndc) const override;

    // Project a 3D segment back to the film plane. The input segment is expressed in
    // world space. The returned segment is expressed in normalized device coordinates.
    // Returns true if the projection was possible, false otherwise.
    bool project_segment(
        const float                     time,
        const foundation::Vector3d&     a,
        const foundation::Vector3d&     b,
        foundation::Vector2d&           a_ndc,
        foundation::Vector2d&           b_ndc) const override;

    // Return a camera representation suitable for rasterization.
    RasterizationCamera get_rasterization_camera() const override;

  protected:
    // Parameters.
    foundation::Vector2d    m_film_dimensions;      // film dimensions in camera space, in meters
    double                  m_focal_length;         // focal length in camera space, in meters
    double                  m_near_z;               // Z value of the near plane in camera space, in meters
    foundation::Vector2d    m_shift;                // camera shift in camera space, in meters

    // Precomputed values.
    double                  m_rcp_film_width;       // film width reciprocal in camera space
    double                  m_rcp_film_height;      // film height reciprocal in camera space
    double                  m_pixel_area;           // pixel area in meters, in camera space

    // Utility function to retrieve the focal length (in meters) from the camera parameters.
    double extract_focal_length(const double film_width) const;

    // Project points between NDC and camera spaces.
    foundation::Vector3d ndc_to_camera(const foundation::Vector2d& point) const;
    foundation::Vector2d camera_to_ndc(const foundation::Vector3d& point) const;
};

}   // namespace renderer
