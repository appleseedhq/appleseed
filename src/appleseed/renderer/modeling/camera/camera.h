
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_CAMERA_CAMERA_H
#define APPLESEED_RENDERER_MODELING_CAMERA_CAMERA_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Frame; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingRay; }

namespace renderer
{

//
// Camera.
//

class APPLESEED_DLLSYMBOL Camera
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    Camera(
        const char*                     name,
        const ParamArray&               params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Access the transform sequence of the camera.
    TransformSequence& transform_sequence();
    const TransformSequence& transform_sequence() const;

    // Get the shutter open time.
    double get_shutter_open_time() const;

    // Get the shutter close time.
    double get_shutter_close_time() const;

    // Get the time at the middle of the shutter interval.
    double get_shutter_middle_time() const;

    // Get the amount of time the shutter is open.
    double get_shutter_open_time_interval() const;

    // This method is called once before rendering.
    // Returns true on success, false otherwise.
    virtual bool on_render_begin(
        const Project&                  project,
        foundation::IAbortSwitch*       abort_switch = 0);

    // This method is called once after rendering.
    virtual void on_render_end(const Project& project);

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&                  project,
        foundation::IAbortSwitch*       abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(const Project& project);

    // Generate a ray directed toward a given point on the film plane,
    // expressed in normalized device coordinates
    // (https://github.com/appleseedhq/appleseed/wiki/Terminology).
    // The generated ray is expressed in world space.
    virtual void spawn_ray(
        SamplingContext&                sampling_context,
        const foundation::Dual2d&       ndc,
        ShadingRay&                     ray) const = 0;

    // Connect a vertex to the camera and return the direction vector from the
    // point to the camera, the normalized device coordinates of the projected
    // point on the camera film and the emitted importance. The direction vector
    // is not unit-length: its length represents the distance from the point to
    // the camera lens. The input point and output direction are expressed in
    // world space. Returns true if the connection was possible, false otherwise.
    virtual bool connect_vertex(
        SamplingContext&                sampling_context,
        const double                    time,
        const foundation::Vector3d&     point,
        foundation::Vector2d&           ndc,
        foundation::Vector3d&           outgoing,
        double&                         importance) const = 0;

    // Project a 3D point back to the film plane. The input point is expressed in
    // world space. The returned point is expressed in normalized device coordinates.
    // Returns true if the projection was possible, false otherwise.
    bool project_point(
        const double                    time,
        const foundation::Vector3d&     point,
        foundation::Vector2d&           ndc) const;

    // Similar to project_point(), except that the input point is expressed in camera space.
    virtual bool project_camera_space_point(
        const foundation::Vector3d&     point,
        foundation::Vector2d&           ndc) const = 0;

    // Project a 3D segment back to the film plane. The input segment is expressed in
    // world space. The returned segment is expressed in normalized device coordinates.
    // Returns true if the projection was possible, false otherwise.
    virtual bool project_segment(
        const double                    time,
        const foundation::Vector3d&     a,
        const foundation::Vector3d&     b,
        foundation::Vector2d&           a_ndc,
        foundation::Vector2d&           b_ndc) const = 0;

  protected:
    TransformSequence   m_transform_sequence;
    double              m_shutter_open_time;
    double              m_shutter_close_time;
    double              m_shutter_open_time_interval;

    // Utility function to retrieve the film dimensions (in meters) from the camera parameters.
    foundation::Vector2d extract_film_dimensions() const;

    // Utility function to retrieve the focal length (in meters) from the camera parameters.
    double extract_focal_length(const double film_width) const;

    // Utility function to retrieve the f-stop value from the camera parameters.
    double extract_f_stop() const;

    // Utility function to retrieve the focal distance (in meters) from the camera parameters.
    void extract_focal_distance(
        bool&                           autofocus_enabled,
        foundation::Vector2d&           autofocus_target,
        double&                         focal_distance) const;

    // Utility function to retrieve the abscissa (in meters) of the near plane from the camera parameters.
    double extract_near_z() const;

    // Initialize a ray but does not set its origin or direction.
    void initialize_ray(
        SamplingContext&                sampling_context,
        ShadingRay&                     ray) const;

  private:
    bool has_param(const char* name) const;
    bool has_params(const char* name1, const char* name2) const;

    double get_greater_than_zero(
        const char*                     name,
        const double                    default_value) const;
};


//
// An incomplete factory class whose main purpose is to factorize the code
// common to all camera models.
//

class APPLESEED_DLLSYMBOL CameraFactory
{
  public:
    // Return a set of input metadata common to all camera models.
    static foundation::DictionaryArray get_input_metadata();
};


//
// Camera class implementation.
//

inline TransformSequence& Camera::transform_sequence()
{
    return m_transform_sequence;
}

inline const TransformSequence& Camera::transform_sequence() const
{
    return m_transform_sequence;
}

inline double Camera::get_shutter_open_time() const
{
    return m_shutter_open_time;
}

inline double Camera::get_shutter_close_time() const
{
    return m_shutter_close_time;
}

inline double Camera::get_shutter_middle_time() const
{
    return 0.5 * (m_shutter_open_time + m_shutter_close_time);
}

inline double Camera::get_shutter_open_time_interval() const
{
    return m_shutter_open_time_interval;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_CAMERA_CAMERA_H
