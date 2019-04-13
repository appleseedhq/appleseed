
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Camera controller.
//

template <typename T>
class CameraController
  : public NonCopyable
{
  public:
    // Types.
    typedef T ValueType;
    typedef Matrix<T, 4, 4> MatrixType;

    // Dragging movements.
    enum Movement
    {
        None,
        Tumble,
        Track,
        Dolly
    };

    // Constructor, sets the camera transformation to the identity
    // and the target to the origin.
    CameraController();

    // Set/get the camera transformation.
    void set_transform(const MatrixType& m);
    MatrixType get_transform() const;

    // Set/get the camera target.
    void set_target(const Vector<T, 3>& target);
    const Vector<T, 3>& get_target() const;

    // Update the camera transform, for instance if the camera target has changed.
    void update_transform();

    // Modify the camera position and orientation using 2D dragging movements.
    // 'point' is a point on the image plane, in normalized device coordinates,
    // that is in [0,1)^2 where (0,0) is at the top-left corner of the image.
    void begin_drag(const Movement movement, const Vector<T, 2>& point);
    void update_drag(const Vector<T, 2>& point);
    void end_drag();
    void cancel_drag();

    // Return the status of the controller.
    bool is_dragging() const;
    bool is_tumbling() const;
    bool is_tracking() const;
    bool is_dollying() const;

  private:
    struct View
    {
        Vector<T, 3>    m_position;         // camera position in world space
        Vector<T, 3>    m_x, m_y, m_z;      // camera basis vectors in world space (unit-length)
        Vector<T, 3>    m_target;           // camera target in world space
    };

    View                m_view;             // current view
    View                m_saved_view;       // view before dragging started
    Vector<T, 2>        m_drag_origin;      // origin in the image plane of the drag movement
    Movement            m_drag_movement;    // dragging movement

    void tumble(const Vector<T, 2>& delta);
    void track(const Vector<T, 2>& delta);
    void dolly(const Vector<T, 2>& delta);
};


//
// CameraController class implementation.
//

template <typename T>
CameraController<T>::CameraController()
  : m_drag_movement(None)
{
    set_transform(MatrixType::identity());
}

template <typename T>
inline void CameraController<T>::set_transform(const MatrixType& m)
{
    assert(m_drag_movement == None);

    // Extract camera position.
    m_view.m_position[0] = m[ 3];
    m_view.m_position[1] = m[ 7];
    m_view.m_position[2] = m[11];

    // Extract camera basis vectors.
    m_view.m_x[0] = m[ 0];
    m_view.m_x[1] = m[ 4];
    m_view.m_x[2] = m[ 8];
    m_view.m_y[0] = m[ 1];
    m_view.m_y[1] = m[ 5];
    m_view.m_y[2] = m[ 9];
    m_view.m_z[0] = m[ 2];
    m_view.m_z[1] = m[ 6];
    m_view.m_z[2] = m[10];

    // Make sure the basis vectors are normalized.
    m_view.m_x = normalize(m_view.m_x);
    m_view.m_y = normalize(m_view.m_y);
    m_view.m_z = normalize(m_view.m_z);

    // Compute initial camera target.
    m_view.m_target = m_view.m_position - m_view.m_z;
}

template <typename T>
inline Matrix<T, 4, 4> CameraController<T>::get_transform() const
{
    MatrixType m;

    // First column.
    m[ 0] = m_view.m_x[0];
    m[ 4] = m_view.m_x[1];
    m[ 8] = m_view.m_x[2];
    m[12] = T(0.0);

    // Second column.
    m[ 1] = m_view.m_y[0];
    m[ 5] = m_view.m_y[1];
    m[ 9] = m_view.m_y[2];
    m[13] = T(0.0);

    // Third column.
    m[ 2] = m_view.m_z[0];
    m[ 6] = m_view.m_z[1];
    m[10] = m_view.m_z[2];
    m[14] = T(0.0);

    // Fourth column.
    m[ 3] = m_view.m_position[0];
    m[ 7] = m_view.m_position[1];
    m[11] = m_view.m_position[2];
    m[15] = T(1.0);

    return m;
}

template <typename T>
inline void CameraController<T>::set_target(const Vector<T, 3>& target)
{
    m_view.m_target = target;
}

template <typename T>
inline const Vector<T, 3>& CameraController<T>::get_target() const
{
    return m_view.m_target;
}

template <typename T>
void CameraController<T>::update_transform()
{
    const Vector<T, 3> Up(T(0.0), T(1.0), T(0.0));

    // Compute view vector.
    const Vector<T, 3> u = m_view.m_position - m_view.m_target;

    // Update camera position.
    m_view.m_position = m_view.m_target + u;

    // Update camera basis vectors.
    m_view.m_z = normalize(u);
    m_view.m_x = normalize(cross(Up, m_view.m_z));
    m_view.m_y = normalize(cross(m_view.m_z, m_view.m_x));
}

template <typename T>
void CameraController<T>::begin_drag(const Movement movement, const Vector<T, 2>& point)
{
    assert(m_drag_movement == None);

    m_saved_view = m_view;
    m_drag_origin = point;
    m_drag_movement = movement;
}

template <typename T>
void CameraController<T>::update_drag(const Vector<T, 2>& point)
{
    assert(m_drag_movement != None);

    // Initial speed.
    T speed = T(1.0);

    // Modulate speed based on the length of the drag movement.
    const T drag_length = norm(point - m_drag_origin);
    speed *= std::pow(T(0.7) + drag_length, T(1.8));

    switch (m_drag_movement)
    {
      case Tumble:
        {
            // Tumble speed is constant.
            speed *= T(2.0);
        }
        break;

      case Track:
      case Dolly:
        {
            // Track/dolly speed is based on the distance to the target.
            const T target_distance = norm(m_view.m_target - m_view.m_position);
            speed *= T(2.0) * target_distance;
        }
        break;

      default:
        break;
    }

    const Vector<T, 2> delta = (point - m_drag_origin) * speed;

    m_drag_origin = point;

    switch (m_drag_movement)
    {
      case Tumble:
        tumble(delta);
        break;

      case Track:
        track(delta);
        break;

      case Dolly:
        dolly(delta);
        break;

      default:
        break;
    }
}

template <typename T>
void CameraController<T>::end_drag()
{
    assert(m_drag_movement != None);

    m_drag_movement = None;
}

template <typename T>
void CameraController<T>::cancel_drag()
{
    assert(m_drag_movement != None);

    m_view = m_saved_view;
    m_drag_movement = None;
}

template <typename T>
inline bool CameraController<T>::is_dragging() const
{
    return m_drag_movement != None;
}

template <typename T>
inline bool CameraController<T>::is_tumbling() const
{
    return m_drag_movement == Tumble;
}

template <typename T>
inline bool CameraController<T>::is_tracking() const
{
    return m_drag_movement == Track;
}

template <typename T>
inline bool CameraController<T>::is_dollying() const
{
    return m_drag_movement == Dolly;
}

template <typename T>
void CameraController<T>::tumble(const Vector<T, 2>& delta)
{
    const Vector<T, 3> Up(T(0.0), T(1.0), T(0.0));
    const T MinAngleToVertical = deg_to_rad(T(5.0));
    const T MaxAngleToVertical = Pi<T>() - MinAngleToVertical;

    // Compute view vector.
    Vector<T, 3> u = m_view.m_position - m_view.m_target;
    const T sq_dist = square_norm(u);

    // No tumbling is possible if the camera and the target share the same position.
    if (sq_dist == T(0.0))
        return;

    // Compute angle to vertical axis.
    const T angle_to_vertical = std::acos(dot(u, Up) / std::sqrt(sq_dist));

    // Clamp rotation around X axis.
    T delta_angle_to_vertical = delta.y * HalfPi<T>();
    if (delta_angle_to_vertical + angle_to_vertical < MinAngleToVertical)
        delta_angle_to_vertical = angle_to_vertical - MinAngleToVertical;
    if (delta_angle_to_vertical + angle_to_vertical > MaxAngleToVertical)
        delta_angle_to_vertical = angle_to_vertical - MaxAngleToVertical;

    // Rotate view vector.
    u = rotate(u, m_view.m_x, delta_angle_to_vertical);
    u = rotate(u, m_view.m_y, -delta.x * HalfPi<T>());

    // Prevent numerical drifting of the camera-target distance.
    u *= sq_dist / square_norm(u);

    // Update camera position.
    m_view.m_position = m_view.m_target + u;

    // Update camera basis vectors.
    m_view.m_z = normalize(u);
    m_view.m_x = normalize(cross(Up, m_view.m_z));
    m_view.m_y = normalize(cross(m_view.m_z, m_view.m_x));
}

template <typename T>
void CameraController<T>::track(const Vector<T, 2>& delta)
{
    // Update camera position and target.
    const Vector<T, 3> u = delta.x * m_view.m_x + delta.y * m_view.m_y;
    m_view.m_position -= u;
    m_view.m_target -= u;
}

template <typename T>
void CameraController<T>::dolly(const Vector<T, 2>& delta)
{
    T length = norm(delta);

    if (delta.y < T(0.0))
    {
        // Mouse movement downwards (left or right).
        length = -length;
    }
    else if (delta.y == T(0.0))
    {
        if (delta.x > T(0.0))
        {
            // Mouse movement only to the right.
            length = -length;
        }
    }

    // Update camera position.
    m_view.m_position -= length * m_view.m_z;
}

}   // namespace foundation
