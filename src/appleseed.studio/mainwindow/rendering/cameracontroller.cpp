
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
#include "cameracontroller.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
#include "foundation/math/distance.h"
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#include "foundation/utility/iostreamop.h"

// Qt headers.
#include <QEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <Qt>
#include <QWidget>

using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// CameraController class implementation.
//
// Terminology:
//
//   Camera
//     The active camera of the scene, if there is one.
//
//   Camera Controller (or simply Controller)
//     This class, allows to control the camera using the mouse.
//
//   Controller Target
//     The point the controller (and thus the camera) "looks at".
//     It should always be along the viewing direction of the camera.
//
//   Pivot Point
//     A point of the scene that can be changed by clicking on objects
//     (or clicking in empty space to reset it) and that can become the
//     controller target if the user wishes so (by pressing 'f').
//

CameraController::CameraController(QWidget* widget, Project& project)
  : m_widget(widget)
  , m_project(project)
  , m_custom_camera(nullptr)
  , m_enabled(true)
{
    configure_controller();
    m_widget->installEventFilter(this);
}

CameraController::CameraController(QWidget* widget, Project& project, Camera* camera)
  : m_widget(widget)
  , m_project(project)
  , m_custom_camera(camera)
  , m_enabled(true)
{
    configure_controller();
    m_widget->installEventFilter(this);
}

CameraController::~CameraController()
{
    m_widget->removeEventFilter(this);
}

void CameraController::set_enabled(const bool enabled)
{
    m_enabled = enabled;
}

Transformd CameraController::get_transform() const
{
    return Transformd::from_local_to_parent(m_controller.get_transform());
}

void CameraController::update_camera_transform()
{
    if (Camera* camera = fetch_camera())
    {
        // Moving the camera kills camera motion blur.
        camera->transform_sequence().clear();

        // Set the scene camera orientation and position based on the controller.
        camera->transform_sequence().set_transform(0.0f, get_transform());
    }
}

void CameraController::save_camera_target()
{
    if (Camera* camera = fetch_camera())
    {
        camera->get_parameters().insert("controller_target", m_controller.get_target());
    }
}

void CameraController::slot_entity_picked(ScenePicker::PickingResult result)
{
    if (result.m_object_instance)
    {
        const GAABB3 object_instance_world_bbox =
            result.m_assembly_instance_transform.to_parent(
                result.m_object_instance->compute_parent_bbox());

        m_pivot = Vector3d(object_instance_world_bbox.center());
    }
    else
    {
        m_pivot = Vector3d(m_project.get_scene()->compute_bbox().center());
    }
}

void CameraController::slot_frame_modified()
{
    configure_controller();
}

bool CameraController::eventFilter(QObject* object, QEvent* event)
{
    if (m_enabled)
    {
        switch (event->type())
        {
          case QEvent::MouseButtonPress:
            if (handle_mouse_button_press_event(static_cast<QMouseEvent*>(event)))
                return true;
            break;

          case QEvent::MouseButtonRelease:
            if (handle_mouse_button_release_event(static_cast<QMouseEvent*>(event)))
                return true;
            break;

          case QEvent::MouseMove:
            if (handle_mouse_move_event(static_cast<QMouseEvent*>(event)))
                return true;
            break;

          case QEvent::KeyPress:
            if (handle_key_press_event(static_cast<QKeyEvent*>(event)))
                return true;
            break;
        }
    }

    return QObject::eventFilter(object, event);
}

Camera* CameraController::fetch_camera()
{
    if (m_custom_camera != nullptr)
        return m_custom_camera;
    else
        return m_project.get_uncached_active_camera();
}

void CameraController::configure_controller()
{
    // By default, the pivot point is the scene's center.
    m_pivot = Vector3d(m_project.get_scene()->compute_bbox().center());

    Camera* camera = fetch_camera();

    // Set the controller orientation and position.
    if (camera != nullptr)
    {
        // Use the scene's camera.
        m_controller.set_transform(
            camera->transform_sequence().get_earliest_transform().get_local_to_parent());
    }
    else
    {
        // Otherwise use a default orientation and position.
        m_controller.set_transform(
            Matrix4d::make_lookat(
                Vector3d(1.0, 1.0, 1.0),    // origin
                Vector3d(0.0, 0.0, 0.0),    // target
                Vector3d(0.0, 1.0, 0.0)));  // up
    }

    // Check whether the camera has a controller target.
    const bool has_target =
        camera != nullptr &&
        camera->get_parameters().strings().exist("controller_target");

    // Retrieve the controller target from the camera.
    Vector3d controller_target;
    if (has_target)
    {
        // The scene's camera has a controller target, retrieve it.
        controller_target =
            camera->get_parameters().get<Vector3d>("controller_target");
    }
    else
    {
        // Otherwise use the pivot point.
        controller_target = m_pivot;
    }

    const Matrix4d& m = m_controller.get_transform();
    const Vector3d camera_position = m.extract_translation();
    const Vector3d camera_direction = normalize(Vector3d(-m[2], -m[6], -m[10]));
    const Vector3d to_target = controller_target - camera_position;

    const double target_to_viewing_vector_distance =
        std::sqrt(
            square_distance_point_line(
                controller_target,
                camera_position,
                camera_direction));

    // Check whether the target is on the viewing vector and in front of the camera.
    const bool target_is_behind = dot(to_target, camera_direction) < 0.0;
    const bool target_is_off = target_to_viewing_vector_distance > 1.0e-4;
    if (has_target && (target_is_behind || target_is_off))
        RENDERER_LOG_WARNING("camera's controller target is off the viewing direction, realigning it.");

    // Realign the controller target if necessary.
    if (target_is_behind)
    {
        // If the target is behind, choose a new valid target.
        controller_target = camera_position + camera_direction;
    }
    else if (target_is_off)
    {
        // If the target is not behind but it is off, move it
        // to the closest point on the viewing vector.
        controller_target =
            camera_position +
            dot(to_target, camera_direction) * camera_direction;
    }

    // Set the controller target.
    m_controller.set_target(controller_target);

    // If the camera had a controller target, update it.
    if (has_target)
        save_camera_target();
}

Vector2d CameraController::get_mouse_position(const QMouseEvent* event) const
{
    const int width = m_widget->width();
    const int height = m_widget->height();

    const double x = static_cast<double>(event->x()) / width;
    const double y = 1.0 - static_cast<double>(event->y()) / height;

    return Vector2d(x, y);
}

bool CameraController::handle_mouse_button_press_event(const QMouseEvent* event)
{
    if (m_controller.is_dragging())
        return false;

    if (!(event->modifiers() & Qt::ControlModifier))
        return false;

    const Vector2d position = get_mouse_position(event);

    switch (event->button())
    {
      case Qt::LeftButton:
        emit signal_camera_change_begin();
        m_controller.begin_drag(
            (event->modifiers() & Qt::AltModifier) ? ControllerType::Track : ControllerType::Tumble,
            position);
        return true;

      case Qt::MidButton:
        emit signal_camera_change_begin();
        m_controller.begin_drag(ControllerType::Track, position);
        return true;

      case Qt::RightButton:
        emit signal_camera_change_begin();
        m_controller.begin_drag(ControllerType::Dolly, position);
        return true;
    }

    return false;
}

bool CameraController::handle_mouse_button_release_event(const QMouseEvent* event)
{
    if (!m_controller.is_dragging())
        return false;

    m_controller.end_drag();
    emit signal_camera_change_end();

    return true;
}

bool CameraController::handle_mouse_move_event(const QMouseEvent* event)
{
    if (!m_controller.is_dragging())
        return false;

    const Vector2d position = get_mouse_position(event);

    m_controller.update_drag(position);
    emit signal_camera_changed();

    return true;
}

bool CameraController::handle_key_press_event(const QKeyEvent* event)
{
    switch (event->key())
    {
      case Qt::Key_F:
        frame_selected_object();
        return true;
    }

    return false;
}

void CameraController::frame_selected_object()
{
    m_controller.set_target(m_pivot);
    m_controller.update_transform();
    emit signal_camera_changed();
}

}   // namespace studio
}   // namespace appleseed
