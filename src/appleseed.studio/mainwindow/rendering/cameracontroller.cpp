
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
#include "cameracontroller.h"

// appleseed.renderer headers.
#include "renderer/api/camera.h"
#include "renderer/api/scene.h"
#include "renderer/api/utility.h"

// appleseed.foundation headers.
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

CameraController::CameraController(
    QWidget*    render_widget,
    Project&    project)
  : m_render_widget(render_widget)
  , m_project(project)
  , m_enabled(false)
{
    configure_controller();
}

CameraController::~CameraController()
{
    set_enabled(false);
}

void CameraController::set_enabled(const bool enabled)
{
    if (enabled == m_enabled)
        return;

    if (enabled)
        m_render_widget->installEventFilter(this);
    else m_render_widget->removeEventFilter(this);

    m_enabled = enabled;
}

Transformd CameraController::get_transform() const
{
    return Transformd::from_local_to_parent(m_controller.get_transform());
}

void CameraController::update_camera_transform()
{
    if (Camera* camera = m_project.get_uncached_active_camera())
    {
        // Moving the camera kills camera motion blur.
        camera->transform_sequence().clear();

        // Set the scene camera orientation and position based on the controller.
        camera->transform_sequence().set_transform(0.0f, get_transform());
    }
}

void CameraController::save_camera_target()
{
    if (Camera* camera = m_project.get_uncached_active_camera())
        camera->get_parameters().insert("controller_target", m_controller.get_target());
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

    return QObject::eventFilter(object, event);
}

void CameraController::configure_controller()
{
    // By default, the pivot point is the scene's center.
    m_pivot = Vector3d(m_project.get_scene()->compute_bbox().center());

    Camera* camera = m_project.get_uncached_active_camera();

    // Set the controller orientation and position.
    if (camera)
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

    // Set the controller target.
    if (camera && camera->get_parameters().strings().exist("controller_target"))
    {
        // The scene's camera already has a target position, use it.
        m_controller.set_target(
            camera->get_parameters().get<Vector3d>("controller_target"));
    }
    else
    {
        // Otherwise use the pivot point.
        m_controller.set_target(m_pivot);
    }
}

Vector2d CameraController::get_mouse_position(const QMouseEvent* event) const
{
    const int width = m_render_widget->width();
    const int height = m_render_widget->height();

    const double x =  static_cast<double>(event->x()) / width;
    const double y = -static_cast<double>(event->y()) / height;

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
