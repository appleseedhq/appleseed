
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "foundation/math/transform.h"

// Qt headers.
#include <QEvent>
#include <QMouseEvent>
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
    Scene*      scene)
  : m_render_widget(render_widget)
  , m_camera(scene->get_camera())
{
    configure_controller(scene);

    m_render_widget->installEventFilter(this);
}

CameraController::~CameraController()
{
    m_render_widget->removeEventFilter(this);

    m_camera->get_parameters().insert("controller_target", m_controller.get_target());
}

void CameraController::update_camera_transform()
{
    m_camera->transform_sequence().earliest_transform() = Transformd(m_controller.get_transform());
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
    }

    return QObject::eventFilter(object, event);
}

void CameraController::configure_controller(const Scene* scene)
{
    // Set the controller orientation and position based on the scene camera.
    m_controller.set_transform(
        m_camera->transform_sequence().earliest_transform().get_local_to_parent());

    if (m_camera->get_parameters().strings().exist("controller_target"))
    {
        // The camera already has a target position, use it.
        m_controller.set_target(
            m_camera->get_parameters().get_optional<Vector3d>(
                "controller_target",
                Vector3d(0.0)));
    }
    else
    {
        // Otherwise, if the scene is not empty, use its center as the target position.
        const GAABB3 scene_bbox = scene->compute_bbox();
        if (scene_bbox.is_valid())
            m_controller.set_target(Vector3d(scene_bbox.center()));
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
        m_controller.begin_drag(
            (event->modifiers() & Qt::AltModifier) ? ControllerType::Track : ControllerType::Tumble,
            position);
        return true;

      case Qt::MidButton:
        m_controller.begin_drag(ControllerType::Track, position);
        return true;

      case Qt::RightButton:
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

}   // namespace studio
}   // namespace appleseed
