
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_CAMERACONTROLLER_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_CAMERACONTROLLER_H

// appleseed.renderer headers.
#include "renderer/api/rendering.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/ui/cameracontroller.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace renderer  { class Project; }
class QEvent;
class QKeyEvent;
class QMouseEvent;
class QWidget;

namespace appleseed {
namespace studio {

class CameraController
  : public QObject
{
    Q_OBJECT

  public:
    // Constructor.
    CameraController(
        QWidget*            render_widget,
        renderer::Project&  project);

    // Destructor.
    ~CameraController();

    foundation::Transformd get_transform() const;

    void update_camera_transform();
    void save_camera_target();

  public slots:
    void slot_entity_picked(renderer::ScenePicker::PickingResult result);
    void slot_frame_modified();

  signals:
    void signal_camera_change_begin();
    void signal_camera_changed();
    void signal_camera_change_end();

  private:
    typedef foundation::CameraController<double> ControllerType;

    QWidget*                m_render_widget;
    renderer::Project&      m_project;
    ControllerType          m_controller;
    foundation::Vector3d    m_pivot;

    void configure_controller();

    virtual bool eventFilter(QObject* object, QEvent* event) APPLESEED_OVERRIDE;

    bool handle_mouse_button_press_event(const QMouseEvent* event);
    bool handle_mouse_button_release_event(const QMouseEvent* event);
    bool handle_mouse_move_event(const QMouseEvent* event);
    bool handle_key_press_event(const QKeyEvent* event);

    foundation::Vector2d get_mouse_position(const QMouseEvent* event) const;

    void frame_selected_object();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_CAMERACONTROLLER_H
