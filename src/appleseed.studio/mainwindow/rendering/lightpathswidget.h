
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_LIGHTPATHSWIDGET_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_LIGHTPATHSWIDGET_H

// appleseed.studio headers.
#include "mainwindow/rendering/renderclipboardhandler.h"

// appleseed.renderer headers.
#include "renderer/api/lighting.h"

// On Windows, <QGLWidget> requires that <windows.h> is included first.
#include "foundation/math/matrix.h"
#include "foundation/math/transform.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

// Qt headers.
#include <QGLWidget>
#include <QObject>

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Camera; }
namespace renderer  { class Project; }
class QKeyEvent;
class QImage;

namespace appleseed {
namespace studio {

//
// A widget providing an hardware-accelerated visualization of recorded light paths.
//

class LightPathsWidget
  : public QGLWidget
  , public ICapturableWidget
{
    Q_OBJECT

  public:
    LightPathsWidget(
        const renderer::Project&            project,
        const size_t                        width,
        const size_t                        height);

    QImage capture() override;

    void set_transform(
        const foundation::Transformd&       transform);

    void set_light_paths(
        const renderer::LightPathArray&     light_paths);

    void set_selected_light_path_index(
        const int                           selected_light_path_index);

  signals:
    void signal_light_path_selection_changed(
        const int                           selected_light_path_index,
        const int                           total_light_paths) const;

  public slots:
    void slot_display_all_light_paths();
    void slot_display_previous_light_path();
    void slot_display_next_light_path();
    void slot_toggle_backface_culling(const bool checked);
    void slot_synchronize_camera();

  private:
    const renderer::Project&                m_project;
    renderer::Camera&                       m_camera;
    foundation::Matrix4d                    m_camera_matrix;

    bool                                    m_backface_culling_enabled;

    renderer::LightPathArray                m_light_paths;
    int                                     m_selected_light_path_index;    // -1 == display all paths

    void initializeGL() override;
    void resizeGL(int w, int h) override;
    void paintGL() override;
    void keyPressEvent(QKeyEvent* event) override;

    void render_geometry() const;
    void render_light_paths() const;
    void render_light_path(const size_t light_path_index) const;

    void dump_selected_light_path() const;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_LIGHTPATHSWIDGET_H
