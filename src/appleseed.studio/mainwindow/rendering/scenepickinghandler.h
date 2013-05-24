
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_RENDERING_SCENEPICKINGHANDLER_H
#define APPLESEED_STUDIO_MAINWINDOW_RENDERING_SCENEPICKINGHANDLER_H

// Qt headers.
#include <QObject>
#include <QString>

// Forward declarations.
namespace appleseed { namespace studio { class MouseCoordinatesTracker; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace renderer  { class Project; }
class QComboBox;
class QEvent;
class QPoint;

namespace appleseed {
namespace studio {

class ScenePickingHandler
  : public QObject
{
    Q_OBJECT

  public:
    ScenePickingHandler(
        QComboBox*                          picking_mode_combo,
        const MouseCoordinatesTracker&      mouse_tracker,
        const ProjectExplorer&              project_explorer,
        const renderer::Project&            project);

    ~ScenePickingHandler();

  public slots:
    void slot_picking_mode_changed(const int index);

  private:
    QComboBox*                              m_picking_mode_combo;
    QString                                 m_picking_mode;
    const MouseCoordinatesTracker&          m_mouse_tracker;
    const ProjectExplorer&                  m_project_explorer;
    const renderer::Project&                m_project;

    virtual bool eventFilter(QObject* object, QEvent* event);

    void pick(const QPoint& point);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_RENDERING_SCENEPICKINGHANDLER_H
