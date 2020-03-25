
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

// appleseed.renderer headers.
#include "renderer/api/rendering.h"
#include "renderer/api/scenepicker.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QMetaType>
#include <QObject>

// Forward declarations.
namespace appleseed { namespace qtcommon { class MouseCoordinatesTracker; } }
namespace appleseed { namespace studio { class ItemBase; } }
namespace appleseed { namespace studio { class ProjectExplorer; } }
namespace renderer  { class Project; }
class QComboBox;
class QEvent;
class QPoint;
class QWidget;

Q_DECLARE_METATYPE(renderer::ScenePicker::PickingResult);

namespace appleseed {
namespace studio {

class ScenePickingHandler
  : public QObject
{
    Q_OBJECT

  public:
    ScenePickingHandler(
        QWidget*                                    widget,
        QComboBox*                                  picking_mode_combo,
        const qtcommon::MouseCoordinatesTracker&    mouse_tracker,
        const ProjectExplorer&                      project_explorer,
        const renderer::Project&                    project);

    ~ScenePickingHandler() override;

    void set_enabled(const bool enabled);

  signals:
    void signal_entity_picked(renderer::ScenePicker::PickingResult result);

  private:
    QWidget*                                        m_widget;
    QComboBox*                                      m_picking_mode_combo;
    const qtcommon::MouseCoordinatesTracker&        m_mouse_tracker;
    const ProjectExplorer&                          m_project_explorer;
    const renderer::Project&                        m_project;
    bool                                            m_enabled;

    bool eventFilter(QObject* object, QEvent* event) override;

    ItemBase* pick(const QPoint& point);
};

}   // namespace studio
}   // namespace appleseed
