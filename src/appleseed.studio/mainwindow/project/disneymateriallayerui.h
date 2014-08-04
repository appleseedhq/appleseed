
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Marius Avram, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALLAYERUI_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALLAYERUI_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// Standard headers.
#include <string>

// Qt headers.
#include <QFrame>
#include <QIcon>
#include <QObject>

// Forward declarations
namespace appleseed { namespace studio { class DisneyMaterialCustomUI; } }
class QVBoxLayout;
class QWidget;
class QToolButton;

namespace appleseed {
namespace studio {

class DisneyMaterialLayerUI
  : public QFrame
{
    Q_OBJECT

  public:
    DisneyMaterialLayerUI(
        const std::string&          layer_name,
        DisneyMaterialCustomUI*     entity_editor,
        QVBoxLayout*                parent_layout,
        QWidget*                    parent = 0);

    void mousePressEvent(QMouseEvent* event);
    void mouseDoubleClickEvent(QMouseEvent* event);
    QFormLayout* get_layout();

  private:
    void update_model(const int new_position, const int offset);
    void fold_layer(const bool update);

  private slots:
    void slot_delete_layer();
    void slot_move_layer_up();
    void slot_move_layer_down();
    void slot_fold();

  private:
    const std::string           m_layer_name;
    DisneyMaterialCustomUI*     m_entity_editor;

    QFormLayout*                m_inner_layout;
    QVBoxLayout*                m_parent_layout;
    QWidget*                    m_spacer;
    QToolButton*                m_fold_button;
    QIcon                       m_fold_arrow_enabled;
    QIcon                       m_fold_arrow_disabled;

    friend class DisneyMaterialCustomUI;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALLAYERUI_H

