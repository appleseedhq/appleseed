
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALENTITYEDITOR_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALENTITYEDITOR_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// Standard headers.
#include <string>
#include <vector>

// Qt headers.
#include <QObject>
#include <QWidget>

// Forward declarations.
namespace Ui { class DisneyMaterialEntityEditor; }
class QVBoxLayout;

namespace appleseed {
namespace studio {

class DisneyMaterialEntityEditor
    : public QWidget
{
    Q_OBJECT

  public:
    DisneyMaterialEntityEditor(
        QWidget*                                        parent,
        const renderer::Project&                        project,
        std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
        std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
        const foundation::Dictionary&                   values = foundation::Dictionary());

    virtual ~DisneyMaterialEntityEditor();

  private slots:
    void add_layer_slot();
    void delete_layer_slot();
    void move_layer_up_slot();
    void move_layer_down_slot();

  private:
    void create_form_layout();
    void create_layer_layout();
    
    std::string unique_layer_name();
    void create_text_input_widgets(const std::string& parameter, const std::string& value);
    void create_color_input_widgets(const std::string& parameter, int index);
    void create_colormap_input_widgets(const std::string& parameter, int index);
    void add_layer();

    Ui::DisneyMaterialEntityEditor* m_ui;
    std::vector<QWidget*> m_layers_widgets;

    int             m_num_created_layers;
    QWidget*        m_parent;
    QWidget*        m_scrollarea;
    QWidget*        m_layer_widget;
    QWidget*        m_selected_layer_widget;
    QVBoxLayout*    m_layer_layout;
    QVBoxLayout*    m_form_layout;
    class           LayerWidget;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALENTITYEDITOR_H
