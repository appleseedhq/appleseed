
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
class QSignalMapper;
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
    void slot_add_layer();
    void slot_delete_layer();
    void slot_move_layer_up();
    void slot_move_layer_down();

    void slot_open_color_picker(const QString& widget_name);
    void slot_color_changed(const QString& widget_name, const QColor& color);

    void slot_open_file_picker(const QString& widget_name);
    void slot_open_seexpr_editor(const QString& widget_name);

  private:
    void create_connections();
    void create_buttons_connections(const QString& widget_name, QLineEdit* line_edit);
    void create_form_layout();
    void create_layer_layout();

    std::string unique_layer_name();
    void create_text_input_widgets(const std::string& parameter, const std::string& value);
    void create_color_input_widgets(const foundation::Dictionary& parameters, const std::string& layer_name);
    void create_colormap_input_widgets(const foundation::Dictionary& parameters, const std::string& layer_name);
    void add_layer();

    Ui::DisneyMaterialEntityEditor* m_ui;
    std::vector<QWidget*> m_layers_widgets;

    QWidget*                        m_parent;
    const renderer::Project&        m_project;
    QWidget*                        m_layer_widget;
    QWidget*                        m_selected_layer_widget;
    QWidget*                        m_color_button;
    QWidget*                        m_texture_button;
    QWidget*                        m_seexpr_button;
    QVBoxLayout*                    m_layer_layout;
    QVBoxLayout*                    m_form_layout;
    int                             m_num_created_layers;

    QSignalMapper*                  m_color_picker_signal_mapper;
    QSignalMapper*                  m_file_picker_signal_mapper;
    QSignalMapper*                  m_seexpr_editor_signal_mapper;

    InputWidgetProxyCollection      m_widget_proxies;

    class           LayerWidget;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALENTITYEDITOR_H
