
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Marius Avram, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H

// appleseed.studio headers.
#include "mainwindow/project/customentityui.h"
#include "utility/inputwidgetproxies.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class DisneyMaterialLayerUI; } }
namespace appleseed { namespace studio { class LineEditForwarder; } }
namespace renderer  { class Project; }
class QFormLayout;
class QPushButton;
class QSignalMapper;
class QVBoxLayout;
class QWidget;

namespace appleseed {
namespace studio {

class DisneyMaterialCustomUI
  : public CustomEntityUI
{
    Q_OBJECT

  public:
    explicit DisneyMaterialCustomUI(const renderer::Project& project);

    virtual void  create_custom_widgets(
        QVBoxLayout*                    layout,
        const foundation::Dictionary&   values) APPLESEED_OVERRIDE;

    virtual foundation::Dictionary get_values() const APPLESEED_OVERRIDE;

  private slots:
    void slot_add_layer();

    void slot_open_color_picker(const QString& widget_name);
    void slot_color_changed(const QString& widget_name, const QColor& color);

    void slot_open_file_picker(const QString& widget_name);
    void slot_open_expression_editor(const QString& widget_name);
    void slot_expression_changed(const QString& widget_name, const QString& expression);
    void slot_line_edit_changed(const QString& widget_name);
    void slot_expression_editor_closed();

  private:
    friend class DisneyMaterialLayerUI;

    void create_connections();
    void create_buttons_connections(const QString& widget_name);
    void create_layer_layout(const std::string& layer_name);

    std::string unique_layer_name();
    std::string texture_to_expression(const QString& path);
    static std::string expression_to_texture(const std::string& expr);

    void create_text_input_widgets(const foundation::Dictionary& parameter, const std::string& group_name);
    void create_color_input_widgets(const foundation::Dictionary& parameters, const std::string& group_name);
    void create_colormap_input_widgets(const foundation::Dictionary& parameters, const std::string& group_name);

    void create_texture_and_expr_buttons();

    void add_layer(const bool update, const foundation::Dictionary& parameters);
    void layer_deleted(DisneyMaterialLayerUI* layer);

    std::vector<QWidget*>       m_layers_widgets;

    QWidget*                    m_parent;
    const renderer::Project&    m_project;
    QWidget*                    m_group_widget;
    QWidget*                    m_selected_layer_widget;
    DisneyMaterialLayerUI*      m_last_layer;
    QPushButton*                m_add_layer_button;
    LineEditForwarder*          m_line_edit;
    QPushButton*                m_texture_button;
    QPushButton*                m_expression_button;
    QFormLayout*                m_group_layout;
    QVBoxLayout*                m_form_layout;
    size_t                      m_num_created_layers;

    QSignalMapper*              m_color_picker_signal_mapper;
    QSignalMapper*              m_file_picker_signal_mapper;
    QSignalMapper*              m_expression_editor_signal_mapper;
    QSignalMapper*              m_line_edit_signal_mapper;

    InputWidgetProxyCollection  m_widget_proxies;
    foundation::Dictionary      m_renames;
    foundation::Dictionary      m_values;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALCUSTOMUI_H
