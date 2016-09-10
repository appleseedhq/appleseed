
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Marius Avram, The appleseedhq Organization
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
#include "utility/inputwidgetproxies.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Qt headers.
#include <QFrame>
#include <QIcon>
#include <QObject>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
class QColor;
class QFormLayout;
class QMouseEvent;
class QSignalMapper;
class QString;
class QToolButton;
class QWidget;

namespace appleseed {
namespace studio {

class DisneyMaterialLayerUI
  : public QFrame
{
    Q_OBJECT

  public:
    DisneyMaterialLayerUI(
        QWidget*                        parent,
        const renderer::Project&        project,
        renderer::ParamArray&           settings,
        const foundation::Dictionary&   values);

    foundation::Dictionary get_values() const;

    virtual void mouseDoubleClickEvent(QMouseEvent* event) APPLESEED_OVERRIDE;

  signals:
    void signal_move_layer_up(QWidget* layer_widget);
    void signal_move_layer_down(QWidget* layer_widget);
    void signal_delete_layer(QWidget* layer_widget);
    void signal_apply();

  private slots:
    void slot_fold_unfold_layer();
    void slot_move_layer_up();
    void slot_move_layer_down();
    void slot_delete_layer();

    void slot_open_color_picker(const QString& widget_name);
    void slot_color_changed(const QString& widget_name, const QColor& color);

    void slot_open_file_picker(const QString& widget_name);

    void slot_open_expression_editor(const QString& widget_name);
    void slot_expression_changed(const QString& widget_name, const QString& expression);

  private:
    const renderer::Project&            m_project;
    renderer::ParamArray&               m_settings;
    const foundation::DictionaryArray   m_input_metadata;

    const QIcon                         m_fold_icon;
    const QIcon                         m_unfold_icon;

    QToolButton*                        m_fold_unfold_button;
    QWidget*                            m_header_widget;
    QFormLayout*                        m_header_layout;
    QWidget*                            m_content_widget;
    QFormLayout*                        m_content_layout;

    InputWidgetProxyCollection          m_widget_proxies;

    QSignalMapper*                      m_color_picker_signal_mapper;
    QSignalMapper*                      m_file_picker_signal_mapper;
    QSignalMapper*                      m_expression_editor_signal_mapper;

    bool                                m_is_folded;

    void create_layer_ui();
    void create_input_widgets(const foundation::Dictionary& values);

    std::auto_ptr<IInputWidgetProxy> create_text_input_widgets(const foundation::Dictionary& metadata);
    std::auto_ptr<IInputWidgetProxy> create_color_input_widgets(const foundation::Dictionary& metadata);
    std::auto_ptr<IInputWidgetProxy> create_colormap_input_widgets(const foundation::Dictionary& metadata);

    QWidget* create_texture_button(const std::string& name);
    QWidget* create_expression_button(const std::string& name);

    void fold();
    void unfold();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_DISNEYMATERIALLAYERUI_H
