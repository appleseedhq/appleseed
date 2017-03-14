
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// Qt headers.
#include <QObject>
#include <QString>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed     { namespace studio { class DoubleSlider; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class Project; }
class QColor;
class QLineEdit;
class QWidget;

namespace appleseed {
namespace studio {

std::string get_entity_name_dialog(
    QWidget*                parent,
    const std::string&      title,
    const std::string&      label,
    const std::string&      text);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    std::auto_ptr<CustomEntityUI>                   custom_entity_ui,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    std::auto_ptr<EntityEditor::IFormFactory>       form_factory,
    std::auto_ptr<EntityEditor::IEntityBrowser>     entity_browser,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void show_error_message_box(const std::string& title, const std::string& text);


//
// Updates the value of LineEdit associated with the given DoubleSlider.
//

class LineEditDoubleSliderAdaptor
  : public QObject
{
    Q_OBJECT

  public:
    LineEditDoubleSliderAdaptor(
        QLineEdit*      line_edit,
        DoubleSlider*   slider);

  public slots:
    void slot_set_line_edit_value(const double value);
    void slot_set_slider_value(const QString& value);
    void slot_apply_slider_value();

  private:
    QLineEdit*      m_line_edit;
    DoubleSlider*   m_slider;

    void adjust_slider(const double new_value);
};


//
// Adds extra information to the signal emitted when the color changes.
//

class ForwardColorChangedSignal
  : public QObject
{
    Q_OBJECT

  public:
    ForwardColorChangedSignal(
        QObject*        parent,
        const QString&  widget_name,
        const QColor&   initial_color);

  public slots:
    void slot_color_changed(const QColor& color);
    void slot_color_reset();

  signals:
    void signal_color_changed(const QString& widget_name, const QColor& color);
    void signal_color_reset(const QString& widget_name, const QColor& color);

  private:
    const QString m_widget_name;
    const QColor  m_initial_color;
    QColor        m_current_color;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_TOOLS_H
