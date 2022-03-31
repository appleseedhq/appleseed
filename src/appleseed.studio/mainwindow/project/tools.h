
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

// appleseed.studio headers.
#include "mainwindow/project/entityeditor.h"

// Qt headers.
#include <QDoubleValidator>
#include <QObject>
#include <QString>

// Standard headers.
#include <memory>
#include <string>

// Forward declarations.
namespace appleseed     { namespace qtcommon { class DoubleSlider; } }
namespace foundation    { class Dictionary; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
class QColor;
class QLineEdit;
class QSlider;
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
    renderer::ParamArray&                           settings,
    std::unique_ptr<EntityEditor::IFormFactory>     form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>   entity_browser,
    std::unique_ptr<CustomEntityUI>                 custom_entity_ui,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    renderer::ParamArray&                           settings,
    std::unique_ptr<EntityEditor::IFormFactory>     form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>   entity_browser,
    const foundation::Dictionary&                   values,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void open_entity_editor(
    QWidget*                                        parent,
    const std::string&                              window_title,
    const renderer::Project&                        project,
    renderer::ParamArray&                           settings,
    std::unique_ptr<EntityEditor::IFormFactory>     form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>   entity_browser,
    QObject*                                        receiver,
    const char*                                     slot_apply,
    const char*                                     slot_accept,
    const char*                                     slot_cancel);

void show_error_message_box(const std::string& title, const std::string& text);


//
// Binds QLineEdit and QSlider controls together such that updading
// the value in one control updates the value in the other.
//

class LineEditSliderAdaptor
  : public QObject
{
    Q_OBJECT

  public:
    LineEditSliderAdaptor(
        QLineEdit*      line_edit,
        QSlider*        slider);

  public slots:
    void slot_set_line_edit_value(const int value);
    void slot_set_slider_value(const QString& value);
    void slot_apply_line_edit_value();

  private:
    QLineEdit*      m_line_edit;
    QSlider*        m_slider;
};


//
// Binds QLineEdit and qtcommon::DoubleSlider controls together such that updating
// the value in one control updates the value in the other.
//

class LineEditDoubleSliderAdaptor
  : public QObject
{
    Q_OBJECT

  public:
    LineEditDoubleSliderAdaptor(
        QLineEdit*              line_edit,
        qtcommon::DoubleSlider* slider);

  public slots:
    void slot_set_line_edit_value(const double value);
    void slot_set_slider_value(const QString& value);
    void slot_apply_line_edit_value();

  private:
    QLineEdit*                  m_line_edit;
    qtcommon::DoubleSlider*     m_slider;

    void adjust_slider(const double new_value);
};


//
// A QDoubleValidator that also accepts a given string value, for instance an empty string.
//

class QDoubleValidatorWithDefault
  : public QDoubleValidator
{
  public:
    explicit QDoubleValidatorWithDefault(const QString& default_value, QObject* parent = nullptr);
    QDoubleValidatorWithDefault(
        const double    bottom,
        const double    top,
        const int       decimals,
        const QString&  default_value,
        QObject*        parent = nullptr);

    QValidator::State validate(QString& input, int& pos) const override;

  private:
    const QString m_default_value;
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

}   // namespace studio
}   // namespace appleseed
