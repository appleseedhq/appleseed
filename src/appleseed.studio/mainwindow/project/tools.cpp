
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

// Interface header.
#include "tools.h"

// appleseed.studio headers.
#include "mainwindow/project/entityeditorwindow.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "widgets/doubleslider.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"

// Qt headers.
#include <QInputDialog>
#include <QLineEdit>
#include <QMessageBox>
#include <QObject>
#include <QString>

// Standard headers.
#include <cmath>
#include <utility>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

std::string get_entity_name_dialog(
    QWidget*             parent,
    const std::string&   title,
    const std::string&   label,
    const std::string&   text)
{
    QString result;
    bool ok;

    do
    {
        result =
            QInputDialog::getText(
                parent,
                QString::fromStdString(title),
                QString::fromStdString(label),
                QLineEdit::Normal,
                QString::fromStdString(text),
                &ok);
    } while (ok && result.isEmpty());

    return ok ? result.toStdString() : std::string();
}

void open_entity_editor(
    QWidget*                                         parent,
    const std::string&                               window_title,
    const Project&                                   project,
    renderer::ParamArray&                            settings,
    std::unique_ptr<EntityEditor::IFormFactory>      form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>    entity_browser,
    std::unique_ptr<CustomEntityUI>                  custom_entity_ui,
    const Dictionary&                                values,
    QObject*                                         receiver,
    const char*                                      slot_apply,
    const char*                                      slot_accept,
    const char*                                      slot_cancel)
{
    EntityEditorWindow* editor_window =
        new EntityEditorWindow(
            parent,
            window_title,
            project,
            settings,
            move(form_factory),
            move(entity_browser),
            move(custom_entity_ui),
            values);

    QObject::connect(
        editor_window, SIGNAL(signal_applied(foundation::Dictionary)),
        receiver, slot_apply);

    QObject::connect(
        editor_window, SIGNAL(signal_accepted(foundation::Dictionary)),
        receiver, slot_accept);

    QObject::connect(
        editor_window, SIGNAL(signal_canceled(foundation::Dictionary)),
        receiver, slot_cancel);

    editor_window->showNormal();
    editor_window->activateWindow();
}

void open_entity_editor(
    QWidget*                                         parent,
    const std::string&                               window_title,
    const Project&                                   project,
    renderer::ParamArray&                            settings,
    std::unique_ptr<EntityEditor::IFormFactory>      form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>    entity_browser,
    const Dictionary&                                values,
    QObject*                                         receiver,
    const char*                                      slot_apply,
    const char*                                      slot_accept,
    const char*                                      slot_cancel)
{
    open_entity_editor(
        parent,
        window_title,
        project,
        settings,
        std::move(form_factory),
        std::move(entity_browser),
        std::unique_ptr<CustomEntityUI>(),
        values,
        receiver,
        slot_apply,
        slot_accept,
        slot_cancel);
}

void open_entity_editor(
    QWidget*                                         parent,
    const std::string&                               window_title,
    const Project&                                   project,
    renderer::ParamArray&                            settings,
    std::unique_ptr<EntityEditor::IFormFactory>      form_factory,
    std::unique_ptr<EntityEditor::IEntityBrowser>    entity_browser,
    QObject*                                         receiver,
    const char*                                      slot_apply,
    const char*                                      slot_accept,
    const char*                                      slot_cancel)
{
    open_entity_editor(
        parent,
        window_title,
        project,
        settings,
        std::move(form_factory),
        std::move(entity_browser),
        std::unique_ptr<CustomEntityUI>(),
        Dictionary(),
        receiver,
        slot_apply,
        slot_accept,
        slot_cancel);
}

void show_error_message_box(const std::string& title, const std::string& text)
{
    QMessageBox msgbox;
    msgbox.setWindowTitle(QString::fromStdString(title));
    msgbox.setIcon(QMessageBox::Critical);
    msgbox.setText(QString::fromStdString(text));
    msgbox.setStandardButtons(QMessageBox::Ok);
    msgbox.exec();
}


//
// LineEditSliderAdaptor class implementation.
//

LineEditSliderAdaptor::LineEditSliderAdaptor(
    QLineEdit*      line_edit,
    QSlider*        slider)
  : QObject(line_edit)
  , m_line_edit(line_edit)
  , m_slider(slider)
{
    slot_set_slider_value(m_line_edit->text());

    // When the slider is moved, update the line edit's value.
    connect(
        m_slider, SIGNAL(valueChanged(const int)),
        SLOT(slot_set_line_edit_value(const int)));

    // When the line edit value is changed, update the slider's position.
    connect(
        m_line_edit, SIGNAL(textChanged(const QString&)),
        SLOT(slot_set_slider_value(const QString&)));

    // When enter is pressed in the line edit, update the slider's position.
    connect(
        m_line_edit, SIGNAL(editingFinished()),
        SLOT(slot_apply_line_edit_value()));
}

void LineEditSliderAdaptor::slot_set_line_edit_value(const int value)
{
    const QString new_line_edit_value = QString("%1").arg(value);

    // Don't block signals here, for live edit to work we want the line edit to signal changes.
    m_line_edit->setText(new_line_edit_value);
}

void LineEditSliderAdaptor::slot_set_slider_value(const QString& value)
{
    if (value.isEmpty())
        return;

    const bool were_signals_blocked = m_slider->blockSignals(true);
    m_slider->setValue(value.toInt());
    m_slider->blockSignals(were_signals_blocked);
}

void LineEditSliderAdaptor::slot_apply_line_edit_value()
{
    slot_set_slider_value(m_line_edit->text());
}


//
// LineEditDoubleSliderAdaptor class implementation.
//

LineEditDoubleSliderAdaptor::LineEditDoubleSliderAdaptor(
    QLineEdit*      line_edit,
    DoubleSlider*   slider)
  : QObject(line_edit)
  , m_line_edit(line_edit)
  , m_slider(slider)
{
    slot_set_slider_value(m_line_edit->text());

    // When the slider is moved, update the line edit's value.
    connect(
        m_slider, SIGNAL(valueChanged(const double)),
        SLOT(slot_set_line_edit_value(const double)));

    // When the line edit value is changed, update the slider's position.
    connect(
        m_line_edit, SIGNAL(textChanged(const QString&)),
        SLOT(slot_set_slider_value(const QString&)));

    // When enter is pressed in the line edit, update the slider's position.
    connect(
        m_line_edit, SIGNAL(editingFinished()),
        SLOT(slot_apply_line_edit_value()));
}

void LineEditDoubleSliderAdaptor::slot_set_line_edit_value(const double value)
{
    // Format integer values such as 2 as "2.0" instead of "2".
    const bool is_integer = std::floor(value) == value;
    const QString format_string = is_integer ? "%1.0" : "%1";
    const QString new_line_edit_value = format_string.arg(value);

    // Don't block signals here, for live edit to work we want the line edit to signal changes.
    m_line_edit->setText(new_line_edit_value);
}

void LineEditDoubleSliderAdaptor::slot_set_slider_value(const QString& value)
{
    if (value.isEmpty())
        return;

    const bool were_signals_blocked = m_slider->blockSignals(true);

    const double new_value = value.toDouble();

    // Adjust range if the new value is outside the current range.
    // The test is intentionally different than the one in slot_apply_line_edit_value().
    if (new_value < m_slider->minimum() ||
        new_value > m_slider->maximum())
        adjust_slider(new_value);

    m_slider->setValue(new_value);

    m_slider->blockSignals(were_signals_blocked);
}

void LineEditDoubleSliderAdaptor::slot_apply_line_edit_value()
{
    const bool were_signals_blocked = m_slider->blockSignals(true);

    const double new_value = m_line_edit->text().toDouble();

    // Adjust range if the new value is outside the current range,
    // or if a value of a significantly smaller magnitude was entered.
    if (new_value < m_slider->minimum() ||
        new_value > m_slider->maximum() ||
        std::abs(new_value) < (m_slider->maximum() - m_slider->minimum()) / 3.0)
        adjust_slider(new_value);

    m_slider->setValue(new_value);

    // Force reformatting of the value in the QLineEdit if necessary.
    if (!m_line_edit->text().isEmpty())
        slot_set_line_edit_value(new_value);

    m_slider->blockSignals(were_signals_blocked);
}

void LineEditDoubleSliderAdaptor::adjust_slider(const double new_value)
{
    const double new_min = new_value >= 0.0 ? 0.0 : -2.0 * std::abs(new_value);
    const double new_max = new_value == 0.0 ? 1.0 : +2.0 * std::abs(new_value);
    m_slider->setRange(new_min, new_max);
    m_slider->setPageStep((new_max - new_min) / 10.0);
}


//
// QDoubleValidatorWithDefault class implementation.
//

QDoubleValidatorWithDefault::QDoubleValidatorWithDefault(const QString& default_value, QObject* parent)
  : QDoubleValidator(parent)
  , m_default_value(default_value)
{
}

QDoubleValidatorWithDefault::QDoubleValidatorWithDefault(
    const double    bottom,
    const double    top,
    const int       decimals,
    const QString&  default_value,
    QObject*        parent)
  : QDoubleValidator(bottom, top, decimals, parent)
  , m_default_value(default_value)
{
}

QValidator::State QDoubleValidatorWithDefault::validate(QString& input, int& pos) const
{
    return
        input == m_default_value
            ? QValidator::Acceptable
            : QDoubleValidator::validate(input, pos);
}


//
// ForwardColorChangedSignal class implementation.
//

ForwardColorChangedSignal::ForwardColorChangedSignal(
    QObject*        parent,
    const QString&  widget_name,
    const QColor&   initial_color)
  : QObject(parent)
  , m_widget_name(widget_name)
  , m_initial_color(initial_color)
  , m_current_color(initial_color)
{
}

void ForwardColorChangedSignal::slot_color_reset()
{
    if (m_current_color != m_initial_color)
        emit signal_color_reset(m_widget_name, m_initial_color);
}

void ForwardColorChangedSignal::slot_color_changed(const QColor& color)
{
    m_current_color = color;
    emit signal_color_changed(m_widget_name, color);
}

}   // namespace studio
}   // namespace appleseed
