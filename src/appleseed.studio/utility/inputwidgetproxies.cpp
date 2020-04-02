
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
#include "inputwidgetproxies.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"

// appleseed.renderer headers.
#include "renderer/api/color.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"

// Qt headers.
#include <QCheckBox>
#include <QColor>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QGroupBox>
#include <QLineEdit>
#include <QRadioButton>
#include <QSpinBox>
#include <QString>
#include <QToolButton>
#include <QVariant>

// Standard headers.
#include <vector>

using namespace appleseed::qtcommon;
using namespace foundation;
using namespace renderer;

namespace appleseed {
namespace studio {

//
// IInputWidgetProxy class implementation.
//

void IInputWidgetProxy::emit_signal_changed()
{
    emit signal_changed();
}

void IInputWidgetProxy::slot_set(const QString& value)
{
    set(value.toStdString());
}


//
// LineEditProxy class implementation.
//

LineEditProxy::LineEditProxy(QLineEdit* line_edit)
  : m_line_edit(line_edit)
{
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));
}

void LineEditProxy::set(const std::string& value)
{
    m_line_edit->setText(QString::fromStdString(value));
}

std::string LineEditProxy::get() const
{
    return m_line_edit->text().toStdString();
}


//
// SpinBoxProxy class implementation.
//

SpinBoxProxy::SpinBoxProxy(QSpinBox* spinbox)
  : m_spinbox(spinbox)
{
    connect(m_spinbox, SIGNAL(valueChanged(int)), SIGNAL(signal_changed()));
}

void SpinBoxProxy::set(const std::string& value)
{
    m_spinbox->setValue(from_string<int>(value));
}

std::string SpinBoxProxy::get() const
{
    return to_string(m_spinbox->value());
}


//
// DoubleSpinBoxProxy class implementation.
//

DoubleSpinBoxProxy::DoubleSpinBoxProxy(QDoubleSpinBox* spinbox)
  : m_spinbox(spinbox)
{
    connect(m_spinbox, SIGNAL(valueChanged(double)), SIGNAL(signal_changed()));
}

void DoubleSpinBoxProxy::set(const std::string& value)
{
    m_spinbox->setValue(from_string<double>(value));
}

std::string DoubleSpinBoxProxy::get() const
{
    return to_string(m_spinbox->value());
}


//
// CheckBoxProxy class implementation.
//

CheckBoxProxy::CheckBoxProxy(QCheckBox* checkbox)
  : m_checkbox(checkbox)
{
    connect(m_checkbox, SIGNAL(stateChanged(int)), SIGNAL(signal_changed()));
}

void CheckBoxProxy::set(const std::string& value)
{
    m_checkbox->setChecked(from_string<bool>(value));
}

std::string CheckBoxProxy::get() const
{
    return to_string(m_checkbox->isChecked());
}


//
// GroupBoxProxy class implementation.
//

GroupBoxProxy::GroupBoxProxy(QGroupBox* groupbox)
  : m_groupbox(groupbox)
{
    connect(m_groupbox, SIGNAL(clicked(bool)), SIGNAL(signal_changed()));
}

void GroupBoxProxy::set(const std::string& value)
{
    m_groupbox->setChecked(from_string<bool>(value));
}

std::string GroupBoxProxy::get() const
{
    return to_string(m_groupbox->isChecked());
}


//
// RadioButtonProxy class implementation.
//

RadioButtonProxy::RadioButtonProxy(QRadioButton* radio_button)
  : m_radio_button(radio_button)
{
    connect(m_radio_button, SIGNAL(toggled(bool)), SIGNAL(signal_changed()));
}

void RadioButtonProxy::set(const std::string& value)
{
    m_radio_button->setChecked(from_string<bool>(value));
}

std::string RadioButtonProxy::get() const
{
    return to_string(m_radio_button->isChecked());
}


//
// ComboBoxProxy class implementation.
//

ComboBoxProxy::ComboBoxProxy(QComboBox* combobox)
  : m_combobox(combobox)
{
    connect(m_combobox, SIGNAL(currentIndexChanged(int)), SIGNAL(signal_changed()));
}

void ComboBoxProxy::set(const std::string& value)
{
    m_combobox->setCurrentIndex(m_combobox->findData(QString::fromStdString(value)));
}

std::string ComboBoxProxy::get() const
{
    const QVariant data = m_combobox->itemData(m_combobox->currentIndex());
    return data.value<QString>().toStdString();
}


//
// ColorPickerProxy class implementation.
//

ColorPickerProxy::ColorPickerProxy(QLineEdit* line_edit, QToolButton* picker_button)
  : m_line_edit(line_edit)
  , m_picker_button(picker_button)
{
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));
    connect(m_line_edit, SIGNAL(textChanged(const QString&)), SLOT(slot_set(const QString&)));
}

namespace
{
    void set_tool_button_color(QToolButton* button, const QColor color)
    {
        button->setStyleSheet(
            QString("background-color: rgb(%1, %2, %3)")
                .arg(color.red())
                .arg(color.green())
                .arg(color.blue()));
    }
}

void ColorPickerProxy::set(const std::string& value)
{
    m_line_edit->setText(QString::fromStdString(value));

    set_tool_button_color(
        m_picker_button,
        color_to_qcolor(
            get_color_from_string(value)));
}

void ColorPickerProxy::set(const std::string& value, const std::string& wavelength_range)
{
    m_line_edit->setText(QString::fromStdString(value));

    set_tool_button_color(
        m_picker_button,
        color_to_qcolor(
            get_color_from_string(value, wavelength_range)));
}

std::string ColorPickerProxy::get() const
{
    return m_line_edit->text().toStdString();
}

namespace
{
    Color3f do_get_color_from_string(
        const std::string&   s,
        const float          low_wavelength,
        const float          high_wavelength)
    {
        try
        {
            std::vector<float> values;
            tokenize(s, Blanks, values);

            if (values.empty())
                return Color3f(0.0f);
            else if (values.size() == 1)
                return Color3f(values[0]);
            else if (values.size() == 3)
                return Color3f(values[0], values[1], values[2]);
            else if (low_wavelength < high_wavelength)
            {
                RegularSpectrum31f output_spectrum;
                spectral_values_to_spectrum(
                    low_wavelength,
                    high_wavelength,
                    values.size(),
                    &values[0],
                    output_spectrum);

                Color3f ciexyz;
                spectrum_to_ciexyz_standard(&output_spectrum[0], &ciexyz[0]);

                return linear_rgb_to_srgb(ciexyz_to_linear_rgb(ciexyz));
            }
            else return Color3f(0.0f);
        }
        catch (const ExceptionStringConversionError&)
        {
            return Color3f(0.0);
        }
    }
}

Color3d ColorPickerProxy::get_color_from_string(const std::string& s)
{
    return Color3d(do_get_color_from_string(s, LowWavelength, HighWavelength));
}

Color3d ColorPickerProxy::get_color_from_string(const std::string& s, const std::string& wavelength_range)
{
    try
    {
        std::vector<float> range;
        tokenize(wavelength_range, Blanks, range);

        return Color3d(do_get_color_from_string(s, range[0], range[1]));
    }
    catch (const ExceptionStringConversionError&)
    {
        return Color3d(0.0);
    }
}


//
// ColorExpressionProxy class implementation.
//

ColorExpressionProxy::ColorExpressionProxy(QLineEdit* line_edit, QToolButton* picker_button)
  : m_line_edit(line_edit)
  , m_picker_button(picker_button)
{
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));
}

void ColorExpressionProxy::set(const std::string& value)
{
    m_line_edit->setText(QString::fromStdString(value));

    set_tool_button_color(
        m_picker_button,
        expression_to_qcolor(value));
}

std::string ColorExpressionProxy::get() const
{
    return m_line_edit->text().toStdString();
}

std::string ColorExpressionProxy::qcolor_to_expression(const QColor& color)
{
    const Color3f srgb_color = qcolor_to_color<Color3f>(color);
    const QString color_expression =
        QString("[%1, %2, %3]")
            .arg(srgb_color.r)
            .arg(srgb_color.g)
            .arg(srgb_color.b);
    return color_expression.toStdString();
}

QColor ColorExpressionProxy::expression_to_qcolor(const std::string& color)
{
    std::vector<std::string> color_components;
    tokenize(color, ",[] ", color_components);

    if (color_components.size() < 3)
        return QColor(0, 0, 0, 0);

    try
    {
        const Color3f srgb_color(
            from_string<float>(color_components[0]),
            from_string<float>(color_components[1]),
            from_string<float>(color_components[2]));
        return color_to_qcolor(srgb_color);
    }
    catch (const ExceptionStringConversionError&)
    {
        return QColor(0, 0, 0, 0);
    }
}


//
// InputWidgetProxyCollection class implementation.
//

InputWidgetProxyCollection::~InputWidgetProxyCollection()
{
    clear();
}

void InputWidgetProxyCollection::clear()
{
    for (const_each<ProxyCollection> i = m_proxies; i; ++i)
        delete i->second;

    m_proxies.clear();
}

void InputWidgetProxyCollection::insert(
    const std::string&                   key,
    std::unique_ptr<IInputWidgetProxy>   proxy)
{
    m_proxies[key] = proxy.release();
}

IInputWidgetProxy* InputWidgetProxyCollection::get(const std::string& key) const
{
    const ProxyCollection::const_iterator i = m_proxies.find(key);
    return i != m_proxies.end() ? i->second : nullptr;
}

Dictionary InputWidgetProxyCollection::get_values() const
{
    Dictionary values;

    for (const_each<ProxyCollection> i = m_proxies; i; ++i)
    {
        const std::string value = i->second->get();

        if (!value.empty())
            values.insert(i->first.c_str(), value);
    }

    return values;
}

}   // namespace studio
}   // namespace appleseed
