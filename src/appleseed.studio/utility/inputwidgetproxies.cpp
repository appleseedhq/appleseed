
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

// appleseed.studio headers.
#include "utility/interop.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/input/colorsource.h"

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

using namespace foundation;
using namespace renderer;
using namespace std;

namespace appleseed {
namespace studio {

//
// LineEditProxy class implementation.
//

LineEditProxy::LineEditProxy(QLineEdit* line_edit)
  : m_line_edit(line_edit)
{
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));
}

void LineEditProxy::set(const string& value)
{
    m_line_edit->setText(QString::fromStdString(value));

    emit signal_changed();
}

string LineEditProxy::get() const
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

void SpinBoxProxy::set(const string& value)
{
    m_spinbox->setValue(from_string<int>(value));
}

string SpinBoxProxy::get() const
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

void DoubleSpinBoxProxy::set(const string& value)
{
    m_spinbox->setValue(from_string<double>(value));
}

string DoubleSpinBoxProxy::get() const
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

void CheckBoxProxy::set(const string& value)
{
    m_checkbox->setChecked(from_string<bool>(value));
}

string CheckBoxProxy::get() const
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

void GroupBoxProxy::set(const string& value)
{
    m_groupbox->setChecked(from_string<bool>(value));
}

string GroupBoxProxy::get() const
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

void RadioButtonProxy::set(const string& value)
{
    m_radio_button->setChecked(from_string<bool>(value));
}

string RadioButtonProxy::get() const
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

void ComboBoxProxy::set(const string& value)
{
    m_combobox->setCurrentIndex(m_combobox->findData(QString::fromStdString(value)));
}

string ComboBoxProxy::get() const
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
}

void ColorPickerProxy::set(const string& value)
{
    m_line_edit->setText(QString::fromStdString(value));

    const QColor color = color_to_qcolor(get_color_from_string(value));

    m_picker_button->setStyleSheet(
        QString("background-color: rgb(%1, %2, %3)")
            .arg(color.red())
            .arg(color.green())
            .arg(color.blue()));

    emit signal_changed();
}

void ColorPickerProxy::set(const string& value, const string& wavelength_range)
{
    m_line_edit->setText(QString::fromStdString(value));

    const QColor color = color_to_qcolor(get_color_from_string(value, wavelength_range));

    m_picker_button->setStyleSheet(
        QString("background-color: rgb(%1, %2, %3)")
            .arg(color.red())
            .arg(color.green())
            .arg(color.blue()));

    emit signal_changed();
}

string ColorPickerProxy::get() const
{
    return m_line_edit->text().toStdString();
}

// This method uses the default wavelength range of [400-700] nm for spectrum to color conversion.
Color3d ColorPickerProxy::get_color_from_string(const string& s)
{
    try
    {
        vector<double> values;
        tokenize(s, Blanks, values);

        if (values.size() == 1)
            return Color3d(values[0]);
        else if (values.size() == 3)
            return Color3d(values[0], values[1], values[2]);
        else
        {
            // Convert Spectral values to visible color to be shown on widget
			ColorValueArray input_spectrum, output_spectrum, ciexyz;
			input_spectrum.resize(values.size());
			output_spectrum.resize(31);										// The standard spectral data is composed of 31 elements
			ciexyz.resize(3);														
            
			for (size_t i = 0; i < values.size(); i++) 
            {
                // Convert all double to floating values
				input_spectrum[i] = static_cast<float>(values[i]);
            }

            spectral_values_to_spectrum(
				LowWavelength,
				HighWavelength,
				input_spectrum,
				output_spectrum);

            spectrum_to_ciexyz_standard(output_spectrum, ciexyz);

            return transform_color(
                Color3f(ciexyz[0], ciexyz[1], ciexyz[2]),
                ColorSpaceCIEXYZ, 
                ColorSpaceSRGB);
        }
    }
    catch (const ExceptionStringConversionError&)
    {
        return Color3d(0.0);
    }
}

// This method uses custom wavelength range provided as input to convert spectrum to color conversion.
Color3d ColorPickerProxy::get_color_from_string(const string& s, const string& wavelength_range) 
{
    try
    {
        vector<double> values;
        vector<double> wavelengths;
        tokenize(s, Blanks, values);
        tokenize(wavelength_range, Blanks, wavelengths);

        if (values.size() == 1)
            return Color3d(values[0]);
        else if (values.size() == 3)
            return Color3d(values[0], values[1], values[2]);
        else
        {
            // Convert Spectral values to visible color to be shown on widget
			ColorValueArray input_spectrum, output_spectrum, ciexyz;
			input_spectrum.resize(values.size());
			ciexyz.resize(3);														
            
			for (size_t i = 0; i < values.size(); i++) 
            {
                // Convert all double to floating values
				input_spectrum[i] = static_cast<float>(values[i]);
            }

            spectral_values_to_spectrum(
				wavelengths[0],
                wavelengths[1],
				input_spectrum,
				output_spectrum);

            spectrum_to_ciexyz_standard(output_spectrum, ciexyz);

            return transform_color(
                Color3f(ciexyz[0], ciexyz[1], ciexyz[2]),
                ColorSpaceCIEXYZ, 
                ColorSpaceSRGB);
        }
    }
    catch (const ExceptionStringConversionError&)
    {
        return Color3d(0.0);
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
    const string&               key,
    auto_ptr<IInputWidgetProxy> proxy)
{
    m_proxies[key] = proxy.release();
}

IInputWidgetProxy* InputWidgetProxyCollection::get(const string& key) const
{
    const ProxyCollection::const_iterator i = m_proxies.find(key);
    return i != m_proxies.end() ? i->second : 0;
}

Dictionary InputWidgetProxyCollection::get_values() const
{
    Dictionary values;

    for (const_each<ProxyCollection> i = m_proxies; i; ++i)
    {
        const string name = i->first;
        const string value = i->second->get();

        if (!value.empty())
            values.insert(name, value);
    }

    return values;
}

}   // namespace studio
}   // namespace appleseed
