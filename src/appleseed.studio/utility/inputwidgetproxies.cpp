
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"

// Qt headers.
#include <QCheckBox>
#include <QColor>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QGroupBox>
#include <QLineEdit>
#include <QSpinBox>
#include <QString>
#include <QToolButton>
#include <QVariant>

// Standard headers.
#include <vector>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

//
// LineEditProxy class implementation.
//

LineEditProxy::LineEditProxy(QLineEdit* line_edit)
  : m_line_edit(line_edit)
{
}

void LineEditProxy::set(const string& value)
{
    m_line_edit->setText(QString::fromStdString(value));
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
// ComboBoxProxy class implementation.
//

ComboBoxProxy::ComboBoxProxy(QComboBox* combobox)
  : m_combobox(combobox)
{
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
}

string ColorPickerProxy::get() const
{
    return m_line_edit->text().toStdString();
}

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
        else return Color3d(0.0);
    }
    catch (const ExceptionStringConversionError&)
    {
        return Color3d(0.0);
    }
}

}   // namespace studio
}   // namespace appleseed
