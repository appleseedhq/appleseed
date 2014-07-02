
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "entityeditorutils.h"

// appleseed.studio headers.
#include "utility/doubleslider.h"
#include "utility/interop.h"

using namespace foundation;

namespace appleseed {
namespace studio {

//
// LineEditDoubleSliderAdaptor class implementation
//
LineEditDoubleSliderAdaptor::LineEditDoubleSliderAdaptor(
    QLineEdit* line_edit,
    DoubleSlider* slider)
  : QObject(line_edit)
  , m_line_edit(line_edit)
  , m_slider(slider)
{
    slot_set_slider_value(m_line_edit->text());
}

void LineEditDoubleSliderAdaptor::slot_set_line_edit_value(const double value)
{
    // Don't block signals here, for live edit to work we want the line edit to signal changes.
    m_line_edit->setText(QString("%1").arg(value));
}

void LineEditDoubleSliderAdaptor::slot_set_slider_value(const QString& value)
{
    m_slider->blockSignals(true);

    const double new_value = value.toDouble();

    // Adjust range max if the new value is greater than current range max.
    if (new_value > m_slider->maximum())
        adjust_slider(new_value);

    m_slider->setValue(new_value);
    m_slider->blockSignals(false);
}

void LineEditDoubleSliderAdaptor::slot_apply_slider_value()
{
    m_slider->blockSignals(true);

    const double new_value = m_line_edit->text().toDouble();

    // Adjust range max if the new value is greater than current range max
    // or less than a certain percentage of current range max.
    if (new_value > m_slider->maximum() ||
        new_value < lerp(m_slider->minimum(), m_slider->maximum(), 1.0 / 3))
        adjust_slider(new_value);

    m_slider->setValue(new_value);
    m_slider->blockSignals(false);
}

void LineEditDoubleSliderAdaptor::adjust_slider(const double new_value)
{
    const double new_max = 2.0 * new_value;
    m_slider->setRange(0.0, new_max);
    m_slider->setPageStep(new_max / 10.0);
}

//
// ForwardColorChangedSignal class implementation.
//
ForwardColorChangedSignal::ForwardColorChangedSignal(
    QObject* parent,
    const QString& widget_name)
  : QObject(parent)
  , m_widget_name(widget_name)
{
}

void ForwardColorChangedSignal::slot_color_changed(const QColor& color)
{
    emit signal_color_changed(m_widget_name, color);
}

//
// LineEditForwarder class implementation.
//
LineEditForwarder::LineEditForwarder(
    const QString& contents,
    QWidget* parent)
  : QLineEdit(contents, parent)
{
    connect(this, SIGNAL(textChanged(const QString&)),
            this, SLOT(slot_text_changed(const QString&)));
}

void LineEditForwarder::slot_text_changed(const QString& text)
{
    emit signal_text_changed();
}

}       // namespace studio
}       // namespace appleseed
