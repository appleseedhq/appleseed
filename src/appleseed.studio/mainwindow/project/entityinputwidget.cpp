
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "entityinputwidget.h"

// appleseed.studio headers.
#include "mainwindow/project/tools.h"

// appleseed.qtcommon headers.
#include "utility/interop.h"
#include "widgets/doubleslider.h"
#include "widgets/mousewheelfocuseventfilter.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Qt headers.
#include <QHBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QStackedWidget>
#include <QString>
#include <QValidator>

// Standard headers.
#include <string>

using namespace appleseed::qtcommon;
using namespace foundation;

namespace appleseed {
namespace studio {

//
// EntityInputWidget class implementation.
//

EntityInputWidget::EntityInputWidget(QWidget* parent)
  : QStackedWidget(parent)
{
    m_line_edit = new QLineEdit(this);
    connect(m_line_edit, SIGNAL(returnPressed()), SLOT(slot_set_value()));
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));

    QWidget* bind_button = new QPushButton("Bind", this);
    bind_button->setObjectName("bind_entity_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(bind_button, SIGNAL(clicked()), SIGNAL(signal_bind_button_clicked()));

    QWidget* line_edit_button_group = new QWidget(this);
    QHBoxLayout* line_edit_button_group_layout = new QHBoxLayout();
    line_edit_button_group_layout->setMargin(0);
    line_edit_button_group_layout->setSpacing(6);
    line_edit_button_group_layout->addWidget(m_line_edit);
    line_edit_button_group_layout->addWidget(bind_button);
    line_edit_button_group->setLayout(line_edit_button_group_layout);

    m_entity_button = new QPushButton(this);

    QPushButton* unbind_button = new QPushButton("Unbind", this);
    unbind_button->setObjectName("unbind_entity_button");
    unbind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(unbind_button, SIGNAL(clicked()), SLOT(slot_unbind()));
    connect(unbind_button, SIGNAL(clicked()), SIGNAL(signal_changed()));

    QWidget* entity_button_group = new QWidget(this);
    QHBoxLayout* entity_button_group_layout = new QHBoxLayout();
    entity_button_group_layout->setMargin(0);
    entity_button_group_layout->setSpacing(6);
    entity_button_group_layout->addWidget(m_entity_button);
    entity_button_group_layout->addWidget(unbind_button);
    entity_button_group->setLayout(entity_button_group_layout);

    addWidget(line_edit_button_group);
    addWidget(entity_button_group);
}

void EntityInputWidget::set_focus()
{
    switch (currentIndex())
    {
      case 0:
        m_line_edit->selectAll();
        m_line_edit->setFocus();
        break;

      case 1:
        m_entity_button->setFocus();
        break;
    }
}

void EntityInputWidget::set_value(const QString& value)
{
    m_line_edit->setText(value);
    m_entity_button->setText(value);
    setCurrentIndex(value.isEmpty() ? 0 : 1);
}

QString EntityInputWidget::get_value() const
{
    return m_line_edit->text();
}

void EntityInputWidget::slot_set_value()
{
    set_value(m_line_edit->text());
}

void EntityInputWidget::slot_unbind()
{
    set_value(QString());
}


//
// EntityInputProxy class implementation.
//

EntityInputProxy::EntityInputProxy(EntityInputWidget* input_widget)
  : m_input_widget(input_widget)
{
    connect(input_widget, SIGNAL(signal_changed()), SIGNAL(signal_changed()));
}

void EntityInputProxy::set(const std::string& value)
{
    m_input_widget->set_value(QString::fromStdString(value));
}

std::string EntityInputProxy::get() const
{
    return m_input_widget->get_value().toStdString();
}


//
// ColorMapInputWidget class implementation.
//

ColorMapInputWidget::ColorMapInputWidget(QWidget* parent)
  : QStackedWidget(parent)
{
    m_line_edit = new QLineEdit(this);
    m_line_edit->setMaximumWidth(120);
    connect(m_line_edit, SIGNAL(returnPressed()), SIGNAL(signal_changed()));

    m_slider = new DoubleSlider(Qt::Horizontal, this);
    m_slider->setRange(0.0, 1.0);
    m_slider->setPageStep(0.1);
    new MouseWheelFocusEventFilter(m_slider);
    m_adaptor = new LineEditDoubleSliderAdaptor(m_line_edit, m_slider);
    connect(m_slider, SIGNAL(valueChanged(int)), SIGNAL(signal_changed()));

    QWidget* bind_button = new QPushButton("Bind", this);
    bind_button->setObjectName("bind_entity_button");
    bind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(bind_button, SIGNAL(clicked()), SIGNAL(signal_bind_button_clicked()));

    QWidget* line_edit_button_group = new QWidget(this);
    QHBoxLayout* line_edit_button_group_layout = new QHBoxLayout();
    line_edit_button_group_layout->setMargin(0);
    line_edit_button_group_layout->setSpacing(6);
    line_edit_button_group_layout->addWidget(m_line_edit);
    line_edit_button_group_layout->addWidget(m_slider);
    line_edit_button_group_layout->addWidget(bind_button);
    line_edit_button_group->setLayout(line_edit_button_group_layout);

    m_entity_button = new QPushButton(this);

    QPushButton* unbind_button = new QPushButton("Unbind", this);
    unbind_button->setObjectName("unbind_entity_button");
    unbind_button->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    connect(unbind_button, SIGNAL(clicked()), SLOT(slot_unbind()));
    connect(unbind_button, SIGNAL(clicked()), SIGNAL(signal_changed()));

    QWidget* entity_button_group = new QWidget(this);
    QHBoxLayout* entity_button_group_layout = new QHBoxLayout();
    entity_button_group_layout->setMargin(0);
    entity_button_group_layout->setSpacing(6);
    entity_button_group_layout->addWidget(m_entity_button);
    entity_button_group_layout->addWidget(unbind_button);
    entity_button_group->setLayout(entity_button_group_layout);

    addWidget(line_edit_button_group);
    addWidget(entity_button_group);
}

void ColorMapInputWidget::set_validator(QValidator* validator)
{
    validator->setParent(m_line_edit);
    m_line_edit->setValidator(validator);
}

void ColorMapInputWidget::set_default_value(const QString& default_value)
{
    m_default_value = default_value;
}

void ColorMapInputWidget::set_focus()
{
    switch (currentIndex())
    {
      case 0:
        m_line_edit->selectAll();
        m_line_edit->setFocus();
        break;

      case 1:
        m_entity_button->setFocus();
        break;
    }
}

void ColorMapInputWidget::set_value(const QString& value)
{
    try
    {
        const double x = from_string<double>(value);
        m_adaptor->slot_set_line_edit_value(x);
        setCurrentIndex(0);
    }
    catch (const ExceptionStringConversionError&)
    {
        m_line_edit->setText(value);
        m_entity_button->setText(value);
        setCurrentIndex(value.isEmpty() ? 0 : 1);   // don't show the button if the line edit is empty
    }
}

QString ColorMapInputWidget::get_value() const
{
    return m_line_edit->text();
}

void ColorMapInputWidget::slot_set_value()
{
    set_value(m_line_edit->text());
}

void ColorMapInputWidget::slot_unbind()
{
    set_value(m_default_value);
}


//
// ColorMapInputProxy class implementation.
//

ColorMapInputProxy::ColorMapInputProxy(ColorMapInputWidget* input_widget)
  : m_input_widget(input_widget)
{
    connect(input_widget, SIGNAL(signal_changed()), SIGNAL(signal_changed()));
}

void ColorMapInputProxy::set(const std::string& value)
{
    m_input_widget->set_value(QString::fromStdString(value));
}

std::string ColorMapInputProxy::get() const
{
    return m_input_widget->get_value().toStdString();
}

}   // namespace studio
}   // namespace appleseed
