
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

#pragma once

// appleseed.studio headers.
#include "utility/inputwidgetproxies.h"

// Qt headers.
#include <QObject>
#include <QStackedWidget>

// Forward declarations.
namespace appleseed { namespace qtcommon { class DoubleSlider; }}
namespace appleseed { namespace studio { class LineEditDoubleSliderAdaptor; }}
class QLineEdit;
class QPushButton;
class QString;
class QValidator;
class QWidget;

namespace appleseed {
namespace studio {

//
// A Qt widget for inputs of type "entity".
//

class EntityInputWidget
  : public QStackedWidget
{
    Q_OBJECT

  public:
    explicit EntityInputWidget(QWidget* parent = nullptr);

    void set_focus();

    QString get_value() const;

  public slots:
    void set_value(const QString& value);

  signals:
    void signal_bind_button_clicked();
    void signal_changed();

  private slots:
    void slot_set_value();
    void slot_unbind();

  private:
    QLineEdit*      m_line_edit;
    QPushButton*    m_entity_button;
};

class EntityInputProxy
  : public IInputWidgetProxy
{
  public:
    explicit EntityInputProxy(EntityInputWidget* input_widget);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    EntityInputWidget* m_input_widget;
};


//
// A Qt widget for inputs of type scalar or "entity".
//

class ColorMapInputWidget
  : public QStackedWidget
{
    Q_OBJECT

  public:
    explicit ColorMapInputWidget(QWidget* parent = nullptr);

    void set_validator(QValidator* validator);
    void set_default_value(const QString& default_value);

    void set_focus();

    QString get_value() const;

  public slots:
    void set_value(const QString& value);

  signals:
    void signal_bind_button_clicked();
    void signal_changed();

  private slots:
    void slot_set_value();
    void slot_unbind();

  private:
    QString                         m_default_value;
    QLineEdit*                      m_line_edit;
    qtcommon::DoubleSlider*         m_slider;
    QPushButton*                    m_entity_button;
    LineEditDoubleSliderAdaptor*    m_adaptor;
};

class ColorMapInputProxy
  : public IInputWidgetProxy
{
  public:
    explicit ColorMapInputProxy(ColorMapInputWidget* input_widget);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    ColorMapInputWidget* m_input_widget;
};

}   // namespace studio
}   // namespace appleseed
