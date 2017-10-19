
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

#ifndef APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H
#define APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/platform/compiler.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <map>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class Dictionary; }
class QCheckBox;
class QColor;
class QComboBox;
class QDoubleSpinBox;
class QGroupBox;
class QLineEdit;
class QRadioButton;
class QSpinBox;
class QString;
class QToolButton;

namespace appleseed {
namespace studio {

//
// A widget proxy provides a uniform string-based read/write access
// to Qt input widgets as well as uniform change notifications.
//

class IInputWidgetProxy
  : public QObject
{
    Q_OBJECT

  public:
    ~IInputWidgetProxy() override {}

    virtual void set(const std::string& value) = 0;
    virtual std::string get() const = 0;

    void emit_signal_changed();

  public slots:
    void slot_set(const QString& value);

  signals:
    void signal_changed();
};


//
// QLineEdit proxy.
//

class LineEditProxy
  : public IInputWidgetProxy
{
  public:
    explicit LineEditProxy(QLineEdit* line_edit);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QLineEdit* m_line_edit;
};


//
// QSpinBox proxy.
//

class SpinBoxProxy
  : public IInputWidgetProxy
{
  public:
    explicit SpinBoxProxy(QSpinBox* spinbox);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QSpinBox* m_spinbox;
};


//
// QDoubleSpinBox proxy.
//

class DoubleSpinBoxProxy
  : public IInputWidgetProxy
{
  public:
    explicit DoubleSpinBoxProxy(QDoubleSpinBox* spinbox);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QDoubleSpinBox* m_spinbox;
};


//
// QCheckBox proxy.
//

class CheckBoxProxy
  : public IInputWidgetProxy
{
  public:
    explicit CheckBoxProxy(QCheckBox* checkbox);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QCheckBox* m_checkbox;
};


//
// QGroupBox proxy.
//

class GroupBoxProxy
  : public IInputWidgetProxy
{
  public:
    explicit GroupBoxProxy(QGroupBox* groupbox);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QGroupBox* m_groupbox;
};


//
// QRadioButton proxy.
//

class RadioButtonProxy
  : public IInputWidgetProxy
{
  public:
    explicit RadioButtonProxy(QRadioButton* radio_button);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QRadioButton* m_radio_button;
};


//
// QComboBox proxy.
//

class ComboBoxProxy
  : public IInputWidgetProxy
{
  public:
    explicit ComboBoxProxy(QComboBox* combobox);

    void set(const std::string& value) override;
    std::string get() const override;

  private:
    QComboBox* m_combobox;
};


//
// Color picker proxy.
//

class ColorPickerProxy
  : public IInputWidgetProxy
{
  public:
    ColorPickerProxy(QLineEdit* line_edit, QToolButton* picker_button);

    void set(const std::string& value) override;
    void set(const std::string& value, const std::string& wavelength_range);

    std::string get() const override;

    static foundation::Color3d get_color_from_string(const std::string& s);
    static foundation::Color3d get_color_from_string(const std::string& s, const std::string& wavelength_range);

  private:
    QLineEdit*      m_line_edit;
    QToolButton*    m_picker_button;
};


//
// Color expression proxy.
//

class ColorExpressionProxy
  : public IInputWidgetProxy
{
  public:
    ColorExpressionProxy(QLineEdit* line_edit, QToolButton* picker_button);

    void set(const std::string& value) override;
    std::string get() const override;

    static std::string qcolor_to_expression(const QColor& color);
    static QColor expression_to_qcolor(const std::string& color);

  private:
    QLineEdit*      m_line_edit;
    QToolButton*    m_picker_button;
};


//
// A collection of named proxies.
//

class InputWidgetProxyCollection
  : public foundation::NonCopyable
{
  public:
    ~InputWidgetProxyCollection();

    void clear();

    void insert(
        const std::string&                  key,
        std::unique_ptr<IInputWidgetProxy>  proxy);

    IInputWidgetProxy* get(const std::string& key) const;

    foundation::Dictionary get_values() const;

  private:
    typedef std::map<std::string, IInputWidgetProxy*> ProxyCollection;

    ProxyCollection m_proxies;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H
