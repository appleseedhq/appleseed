
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

#ifndef APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H
#define APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"

// Qt headers.
#include <QObject>

// Standard headers.
#include <map>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class Dictionary; }
class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QGroupBox;
class QLineEdit;
class QRadioButton;
class QSpinBox;
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
    virtual ~IInputWidgetProxy() {}

    virtual void set(const std::string& value) = 0;

    // If not implementation exists, we fall back to noop kind of functionality.
    virtual void set(const std::string& value, const std::string& value2) {}

    virtual std::string get() const = 0;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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

    virtual void set(const std::string& value);
    virtual std::string get() const;

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
    explicit ColorPickerProxy(QLineEdit* line_edit, QToolButton* picker_button);

    virtual void set(const std::string& value);
    virtual void set(const std::string& value, const std::string& wavelength_range);
    virtual std::string get() const;

    static foundation::Color3d get_color_from_string(const std::string& s);
    static foundation::Color3d get_color_from_string(const std::string& s, const std::string& wavelength_range);

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
        std::auto_ptr<IInputWidgetProxy>    proxy);

    IInputWidgetProxy* get(const std::string& key) const;

    foundation::Dictionary get_values() const;

  private:
    typedef std::map<std::string, IInputWidgetProxy*> ProxyCollection;

    ProxyCollection m_proxies;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H
