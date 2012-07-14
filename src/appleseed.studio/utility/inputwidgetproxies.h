
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// Standard headers.
#include <string>

// Forward declarations.
class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QGroupBox;
class QLineEdit;
class QSpinBox;
class QToolButton;

namespace appleseed {
namespace studio {

//
// A widget proxy provides a uniform string-based read/write access to Qt input widgets.
//

class IInputWidgetProxy
  : public foundation::NonCopyable
{
  public:
    virtual ~IInputWidgetProxy() {}

    virtual void set(const std::string& value) = 0;
    virtual std::string get() const = 0;
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
    virtual std::string get() const;

    static foundation::Color3d get_color_from_string(const std::string& s);

  private:
    QLineEdit*      m_line_edit;
    QToolButton*    m_picker_button;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_UTILITY_INPUTWIDGETPROXIES_H
