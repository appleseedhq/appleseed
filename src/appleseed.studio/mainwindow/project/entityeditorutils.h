
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORUTILS_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_ENTITYEDITORUTILS_H

// Qt headers.
#include <QLineEdit>
#include <QObject>
#include <QString>
#include <QWidget>

// Forward declarations.
namespace appleseed { namespace studio { class DoubleSlider; } }

namespace appleseed {
namespace studio {

//
// Updates the value of LineEdit associated with the given DoubleSlider.
//
class LineEditDoubleSliderAdaptor
  : public QObject
{
    Q_OBJECT

  public:
    LineEditDoubleSliderAdaptor(QLineEdit* line_edit, DoubleSlider* slider);

  public slots:
    void slot_set_line_edit_value(const double value);
    void slot_set_slider_value(const QString& value);
    void slot_apply_slider_value();

  private:
    QLineEdit*      m_line_edit;
    DoubleSlider*   m_slider;

    void adjust_slider(const double new_value);
};

//
// Adds extra information to the signal emitted when the color changes.
//
class ForwardColorChangedSignal
  : public QObject
{
    Q_OBJECT

  public:
    ForwardColorChangedSignal(QObject* parent, const QString& widget_name);

  public slots:
    void slot_color_changed(const QColor& color);

  signals:
    void signal_color_changed(const QString& widget_name, const QColor& color);

  private:
    const QString m_widget_name;
};

//
// Simplifies information passed by the signal emitted on QLineEdit change.
//
class LineEditForwarder
  : public QLineEdit
{
    Q_OBJECT

  public:
    LineEditForwarder(const QString& contents, QWidget* parent = 0);

  public slots:
    void slot_text_changed(const QString& text);

  signals:
    void signal_text_changed();
};

}       // namespace studio
}       // namespace appleseed

#endif
