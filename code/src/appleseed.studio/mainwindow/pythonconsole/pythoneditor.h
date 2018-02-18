
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONEDITOR_H
#define APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONEDITOR_H

// appleseed.studio headers.
#include "mainwindow/pythonconsole/zoomableplaintextedit.h"

// Qt headers.
#include <QObject>
#include <QPlainTextEdit>

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
class QWidget;
namespace appleseed { namespace studio { class LineNumberArea; } }

namespace appleseed {
namespace studio {

class PythonEditor
  : public ZoomablePlainTextEdit
{
    Q_OBJECT

  public:
    explicit PythonEditor(QWidget* parent = nullptr);

  protected:
    // Event used to update line number area.
    void resizeEvent(QResizeEvent* event) override;

    void keyPressEvent(QKeyEvent* event) override;

  private slots:
    void slot_highlight_current_line();

  private:
    friend class LineNumberArea;

    LineNumberArea* m_line_number_area;

    void indent();
    void indent_like_previous(const std::string& previous);
    void insert_spaces(const size_t count);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONEDITOR_H
