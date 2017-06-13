
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONINPUT_H
#define APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONINPUT_H

// appleseed.studio headers
#include "python/pythoninterpreter.h"
#include "linenumberarea.h"

// Qt headers.
#include <QPlainTextEdit>

// Forward declarations.
class QWidget;

namespace appleseed {
namespace studio {

class PythonEditor
  : public QPlainTextEdit
{
    Q_OBJECT

  public:
    explicit PythonEditor(QWidget* parent = 0);

    // These function exposes protected functions of QPlainTextEdit to use in LineNumberArea class
    void set_left_margin(int left_margin);
    QTextBlock get_first_visible_block();
    int get_top_of_first_block(QTextBlock block);
    int get_block_height(QTextBlock block);

  protected:
    // Event used to update line number area.
    void resizeEvent(QResizeEvent* event);
    // Event used to autoindent new lines and replace tabs with spaces.
    void keyPressEvent(QKeyEvent* event);

  private slots:
    void slot_highlight_current_line();

  private:
    void indent();
    void indent_like_previous(const std::string& previous);
    void insert_spaces(const size_t count);

    LineNumberArea* line_number_area;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONINPUT_H
