
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_CONSOLEWIDGET_H
#define APPLESEED_STUDIO_MAINWINDOW_CONSOLEWIDGET_H

// appleseed.studio headers.
#include "python/pythoninterpreter.h"

// Qt headers.
#include <QObject>
#include <QSplitter>

// Forward declarations.
class QAction;
class QString;
class QWidget;
class QTextEdit;

namespace appleseed {
namespace studio {

class ConsoleWidget
  : public QSplitter
{
  Q_OBJECT

  public:
    explicit ConsoleWidget(QWidget* parent = 0);

  public slots:
    void slot_execute_selection();
    void slot_execute_all();
    void slot_clear_output();

  private:
    QTextEdit* output;
    QTextEdit* input;

    QAction* m_action_execute_selection;
    QAction* m_action_execute_all;
    QAction* m_action_clear_selection;
    QAction* m_action_focus_on_input;

    void execute(const QString& script);

    void init_actions();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_CONSOLEWIDGET_H
