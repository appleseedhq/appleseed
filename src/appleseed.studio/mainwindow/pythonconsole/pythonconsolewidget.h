
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONCONSOLEWIDGET_H
#define APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONCONSOLEWIDGET_H

// appleseed.studio headers.
#include "python/pythoninterpreter.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <string>

// Forward declarations.
class QAction;
class QPlainTextEdit;
class QString;
class QWheelEvent;

namespace appleseed {
namespace studio {

class PythonConsoleWidget
  : public QWidget
{
  Q_OBJECT

  public:
    explicit PythonConsoleWidget(QWidget* parent = nullptr);

  protected:
    void wheelEvent(QWheelEvent* event) override;

  public slots:
    void slot_execute_selection();
    void slot_execute_all();
    void slot_clear_output();

  private slots:
    void slot_new_file();
    void slot_open_file();
    void slot_save_file();
    void slot_save_file_as();

    void slot_file_changed();

    void slot_change_exec_selection_button_state();

  private:
    QPlainTextEdit* m_output;
    QPlainTextEdit* m_input;

    QAction*        m_action_new_file;
    QAction*        m_action_open_file;
    QAction*        m_action_save_file;
    QAction*        m_action_save_file_as;
    QAction*        m_action_execute_all;
    QAction*        m_action_execute_selection;
    QAction*        m_action_clear_selection;
    QAction*        m_action_focus_on_input;

    bool            m_is_file_dirty;
    std::string     m_opened_filepath;

    bool can_close_file();
    bool has_file_path();

    void open_file(const std::string& filepath);
    void save_file(std::string filepath);
    void close_file();

    void execute(const QString& script);
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PYTHONCONSOLE_PYTHONCONSOLEWIDGET_H
