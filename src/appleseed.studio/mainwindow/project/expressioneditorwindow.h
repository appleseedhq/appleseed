
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Marius Avram, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_MAINWINDOW_PROJECT_EXPRESSIONEDITORWINDOW_H
#define APPLESEED_STUDIO_MAINWINDOW_PROJECT_EXPRESSIONEDITORWINDOW_H

// Qt headers.
#include <QObject>
#include <QString>
#include <QWidget>

// Standard headers.
#include <string>

// Forward declarations.
namespace renderer  { class ParamArray; }
namespace renderer  { class Project; }
namespace Ui        { class ExpressionEditorWindow; }
class QLabel;
class SeExprEdBrowser;
class SeExprEditor;

namespace appleseed {
namespace studio {

class ExpressionEditorWindow
  : public QWidget
{
    Q_OBJECT

  public:
    ExpressionEditorWindow(
        QWidget*                    parent,
        const renderer::Project&    project,
        renderer::ParamArray&       settings,
        const QString&              widget_name,
        const std::string&          expression);

    void apply_expression();

  public slots:
    void slot_accept();
    void slot_apply();
    void slot_cancel();

    void slot_clear_expression();
    void slot_save_script();
    void slot_load_script();
    void slot_show_examples();
    void slot_show_help();

  signals:
    void signal_expression_applied(const QString& widget_name, const QString& expression);
    void signal_editor_closed();

  protected:
    virtual void closeEvent(QCloseEvent* e);

  private:
    Ui::ExpressionEditorWindow*     m_ui;
    const renderer::Project&        m_project;
    renderer::ParamArray&           m_settings;
    const QString                   m_widget_name;
    SeExprEditor*                   m_editor;
    SeExprEdBrowser*                m_browser;
    QLabel*                         m_error;
    std::string                     m_script_filepath;
    bool                            m_show_examples;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_MAINWINDOW_PROJECT_EXPRESSIONEDITORWINDOW_H
