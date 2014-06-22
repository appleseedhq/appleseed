
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// Interface header.
#include "expressioneditorwindow.h"

// SeExpr Editor headers.
#include <SeExprEditor/SeExprEditor.h>
#include <SeExprEditor/SeExprEdControlCollection.h>

// Ui definition headers.
#include "ui_expressioneditorwindow.h"

// Qt headers.
#include <QPushButton>
#include <QScrollArea>
#include <QVBoxLayout>

using namespace std;

namespace appleseed {
namespace studio {

ExpressionEditorWindow::ExpressionEditorWindow(
    const QString& widget_name,
    const string& expression,
    QWidget* parent)
  : m_widget_name(widget_name)
  , m_ui(new Ui::ExpressionEditorWindow())
  , QWidget(parent)
{
    m_ui->setupUi(this);
    setWindowFlags(Qt::Tool);
    setAttribute(Qt::WA_DeleteOnClose);
    QVBoxLayout* root_layout = new QVBoxLayout(m_ui->scrollarea);

    // Expression controls
    SeExprEdControlCollection *controls = new SeExprEdControlCollection();
    QScrollArea* controls_scrollarea = new QScrollArea(this);
    controls_scrollarea->setObjectName("expression_controls");
    controls_scrollarea->setMinimumHeight(200);
    controls_scrollarea->setWidgetResizable(true);
    controls_scrollarea->setWidget(controls);
    root_layout->addWidget(controls_scrollarea);

    m_editor = new SeExprEditor(this, controls);
    // setObjectName does not have effect on stylesheet
    m_editor->setStyleSheet("background-color: rgb(30, 30, 30);");
    m_editor->setExpr(expression, true);
    root_layout->addWidget(m_editor);

    QPushButton* apply_button = m_ui->buttonbox->button(QDialogButtonBox::Apply);

    // Create connections
    connect(m_ui->buttonbox, SIGNAL(accepted()), SLOT(slot_accept()));
    connect(apply_button, SIGNAL(clicked()), SLOT(slot_apply()));
    connect(m_ui->buttonbox, SIGNAL(rejected()), SLOT(slot_cancel()));
}

void ExpressionEditorWindow::apply_expression()
{
    const QString expression = QString::fromStdString(m_editor->getExpr());
    if (!expression.isEmpty())
        emit signal_expression_applied(m_widget_name, expression);
}

void ExpressionEditorWindow::slot_accept()
{
    apply_expression();
    close();
}

void ExpressionEditorWindow::slot_apply()
{
    apply_expression();
}

void ExpressionEditorWindow::slot_cancel()
{
    close();
}

}       // namespace studio
}       // namespace appleseed

#include "mainwindow/project/moc_cpp_expressioneditorwindow.cxx"
