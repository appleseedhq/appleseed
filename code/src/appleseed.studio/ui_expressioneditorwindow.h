/********************************************************************************
** Form generated from reading UI file 'expressioneditorwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_EXPRESSIONEDITORWINDOW_H
#define UI_EXPRESSIONEDITORWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QScrollArea>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_ExpressionEditorWindow
{
public:
    QGridLayout *gridLayout;
    QScrollArea *scrollarea;
    QWidget *scrollarea_contents;
    QHBoxLayout *buttonbox_layout;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *ExpressionEditorWindow)
    {
        if (ExpressionEditorWindow->objectName().isEmpty())
            ExpressionEditorWindow->setObjectName(QString::fromUtf8("ExpressionEditorWindow"));
        ExpressionEditorWindow->setWindowModality(Qt::ApplicationModal);
        ExpressionEditorWindow->resize(400, 600);
        gridLayout = new QGridLayout(ExpressionEditorWindow);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        scrollarea = new QScrollArea(ExpressionEditorWindow);
        scrollarea->setObjectName(QString::fromUtf8("scrollarea"));
        scrollarea->setWidgetResizable(true);
        scrollarea_contents = new QWidget();
        scrollarea_contents->setObjectName(QString::fromUtf8("scrollarea_contents"));
        scrollarea_contents->setGeometry(QRect(0, 0, 380, 545));
        scrollarea->setWidget(scrollarea_contents);

        gridLayout->addWidget(scrollarea, 0, 0, 1, 1);

        buttonbox_layout = new QHBoxLayout();
        buttonbox_layout->setObjectName(QString::fromUtf8("buttonbox_layout"));
        buttonbox = new QDialogButtonBox(ExpressionEditorWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Apply|QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        buttonbox_layout->addWidget(buttonbox);


        gridLayout->addLayout(buttonbox_layout, 1, 0, 1, 1);


        retranslateUi(ExpressionEditorWindow);

        QMetaObject::connectSlotsByName(ExpressionEditorWindow);
    } // setupUi

    void retranslateUi(QWidget *ExpressionEditorWindow)
    {
        ExpressionEditorWindow->setWindowTitle(QApplication::translate("ExpressionEditorWindow", "Expression Editor", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class ExpressionEditorWindow: public Ui_ExpressionEditorWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_EXPRESSIONEDITORWINDOW_H
