/********************************************************************************
** Form generated from reading UI file 'materialassignmenteditorwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MATERIALASSIGNMENTEDITORWINDOW_H
#define UI_MATERIALASSIGNMENTEDITORWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QScrollArea>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MaterialAssignmentEditorWindow
{
public:
    QGridLayout *gridLayout;
    QDialogButtonBox *buttonbox;
    QScrollArea *scrollarea;
    QWidget *scrollarea_contents;

    void setupUi(QWidget *MaterialAssignmentEditorWindow)
    {
        if (MaterialAssignmentEditorWindow->objectName().isEmpty())
            MaterialAssignmentEditorWindow->setObjectName(QString::fromUtf8("MaterialAssignmentEditorWindow"));
        MaterialAssignmentEditorWindow->setWindowModality(Qt::ApplicationModal);
        MaterialAssignmentEditorWindow->resize(400, 300);
        gridLayout = new QGridLayout(MaterialAssignmentEditorWindow);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        buttonbox = new QDialogButtonBox(MaterialAssignmentEditorWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        gridLayout->addWidget(buttonbox, 1, 0, 1, 1);

        scrollarea = new QScrollArea(MaterialAssignmentEditorWindow);
        scrollarea->setObjectName(QString::fromUtf8("scrollarea"));
        scrollarea->setWidgetResizable(true);
        scrollarea_contents = new QWidget();
        scrollarea_contents->setObjectName(QString::fromUtf8("scrollarea_contents"));
        scrollarea_contents->setGeometry(QRect(0, 0, 380, 251));
        scrollarea->setWidget(scrollarea_contents);

        gridLayout->addWidget(scrollarea, 0, 0, 1, 1);


        retranslateUi(MaterialAssignmentEditorWindow);

        QMetaObject::connectSlotsByName(MaterialAssignmentEditorWindow);
    } // setupUi

    void retranslateUi(QWidget *MaterialAssignmentEditorWindow)
    {
        MaterialAssignmentEditorWindow->setWindowTitle(QApplication::translate("MaterialAssignmentEditorWindow", "Assign Materials", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MaterialAssignmentEditorWindow: public Ui_MaterialAssignmentEditorWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MATERIALASSIGNMENTEDITORWINDOW_H
