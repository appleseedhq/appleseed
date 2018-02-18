/********************************************************************************
** Form generated from reading UI file 'entityeditorwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ENTITYEDITORWINDOW_H
#define UI_ENTITYEDITORWINDOW_H

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

class Ui_EntityEditorWindow
{
public:
    QGridLayout *gridLayout;
    QDialogButtonBox *buttonbox;
    QScrollArea *scrollarea;
    QWidget *scrollarea_contents;

    void setupUi(QWidget *EntityEditorWindow)
    {
        if (EntityEditorWindow->objectName().isEmpty())
            EntityEditorWindow->setObjectName(QString::fromUtf8("EntityEditorWindow"));
        EntityEditorWindow->setWindowModality(Qt::ApplicationModal);
        EntityEditorWindow->resize(500, 400);
        gridLayout = new QGridLayout(EntityEditorWindow);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        buttonbox = new QDialogButtonBox(EntityEditorWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        gridLayout->addWidget(buttonbox, 1, 0, 1, 1);

        scrollarea = new QScrollArea(EntityEditorWindow);
        scrollarea->setObjectName(QString::fromUtf8("scrollarea"));
        scrollarea->setWidgetResizable(true);
        scrollarea_contents = new QWidget();
        scrollarea_contents->setObjectName(QString::fromUtf8("scrollarea_contents"));
        scrollarea_contents->setGeometry(QRect(0, 0, 480, 351));
        scrollarea->setWidget(scrollarea_contents);

        gridLayout->addWidget(scrollarea, 0, 0, 1, 1);


        retranslateUi(EntityEditorWindow);

        QMetaObject::connectSlotsByName(EntityEditorWindow);
    } // setupUi

    void retranslateUi(QWidget *EntityEditorWindow)
    {
        EntityEditorWindow->setWindowTitle(QApplication::translate("EntityEditorWindow", "Edit Entity", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class EntityEditorWindow: public Ui_EntityEditorWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ENTITYEDITORWINDOW_H
