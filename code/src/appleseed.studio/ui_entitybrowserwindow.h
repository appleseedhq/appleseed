/********************************************************************************
** Form generated from reading UI file 'entitybrowserwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ENTITYBROWSERWINDOW_H
#define UI_ENTITYBROWSERWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_EntityBrowserWindow
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *label_filter;
    QLineEdit *lineedit_filter;
    QPushButton *pushbutton_clear_filter;
    QTabWidget *tab_widget;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *EntityBrowserWindow)
    {
        if (EntityBrowserWindow->objectName().isEmpty())
            EntityBrowserWindow->setObjectName(QString::fromUtf8("EntityBrowserWindow"));
        EntityBrowserWindow->setWindowModality(Qt::ApplicationModal);
        EntityBrowserWindow->resize(400, 300);
        verticalLayout = new QVBoxLayout(EntityBrowserWindow);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label_filter = new QLabel(EntityBrowserWindow);
        label_filter->setObjectName(QString::fromUtf8("label_filter"));

        horizontalLayout->addWidget(label_filter);

        lineedit_filter = new QLineEdit(EntityBrowserWindow);
        lineedit_filter->setObjectName(QString::fromUtf8("lineedit_filter"));

        horizontalLayout->addWidget(lineedit_filter);

        pushbutton_clear_filter = new QPushButton(EntityBrowserWindow);
        pushbutton_clear_filter->setObjectName(QString::fromUtf8("pushbutton_clear_filter"));

        horizontalLayout->addWidget(pushbutton_clear_filter);


        verticalLayout->addLayout(horizontalLayout);

        tab_widget = new QTabWidget(EntityBrowserWindow);
        tab_widget->setObjectName(QString::fromUtf8("tab_widget"));

        verticalLayout->addWidget(tab_widget);

        buttonbox = new QDialogButtonBox(EntityBrowserWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonbox);


        retranslateUi(EntityBrowserWindow);

        tab_widget->setCurrentIndex(-1);


        QMetaObject::connectSlotsByName(EntityBrowserWindow);
    } // setupUi

    void retranslateUi(QWidget *EntityBrowserWindow)
    {
        EntityBrowserWindow->setWindowTitle(QApplication::translate("EntityBrowserWindow", "Entity Browser", 0, QApplication::UnicodeUTF8));
        label_filter->setText(QApplication::translate("EntityBrowserWindow", "Filter:", 0, QApplication::UnicodeUTF8));
        pushbutton_clear_filter->setText(QApplication::translate("EntityBrowserWindow", "Clear", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class EntityBrowserWindow: public Ui_EntityBrowserWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ENTITYBROWSERWINDOW_H
