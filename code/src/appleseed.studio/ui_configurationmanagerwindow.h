/********************************************************************************
** Form generated from reading UI file 'configurationmanagerwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CONFIGURATIONMANAGERWINDOW_H
#define UI_CONFIGURATIONMANAGERWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTreeWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_ConfigurationManagerWindow
{
public:
    QVBoxLayout *verticalLayout_2;
    QHBoxLayout *horizontallayout;
    QTreeWidget *treewidget_configurations;
    QVBoxLayout *verticallayout;
    QPushButton *pushbutton_new;
    QPushButton *pushbutton_clone;
    QPushButton *pushbutton_rename;
    QPushButton *pushbutton_delete;
    QSpacerItem *verticalspacer;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *ConfigurationManagerWindow)
    {
        if (ConfigurationManagerWindow->objectName().isEmpty())
            ConfigurationManagerWindow->setObjectName(QString::fromUtf8("ConfigurationManagerWindow"));
        ConfigurationManagerWindow->setWindowModality(Qt::WindowModal);
        ConfigurationManagerWindow->resize(400, 300);
        verticalLayout_2 = new QVBoxLayout(ConfigurationManagerWindow);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        horizontallayout = new QHBoxLayout();
        horizontallayout->setObjectName(QString::fromUtf8("horizontallayout"));
        treewidget_configurations = new QTreeWidget(ConfigurationManagerWindow);
        QTreeWidgetItem *__qtreewidgetitem = new QTreeWidgetItem();
        __qtreewidgetitem->setText(0, QString::fromUtf8("Configurations"));
        treewidget_configurations->setHeaderItem(__qtreewidgetitem);
        treewidget_configurations->setObjectName(QString::fromUtf8("treewidget_configurations"));

        horizontallayout->addWidget(treewidget_configurations);

        verticallayout = new QVBoxLayout();
        verticallayout->setObjectName(QString::fromUtf8("verticallayout"));
        pushbutton_new = new QPushButton(ConfigurationManagerWindow);
        pushbutton_new->setObjectName(QString::fromUtf8("pushbutton_new"));

        verticallayout->addWidget(pushbutton_new);

        pushbutton_clone = new QPushButton(ConfigurationManagerWindow);
        pushbutton_clone->setObjectName(QString::fromUtf8("pushbutton_clone"));

        verticallayout->addWidget(pushbutton_clone);

        pushbutton_rename = new QPushButton(ConfigurationManagerWindow);
        pushbutton_rename->setObjectName(QString::fromUtf8("pushbutton_rename"));

        verticallayout->addWidget(pushbutton_rename);

        pushbutton_delete = new QPushButton(ConfigurationManagerWindow);
        pushbutton_delete->setObjectName(QString::fromUtf8("pushbutton_delete"));

        verticallayout->addWidget(pushbutton_delete);

        verticalspacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticallayout->addItem(verticalspacer);


        horizontallayout->addLayout(verticallayout);


        verticalLayout_2->addLayout(horizontallayout);

        buttonbox = new QDialogButtonBox(ConfigurationManagerWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout_2->addWidget(buttonbox);


        retranslateUi(ConfigurationManagerWindow);

        QMetaObject::connectSlotsByName(ConfigurationManagerWindow);
    } // setupUi

    void retranslateUi(QWidget *ConfigurationManagerWindow)
    {
        ConfigurationManagerWindow->setWindowTitle(QApplication::translate("ConfigurationManagerWindow", "Configuration Manager", 0, QApplication::UnicodeUTF8));
        pushbutton_new->setText(QApplication::translate("ConfigurationManagerWindow", "New", 0, QApplication::UnicodeUTF8));
        pushbutton_clone->setText(QApplication::translate("ConfigurationManagerWindow", "Clone", 0, QApplication::UnicodeUTF8));
        pushbutton_rename->setText(QApplication::translate("ConfigurationManagerWindow", "Rename", 0, QApplication::UnicodeUTF8));
        pushbutton_delete->setText(QApplication::translate("ConfigurationManagerWindow", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class ConfigurationManagerWindow: public Ui_ConfigurationManagerWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CONFIGURATIONMANAGERWINDOW_H
