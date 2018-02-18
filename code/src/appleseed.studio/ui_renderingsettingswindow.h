/********************************************************************************
** Form generated from reading UI file 'renderingsettingswindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_RENDERINGSETTINGSWINDOW_H
#define UI_RENDERINGSETTINGSWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QScrollArea>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_RenderingSettingsWindow
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *layout_configurations;
    QLabel *label_configuration;
    QComboBox *combobox_configurations;
    QPushButton *pushbutton_manage;
    QScrollArea *scrollarea;
    QWidget *scrollareawidget;
    QVBoxLayout *verticalLayout_3;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *RenderingSettingsWindow)
    {
        if (RenderingSettingsWindow->objectName().isEmpty())
            RenderingSettingsWindow->setObjectName(QString::fromUtf8("RenderingSettingsWindow"));
        RenderingSettingsWindow->resize(550, 600);
        RenderingSettingsWindow->setMinimumSize(QSize(550, 600));
        verticalLayout = new QVBoxLayout(RenderingSettingsWindow);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        layout_configurations = new QHBoxLayout();
        layout_configurations->setObjectName(QString::fromUtf8("layout_configurations"));
        label_configuration = new QLabel(RenderingSettingsWindow);
        label_configuration->setObjectName(QString::fromUtf8("label_configuration"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(label_configuration->sizePolicy().hasHeightForWidth());
        label_configuration->setSizePolicy(sizePolicy);

        layout_configurations->addWidget(label_configuration);

        combobox_configurations = new QComboBox(RenderingSettingsWindow);
        combobox_configurations->setObjectName(QString::fromUtf8("combobox_configurations"));

        layout_configurations->addWidget(combobox_configurations);

        pushbutton_manage = new QPushButton(RenderingSettingsWindow);
        pushbutton_manage->setObjectName(QString::fromUtf8("pushbutton_manage"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(pushbutton_manage->sizePolicy().hasHeightForWidth());
        pushbutton_manage->setSizePolicy(sizePolicy1);

        layout_configurations->addWidget(pushbutton_manage);


        verticalLayout->addLayout(layout_configurations);

        scrollarea = new QScrollArea(RenderingSettingsWindow);
        scrollarea->setObjectName(QString::fromUtf8("scrollarea"));
        scrollarea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        scrollarea->setWidgetResizable(true);
        scrollarea->setAlignment(Qt::AlignHCenter|Qt::AlignTop);
        scrollareawidget = new QWidget();
        scrollareawidget->setObjectName(QString::fromUtf8("scrollareawidget"));
        scrollareawidget->setGeometry(QRect(0, 0, 513, 520));
        verticalLayout_3 = new QVBoxLayout(scrollareawidget);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setSizeConstraint(QLayout::SetFixedSize);
        scrollarea->setWidget(scrollareawidget);

        verticalLayout->addWidget(scrollarea);

        buttonbox = new QDialogButtonBox(RenderingSettingsWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonbox);


        retranslateUi(RenderingSettingsWindow);

        QMetaObject::connectSlotsByName(RenderingSettingsWindow);
    } // setupUi

    void retranslateUi(QWidget *RenderingSettingsWindow)
    {
        RenderingSettingsWindow->setWindowTitle(QApplication::translate("RenderingSettingsWindow", "Rendering Settings", 0, QApplication::UnicodeUTF8));
        label_configuration->setText(QApplication::translate("RenderingSettingsWindow", "Configuration:", 0, QApplication::UnicodeUTF8));
        pushbutton_manage->setText(QApplication::translate("RenderingSettingsWindow", "Manage", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class RenderingSettingsWindow: public Ui_RenderingSettingsWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_RENDERINGSETTINGSWINDOW_H
