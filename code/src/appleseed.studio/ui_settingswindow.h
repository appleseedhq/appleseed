/********************************************************************************
** Form generated from reading UI file 'settingswindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_SETTINGSWINDOW_H
#define UI_SETTINGSWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QScrollArea>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_SettingsWindow
{
public:
    QVBoxLayout *verticalLayout;
    QScrollArea *scrollarea;
    QWidget *scrollareawidget;
    QVBoxLayout *verticalLayout_3;
    QGroupBox *groupbox_user_interface;
    QGridLayout *gridLayout;
    QGridLayout *layout_message_verbosity;
    QComboBox *combobox_message_verbosity;
    QLabel *label_message_verbosity;
    QCheckBox *checkbox_render_region_triggers_rendering;
    QGroupBox *groupbox_rendering;
    QGridLayout *gridLayout_2;
    QGridLayout *layout_sampling_mode;
    QComboBox *combobox_sampling_mode;
    QLabel *label_sampling_mode;
    QCheckBox *checkbox_autosave;
    QCheckBox *checkbox_print_final_average_luminance;
    QSpacerItem *verticalspacer;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *SettingsWindow)
    {
        if (SettingsWindow->objectName().isEmpty())
            SettingsWindow->setObjectName(QString::fromUtf8("SettingsWindow"));
        SettingsWindow->resize(550, 600);
        SettingsWindow->setMinimumSize(QSize(550, 600));
        verticalLayout = new QVBoxLayout(SettingsWindow);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        scrollarea = new QScrollArea(SettingsWindow);
        scrollarea->setObjectName(QString::fromUtf8("scrollarea"));
        scrollarea->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
        scrollarea->setWidgetResizable(true);
        scrollarea->setAlignment(Qt::AlignHCenter|Qt::AlignTop);
        scrollareawidget = new QWidget();
        scrollareawidget->setObjectName(QString::fromUtf8("scrollareawidget"));
        verticalLayout_3 = new QVBoxLayout(scrollareawidget);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        groupbox_user_interface = new QGroupBox(scrollareawidget);
        groupbox_user_interface->setObjectName(QString::fromUtf8("groupbox_user_interface"));
        gridLayout = new QGridLayout(groupbox_user_interface);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        layout_message_verbosity = new QGridLayout();
        layout_message_verbosity->setObjectName(QString::fromUtf8("layout_message_verbosity"));
        combobox_message_verbosity = new QComboBox(groupbox_user_interface);
        combobox_message_verbosity->setObjectName(QString::fromUtf8("combobox_message_verbosity"));

        layout_message_verbosity->addWidget(combobox_message_verbosity, 0, 1, 1, 1);

        label_message_verbosity = new QLabel(groupbox_user_interface);
        label_message_verbosity->setObjectName(QString::fromUtf8("label_message_verbosity"));

        layout_message_verbosity->addWidget(label_message_verbosity, 0, 0, 1, 1);


        gridLayout->addLayout(layout_message_verbosity, 0, 0, 1, 1);

        checkbox_render_region_triggers_rendering = new QCheckBox(groupbox_user_interface);
        checkbox_render_region_triggers_rendering->setObjectName(QString::fromUtf8("checkbox_render_region_triggers_rendering"));
        checkbox_render_region_triggers_rendering->setChecked(true);

        gridLayout->addWidget(checkbox_render_region_triggers_rendering, 1, 0, 1, 1);


        verticalLayout_3->addWidget(groupbox_user_interface);

        groupbox_rendering = new QGroupBox(scrollareawidget);
        groupbox_rendering->setObjectName(QString::fromUtf8("groupbox_rendering"));
        gridLayout_2 = new QGridLayout(groupbox_rendering);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        layout_sampling_mode = new QGridLayout();
        layout_sampling_mode->setObjectName(QString::fromUtf8("layout_sampling_mode"));
        combobox_sampling_mode = new QComboBox(groupbox_rendering);
        combobox_sampling_mode->setObjectName(QString::fromUtf8("combobox_sampling_mode"));

        layout_sampling_mode->addWidget(combobox_sampling_mode, 0, 1, 1, 1);

        label_sampling_mode = new QLabel(groupbox_rendering);
        label_sampling_mode->setObjectName(QString::fromUtf8("label_sampling_mode"));

        layout_sampling_mode->addWidget(label_sampling_mode, 0, 0, 1, 1);


        gridLayout_2->addLayout(layout_sampling_mode, 0, 0, 1, 1);

        checkbox_autosave = new QCheckBox(groupbox_rendering);
        checkbox_autosave->setObjectName(QString::fromUtf8("checkbox_autosave"));

        gridLayout_2->addWidget(checkbox_autosave, 1, 0, 1, 1);

        checkbox_print_final_average_luminance = new QCheckBox(groupbox_rendering);
        checkbox_print_final_average_luminance->setObjectName(QString::fromUtf8("checkbox_print_final_average_luminance"));

        gridLayout_2->addWidget(checkbox_print_final_average_luminance, 2, 0, 1, 1);


        verticalLayout_3->addWidget(groupbox_rendering);

        verticalspacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalspacer);

        scrollarea->setWidget(scrollareawidget);

        verticalLayout->addWidget(scrollarea);

        buttonbox = new QDialogButtonBox(SettingsWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonbox);


        retranslateUi(SettingsWindow);

        combobox_message_verbosity->setCurrentIndex(1);
        combobox_sampling_mode->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(SettingsWindow);
    } // setupUi

    void retranslateUi(QWidget *SettingsWindow)
    {
        SettingsWindow->setWindowTitle(QApplication::translate("SettingsWindow", "Settings", 0, QApplication::UnicodeUTF8));
        groupbox_user_interface->setTitle(QApplication::translate("SettingsWindow", "User Interface", 0, QApplication::UnicodeUTF8));
        combobox_message_verbosity->clear();
        combobox_message_verbosity->insertItems(0, QStringList()
         << QApplication::translate("SettingsWindow", "Debug", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("SettingsWindow", "Info", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("SettingsWindow", "Warning", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("SettingsWindow", "Error", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("SettingsWindow", "Fatal", 0, QApplication::UnicodeUTF8)
        );
        label_message_verbosity->setText(QApplication::translate("SettingsWindow", "Message Verbosity", 0, QApplication::UnicodeUTF8));
        checkbox_render_region_triggers_rendering->setText(QApplication::translate("SettingsWindow", "Start rendering after a render region is defined", 0, QApplication::UnicodeUTF8));
        groupbox_rendering->setTitle(QApplication::translate("SettingsWindow", "Rendering", 0, QApplication::UnicodeUTF8));
        combobox_sampling_mode->clear();
        combobox_sampling_mode->insertItems(0, QStringList()
         << QApplication::translate("SettingsWindow", "RNG", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("SettingsWindow", "QMC", 0, QApplication::UnicodeUTF8)
        );
        label_sampling_mode->setText(QApplication::translate("SettingsWindow", "Sampling Mode", 0, QApplication::UnicodeUTF8));
        checkbox_autosave->setText(QApplication::translate("SettingsWindow", "Automatically save all renders to disk", 0, QApplication::UnicodeUTF8));
        checkbox_print_final_average_luminance->setText(QApplication::translate("SettingsWindow", "Print average luminance when render ends", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class SettingsWindow: public Ui_SettingsWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SETTINGSWINDOW_H
