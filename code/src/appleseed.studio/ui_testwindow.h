/********************************************************************************
** Form generated from reading UI file 'testwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_TESTWINDOW_H
#define UI_TESTWINDOW_H

#include <QtCore/QLocale>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSplitter>
#include <QtGui/QTreeWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_TestWindow
{
public:
    QGridLayout *gridLayout_2;
    QGridLayout *test_window_contents;
    QSplitter *splitter;
    QWidget *tests_contents;
    QVBoxLayout *layout_tests_contents;
    QLabel *label_tests;
    QHBoxLayout *layout_filter;
    QLabel *label_filter;
    QLineEdit *lineedit_filter;
    QPushButton *pushbutton_clear_filter;
    QTreeWidget *treewidget_tests;
    QLabel *label_checked_tests;
    QFrame *frame_tests;
    QHBoxLayout *horizontalLayout_4;
    QPushButton *pushbutton_check_all;
    QPushButton *pushbutton_check_only_visible;
    QPushButton *pushbutton_uncheck_all;
    QSpacerItem *horizontalspacer_tests;
    QWidget *output_contents;
    QVBoxLayout *layout_output_contents;
    QLabel *label_results;
    QTreeWidget *treewidget_output;
    QLabel *label_tests_results;
    QFrame *frame_output;
    QHBoxLayout *horizontalLayout_3;
    QPushButton *pushbutton_run;
    QPushButton *pushbutton_clear;
    QCheckBox *checkbox_show_all;
    QSpacerItem *horizontalspacer_output;
    QDialogButtonBox *buttonbox;

    void setupUi(QWidget *TestWindow)
    {
        if (TestWindow->objectName().isEmpty())
            TestWindow->setObjectName(QString::fromUtf8("TestWindow"));
        TestWindow->resize(800, 600);
        TestWindow->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
        gridLayout_2 = new QGridLayout(TestWindow);
        gridLayout_2->setSpacing(6);
        gridLayout_2->setContentsMargins(9, 9, 9, 9);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        test_window_contents = new QGridLayout();
        test_window_contents->setSpacing(6);
        test_window_contents->setObjectName(QString::fromUtf8("test_window_contents"));
        splitter = new QSplitter(TestWindow);
        splitter->setObjectName(QString::fromUtf8("splitter"));
        splitter->setOrientation(Qt::Horizontal);
        tests_contents = new QWidget(splitter);
        tests_contents->setObjectName(QString::fromUtf8("tests_contents"));
        layout_tests_contents = new QVBoxLayout(tests_contents);
        layout_tests_contents->setSpacing(6);
        layout_tests_contents->setContentsMargins(9, 9, 9, 9);
        layout_tests_contents->setObjectName(QString::fromUtf8("layout_tests_contents"));
        layout_tests_contents->setContentsMargins(0, 0, 0, 0);
        label_tests = new QLabel(tests_contents);
        label_tests->setObjectName(QString::fromUtf8("label_tests"));

        layout_tests_contents->addWidget(label_tests);

        layout_filter = new QHBoxLayout();
        layout_filter->setSpacing(6);
        layout_filter->setObjectName(QString::fromUtf8("layout_filter"));
        layout_filter->setContentsMargins(-1, 0, -1, -1);
        label_filter = new QLabel(tests_contents);
        label_filter->setObjectName(QString::fromUtf8("label_filter"));

        layout_filter->addWidget(label_filter);

        lineedit_filter = new QLineEdit(tests_contents);
        lineedit_filter->setObjectName(QString::fromUtf8("lineedit_filter"));

        layout_filter->addWidget(lineedit_filter);

        pushbutton_clear_filter = new QPushButton(tests_contents);
        pushbutton_clear_filter->setObjectName(QString::fromUtf8("pushbutton_clear_filter"));

        layout_filter->addWidget(pushbutton_clear_filter);


        layout_tests_contents->addLayout(layout_filter);

        treewidget_tests = new QTreeWidget(tests_contents);
        QTreeWidgetItem *__qtreewidgetitem = new QTreeWidgetItem();
        __qtreewidgetitem->setText(0, QString::fromUtf8("1"));
        treewidget_tests->setHeaderItem(__qtreewidgetitem);
        treewidget_tests->setObjectName(QString::fromUtf8("treewidget_tests"));
        treewidget_tests->setSortingEnabled(true);
        treewidget_tests->setAllColumnsShowFocus(true);
        treewidget_tests->header()->setVisible(true);

        layout_tests_contents->addWidget(treewidget_tests);

        label_checked_tests = new QLabel(tests_contents);
        label_checked_tests->setObjectName(QString::fromUtf8("label_checked_tests"));
        label_checked_tests->setTextFormat(Qt::RichText);

        layout_tests_contents->addWidget(label_checked_tests);

        frame_tests = new QFrame(tests_contents);
        frame_tests->setObjectName(QString::fromUtf8("frame_tests"));
        frame_tests->setEnabled(true);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(frame_tests->sizePolicy().hasHeightForWidth());
        frame_tests->setSizePolicy(sizePolicy);
        horizontalLayout_4 = new QHBoxLayout(frame_tests);
        horizontalLayout_4->setSpacing(6);
        horizontalLayout_4->setContentsMargins(9, 9, 9, 9);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        pushbutton_check_all = new QPushButton(frame_tests);
        pushbutton_check_all->setObjectName(QString::fromUtf8("pushbutton_check_all"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(pushbutton_check_all->sizePolicy().hasHeightForWidth());
        pushbutton_check_all->setSizePolicy(sizePolicy1);

        horizontalLayout_4->addWidget(pushbutton_check_all);

        pushbutton_check_only_visible = new QPushButton(frame_tests);
        pushbutton_check_only_visible->setObjectName(QString::fromUtf8("pushbutton_check_only_visible"));

        horizontalLayout_4->addWidget(pushbutton_check_only_visible);

        pushbutton_uncheck_all = new QPushButton(frame_tests);
        pushbutton_uncheck_all->setObjectName(QString::fromUtf8("pushbutton_uncheck_all"));
        sizePolicy1.setHeightForWidth(pushbutton_uncheck_all->sizePolicy().hasHeightForWidth());
        pushbutton_uncheck_all->setSizePolicy(sizePolicy1);

        horizontalLayout_4->addWidget(pushbutton_uncheck_all);

        horizontalspacer_tests = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalspacer_tests);


        layout_tests_contents->addWidget(frame_tests);

        splitter->addWidget(tests_contents);
        output_contents = new QWidget(splitter);
        output_contents->setObjectName(QString::fromUtf8("output_contents"));
        layout_output_contents = new QVBoxLayout(output_contents);
        layout_output_contents->setSpacing(6);
        layout_output_contents->setContentsMargins(9, 9, 9, 9);
        layout_output_contents->setObjectName(QString::fromUtf8("layout_output_contents"));
        layout_output_contents->setContentsMargins(0, 0, 0, 0);
        label_results = new QLabel(output_contents);
        label_results->setObjectName(QString::fromUtf8("label_results"));

        layout_output_contents->addWidget(label_results);

        treewidget_output = new QTreeWidget(output_contents);
        QTreeWidgetItem *__qtreewidgetitem1 = new QTreeWidgetItem();
        __qtreewidgetitem1->setText(2, QString::fromUtf8("3"));
        __qtreewidgetitem1->setText(1, QString::fromUtf8("2"));
        __qtreewidgetitem1->setText(0, QString::fromUtf8("1"));
        treewidget_output->setHeaderItem(__qtreewidgetitem1);
        treewidget_output->setObjectName(QString::fromUtf8("treewidget_output"));
        treewidget_output->setSortingEnabled(true);
        treewidget_output->setAllColumnsShowFocus(true);
        treewidget_output->setColumnCount(3);
        treewidget_output->header()->setDefaultSectionSize(100);
        treewidget_output->header()->setHighlightSections(false);
        treewidget_output->header()->setProperty("showSortIndicator", QVariant(true));

        layout_output_contents->addWidget(treewidget_output);

        label_tests_results = new QLabel(output_contents);
        label_tests_results->setObjectName(QString::fromUtf8("label_tests_results"));
        label_tests_results->setTextFormat(Qt::RichText);

        layout_output_contents->addWidget(label_tests_results);

        frame_output = new QFrame(output_contents);
        frame_output->setObjectName(QString::fromUtf8("frame_output"));
        sizePolicy.setHeightForWidth(frame_output->sizePolicy().hasHeightForWidth());
        frame_output->setSizePolicy(sizePolicy);
        horizontalLayout_3 = new QHBoxLayout(frame_output);
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setContentsMargins(9, 9, 9, 9);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        pushbutton_run = new QPushButton(frame_output);
        pushbutton_run->setObjectName(QString::fromUtf8("pushbutton_run"));
        sizePolicy1.setHeightForWidth(pushbutton_run->sizePolicy().hasHeightForWidth());
        pushbutton_run->setSizePolicy(sizePolicy1);

        horizontalLayout_3->addWidget(pushbutton_run);

        pushbutton_clear = new QPushButton(frame_output);
        pushbutton_clear->setObjectName(QString::fromUtf8("pushbutton_clear"));
        sizePolicy1.setHeightForWidth(pushbutton_clear->sizePolicy().hasHeightForWidth());
        pushbutton_clear->setSizePolicy(sizePolicy1);

        horizontalLayout_3->addWidget(pushbutton_clear);

        checkbox_show_all = new QCheckBox(frame_output);
        checkbox_show_all->setObjectName(QString::fromUtf8("checkbox_show_all"));

        horizontalLayout_3->addWidget(checkbox_show_all);

        horizontalspacer_output = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalspacer_output);


        layout_output_contents->addWidget(frame_output);

        splitter->addWidget(output_contents);

        test_window_contents->addWidget(splitter, 0, 0, 1, 1);

        buttonbox = new QDialogButtonBox(TestWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setOrientation(Qt::Horizontal);
        buttonbox->setStandardButtons(QDialogButtonBox::Close);

        test_window_contents->addWidget(buttonbox, 4, 0, 1, 1);


        gridLayout_2->addLayout(test_window_contents, 0, 0, 1, 1);


        retranslateUi(TestWindow);

        QMetaObject::connectSlotsByName(TestWindow);
    } // setupUi

    void retranslateUi(QWidget *TestWindow)
    {
        TestWindow->setWindowTitle(QApplication::translate("TestWindow", "Tests", 0, QApplication::UnicodeUTF8));
        label_tests->setText(QApplication::translate("TestWindow", "Tests", 0, QApplication::UnicodeUTF8));
        label_filter->setText(QApplication::translate("TestWindow", "Filter:", 0, QApplication::UnicodeUTF8));
        pushbutton_clear_filter->setText(QApplication::translate("TestWindow", "Clear", 0, QApplication::UnicodeUTF8));
        label_checked_tests->setText(QString());
        frame_tests->setStyleSheet(QApplication::translate("TestWindow", "QFrame\n"
"{\n"
"	border: 0px;\n"
"}\n"
"", 0, QApplication::UnicodeUTF8));
        pushbutton_check_all->setText(QApplication::translate("TestWindow", "&Check All", 0, QApplication::UnicodeUTF8));
        pushbutton_check_only_visible->setText(QApplication::translate("TestWindow", "Check Only &Visible", 0, QApplication::UnicodeUTF8));
        pushbutton_uncheck_all->setText(QApplication::translate("TestWindow", "&Uncheck All", 0, QApplication::UnicodeUTF8));
        label_results->setText(QApplication::translate("TestWindow", "Results", 0, QApplication::UnicodeUTF8));
        label_tests_results->setText(QString());
        frame_output->setStyleSheet(QApplication::translate("TestWindow", "QFrame\n"
"{\n"
"	border: 0px;\n"
"}\n"
"", 0, QApplication::UnicodeUTF8));
        pushbutton_run->setText(QApplication::translate("TestWindow", "&Run", 0, QApplication::UnicodeUTF8));
        pushbutton_clear->setText(QApplication::translate("TestWindow", "Clear &Output", 0, QApplication::UnicodeUTF8));
        checkbox_show_all->setText(QApplication::translate("TestWindow", "Show &All", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class TestWindow: public Ui_TestWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TESTWINDOW_H
