/********************************************************************************
** Form generated from reading UI file 'benchmarkwindow.ui'
**
** Created by: Qt User Interface Compiler version 4.8.7
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_BENCHMARKWINDOW_H
#define UI_BENCHMARKWINDOW_H

#include <QtCore/QLocale>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDialogButtonBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSplitter>
#include <QtGui/QTreeWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_BenchmarkWindow
{
public:
    QGridLayout *gridLayout;
    QDialogButtonBox *buttonbox;
    QSplitter *splitter;
    QWidget *graphs_contents;
    QVBoxLayout *layout_graphs_contents;
    QWidget *benchmarks_contents;
    QVBoxLayout *layout_benchmarks_contents;
    QTreeWidget *treewidget_benchmarks;
    QHBoxLayout *horizontalLayout;
    QPushButton *pushbutton_run;
    QCheckBox *checkbox_equidistant;
    QSpacerItem *horizontalSpacer;

    void setupUi(QWidget *BenchmarkWindow)
    {
        if (BenchmarkWindow->objectName().isEmpty())
            BenchmarkWindow->setObjectName(QString::fromUtf8("BenchmarkWindow"));
        BenchmarkWindow->resize(800, 600);
        BenchmarkWindow->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
        gridLayout = new QGridLayout(BenchmarkWindow);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(9, 9, 9, 9);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        buttonbox = new QDialogButtonBox(BenchmarkWindow);
        buttonbox->setObjectName(QString::fromUtf8("buttonbox"));
        buttonbox->setOrientation(Qt::Horizontal);
        buttonbox->setStandardButtons(QDialogButtonBox::Close);

        gridLayout->addWidget(buttonbox, 1, 0, 1, 1);

        splitter = new QSplitter(BenchmarkWindow);
        splitter->setObjectName(QString::fromUtf8("splitter"));
        splitter->setOrientation(Qt::Vertical);
        graphs_contents = new QWidget(splitter);
        graphs_contents->setObjectName(QString::fromUtf8("graphs_contents"));
        layout_graphs_contents = new QVBoxLayout(graphs_contents);
        layout_graphs_contents->setSpacing(6);
        layout_graphs_contents->setContentsMargins(9, 9, 9, 9);
        layout_graphs_contents->setObjectName(QString::fromUtf8("layout_graphs_contents"));
        layout_graphs_contents->setContentsMargins(0, 0, 0, 0);
        splitter->addWidget(graphs_contents);
        benchmarks_contents = new QWidget(splitter);
        benchmarks_contents->setObjectName(QString::fromUtf8("benchmarks_contents"));
        layout_benchmarks_contents = new QVBoxLayout(benchmarks_contents);
        layout_benchmarks_contents->setSpacing(6);
        layout_benchmarks_contents->setContentsMargins(9, 9, 9, 9);
        layout_benchmarks_contents->setObjectName(QString::fromUtf8("layout_benchmarks_contents"));
        layout_benchmarks_contents->setContentsMargins(0, 0, 0, 0);
        treewidget_benchmarks = new QTreeWidget(benchmarks_contents);
        QTreeWidgetItem *__qtreewidgetitem = new QTreeWidgetItem();
        __qtreewidgetitem->setText(0, QString::fromUtf8("1"));
        treewidget_benchmarks->setHeaderItem(__qtreewidgetitem);
        treewidget_benchmarks->setObjectName(QString::fromUtf8("treewidget_benchmarks"));
        treewidget_benchmarks->setSelectionMode(QAbstractItemView::ExtendedSelection);

        layout_benchmarks_contents->addWidget(treewidget_benchmarks);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        pushbutton_run = new QPushButton(benchmarks_contents);
        pushbutton_run->setObjectName(QString::fromUtf8("pushbutton_run"));

        horizontalLayout->addWidget(pushbutton_run);

        checkbox_equidistant = new QCheckBox(benchmarks_contents);
        checkbox_equidistant->setObjectName(QString::fromUtf8("checkbox_equidistant"));
        checkbox_equidistant->setChecked(true);

        horizontalLayout->addWidget(checkbox_equidistant);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        layout_benchmarks_contents->addLayout(horizontalLayout);

        splitter->addWidget(benchmarks_contents);

        gridLayout->addWidget(splitter, 0, 0, 1, 1);


        retranslateUi(BenchmarkWindow);

        QMetaObject::connectSlotsByName(BenchmarkWindow);
    } // setupUi

    void retranslateUi(QWidget *BenchmarkWindow)
    {
        BenchmarkWindow->setWindowTitle(QApplication::translate("BenchmarkWindow", "Benchmarks", 0, QApplication::UnicodeUTF8));
        pushbutton_run->setText(QApplication::translate("BenchmarkWindow", "&Run", 0, QApplication::UnicodeUTF8));
        checkbox_equidistant->setText(QApplication::translate("BenchmarkWindow", "&Equidistant", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class BenchmarkWindow: public Ui_BenchmarkWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_BENCHMARKWINDOW_H
