
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "testwindow.h"

// UI definition header.
#include "ui_testwindow.h"

// appleseed.studio headers.
#include "debug/tests/testoutputitem.h"
#include "utility/miscellaneous.h"
#include "utility/settingskeys.h"

// appleseed.foundation headers.
#include "foundation/utility/test.h"

// Qt headers.
#include <QDialogButtonBox>
#include <QKeySequence>
#include <QMetaType>
#include <QPushButton>
#include <QSettings>
#include <QShortcut>
#include <QStringList>
#include <Qt>
#include <QTreeWidget>

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace std;

Q_DECLARE_METATYPE(ITestCaseFactory*);
Q_DECLARE_METATYPE(TestSuite*);

namespace appleseed {
namespace studio {

//
// TestWindow class implementation.
//

TestWindow::TestWindow(QWidget* parent)
  : QWidget(parent)
  , m_ui(new Ui::TestWindow())
{
    m_ui->setupUi(this);

    setWindowFlags(Qt::Window);

    m_ui->splitter->setSizes(QList<int>() << 300 << 600);

    disable_osx_focus_rect(m_ui->treewidget_tests);
    disable_osx_focus_rect(m_ui->treewidget_output);

    m_output_widget.reset(new TestOutputWidgetDecorator(m_ui->treewidget_output));
    m_result_widget.reset(new TestResultWidgetDecorator(m_ui->label_tests_results));

    build_connections();

    m_ui->treewidget_tests->setHeaderLabels(QStringList() << "Test");
    m_ui->treewidget_tests->sortItems(0, Qt::AscendingOrder);

    populate_tests_treeview();
    slot_check_all_tests();

    m_ui->treewidget_output->setHeaderLabels(QStringList() << "Test" << "Status" << "Time");
    m_ui->treewidget_output->header()->resizeSection(0, 500);
    m_ui->treewidget_output->sortItems(0, Qt::AscendingOrder);

    const QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    restoreGeometry(settings.value("test_window_geometry").toByteArray());

    create_test_runner_thread();
}

TestWindow::~TestWindow()
{
    delete m_ui;
}

void TestWindow::closeEvent(QCloseEvent* event)
{
    QSettings settings(SETTINGS_ORGANIZATION, SETTINGS_APPLICATION);
    settings.setValue("test_window_geometry", saveGeometry());
}

void TestWindow::create_test_runner_thread()
{
    m_test_runner_thread.reset(
        new TestRunnerThread(
            m_test_suite_repository,
            m_output_widget.get(),
            m_result_widget.get()));

    connect(
        m_test_runner_thread.get(), SIGNAL(signal_finished(const int, const int)),
        this, SLOT(slot_on_tests_execution_complete(const int, const int)));
}

void TestWindow::build_connections()
{
    connect(
        m_ui->treewidget_tests, SIGNAL(itemChanged(QTreeWidgetItem*, int)),
        this, SLOT(slot_on_test_item_check_state_changed(QTreeWidgetItem*, int)));

    connect(
        m_ui->lineEdit_filter, SIGNAL(textChanged(const QString&)),
        this, SLOT(slot_filter_text_changed(const QString&)));

    connect(
        m_ui->pushButton_clear_filter, SIGNAL(clicked()),
        this, SLOT(slot_clear_filter_text()));

    connect(
        m_ui->buttonbox->button(QDialogButtonBox::Close), SIGNAL(clicked()),
        this, SLOT(close()));

    connect(
        new QShortcut(QKeySequence(Qt::Key_Escape), this), SIGNAL(activated()),
        this, SLOT(close()));

    connect(
        m_ui->pushbutton_check_all, SIGNAL(clicked()),
        this, SLOT(slot_check_all_tests()));

    connect(
        m_ui->pushbutton_uncheck_all, SIGNAL(clicked()),
        this, SLOT(slot_uncheck_all_tests()));

    connect(
        m_ui->pushbutton_run, SIGNAL(clicked()),
        this, SLOT(slot_run_tests()));

    connect(
        m_ui->pushbutton_clear, SIGNAL(clicked()),
        this, SLOT(slot_clear_output_treeview()));

    connect(
        m_ui->checkbox_show_all, SIGNAL(stateChanged(int)),
        this, SLOT(slot_filter_output_treeview()));
}

namespace
{
    void add_test_cases_to_tests_treeview(
        QTreeWidgetItem*    parent,
        TestSuite*          suite)
    {
        for (size_t i = 0; i < suite->get_case_count(); ++i)
        {
            ITestCaseFactory* factory = suite->get_case_factory(i);

            QTreeWidgetItem* item =
                new QTreeWidgetItem(
                    parent,
                    QStringList(factory->get_name()));

            item->setData(0, Qt::UserRole, QVariant::fromValue(factory));

            parent->addChild(item);
        }
    }

    void add_test_suite_to_tests_treeview(
        QTreeWidget*        parent,
        TestSuite*          suite)
    {
        QTreeWidgetItem* item =
            new QTreeWidgetItem(
                parent,
                QStringList(suite->get_name()));

        item->setData(0, Qt::UserRole, QVariant::fromValue(suite));

        parent->addTopLevelItem(item);

        add_test_cases_to_tests_treeview(item, suite);
    }
}

void TestWindow::populate_tests_treeview() const
{
    TestSuiteRepository& suite_repository = TestSuiteRepository::instance();

    for (size_t i = 0; i < suite_repository.get_suite_count(); ++i)
    {
        TestSuite* suite = suite_repository.get_suite(i);
        add_test_suite_to_tests_treeview(m_ui->treewidget_tests, suite);
    }
}

namespace
{
    size_t count_checked_cases(const QTreeWidgetItem* item)
    {
        size_t checked_case_count = 0;

        for (int i = 0; i < item->childCount(); ++i)
        {
            QTreeWidgetItem* child = item->child(i);

            if (child->checkState(0) == Qt::Checked)
                ++checked_case_count;
        }

        return checked_case_count;
    }

    void count_items(
        const QTreeWidget*  widget,
        size_t&             test_case_count,
        size_t&             checked_test_case_count)
    {
        test_case_count = 0;
        checked_test_case_count = 0;

        for (int i = 0; i < widget->topLevelItemCount(); ++i)
        {
            const QTreeWidgetItem* item = widget->topLevelItem(i);
            const Qt::CheckState state = item->checkState(0);

            test_case_count += item->childCount();

            if (state == Qt::Checked || state == Qt::PartiallyChecked)
                checked_test_case_count += count_checked_cases(item);
        }
    }
}

void TestWindow::update_checked_tests_label() const
{
    size_t test_case_count;
    size_t checked_test_case_count;

    count_items(
        m_ui->treewidget_tests,
        test_case_count,
        checked_test_case_count);

    m_ui->label_checked_tests->setText(
        QString("<b>%1</b> out of <b>%2</b> test%3 selected")
            .arg(checked_test_case_count)
            .arg(test_case_count)
            .arg(test_case_count > 1 ? "s" : ""));
}

namespace
{
    void set_child_items_check_state(
        const QTreeWidgetItem*  parent,
        const int               column,
        const Qt::CheckState    state)
    {
        for (int i = 0; i < parent->childCount(); ++i)
        {
            QTreeWidgetItem* item = parent->child(i);
            item->setCheckState(column, state);
            set_child_items_check_state(item, column, state);
        }
    }

    void set_all_items_check_state(
        QTreeWidget*            widget,
        const int               column,
        const Qt::CheckState    state)
    {
        const bool previous_block = widget->blockSignals(true);

        for (int i = 0; i < widget->topLevelItemCount(); ++i)
        {
            QTreeWidgetItem* item = widget->topLevelItem(i);
            item->setCheckState(column, state);
            set_child_items_check_state(item, column, state);
        }

        widget->blockSignals(previous_block);
    }

    bool has_child_items(
        const QTreeWidgetItem*  parent,
        const int               column,
        const Qt::CheckState    state)
    {
        for (int i = 0; i < parent->childCount(); ++i)
        {
            QTreeWidgetItem* item = parent->child(i);
            if (item->checkState(column) == state)
                return true;
        }

        return false;
    }

    Qt::CheckState determinate_check_state(
        const bool              has_checked_children,
        const bool              has_unchecked_children,
        const bool              has_partially_checked_children)
    {
        if (has_checked_children)
        {
            if (has_unchecked_children || has_partially_checked_children)
                return Qt::PartiallyChecked;
            else return Qt::Checked;
        }
        else
        {
            if (has_partially_checked_children)
                return Qt::PartiallyChecked;
            else return Qt::Unchecked;
        }
    }

    void update_parent_items_check_state(
        QTreeWidgetItem*        item,
        const int               column)
    {
        if (item)
        {
            const Qt::CheckState state =
                determinate_check_state(
                    has_child_items(item, column, Qt::Checked),
                    has_child_items(item, column, Qt::Unchecked),
                    has_child_items(item, column, Qt::PartiallyChecked));

            item->setCheckState(column, state);

            update_parent_items_check_state(item->parent(), column);
        }
    }

    void update_item_check_state(
        QTreeWidget*            widget,
        QTreeWidgetItem*        item,
        const int               column)
    {
        const bool previous_block = widget->blockSignals(true);

        const Qt::CheckState state = item->checkState(column);
        set_child_items_check_state(item, column, state);
        update_parent_items_check_state(item->parent(), column);

        widget->blockSignals(previous_block);
    }
}

void TestWindow::slot_on_test_item_check_state_changed(QTreeWidgetItem* item, int column) const
{
    update_item_check_state(m_ui->treewidget_tests, item, column);
    update_checked_tests_label();
}

namespace
{
    bool do_filter_items(QTreeWidgetItem* item, const QRegExp& regexp)
    {
        bool any_children_visible = false;

        for (int i = 0; i < item->childCount(); ++i)
        {
            if (do_filter_items(item->child(i), regexp))
            {
                any_children_visible = true;
                break;
            }
        }

        const bool visible = any_children_visible || regexp.indexIn(item->text(0)) >= 0;

        item->setHidden(!visible);

        return visible;
    }
}

void TestWindow::slot_filter_text_changed(const QString& pattern) const
{
    const QRegExp regexp(pattern);

    for (int i = 0; i < m_ui->treewidget_tests->topLevelItemCount(); ++i) 
    {
        do_filter_items(m_ui->treewidget_tests->topLevelItem(i), regexp);
    }
}

void TestWindow::slot_clear_filter_text() const
{
    m_ui->lineEdit_filter->clear();
}

void TestWindow::slot_check_all_tests() const
{
    set_all_items_check_state(m_ui->treewidget_tests, 0, Qt::Checked);
    update_checked_tests_label();
}

void TestWindow::slot_uncheck_all_tests() const
{
    set_all_items_check_state(m_ui->treewidget_tests, 0, Qt::Unchecked);
    update_checked_tests_label();
}

namespace
{
    void collect_checked_test_cases(
        const QTreeWidgetItem*  parent,
        TestSuite*              suite,
        TestSuiteRepository&    repository)
    {
        for (int i = 0; i < parent->childCount(); ++i)
        {
            const QTreeWidgetItem* item = parent->child(i);

            if (item->checkState(0) == Qt::Checked)
            {
                const QVariant& item_data = item->data(0, Qt::UserRole);
                ITestCaseFactory* factory = item_data.value<ITestCaseFactory*>();

                suite->register_case(factory);
            }
        }
    }

    void collect_checked_test_suites(
        const QTreeWidget*      widget,
        TestSuiteRepository&    repository)
    {
        for (int i = 0; i < widget->topLevelItemCount(); ++i)
        {
            const QTreeWidgetItem* item = widget->topLevelItem(i);
            const Qt::CheckState state = item->checkState(0);

            if (state == Qt::Checked || state == Qt::PartiallyChecked)
            {
                const QVariant& item_data = item->data(0, Qt::UserRole);
                TestSuite* original_suite = item_data.value<TestSuite*>();
                TestSuite* suite = new TestSuite(original_suite->get_name());

                repository.register_suite(suite);

                collect_checked_test_cases(item, suite, repository);
            }
        }
    }
}

void TestWindow::slot_run_tests()
{
    if (!m_test_runner_thread->isRunning())
    {
        enable_widgets(false);

        m_test_suite_repository.clear();

        collect_checked_test_suites(
            m_ui->treewidget_tests,
            m_test_suite_repository);

        m_test_runner_thread->start();
    }
}

void TestWindow::slot_on_tests_execution_complete(const int passed_count, const int failed_count) const
{
    if (failed_count == 0)
        m_result_widget->set_all_passed();

    enable_widgets(true);
}

void TestWindow::enable_widgets(const bool enabled) const
{
    m_ui->treewidget_tests->setEnabled(enabled);
    m_ui->pushbutton_check_all->setEnabled(enabled);
    m_ui->pushbutton_uncheck_all->setEnabled(enabled);
    m_ui->pushbutton_run->setEnabled(enabled);
    m_ui->pushbutton_clear->setEnabled(enabled);
    m_ui->pushButton_clear_filter->setEnabled(enabled);
    m_ui->lineEdit_filter->setEnabled(enabled);
}

void TestWindow::slot_clear_output_treeview() const
{
    m_ui->treewidget_output->clear();
    m_ui->label_tests_results->clear();
}

namespace
{
    void filter_output_item(TestOutputItem* item, const bool show_passed)
    {
        item->setHidden(item->get_passed() && !show_passed);

        for (int i = 0; i < item->childCount(); ++i)
        {
            TestOutputItem* child = reinterpret_cast<TestOutputItem*>(item->child(i));
            filter_output_item(child, show_passed);
        }
    }

    void filter_output_items(QTreeWidget* widget, const bool show_passed)
    {
        for (int i = 0; i < widget->topLevelItemCount(); ++i)
        {
            TestOutputItem* item = reinterpret_cast<TestOutputItem*>(widget->topLevelItem(i));
            filter_output_item(item, show_passed);
        }
    }
}

void TestWindow::slot_filter_output_treeview() const
{
    const bool show_passed =
        m_ui->checkbox_show_all->checkState() == Qt::Checked;

    m_output_widget->set_show_passed(show_passed);

    filter_output_items(m_ui->treewidget_output, show_passed);
}

}   // namespace studio
}   // namespace appleseed
