
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_STUDIO_DEBUG_TESTS_TESTWINDOW_H
#define APPLESEED_STUDIO_DEBUG_TESTS_TESTWINDOW_H

// appleseed.studio headers.
#include "debug/tests/autodeletetestsuiterepository.h"
#include "debug/tests/testoutputwidgetdecorator.h"
#include "debug/tests/testresultwidgetdecorator.h"
#include "debug/tests/testrunnerthread.h"

// Qt headers.
#include <QObject>
#include <QWidget>

// Standard headers.
#include <memory>

// Forward declarations.
namespace Ui    { class TestWindow; }
class QTreeWidgetItem;

namespace appleseed {
namespace studio {

//
// Test execution window.
//

class TestWindow
  : public QWidget
{
    Q_OBJECT

  public:
    // Constructor.
    explicit TestWindow(QWidget* parent = 0);

    // Destructor.
    ~TestWindow();

  private:
    // Not wrapped in std::auto_ptr<> to avoid pulling in the UI definition code.
    Ui::TestWindow*                             m_ui;

    std::auto_ptr<TestOutputWidgetDecorator>    m_output_widget;
    std::auto_ptr<TestResultWidgetDecorator>    m_result_widget;

    AutoDeleteTestSuiteRepository               m_test_suite_repository;
    std::auto_ptr<TestRunnerThread>             m_test_runner_thread;

    void create_test_runner_thread();

    void build_connections();

    void configure_tests_treeview();
    void populate_tests_treeview();
    void update_checked_tests_label();

    void configure_output_treeview();

    void enable_widgets(const bool enabled);

  private slots:
    void slot_on_test_item_check_state_changed(QTreeWidgetItem* item, int column);

    void slot_check_all_tests();
    void slot_uncheck_all_tests();

    void slot_run_tests();
    void slot_on_tests_execution_complete(const int passed_count, const int failed_count);

    void slot_clear_output_treeview();
    void slot_filter_output_treeview();
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_DEBUG_TESTS_TESTWINDOW_H
