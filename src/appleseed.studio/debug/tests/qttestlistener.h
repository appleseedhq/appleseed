
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

#ifndef APPLESEED_STUDIO_DEBUG_TESTS_QTTESTLISTENER_H
#define APPLESEED_STUDIO_DEBUG_TESTS_QTTESTLISTENER_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/test.h"

// Qt headers.
#include <QObject>

// Forward declarations.
namespace appleseed { namespace studio { class TestOutputItem; } }
namespace appleseed { namespace studio { class TestOutputWidgetDecorator; } }
namespace appleseed { namespace studio { class TestResultWidgetDecorator; } }

namespace appleseed {
namespace studio {

//
// A test listener that outputs results to a QTreeView.
//

class QtTestListener
  : public QObject
  , public foundation::TestListenerBase
{
    Q_OBJECT

  public:
    // Constructor.
    QtTestListener(
        TestOutputWidgetDecorator*          output_widget,
        TestResultWidgetDecorator*          result_widget);

    // Delete this instance.
    void release() override;

    // Called before each test suite is run.
    void begin_suite(
        const foundation::TestSuite&        test_suite) override;

    // Called after each test suite is run.
    void end_suite(
        const foundation::TestSuite&        test_suite,
        const foundation::TestResult&       test_suite_result,
        const foundation::TestResult&       cumulated_result) override;

    // Called before each test case is run.
    void begin_case(
        const foundation::TestSuite&        test_suite,
        const char*                         test_case_name) override;

    // Called after each test case is run.
    void end_case(
        const foundation::TestSuite&        test_suite,
        const char*                         test_case_name,
        const foundation::TestResult&       test_suite_result,
        const foundation::TestResult&       test_case_result,
        const foundation::TestResult&       cumulated_result) override;

    // Write a message.
    void write(
        const foundation::TestSuite&        test_suite,
        const char*                         test_case_name,
        const char*                         file,
        const size_t                        line,
        const foundation::TestMessage::Type message_type,
        const char*                         message) override;

  signals:
    void signal_add_top_level_item(TestOutputItem* item);
    void signal_update(const int passed_count, const int failed_count);

  private:
    typedef foundation::Stopwatch<foundation::DefaultWallclockTimer> Stopwatch;

    TestOutputItem*     m_suite_item;
    TestOutputItem*     m_case_item;

    Stopwatch           m_suite_stopwatch;
    Stopwatch           m_case_stopwatch;
};

}       // namespace studio
}       // namespace appleseed

#endif  // !APPLESEED_STUDIO_DEBUG_TESTS_QTTESTLISTENER_H
