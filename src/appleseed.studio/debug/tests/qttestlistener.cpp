
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "qttestlistener.h"

// appleseed.studio headers.
#include "debug/tests/testoutputitem.h"
#include "debug/tests/testoutputwidgetdecorator.h"
#include "debug/tests/testresultwidgetdecorator.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cstddef>
#include <sstream>
#include <string>
#include <vector>

using namespace foundation;

namespace appleseed {
namespace studio {

//
// QtTestListener class implementation.
//

QtTestListener::QtTestListener(
    TestOutputWidgetDecorator*  output_widget,
    TestResultWidgetDecorator*  result_widget)
  : m_suite_item(nullptr)
  , m_case_item(nullptr)
{
    connect(
        this, SIGNAL(signal_add_top_level_item(TestOutputItem*)),
        output_widget, SLOT(slot_add_top_level_item(TestOutputItem*)));

    connect(
        this, SIGNAL(signal_update(const int, const int)),
        result_widget, SLOT(slot_update(const int, const int)));
}

void QtTestListener::release()
{
    delete this;
}

void QtTestListener::begin_suite(
    const TestSuite&        test_suite)
{
    m_suite_item = new TestOutputItem();

    m_suite_stopwatch.start();
}

void QtTestListener::end_suite(
    const TestSuite&        test_suite,
    const TestResult&       test_suite_result,
    const TestResult&       cumulated_result)
{
    m_suite_stopwatch.measure();

    m_suite_item->set(
        test_suite.get_name(),
        test_suite_result.get_suite_failure_count() == 0,
        m_suite_stopwatch.get_seconds());

    emit signal_add_top_level_item(m_suite_item);

    m_suite_item = nullptr;
}

void QtTestListener::begin_case(
    const TestSuite&        test_suite,
    const char*             test_case_name)
{
    m_case_item = new TestOutputItem();

    m_case_stopwatch.start();
}

void QtTestListener::end_case(
    const TestSuite&        test_suite,
    const char*             test_case_name,
    const TestResult&       test_suite_result,
    const TestResult&       test_case_result,
    const TestResult&       cumulated_result)
{
    m_case_stopwatch.measure();

    m_case_item->set(
        test_case_name,
        test_case_result.get_case_failure_count() == 0,
        m_case_stopwatch.get_seconds());

    m_suite_item->addChild(m_case_item);

    m_case_item = nullptr;

    const int failed_count = static_cast<int>(cumulated_result.get_case_failure_count());
    const int passed_count = static_cast<int>(cumulated_result.get_case_execution_count()) - failed_count;

    emit signal_update(passed_count, failed_count);
}

namespace
{
    std::string build_message_string(
        const TestSuite&        test_suite,
        const char*             test_case_name,
        const char*             file,
        const size_t            line,
        const TestMessage::Type message_type,
        const char*             message)
    {
        std::stringstream sstr;

        // Print the message header.
        sstr << TestMessage::name(message_type) << ":" << std::endl;

        // Split the message into multiple components, one for each line.
        std::vector<std::string> tokens;
        split(message, "\n", tokens);

        // Print the message.
        for (const_each<std::vector<std::string>> i = tokens; i; ++i)
            sstr << "  " << *i << std::endl;

        // Print the message footer.
        sstr << file << ", line " << line;

        return sstr.str();
    }
}

void QtTestListener::write(
    const TestSuite&        test_suite,
    const char*             test_case_name,
    const char*             file,
    const size_t            line,
    const TestMessage::Type message_type,
    const char*             message)
{
    const std::string message_string =
        build_message_string(
            test_suite,
            test_case_name,
            file,
            line,
            message_type,
            message);

    TestOutputItem* item = new TestOutputItem();

    item->set(
        message_string.c_str(),
        false,
        0.0);

    m_case_item->addChild(item);
}

}   // namespace studio
}   // namespace appleseed
