
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
#include "testrunnerthread.h"

// appleseed.studio headers.
#include "debug/tests/qttestlistener.h"

// appleseed.common headers.
#include "application/application.h"

// appleseed.renderer headers.
#include "renderer/api/log.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/utility/test.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"

using namespace appleseed::common;
using namespace foundation;
using namespace renderer;
namespace bf = boost::filesystem;

namespace appleseed {
namespace studio {

//
// TestRunnerThread class implementation.
//

TestRunnerThread::TestRunnerThread(
    const TestSuiteRepository&  repository,
    TestOutputWidgetDecorator*  output_widget,
    TestResultWidgetDecorator*  result_widget)
  : m_repository(repository)
  , m_output_widget(output_widget)
  , m_result_widget(result_widget)
{
}

void TestRunnerThread::run()
{
    set_current_thread_name("tests");

    QtTestListener listener(m_output_widget, m_result_widget);

    global_logger().set_enabled(false);

    // Change current directory to the tests' root directory.
    const bf::path old_current_path =
        Application::change_current_directory_to_tests_root_path();

    // Create unit tests output directories.
    // todo: how should errors be handled here?
    Application::create_unit_tests_output_directories();

    TestResult result;

    // Run test suites.
    m_repository.run(listener, result);

    // Restore the current directory.
    bf::current_path(old_current_path);

    global_logger().set_enabled(true);

    const int failed_count = static_cast<int>(result.get_case_failure_count());
    const int passed_count = static_cast<int>(result.get_case_execution_count()) - failed_count;

    emit signal_finished(passed_count, failed_count);
}

}   // namespace studio
}   // namespace appleseed
