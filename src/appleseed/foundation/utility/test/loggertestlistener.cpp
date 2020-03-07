
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
#include "loggertestlistener.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/test.h"
#include "foundation/utility/test/testlistenerbase.h"
#include "foundation/utility/test/testsuite.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

namespace foundation
{

namespace
{
    //
    // LoggerTestListener class implementation.
    //

    class LoggerTestListener
      : public TestListenerBase
    {
      public:
        LoggerTestListener(Logger& logger, const bool verbose)
          : m_logger(logger)
          , m_verbose(verbose)
          , m_suite_name_printed(false)
          , m_case_name_printed(false)
        {
        }

        void release() override
        {
            delete this;
        }

        void begin_suite(
            const TestSuite&        test_suite) override
        {
            m_suite_name_printed = false;
        }

        void begin_case(
            const TestSuite&        test_suite,
            const char*             test_case_name) override
        {
            m_case_name_printed = false;
        }

        void end_case(
            const TestSuite&        test_suite,
            const char*             test_case_name,
            const TestResult&       test_suite_result,
            const TestResult&       test_case_result,
            const TestResult&       cumulated_result) override
        {
            if (m_verbose)
            {
                if (!m_case_name_printed)
                {
                    if (!m_suite_name_printed)
                    {
                        LOG_INFO(m_logger, "%s:", test_suite.get_name());
                        m_suite_name_printed = true;
                    }

                    LOG_INFO(m_logger, "  [passed] %s", test_case_name);
                    m_case_name_printed = true;
                }
            }
        }

        void write(
            const TestSuite&        test_suite,
            const char*             test_case_name,
            const char*             file,
            const size_t            line,
            const TestMessage::Type message_type,
            const char*             message) override
        {
            if (!m_case_name_printed)
            {
                if (!m_suite_name_printed)
                {
                    LOG(
                        m_logger,
                        m_verbose ? LogMessage::Info : LogMessage::Error,
                        "%s:",
                        test_suite.get_name());
                    m_suite_name_printed = true;
                }

                LOG_ERROR(m_logger, "  [failed] %s", test_case_name);
                m_case_name_printed = true;
            }

            // Print the message type and the location in the source code of the failure.
            if (message_type == TestMessage::AssertionFailure)
            {
                LOG_ERROR(
                    m_logger,
                    "    %s in %s, line " FMT_SIZE_T ":",
                    TestMessage::name(message_type),
                    file,
                    line);
            }
            else
            {
                LOG_ERROR(m_logger, "    %s:", TestMessage::name(message_type));
            }

            // Split the message into multiple components, one for each line.
            std::vector<std::string> tokens;
            split(message, "\n", tokens);

            // Print the message.
            for (const_each<std::vector<std::string>> i = tokens; i; ++i)
                LOG_ERROR(m_logger, "      %s", i->c_str());
        }

      private:
        Logger&     m_logger;
        const bool  m_verbose;
        bool        m_suite_name_printed;
        bool        m_case_name_printed;
    };
}

ITestListener* create_logger_test_listener(Logger& logger, const bool verbose)
{
    return new LoggerTestListener(logger, verbose);
}

}   // namespace foundation
