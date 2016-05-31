
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/utility/log.h"

using namespace appleseed::shared;
using namespace foundation;
using namespace std;

namespace appleseed {
namespace updateprojectfile {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("updateprojectfile")
{
    add_default_options();

    m_filename.set_exact_value_count(1);
    parser().set_default_option_handler(&m_filename);

    m_to_revision.add_name("--to-revision");
    m_to_revision.add_name("-r");
    m_to_revision.set_description("update the project to this revision (by default, update to the latest revision)");
    m_to_revision.set_syntax("revision");
    m_to_revision.set_exact_value_count(1);
    parser().add_option_handler(&m_to_revision);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [options] project.appleseed", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);
}

}   // namespace updateprojectfile
}   // namespace appleseed
