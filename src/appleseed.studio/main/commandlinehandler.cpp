
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

// Interface header.
#include "commandlinehandler.h"

// appleseed.shared headers.
#include "application/superlogger.h"

// appleseed.foundation headers.
#include "foundation/utility/log.h"

// Qt headers.
#include <QMessageBox>
#include <QString>

using namespace appleseed::shared;
using namespace foundation;

namespace appleseed {
namespace studio {

CommandLineHandler::CommandLineHandler()
  : CommandLineHandlerBase("appleseed.studio")
{
    add_help_option();

    m_filename.set_min_value_count(0);
    m_filename.set_max_value_count(1);
    parser().set_default_option_handler(&m_filename);

    m_render.add_name("--render");
    m_render.set_description("start rendering using the specified configuration");
    m_render.set_exact_value_count(1);
    parser().add_option_handler(&m_render);
}

void CommandLineHandler::print_program_usage(
    const char*     program_name,
    SuperLogger&    logger) const
{
    SaveLogFormatterConfig save_config(logger);
    logger.set_format(LogMessage::Info, "{message}");

    LOG_INFO(logger, "usage: %s [project.appleseed]", program_name);
    LOG_INFO(logger, "options:");

    parser().print_usage(logger);

#ifdef _WIN32

    const StringLogTarget& target =
        static_cast<const StringLogTarget&>(logger.get_log_target());
    const QString str = QString::fromStdString(target.get_string());

    QMessageBox msgbox;
    msgbox.setWindowTitle("appleseed.studio Program Usage");
    msgbox.setIcon(QMessageBox::Information);
    msgbox.setText("<pre>" + str + "</pre>");
    msgbox.setStandardButtons(QMessageBox::Ok);
    msgbox.setDefaultButton(QMessageBox::Ok);
    msgbox.exec();

#endif
}

}   // namespace studio
}   // namespace appleseed
