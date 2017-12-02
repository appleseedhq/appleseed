
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
#include "qtlogtarget.h"

// appleseed.studio headers.
#include "mainwindow/logwidget.h"
#include "utility/interop.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <vector>

using namespace foundation;
using namespace std;

namespace appleseed {
namespace studio {

//
// QtLogTarget class implementation.
//

QtLogTarget::QtLogTarget(LogWidget* log_widget)
{
    connect(
        this, SIGNAL(signal_append_item(const QColor&, const QString&)),
        log_widget, SLOT(slot_append_item(const QColor&, const QString&)));
}

void QtLogTarget::release()
{
    delete this;
}

namespace
{
    QColor get_text_color_for_category(const LogMessage::Category category)
    {
        switch (category)
        {
          default:
          case LogMessage::Info:
            return QColor(170, 170, 170);

          case LogMessage::Debug:
            return QColor(107, 239, 55);

          case LogMessage::Warning:
            return QColor(242, 59, 205);

          case LogMessage::Error:
          case LogMessage::Fatal:
            return QColor(239, 55, 55);
        }
    }
}

void QtLogTarget::write(
    const LogMessage::Category  category,
    const char*                 file,
    const size_t                line_number,
    const char*                 header,
    const char*                 message)
{
    const QColor color = get_text_color_for_category(category);
    const QString header_string(header);

    vector<QString> lines;
    split(message, "\n", lines);

    for (const_each<vector<QString>> i = lines; i; ++i)
    {
        const QString line = header_string + *i + "\n";
        emit signal_append_item(color, line);
    }
}

}   // namespace studio
}   // namespace appleseed
