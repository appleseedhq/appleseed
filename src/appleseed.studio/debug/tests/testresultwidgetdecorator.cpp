
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
#include "testresultwidgetdecorator.h"

// Qt headers.
#include <QLabel>
#include <QString>

namespace appleseed {
namespace studio {

//
// TestResultWidgetDecorator class implementation.
//

// Constructor.
TestResultWidgetDecorator::TestResultWidgetDecorator(QLabel* widget)
  : m_widget(widget)
{
}

void TestResultWidgetDecorator::set_all_passed()
{
    m_widget->setText("<font color=\"green\"><b>All tests PASSED</b></font>");
}

void TestResultWidgetDecorator::slot_update(const int passed_count, const int failed_count)
{
    m_widget->setText(
        QString("<font color=\"%1\"><b>%2</b> test%3 passed, <b>%4</b> test%5 failed</font>")
            .arg(failed_count > 0 ? "red" : "green")
            .arg(passed_count)
            .arg(passed_count > 1 ? "s" : "")
            .arg(failed_count)
            .arg(failed_count > 1 ? "s" : ""));
}

}   // namespace studio
}   // namespace appleseed
