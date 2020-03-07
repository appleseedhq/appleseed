
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
#include "testoutputitem.h"

// appleseed.studio headers.
#include "debug/tests/testoutputwidgetdecorator.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"

// Qt headers.
#include <QColor>
#include <QString>

// Standard headers.
#include <string>

using namespace foundation;

namespace appleseed {
namespace studio {

//
// TestOutputItem class implementation.
//

namespace
{
    double get_time_helper(const QTreeWidgetItem& item)
    {
        return item.data(TestOutputItem::TimeColumnIndex, Qt::UserRole).value<double>();
    }
}

bool TestOutputItem::operator<(const QTreeWidgetItem& rhs) const
{
    const int column = treeWidget()->sortColumn();

    if (column == TimeColumnIndex)
    {
        return get_time() < get_time_helper(rhs);
    }
    else
    {
        return text(column) < rhs.text(column);
    }
}

void TestOutputItem::set_passed(const bool passed)
{
    setData(StatusColumnIndex, Qt::UserRole, passed);
}

bool TestOutputItem::get_passed() const
{
    return data(StatusColumnIndex, Qt::UserRole).value<bool>();
}

void TestOutputItem::set_time(const double time)
{
    setData(TimeColumnIndex, Qt::UserRole, time);
}

double TestOutputItem::get_time() const
{
    return get_time_helper(*this);
}

namespace
{
    QString time_to_qstring(const double time)
    {
        const std::string s = pretty_time(time);
        return QString::fromStdString(s);
    }

    void set_all_columns_text_color(QTreeWidgetItem* item, const QColor& color)
    {
        for (int i = 0; i < item->columnCount(); ++i)
            item->setForeground(i, color);
    }
}

void TestOutputItem::set(
    const char*     text,
    const bool      passed,
    const double    time)
{
    setText(TextColumnIndex, text);

    setText(StatusColumnIndex, passed ? "Passed" : "Failed");
    set_passed(passed);

    setText(TimeColumnIndex, time_to_qstring(time));
    set_time(time);

    if (passed)
    {
        setForeground(StatusColumnIndex, QColor("green"));
    }
    else
    {
        set_all_columns_text_color(this, QColor("red"));
    }
}

}   // namespace studio
}   // namespace appleseed
