
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
#include "treewidget.h"

// Qt headers.
#include <QString>
#include <QTreeWidget>
#include <QTreeWidgetItem>

// Standard headers.
#include <cassert>

namespace appleseed {
namespace qtcommon {

int find_sorted_position(QTreeWidget* parent, const QString& item_text)
{
    assert(parent);

    int index = 0;
    int end = parent->topLevelItemCount();

    if (end > 0)
    {
        while (end - index > 0)
        {
            const int middle = (index + end) / 2;

            if (QString::localeAwareCompare(parent->topLevelItem(middle)->text(0), item_text) > 0)
                end = middle;
            else index = middle + 1;
        }
    }

    return index;
}

int find_sorted_position(QTreeWidgetItem* parent, const QString& item_text)
{
    assert(parent);

    int index = 0;
    int end = parent->childCount();

    if (end > 0)
    {
        while (end - index > 0)
        {
            const int middle = (index + end) / 2;

            if (QString::localeAwareCompare(parent->child(middle)->text(0), item_text) > 0)
                end = middle;
            else index = middle + 1;
        }
    }

    return index;
}

void move_to_sorted_position(QTreeWidgetItem* item)
{
    assert(item);

    const bool was_selected = item->isSelected();

    if (item->parent())
    {
        QTreeWidgetItem* parent = item->parent();
        parent->removeChild(item);
        parent->insertChild(find_sorted_position(parent, item->text(0)), item);
    }
    else if (item->treeWidget())
    {
        QTreeWidget* parent = item->treeWidget();
        parent->takeTopLevelItem(parent->indexOfTopLevelItem(item));
        parent->insertTopLevelItem(find_sorted_position(parent, item->text(0)), item);
    }

    item->setSelected(was_selected);
}

}   // namespace qtcommon
}   // namespace appleseed
