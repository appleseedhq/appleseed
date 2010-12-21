
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "itembase.h"

// Qt headers.
#include <QString>
#include <QStringList>

using namespace foundation;

namespace appleseed {
namespace studio {

ItemBase::ItemBase(const UniqueID class_uid)
  : m_class_uid(class_uid)
{
}

ItemBase::ItemBase(const UniqueID class_uid, const QString& title)
  : m_class_uid(class_uid)
{
    set_title(title);
}

UniqueID ItemBase::get_class_uid() const
{
    return m_class_uid;
}

void ItemBase::set_title(const QString& title)
{
    setText(0, title);
}

QMenu* ItemBase::get_single_item_context_menu() const
{
    return 0;
}

QMenu* ItemBase::get_multiple_items_context_menu(const QList<ItemBase*>& items) const
{
    return 0;
}

void ItemBase::activate()
{
}

}   // namespace studio
}   // namespace appleseed
